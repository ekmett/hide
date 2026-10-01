{-# LANGUAGE CPP, ForeignFunctionInterface, ScopedTypeVariables #-}
module THC.Edit.RemoteEndpoint
  (sessionEndpoint, connectEndpoint, connectEndpointWithShutdown, socketToEndpoint, endpointExists, withEndpointListener, randomIdentity, spawnDetached) where
import Control.Exception
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Char (isHexDigit, isDigit, isLower)
import qualified Network.Socket as N
import Numeric (showHex)
import System.Directory (removeFile)
import System.FilePath ((</>))
import System.IO
import System.Process (ProcessHandle)
#ifdef mingw32_HOST_OS
import Control.Concurrent.Async (withAsync, wait)
import Data.Bits ((.|.), xor)
import qualified Data.ByteString.Char8 as B8
import Data.Word (Word8, Word32)
import Foreign (Ptr, alloca, allocaBytes, castPtr, peek, nullPtr)
import Foreign.C.String (CWString, withCWString)
import System.Directory (getHomeDirectory)
import System.Timeout (timeout)
import System.Process.Internals (mkProcessHandle, translate)
import Text.Read (readMaybe)
#else
import System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import System.Process (createProcess, proc, CreateProcess(..), StdStream(UseHandle))
import qualified System.Posix.Directory as P
import System.Posix.Files
import System.Posix.User (getEffectiveUserID)
#endif

failure :: String -> IO a
failure = ioError . userError
validIdentity :: String -> Bool
validIdentity s = length s==48 && all (\c -> isHexDigit c && (isDigit c || isLower c)) s
randomIdentity :: IO String
randomIdentity = do
  bytes <- randomBytes 24
  pure (concatMap (\n -> let s=showHex n "" in replicate (2-length s) '0'++s) (BS.unpack bytes))

sessionEndpoint :: String -> IO FilePath
sessionEndpoint session = do
  unless (validIdentity session) (failure "Invalid remote session identifier")
#ifdef mingw32_HOST_OS
  directory <- (</> ".thc-edit-remote") <$> getHomeDirectory
  withCWString directory $ \path -> c_privateDirectory path >>= checkWindows "Secure remote session directory"
#else
  uid <- getEffectiveUserID
  let directory="/tmp/thc-edit-"++show uid
  P.createDirectory directory ownerModes `catch` \e -> unless (isAlreadyExistsError e) (throwIO e)
  st <- getSymbolicLinkStatus directory
  unless (isDirectory st && fileOwner st==uid) (failure "Unsafe remote session directory")
  setFileMode directory ownerModes
#endif
  pure (directory </> session)

-- The shutdown action must run while its Handle is still open. It wakes the
-- peer reader before Windows waits for a blocked local reader to be cancelled.
connectEndpoint :: FilePath -> IO Handle
connectEndpoint path = fst <$> connectEndpointWithShutdown path

-- The Socket transfers ownership to the Handle. Keep the wakeup only until
-- that Handle closes; calling it afterwards could target a reused descriptor.
socketToEndpoint :: N.Socket -> IO (Handle, IO ())
socketToEndpoint sock = mask_ $ do
#ifdef mingw32_HOST_OS
  descriptorNumber <- N.withFdSocket sock pure
  let shutdown=c_shutdown (fromIntegral descriptorNumber)
#else
  let shutdown=pure ()
#endif
  h <- N.socketToHandle sock ReadWriteMode
  flip onException (hClose h) $ do
    hSetBinaryMode h True
    hSetBuffering h NoBuffering
    pure (h,shutdown)

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "thc_remote_shutdown" c_shutdown :: Word32 -> IO ()
foreign import ccall unsafe "thc_remote_spawn" c_spawn :: CWString -> CWString -> CWString -> Ptr (Ptr ()) -> IO Word32
foreign import ccall unsafe "thc_remote_private_directory" c_privateDirectory :: CWString -> IO Word32
foreign import ccall unsafe "thc_remote_descriptor_write" c_writeDescriptor :: CWString -> Ptr Word8 -> Word32 -> IO Word32
foreign import ccall unsafe "thc_remote_descriptor_read" c_readDescriptor :: CWString -> Ptr Word8 -> Word32 -> Ptr Word32 -> IO Word32
foreign import ccall unsafe "thc_remote_random" c_random :: Ptr Word8 -> Word32 -> IO Word32
foreign import ccall unsafe "thc_remote_hmac" c_hmac :: Ptr Word8 -> Ptr Word8 -> Word32 -> Ptr Word8 -> IO Word32

spawnDetached :: FilePath -> [String] -> FilePath -> IO ProcessHandle
spawnDetached executable args logfile = mask_ $ withCWString executable $ \application ->
  withCWString (unwords (map translate (executable:args))) $ \command ->
  withCWString logfile $ \logPath -> alloca $ \result -> do
    c_spawn application command logPath result >>= checkWindows "Start persistent remote process outside SSH job"
    childHandle <- peek result
    mkProcessHandle childHandle False nullPtr

checkWindows :: String -> Word32 -> IO ()
checkWindows context code = unless (code==0) (failure (context++": Windows error "++show code))
randomBytes :: Int -> IO BS.ByteString
randomBytes n = allocaBytes n $ \p -> do
  c_random p (fromIntegral n) >>= checkWindows "Generate remote session randomness"
  BS.packCStringLen (castPtr p,n)

readDescriptor :: FilePath -> IO (Maybe BS.ByteString)
readDescriptor path = withCWString path $ \name -> allocaBytes 256 $ \bytes -> alloca $ \lengthPtr -> do
  code <- c_readDescriptor name bytes 256 lengthPtr
  if code `elem` [2,3] then pure Nothing else do
    checkWindows "Read private remote endpoint" code
    count <- fromIntegral <$> peek lengthPtr
    Just <$> BS.packCStringLen (castPtr bytes,count)
writeDescriptor :: FilePath -> BS.ByteString -> IO ()
writeDescriptor path bytes = withCWString path $ \name -> BS.useAsCStringLen bytes $ \(p,n) ->
  c_writeDescriptor name (castPtr p) (fromIntegral n) >>= checkWindows "Create private remote endpoint"
endpointExists :: FilePath -> IO Bool
endpointExists path = maybe False (const True) <$> readDescriptor path

-- Both peers prove knowledge of the private descriptor token. Neither sends it
-- across TCP, including when a crashed daemon's old port has been reused.
mac :: String -> BS.ByteString -> BS.ByteString -> BS.ByteString -> IO BS.ByteString
mac role token first second = BS.useAsCString token $ \key ->
  BS.useAsCStringLen (B8.pack role<>first<>second) $ \(message,n) -> allocaBytes 32 $ \output -> do
    c_hmac (castPtr key) (castPtr message) (fromIntegral n) output >>= checkWindows "Authenticate remote endpoint"
    BS.packCStringLen (castPtr output,32)
sameBytes :: BS.ByteString -> BS.ByteString -> Bool
sameBytes a b = BS.length a==BS.length b && foldr (.|.) 0 (BS.zipWith xor a b)==0
readExact :: Handle -> Int -> IO BS.ByteString
readExact h n = do
  bytes <- BS.hGet h n
  unless (BS.length bytes==n) (failure "Remote endpoint authentication ended early")
  pure bytes
boundedAuthentication :: IO () -> IO ()
boundedAuthentication action = timeout 5000000 action >>= maybe (failure "Remote endpoint authentication timed out") pure

connectEndpointWithShutdown :: FilePath -> IO (Handle, IO ())
connectEndpointWithShutdown path = do
  descriptor <- readDescriptor path >>= maybe (failure "Remote endpoint is not ready") pure
  (port,token) <- case B8.words descriptor of
    [p,key] | Just number <- readMaybe (B8.unpack p), number>0, number<=65535, validIdentity (B8.unpack key) -> pure (number::Int,key)
    _ -> failure "Invalid private remote endpoint descriptor"
  bracketOnError (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $ \sock -> do
    N.connect sock (N.SockAddrInet (fromIntegral port) (N.tupleToHostAddress (127,0,0,1)))
    (h,shutdown) <- socketToEndpoint sock
    flip onException (hClose h) $ do
      let authenticate=do
            challenge <- randomBytes 24
            BS.hPut h challenge
            reply <- readExact h 56
            let (nonce,proof)=BS.splitAt 24 reply
            expected <- mac "server" token challenge nonce
            unless (sameBytes proof expected) (failure "Remote endpoint server authentication failed")
            mac "client" token challenge nonce >>= BS.hPut h
      -- The callback is not published until authentication completes. Wake
      -- this private reader on timeout/cancellation before joining it too.
      withAsync authenticate $ \worker -> boundedAuthentication (wait worker) `onException` shutdown
      pure (h,shutdown)

withEndpointListener :: FilePath -> (N.Socket -> (Handle -> IO ()) -> IO a) -> IO a
withEndpointListener path action = bracket (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $ \sock -> do
  N.bind sock (N.SockAddrInet 0 (N.tupleToHostAddress (127,0,0,1)))
  N.listen sock 8
  address <- N.getSocketName sock
  port <- case address of N.SockAddrInet p _ -> pure p; _ -> failure "Unexpected remote listener address"
  token <- B8.pack <$> randomIdentity
  writeDescriptor path (B8.pack (show (fromIntegral port::Int))<>B8.pack " "<>token<>B8.pack "\n")
  let authenticate h = boundedAuthentication $ do
        challenge <- readExact h 24
        nonce <- randomBytes 24
        proof <- mac "server" token challenge nonce
        BS.hPut h (nonce<>proof)
        response <- readExact h 32
        expected <- mac "client" token challenge nonce
        unless (sameBytes response expected) (failure "Remote endpoint client authentication failed")
  action sock authenticate `finally` removeFile path
#else
spawnDetached :: FilePath -> [String] -> FilePath -> IO ProcessHandle
spawnDetached executable args logfile = withBinaryFile "/dev/null" ReadWriteMode $ \nullHandle ->
  withBinaryFile logfile WriteMode $ \logHandle -> do
    (_,_,_,child) <- createProcess (proc executable args)
      {std_in=UseHandle nullHandle,std_out=UseHandle nullHandle,std_err=UseHandle logHandle,close_fds=True,new_session=True}
    pure child

randomBytes :: Int -> IO BS.ByteString
randomBytes n = withBinaryFile "/dev/urandom" ReadMode $ \h -> do
  bytes <- BS.hGet h n
  unless (BS.length bytes==n) (failure "Cannot obtain session randomness")
  pure bytes
endpointExists :: FilePath -> IO Bool
endpointExists path = do
  result <- try (getSymbolicLinkStatus path)
  case result of Right _ -> pure True; Left e | isDoesNotExistError e -> pure False; Left e -> throwIO e
connectEndpointWithShutdown :: FilePath -> IO (Handle, IO ())
connectEndpointWithShutdown path = bracketOnError (N.socket N.AF_UNIX N.Stream N.defaultProtocol) N.close $ \sock -> do
  N.connect sock (N.SockAddrUnix path)
  socketToEndpoint sock
withEndpointListener :: FilePath -> (N.Socket -> (Handle -> IO ()) -> IO a) -> IO a
withEndpointListener path action = bracket (N.socket N.AF_UNIX N.Stream N.defaultProtocol) N.close $ \sock -> do
  N.bind sock (N.SockAddrUnix path)
  flip finally (removeFile path) $ do
    setFileMode path (ownerReadMode `unionFileModes` ownerWriteMode)
    N.listen sock 8
    action sock (const (pure ()))
#endif
