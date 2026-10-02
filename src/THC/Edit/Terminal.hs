{-# LANGUAGE CPP, ForeignFunctionInterface, OverloadedStrings #-}
module THC.Edit.Terminal
  ( Terminal, TerminalConfig(..), TerminalCell(..), TerminalSnapshot(..)
  , terminalAvailable, startTerminal, withTerminal, writeTerminal, resizeTerminal
  , setTerminalAppearance, pollTerminal, killTerminal, closeTerminal
  ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word32)
#ifdef WITH_TERMINAL
import Control.Concurrent.MVar
import Control.Exception (IOException, try, mask_, onException)
import Control.Monad (forM)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Foreign hiding (Word32)
import Foreign.C
import System.Directory (doesFileExist, getPermissions, executable, makeAbsolute)
import System.Environment (getEnvironment)
import System.FilePath ((</>), isAbsolute, isPathSeparator, splitSearchPath)
#ifdef mingw32_HOST_OS
import Data.Char (toUpper)
import Data.List (sortOn)
import System.FilePath (takeExtension)
import System.Process.Internals (translate)
#endif
#endif

data TerminalConfig = TerminalConfig
  { terminalCommand :: FilePath
  , terminalArguments :: [String]
  , terminalEnvironment :: [(String, String)] -- Overrides inherited variables.
  , terminalDirectory :: FilePath
  , terminalColumns :: Int
  , terminalRows :: Int
  } deriving (Eq, Show)

data TerminalCell = TerminalCell
  { cellText :: Text
  , cellForeground :: Word32 -- 0xRRGGBB
  , cellBackground :: Word32
  , cellAttributes :: Word32 -- bold=1, italic=2, underline=4, strike=8, faint=16
  , cellWidth :: Int -- 0 for the continuation of a wide glyph.
  } deriving (Eq, Show)

data TerminalSnapshot = TerminalSnapshot
  { snapshotColumns :: Int
  , snapshotRows :: Int
  , snapshotCells :: [TerminalCell] -- Row-major, including wide continuations.
  , snapshotCursor :: Maybe (Int, Int) -- Column, row, zero-based.
  , snapshotOutput :: ByteString -- Raw output since the last poll, bounded to 256 KiB.
  , snapshotExitCode :: Maybe Int -- Signals are represented as 128 + signal number.
  } deriving (Eq, Show)

withTerminal :: TerminalConfig -> (Terminal -> IO a) -> IO (Either Text a)
withTerminal config action = bracket (startTerminal config) release use
  where
    release = either (const (pure ())) closeTerminal
    use = either (pure . Left) (fmap Right . action)

#ifdef WITH_TERMINAL
data NativeTerminal
data Terminal = Terminal (MVar (Maybe (Ptr NativeTerminal)))

foreign import ccall unsafe "thc_terminal_appearance" c_appearance :: Ptr NativeTerminal -> CInt -> IO CInt
foreign import ccall unsafe "thc_terminal_new" c_new :: CInt -> CInt -> IO (Ptr NativeTerminal)
#ifdef mingw32_HOST_OS
foreign import ccall safe "thc_terminal_spawn_windows" c_spawn :: Ptr NativeTerminal -> CWString -> CWString -> CWString -> CWString -> IO CInt
#else
foreign import ccall safe "thc_terminal_spawn" c_spawn :: Ptr NativeTerminal -> CString -> Ptr CString -> Ptr CString -> CString -> IO CInt
#endif
foreign import ccall unsafe "thc_terminal_write" c_write :: Ptr NativeTerminal -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall safe "thc_terminal_resize" c_resize :: Ptr NativeTerminal -> CInt -> CInt -> IO CInt
foreign import ccall safe "thc_terminal_poll" c_poll :: Ptr NativeTerminal -> IO CInt
foreign import ccall unsafe "thc_terminal_cells" c_cells :: Ptr NativeTerminal -> IO (Ptr Word32)
foreign import ccall unsafe "thc_terminal_text" c_text :: Ptr NativeTerminal -> Ptr CSize -> IO (Ptr Word8)
foreign import ccall unsafe "thc_terminal_output" c_output :: Ptr NativeTerminal -> Ptr CSize -> IO (Ptr Word8)
foreign import ccall unsafe "thc_terminal_info" c_info :: Ptr NativeTerminal -> Ptr CInt -> IO ()
foreign import ccall unsafe "thc_terminal_error" c_error :: Ptr NativeTerminal -> IO CString
foreign import ccall safe "thc_terminal_kill" c_kill :: Ptr NativeTerminal -> IO ()
foreign import ccall safe "thc_terminal_free" c_free :: Ptr NativeTerminal -> IO ()

setTerminalAppearance :: Terminal -> Bool -> IO (Either Text ())
setTerminalAppearance terminal dark = withOpen terminal $ \ptr -> do
  ok<-c_appearance ptr (if dark then 1 else 0)
  if ok==0 then Left <$> nativeError ptr else pure (Right ())

terminalAvailable :: Bool
terminalAvailable = True

nativeError :: Ptr NativeTerminal -> IO Text
nativeError ptr = c_error ptr >>= BS.packCString >>= pure . TE.decodeUtf8With lenientDecode

validSize :: Int -> Int -> Bool
validSize columns rows = columns > 0 && rows > 0 && columns <= 1000 && rows <= 1000

startTerminal :: TerminalConfig -> IO (Either Text Terminal)
startTerminal config
  | not (validSize (terminalColumns config) (terminalRows config)) = pure (Left "Terminal size must be between 1 and 1000 rows and columns")
  | any (elem '\0') (terminalCommand config : terminalDirectory config : terminalArguments config ++ concatMap (\(k,v) -> [k,v]) (terminalEnvironment config)) = pure (Left "Terminal command contains a NUL byte")
  | any (\(k,_) -> null k || '=' `elem` k) (terminalEnvironment config) = pure (Left "Invalid terminal environment variable name")
  | otherwise = do
      outcome <- try start
      pure $ either (Left . T.pack . show) id (outcome :: Either IOException (Either Text Terminal))
  where
    start = mask_ $ do
      inherited <- getEnvironment
      cwd <- makeAbsolute (terminalDirectory config)
      let overrides = terminalEnvironment config
          environment = overrides ++ [(k,v) | (k,v) <- inherited, variableKey k `notElem` map (variableKey . fst) overrides, variableKey k /= "TERM"]
          withTerm = if "TERM" `elem` map (variableKey . fst) overrides then environment else ("TERM","xterm-256color") : environment
          command = terminalCommand config
          absolute path = if isAbsolute path then path else cwd </> path
          paths = if isAbsolute command || any isPathSeparator command then [absolute command]
            else [absolute dir </> command | dir <- splitSearchPath (maybe defaultPath id (lookup "PATH" [(variableKey k,v) | (k,v)<-withTerm]))]
          candidates = concatMap executablePaths paths
      found <- findExecutableIn candidates
      case found of
        Nothing -> pure (Left ("Terminal command not found: " <> T.pack command))
        Just path -> do
          ptr <- c_new (fromIntegral (terminalColumns config)) (fromIntegral (terminalRows config))
          if ptr == nullPtr then pure (Left "Could not allocate libghostty-vt terminal") else
            (do
#ifdef mingw32_HOST_OS
                ok <- withCWString path $ \exe -> withCWString cwd $ \directory ->
                  withCWString (unwords (map quoteArgument (path : terminalArguments config))) $ \args ->
                  withCWString (concat [k ++ "=" ++ v ++ "\0" | (k,v)<-sortOn (variableKey . fst) withTerm] ++ "\0") $ \env ->
                    c_spawn ptr exe args env directory
#else
                ok <- withCString path $ \exe -> withCString cwd $ \directory ->
                  withStrings (command : terminalArguments config) $ \args ->
                  withStrings [k ++ "=" ++ v | (k,v) <- withTerm] $ \env -> c_spawn ptr exe args env directory
#endif
                if ok == 0 then do
                  err <- nativeError ptr
                  c_free ptr
                  pure (Left err)
                else Right . Terminal <$> newMVar (Just ptr)) `onException` c_free ptr
    findExecutableIn [] = pure Nothing
    findExecutableIn (p:ps) = do
      exists <- doesFileExist p
      runnable <- if exists then executable <$> getPermissions p else pure False
      if runnable then pure (Just p) else findExecutableIn ps
#ifdef mingw32_HOST_OS
    -- cmd.exe treats quoted switches as command text, unlike CRT argv parsers.
    -- Leave unambiguous tokens bare; use the process library for actual quoting.
    quoteArgument arg
      | null arg || any (`elem` (" \t\"" :: String)) arg = translate arg
      | otherwise = arg
    variableKey = map toUpper
    defaultPath = ""
    executablePaths path = path : [path ++ ".exe" | null (takeExtension path)]
#else
    variableKey = id
    defaultPath = "/usr/bin:/bin"
    executablePaths path = [path]
    withStrings strings action = withMany withCString strings $ \pointers -> withArray0 nullPtr pointers action
#endif

withOpen :: Terminal -> (Ptr NativeTerminal -> IO (Either Text a)) -> IO (Either Text a)
withOpen (Terminal state) action = withMVar state $ maybe (pure (Left "Terminal has been released")) action

writeTerminal :: Terminal -> ByteString -> IO (Either Text ())
writeTerminal terminal bytes = withOpen terminal $ \ptr -> BS.useAsCStringLen bytes $ \(buffer,len) -> do
  ok <- c_write ptr (castPtr buffer) (fromIntegral len)
  if ok == 0 then Left <$> nativeError ptr else pure (Right ())

resizeTerminal :: Terminal -> Int -> Int -> IO (Either Text ())
resizeTerminal terminal columns rows
  | not (validSize columns rows) = pure (Left "Terminal size must be between 1 and 1000 rows and columns")
  | otherwise = withOpen terminal $ \ptr -> do
      ok <- c_resize ptr (fromIntegral columns) (fromIntegral rows)
      if ok == 0 then Left <$> nativeError ptr else pure (Right ())

pollTerminal :: Terminal -> IO (Either Text TerminalSnapshot)
pollTerminal terminal = withOpen terminal $ \ptr -> do
  ok <- c_poll ptr
  if ok == 0 then Left <$> nativeError ptr else allocaArray 6 $ \info -> do
    c_info ptr info
    values <- map fromIntegral <$> peekArray 6 info
    case values of
      [columns,rows,cx,cy,exited,code] -> do
        text <- copyBuffer c_text ptr
        output <- copyBuffer c_output ptr
        cells <- c_cells ptr
        decoded <- forM [0 .. columns * rows - 1] $ \i -> do
          words' <- peekArray 6 (cells `advancePtr` (i * 6))
          case words' of
            [offset,len,fg,bg,attrs,width] -> pure TerminalCell
              { cellText = if len == 0 && width /= 0 then " " else TE.decodeUtf8With lenientDecode (BS.take (fromIntegral len) (BS.drop (fromIntegral offset) text))
              , cellForeground = fg, cellBackground = bg, cellAttributes = attrs, cellWidth = fromIntegral width }
            _ -> error "terminal cell ABI"
        pure $ Right TerminalSnapshot
          { snapshotColumns = columns, snapshotRows = rows, snapshotCells = decoded
          , snapshotCursor = if cx < 0 || cy < 0 then Nothing else Just (cx,cy)
          , snapshotOutput = output, snapshotExitCode = if exited == 0 then Nothing else Just (nativeExit code) }
      _ -> error "terminal info ABI"
  where
#ifdef mingw32_HOST_OS
    nativeExit code = fromIntegral (fromIntegral code :: Word32)
#else
    nativeExit code = code
#endif
    copyBuffer getter ptr = alloca $ \len -> do
      buffer <- getter ptr len
      n <- fromIntegral <$> peek len
      if n == 0 then pure BS.empty else BS.packCStringLen (castPtr buffer,n)

killTerminal :: Terminal -> IO ()
killTerminal (Terminal state) = withMVar state $ maybe (pure ()) c_kill

closeTerminal :: Terminal -> IO ()
closeTerminal (Terminal state) = mask_ $ modifyMVar_ state $ \current -> do
  maybe (pure ()) c_free current
  pure Nothing
#else
setTerminalAppearance :: Terminal -> Bool -> IO (Either Text ())
setTerminalAppearance _ _ = pure (Right ())

data Terminal

terminalAvailable :: Bool
terminalAvailable = False

unavailable :: IO (Either Text a)
unavailable = pure (Left "Terminal support is unavailable: rebuild with -fterminal and the pinned libghostty-vt dependency")

startTerminal :: TerminalConfig -> IO (Either Text Terminal)
startTerminal _ = unavailable
writeTerminal :: Terminal -> ByteString -> IO (Either Text ())
writeTerminal _ _ = unavailable
resizeTerminal :: Terminal -> Int -> Int -> IO (Either Text ())
resizeTerminal _ _ _ = unavailable
pollTerminal :: Terminal -> IO (Either Text TerminalSnapshot)
pollTerminal _ = unavailable
killTerminal :: Terminal -> IO ()
killTerminal _ = pure ()
closeTerminal :: Terminal -> IO ()
closeTerminal _ = pure ()
#endif
