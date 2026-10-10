{-# LANGUAGE CPP, ForeignFunctionInterface, OverloadedStrings, ScopedTypeVariables #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : Hide.SystemOneNative
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : CPP, ForeignFunctionInterface, OverloadedStrings, ScopedTypeVariables
--
-- Pinned Laya inference in the host process. Acquisition verifies the manifest
-- and every payload, then retains one CPU session pair and tokenizer. The
-- allocation limit meters ORT model/tensor allocations, not total process RSS.
module Hide.SystemOneNative
  ( NativeConfig(..)
  , nativeProvider
  ) where

import Data.Text (Text)
import Data.Word (Word64)
import Hide.Plugin.SystemOne

#ifdef WITH_SYSTEM_ONE_NATIVE
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM (STM,atomically,check)
import Control.Exception (bracket,finally,mask,onException,throwIO,uninterruptibleMask_)
import Control.Monad (unless,when)
import qualified Crypto.Hash.SHA256 as SHA
import Data.Aeson (eitherDecodeStrict',withObject,(.:))
import Data.Aeson.Types (Parser,parseEither)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.List (mapAccumL)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8,Word32)
import Foreign (Ptr,alloca,allocaArray,castPtr,nullPtr,peek,peekArray,poke,withArray)
import Foreign.C
import Numeric (showHex)
import System.FilePath ((</>),isAbsolute,takeFileName)
import System.IO (IOMode(ReadMode),withBinaryFile)
#endif

-- | Explicit artifact selection. Constructing the provider performs no IO.
-- The directory is an immutable acquired bundle; its manifest digest is the
-- published artifact identity. Acquisition and all encoding stay on workers.
data NativeConfig = NativeConfig
  { nativeBundleDirectory :: !FilePath
  , nativeManifestSHA256 :: !Text
  , nativeAllocationLimit :: !Word64
  , nativeThreads :: !Int
  } deriving (Eq,Show)

-- | Validate selection without loading weights. An unlinked build is explicitly
-- unavailable; it cannot fall back to another supplier or data destination.
nativeProvider :: NativeConfig -> Either Text DecisionProvider
#ifndef WITH_SYSTEM_ONE_NATIVE
nativeProvider _=Left "In-process Laya support was not built"
#else
nativeProvider config=do
  unless (isAbsolute (nativeBundleDirectory config) && '\0' `notElem` nativeBundleDirectory config)
    (Left "Laya requires an absolute bundle directory")
  unless (validDigest (nativeManifestSHA256 config)) (Left "Laya requires a SHA256 manifest identity")
  unless (nativeAllocationLimit config>0 && nativeThreads config>=1 && nativeThreads config<=8)
    (Left "Invalid Laya allocation or thread limit")
  pure DecisionProvider
    { decisionProviderDescription=SupplierDescription "Laya native CPU" InProcess
        (PinnedArtifact (nativeManifestSHA256 config)) (Just (nativeAllocationLimit config)) 0
    , withDecisionDriver= \retired action->bracket (acquire config retired) release $ \resources->do
        gate<-newMVar (Just resources)
        let invoke input cancelled=mask $ \restore->do
              slot<-tryTakeMVar gate
              case slot of
                Nothing->pure (Left DecisionBusy)
                Just Nothing->putMVar gate Nothing>>pure (Left DecisionClosed)
                Just (Just current)->restore (runNative config current retired cancelled input)
                  `finally` putMVar gate (Just current)
            closeGate=do
              cancelNative (resourceModel resources)
              -- A retained invocation must drain before native pointers die.
              uninterruptibleMask_ (modifyMVar_ gate (const (pure Nothing)))
        withAsync (waitCancelled retired>>cancelNative (resourceModel resources))
          (\_->action (DecisionDriver invoke)) `finally` closeGate
    }

data Native
data Tokenizer
data Resources = Resources
  { resourceModel :: !(Ptr Native)
  , resourceTokenizer :: !(Ptr Tokenizer)
  , resourceTemperatures :: ![Double]
  , resourceBuckets :: !(M.Map Text Double)
  }

foreign import ccall safe "hide_system_one_create" createNative :: Word64 -> CUInt -> Ptr (Ptr Native) -> IO CInt
foreign import ccall safe "hide_system_one_load" loadNative :: Ptr Native -> CString -> CString -> IO CInt
foreign import ccall unsafe "hide_system_one_begin" beginNative :: Ptr Native -> IO CInt
foreign import ccall safe "hide_system_one_cancel" cancelNative :: Ptr Native -> IO ()
foreign import ccall safe "hide_system_one_run" inferNative :: Ptr Native -> Ptr Int64 -> CSize -> Ptr Int64 -> CSize -> Int64 -> Ptr CFloat -> IO CInt
foreign import ccall safe "hide_system_one_free" freeNative :: Ptr Native -> IO Word64
foreign import ccall safe "hide_tokenizer_create" createTokenizer :: Ptr Word8 -> CSize -> Ptr (Ptr Tokenizer) -> IO CInt
foreign import ccall safe "hide_tokenizer_encode" encodeTokenizer :: Ptr Tokenizer -> Ptr Word8 -> CSize -> Ptr Word32 -> CSize -> Ptr CSize -> IO CInt
foreign import ccall safe "hide_tokenizer_free" freeTokenizer :: Ptr Tokenizer -> IO ()

unavailable :: IO a
unavailable=throwIO (userError "Laya artifact acquisition failed")

waitCancelled :: STM Bool -> IO ()
waitCancelled flag=atomically (flag >>= check)

checkAlive :: STM Bool -> IO ()
checkAlive flag=atomically flag >>= flip when unavailable

acquire :: NativeConfig -> STM Bool -> IO Resources
acquire config retired=do
  checkAlive retired
  verifyBundle config retired
  tokenizerBytes<-readBounded (directory </> "tokenizer.json") (4*1024*1024)
  cfgBytes<-readBounded (directory </> "rl_agent_config.json") 65536
  (temperatures,buckets)<-either (const unavailable) pure
    (eitherDecodeStrict' cfgBytes >>= parseEither configParser)
  mask $ \restore->alloca $ \out->do
    poke out nullPtr
    code<-createNative (nativeAllocationLimit config) (fromIntegral (nativeThreads config)) out
    model<-peek out
    let freeModel=do remaining<-freeNative model; unless (remaining==0) unavailable
    when (code/=0 || model==nullPtr) (freeModel>>unavailable)
    tokenizer<-(alloca $ \result->do
      poke result nullPtr
      statusCode<-BS.useAsCStringLen tokenizerBytes $ \(bytes,size)->
        createTokenizer (castPtr bytes) (fromIntegral size) result
      pointer<-peek result
      when (statusCode/=0 || pointer==nullPtr) unavailable
      pure pointer) `onException` freeModel
    let resources=Resources model tokenizer temperatures buckets
    restore (withAsync (waitCancelled retired>>cancelNative model) $ \_->do
      loaded<-withUTF8 (directory </> "encoder.onnx") $ \encoder->
        withUTF8 (directory </> "head.onnx") $ \headPath->loadNative model encoder headPath
      when (loaded/=0) unavailable
      checkAlive retired
      pure resources) `onException` release resources
  where
    directory=nativeBundleDirectory config
    configParser=withObject "Laya configuration" $ \value->do
      size<-value .: "max_len" :: Parser Int
      headSize<-value .: "head_max_len" :: Parser Int
      temperatures<-value .: "temperature"
      buckets<-value .: "temperature_by_options"
      unless (size==512 && headSize==192 && length temperatures==3
        && all finite (temperatures++M.elems buckets)) (fail "Unsupported Laya configuration")
      pure (map clamp temperatures,M.map clamp buckets)
    clamp=max 0.5 . min 5

release :: Resources -> IO ()
release resources=do
  freeTokenizer (resourceTokenizer resources)
  remaining<-freeNative (resourceModel resources)
  unless (remaining==0) unavailable

withUTF8 :: FilePath -> (CString -> IO a) -> IO a
withUTF8 path=BS.useAsCString (TE.encodeUtf8 (T.pack path))

readBounded :: FilePath -> Int -> IO BS.ByteString
readBounded path limit=withBinaryFile path ReadMode $ \handle->do
  bytes<-BS.hGet handle (limit+1)
  if BS.length bytes<=limit then pure bytes else unavailable

validDigest :: Text -> Bool
validDigest value=T.length value==64 && T.all (\c->c>='0' && c<='9' || c>='a' && c<='f') value

hex :: BS.ByteString -> Text
hex=T.pack . concatMap (\byte->let s=showHex byte "" in if length s==1 then '0':s else s) . BS.unpack

verifyBundle :: NativeConfig -> STM Bool -> IO ()
verifyBundle config retired=do
  bytes<-readBounded (directory </> "manifest.json") (1024*1024)
  unless (hex (SHA.hash bytes)==nativeManifestSHA256 config) unavailable
  files<-either (const unavailable) pure (eitherDecodeStrict' bytes >>= parseEither manifestParser)
  let names=map (\(name,_,_)->name) files
      required=["encoder.onnx","encoder.onnx.data","head.onnx","head.onnx.data","tokenizer.json","rl_agent_config.json"]
  unless (length files<=16 && S.size (S.fromList names)==length names && all (`elem` names) required) unavailable
  mapM_ verify files
  where
    directory=nativeBundleDirectory config
    manifestParser=withObject "Laya manifest" $ \value->do
      format<-value .: "format" :: Parser Text
      version<-value .: "version" :: Parser Int
      unless (format=="laya-split-onnx" && version==1) (fail "Unsupported bundle")
      entries<-value .: "files"
      mapM (withObject "Bundle file" $ \entry->(,,) <$> entry .: "path" <*> entry .: "bytes" <*> entry .: "sha256") entries
    verify (name,size,digest)=do
      unless (not (null name) && name==takeFileName name && name/="." && name/=".."
        && not (isAbsolute name) && '\0' `notElem` name && size>=0 && size<=2147483648 && validDigest digest) unavailable
      withBinaryFile (directory </> name) ReadMode $ \handle->do
        let loop context count=do
              checkAlive retired
              chunk<-BS.hGetSome handle (1024*1024)
              if BS.null chunk then unless (count==size && hex (SHA.finalize context)==digest) unavailable
              else do
                let next=count+toInteger (BS.length chunk)
                when (next>size) unavailable
                let updated=SHA.update context chunk
                updated `seq` loop updated next
        loop SHA.init 0

tokenize :: Ptr Tokenizer -> Text -> IO (Either DecisionFailure [Int64])
tokenize tokenizer text
  | BS.length bytes>1048576=pure (Left (DecisionInvalid "Laya text exceeds tokenizer byte limit"))
  | otherwise=BS.useAsCStringLen bytes $ \(input,size)->allocaArray 512 $ \tokens->alloca $ \written->do
      result<-encodeTokenizer tokenizer (castPtr input) (fromIntegral size) tokens 512 written
      if result==0 then do
        count<-peek written
        Right . map fromIntegral <$> peekArray (fromIntegral count) tokens
      else pure (Left (DecisionInvalid "Laya text exceeds tokenizer/context limits"))
  where bytes=TE.encodeUtf8 (T.replace "[MASK]" " " text)

data Prepared = Prepared !DecisionQuestion ![Int64] ![Int64] !Int64

prepare :: Resources -> [Int64] -> DecisionQuestion -> IO (Either DecisionFailure Prepared)
prepare resources state question=do
  headTokens<-tokenize tokenizer (tag<>" question: "<>questionInstructions question)
  optionTokens<-mapM (tokenize tokenizer . (" "<>)) options
  pure $ do
    instruction<-headTokens
    encoded<-sequence optionTokens
    unless (not (null encoded) && length encoded<=255 && all ((<=48) . length) encoded)
      (Left overBudget)
    let marked=map (50284:) encoded
        budget=192-sum (map length marked)
        per=max 4 ((192-16) `div` length marked)
    unless ((budget>=16 || all ((<=per) . length) marked) && length instruction<=max 8 budget)
      (Left overBudget)
    let prefix=[50281]++instruction++[50282]
        (_,positions)=mapAccumL (\offset row->(offset+length row,fromIntegral offset)) (length prefix) marked
        ids=prefix++concat marked++[50282]++state++[50282]
    unless (length ids<=512) (Left overBudget)
    pure (Prepared question ids positions qtype)
  where
    tokenizer=resourceTokenizer resources
    overBudget=DecisionInvalid "Laya input would truncate state, instructions or options"
    (tag,qtype,options)=case questionKind question of
      BinaryDecision no yes->("noul",2,["false: "<>fallback "no, the statement does not hold" no,"true: "<>fallback "yes, the statement holds" yes])
      ChoiceDecision values->("choice",0,[optionLabel option<>(if T.null (optionCriterion option) then "" else ": "<>optionCriterion option) | option<-values])
      ScoreDecision levels->("score",1,["level "<>T.pack (show index)<> ": "<>level | (index,level)<-zip [0::Int ..] levels])
    fallback def value=if T.null value then def else value

runNative :: NativeConfig -> Resources -> STM Bool -> STM Bool -> DecisionInput -> IO (Either DecisionFailure DecisionOutput)
runNative config resources retired cancelled input=do
  stopped<-atomically stop
  if stopped then pure (Left DecisionCancelled)
  else if null questions || length questions>64 then pure (Left (DecisionInvalid "Laya requires 1 to 64 questions"))
  else do
    encoded<-tokenize (resourceTokenizer resources) (decisionState input)
    prepared<-case encoded of
      Left failure->pure (Left failure)
      Right state->fmap sequence (mapM (prepare resources state) questions)
    case prepared of
      Left failure->pure (Left failure)
      Right rows->do
        begun<-beginNative model
        if begun/=0 then pure (Left (DecisionProviderFailed "Laya runtime unavailable"))
        else withAsync (waitCancelled stop>>cancelNative model) $ \_->do
          answers<-go rows []
          stoppedNow<-atomically stop
          pure $ if stoppedNow then Left DecisionCancelled else do
            values<-answers
            pure (DecisionOutput (PinnedArtifact (nativeManifestSHA256 config)) values
              (Just (DecisionUsage (Just (sum [length ids | Prepared _ ids _ _<-rows])) (Just 0))))
  where
    questions=decisionQuestions input
    model=resourceModel resources
    stop=(||) <$> retired <*> cancelled
    go [] answers=pure (Right (reverse answers))
    go (row:rows) answers=do
      stopped<-atomically stop
      if stopped then pure (Left DecisionCancelled) else do
        result<-infer resources row
        case result of Left failure->pure (Left failure); Right answer->go rows (answer:answers)

infer :: Resources -> Prepared -> IO (Either DecisionFailure DecisionAnswer)
infer resources (Prepared question ids positions qtype)=
  withArray ids $ \input->withArray positions $ \markers->allocaArray count $ \output->do
    result<-inferNative (resourceModel resources) input (fromIntegral (length ids)) markers (fromIntegral count) qtype output
    case result of
      0->do
        logits<-map realToFrac <$> peekArray count output
        pure $ if all finite logits then
          let maximumLogit=maximum logits
              exps=map (exp . (\value->(value-maximumLogit)/temperature)) logits
              probabilities=map (/sum exps) exps
              confidence=if qtype==2 then maximum probabilities else entropyConfidence probabilities
          in Right (DecisionAnswer (questionName question) kind probabilities (Just confidence))
          else Left (DecisionProviderFailed "Laya returned invalid logits")
      2->pure (Left DecisionCancelled)
      4->pure (Left (DecisionProviderFailed "Laya allocation limit reached"))
      _->pure (Left (DecisionProviderFailed "Laya inference failed"))
  where
    count=length positions
    kind=case qtype of 0->ChoiceAnswer; 1->ScoreAnswer; _->BinaryAnswer
    label=case qtype of 0->"choice"; 1->"score"; _->"noul"
    bucket | count<=2="2" | count<=5="3-5" | count<=10="6-10" | otherwise="11+"
    temperature=M.findWithDefault (resourceTemperatures resources!!fromIntegral qtype) (label<>":"<>bucket) (resourceBuckets resources)

finite :: Double -> Bool
finite value=not (isNaN value || isInfinite value)

entropyConfidence :: [Double] -> Double
entropyConfidence values
  | length values<2=1
  | otherwise=max 0 (min 1 (1+sum [value*log (max value 1e-12) | value<-values]/log (fromIntegral (length values))))
#endif
