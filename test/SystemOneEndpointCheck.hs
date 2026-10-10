{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
-- | Module      : SystemOneEndpointCheck
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- The public wire adapter against a scoped loopback server. Each invocation owns
-- its socket and observes the actual request, without config or directory state.
module SystemOneEndpointCheck (checks) where

import Control.Concurrent.Async (Async,withAsync,wait)
import Control.Exception (bracket,onException)
import Control.Monad (unless)
import Data.Aeson hiding (decode)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Plugin.SystemOne
import Hide.SystemOneEndpoint
import Hide.SystemOneConfig (loadSystemOneProvider)
import Network.Socket
import System.IO
import System.Environment (lookupEnv,setEnv,unsetEnv)
import System.Timeout (timeout)

checks :: IO ()
checks=do
  let input=DecisionInput "revision-7" "Already redacted context"
        [DecisionQuestion "check" "Is it ready?" (BinaryDecision "not ready" "ready")
        ,DecisionQuestion "pick" "Choose" (ChoiceDecision [DecisionOption "10" "tenth",DecisionOption "2" "second"])
        ,DecisionQuestion "rank" "Rank" (ScoreDecision ["low","high"])] SelectedSupplier
      body=object ["model" .= ("test-model"::Text),"answers" .= object
        ["check" .= object ["type" .= ("noul"::Text),"noul" .= (0.75::Double)]
        ,"pick" .= object ["type" .= ("choice"::Text),"choice" .= ("2"::Text),"confidence" .= (0.5::Double),"probabilities" .= object ["2" .= (0.75::Double),"10" .= (0.25::Double)]]
        ,"rank" .= object ["type" .= ("score"::Text),"score" .= (0.75::Double),"legend" .= object ["0" .= ("low"::Text),"1" .= ("high"::Text)],"probabilities" .= object ["1" .= (0.75::Double),"0" .= (0.25::Double)]]]
        ,"usage" .= object ["input_tokens" .= (17::Int),"output_tokens" .= (8::Int)]]
      encoded=BL.toStrict (encodeSystemOneInput "test-model" input)
      decode value=decodeSystemOneOutput "test-model" input (BL.toStrict (encode value))
  check "choice wire preserves submitted labels, criteria and order"
    ("\"10\":\"tenth\",\"2\":\"second\"" `BS.isInfixOf` encoded)
  expected<-right (decode body)
  check "answers restore submitted order, independent of response object order"
    (map answerProbabilities (outputAnswers expected)==replicate 3 [0.25,0.75])
  let changed key value=case body of Object entries->Object (KM.insert key value entries); _->error "object"
  check "truncation is a failure" (isLeft (decode (changed "truncated" (Bool True))))
  check "missing answer is a failure" (isLeft (decode (changed "answers" (object []))))
  check "model echo must match requested name" (isLeft (decode (changed "model" (String "different"))))
  let many=DecisionInput "many" "small" [DecisionQuestion "q" "" (ChoiceDecision [DecisionOption ("label"<>T.pack (show n)) "" | n<-[0::Int ..254]])] SelectedSupplier
      rounded=object ["model" .= ("test-model"::Text),"answers" .= object ["q" .= object
        ["type" .= ("choice"::Text),"choice" .= ("label0"::Text)
        ,"probabilities" .= object [K.fromText ("label"<>T.pack (show n)) .= (0.0039::Double) | n<-[0::Int ..254]]]]]
  _<-right (decodeSystemOneOutput "test-model" many (BL.toStrict (encode rounded)))
  check "credentials cannot enter a public endpoint URL" (isLeft (endpointProvider (EndpointConfig "https://secret@example.com/v1/systemone" "test" Nothing)))
  check "header injection refused" (isLeft (endpointProvider (EndpointConfig "https://example.com/v1/systemone" "test" (Just "token\r\nInjected: value"))))
  configured<-loadSystemOneProvider (object ["provider" .= ("off"::Text)])
  check "inference disabled by explicit off" (case configured of Right Nothing->True; _->False)
  bracket (lookupEnv "HIDE_SYSTEMONE_CHECK_VALUE")
    (maybe (unsetEnv "HIDE_SYSTEMONE_CHECK_VALUE") (setEnv "HIDE_SYSTEMONE_CHECK_VALUE")) $ \_->do
      setEnv "HIDE_SYSTEMONE_CHECK_VALUE" "test-bearer-canary"
      ordinaryName<-loadSystemOneProvider (object ["provider" .= ("endpoint"::Text),
        "endpoint" .= ("http://127.0.0.1:1/v1/systemone"::Text),"model" .= ("test"::Text),
        "tokenEnv" .= ("HIDE_SYSTEMONE_CHECK_VALUE"::Text)])
      check "bearer cannot use an environment name exposed by inspection tools" (isLeft ordinaryName)
  withServer (BL.toStrict (encode body)) "200 OK" $ \url captured->do
    provider<-right (endpointProvider (EndpointConfig url "test-model" (Just "test-token")))
    actual<-withDecisionDriver provider (pure False) $ \driver->runDecision driver input (pure False)
    check "real HTTP request decodes exact result" (actual==Right expected)
    (headers,payload)<-wait captured
    check "actual request carries only intended authorization" ("Authorization: Bearer test-token" `elem` headers)
    check "actual payload is ordered request" (payload==encoded)
  withServer "" "302 Found\r\nLocation: http://127.0.0.1:1/forbidden" $ \url captured->do
    provider<-right (endpointProvider (EndpointConfig url "test-model" Nothing))
    actual<-withDecisionDriver provider (pure False) $ \driver->runDecision driver input (pure False)
    check "redirect remains explicit failure" (actual==Left (DecisionProviderFailed "System One returned HTTP 302"))
    _<-wait captured
    pure ()
  putStrLn "System One endpoint checks passed"
  where
    check name value=unless value (error name)
    right :: Show e => Either e a -> IO a
    right=either (error.show) pure

withServer :: BS.ByteString -> BS.ByteString -> (Text -> Async ([BS.ByteString],BS.ByteString) -> IO a) -> IO a
withServer body status action=withSocketsDo $ bracket acquire close $ \listener->do
  address<-getSocketName listener
  let port=case address of SockAddrInet value _->value; _->error "IPv4 listener"
      url="http://127.0.0.1:"<>T.pack (show port)<>"/v1/systemone"
  withAsync (serve listener) $ \worker->do
    outcome<-timeout 5000000 (action url worker)
    case outcome of Nothing->error "System One endpoint check timed out"; Just value->pure value
  where
    acquire=do
      listener<-socket AF_INET Stream defaultProtocol
      (do
        bind listener (SockAddrInet 0 (tupleToHostAddress (127,0,0,1)))
        listen listener 1
        pure listener) `onException` close listener
    serve listener=bracket (fst <$> accept listener >>= \connection->socketToHandle connection ReadWriteMode) hClose $ \handle->do
      hSetBinaryMode handle True
      hSetBuffering handle NoBuffering
      headers<-readHeaders handle
      let lengthLine=maybe (error "Missing Content-Length") id (find ("Content-Length:" `BS.isPrefixOf`) headers)
          size=case BC.readInt (BC.dropWhile (==' ') (BS.drop 15 lengthLine)) of Just (n,_) | n>=0 && n<=1048576->n; _->error "Invalid Content-Length"
      payload<-BS.hGet handle size
      BS.hPut handle ("HTTP/1.1 "<>status<>"\r\nContent-Type: application/json\r\nConnection: close\r\nContent-Length: "<>BC.pack (show (BS.length body))<>"\r\n\r\n"<>body)
      pure (headers,payload)
    readHeaders handle=do
      line<-BC.filter (/='\r') <$> BC.hGetLine handle
      if BS.null line then pure [] else (line:) <$> readHeaders handle
