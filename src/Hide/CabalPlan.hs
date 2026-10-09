{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hide.CabalPlan
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Bounded, privacy-filtered inspection of an existing Cabal plan.json.
--
-- This module never invokes Cabal. It prioritizes local units and limits file,
-- row and encoded-response sizes. Source paths must remain in the workspace and
-- pass lexical/canonical privacy checks. Freshness can be stale or unknown;
-- unchanged timestamps are not evidence that a plan is current. Raw configuration
-- and repository credentials are not part of the returned plan view.
module Hide.CabalPlan (cabalPlan, publicProjectPath) where

import Control.Monad (unless, forM, filterM)
import Data.Aeson
import Data.Aeson.Types (Parser, Pair, parseEither, parseMaybe)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (nub, partition)
import Data.Char (isAscii, isAlphaNum)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (canonicalizePath, doesFileExist, getModificationTime, listDirectory)
import System.IO (withBinaryFile, IOMode(ReadMode))
import System.FilePath ((</>), isAbsolute, makeRelative, splitDirectories, takeExtension)
import System.IO.Error (tryIOError)
import Hide.Buffer
import Hide.Files (filePath)
import Hide.GuestAccess (protectedPath)
import Hide.Model

-- | Read and sanitize the already-generated plan with bounded status and truncation metadata.
cabalPlan :: Desktop -> FilePath -> IO Value
cabalPlan desktop root=do
  attempted<-tryIOError $ do
    let relative="dist-newstyle/cache/plan.json"
        original=root </> relative
        unavailable state=pure (object ["status" .= (state::Text),"path" .= relative])
    checked<-publicProjectPath desktop root original
    case checked of
      Nothing -> unavailable "unavailable"
      Just path -> do
        exists<-doesFileExist path
        if not exists then unavailable "missing" else do
          bytes<-withBinaryFile path ReadMode (\handle->BS.hGet handle (8*1024*1024+1))
          if BS.length bytes>8*1024*1024 then unavailable "too-large" else
            case eitherDecodeStrict' bytes >>= parseEither (withObject "Cabal plan" (\o->(o,) <$> o .: "install-plan")) of
              Left _ -> unavailable "invalid"
              Right (header,rows) -> do
                modified<-getModificationTime path
                now<-getCurrentTime
                let (localRows,otherRows)=partition (\value->parseMaybe (withObject "unit" (.:? "style")) value==Just (Just ("local"::Text))) rows
                    inspectedRows=take 4096 (localRows++otherRows)
                    validUnits=mapMaybe (either (const Nothing) Just . parseEither planUnit) inspectedRows
                normalized<-mapM (\(fields,local,source,components)->do
                  safe<-if local then maybe (pure Nothing) (publicProjectPath desktop root . (root </>)) source else pure Nothing
                  let relativeSource=makeRelative root <$> safe
                  pure (object (fields++["sourceRoot" .= relativeSource]),components,safe)) validUnits
                freshness<-planFreshness desktop root modified (mapMaybe (\(_,_,source)->source) normalized)
                let omitted=length inspectedRows-length validUnits
                    metadata=[key .= (fromMaybe Nothing (parseMaybe (optionalPlanText source) header))
                      | (key,source)<-[("compilerId","compiler-id"),("cabalVersion","cabal-version"),("os","os"),("arch","arch")]]
                    result selected=object (metadata++
                      ["status" .= ("available"::Text),"path" .= relative,"provenance" .= ("Cabal install-plan"::Text),
                       "modifiedAt" .= modified,"ageSeconds" .= (max 0 (realToFrac (diffUTCTime now modified))::Double),
                       "freshness" .= freshness,"totalUnits" .= length rows,"omittedUnits" .= omitted,
                       "truncatedUnits" .= (length rows-omitted-length selected),
                       "graphComplete" .= (length rows==length selected),
                       "units" .= [unit | (unit,_,_)<-selected],"localComponents" .= concat [components | (_,components,_)<-selected]])
                    bounded selected=let value=result selected in if BL.length (encode value)<=524288
                      then value else if null selected then object ["status" .= ("too-large"::Text),"path" .= relative]
                      else bounded (take (length selected `div` 2) selected)
                pure (bounded normalized)
  pure (either (const (object ["status" .= ("unavailable"::Text)])) id attempted)

-- | Resolve a source path inside the supplied canonical workspace root and
-- reject private paths before exposing it.
publicProjectPath :: Desktop -> FilePath -> FilePath -> IO (Maybe FilePath)
publicProjectPath desktop root raw
  | null raw || length raw>32768 || any (< ' ') raw || protectedPath desktop raw=pure Nothing
  | otherwise=do
      result<-tryIOError (canonicalizePath raw)
      pure $ case result of
        Right path | within path && not (protectedPath desktop path) -> Just path
        _ -> Nothing
  where within path=let relative=makeRelative root path in not (isAbsolute relative) && ".." `notElem` splitDirectories relative

optionalPlanText :: K.Key -> Object -> Parser (Maybe Text)
optionalPlanText key fields=do
  value<-fields .:? key
  mapM_ validPlanText value
  pure value
validPlanText :: Text -> Parser ()
validPlanText text=unless (not (T.null text) && T.length text<=512 && T.all (\c->isAscii c && (isAlphaNum c || c `elem` ("-_.:+"::String))) text)
  (fail "Invalid plan identifier")

planUnit :: Value -> Parser ([Pair],Bool,Maybe FilePath,[Value])
planUnit=withObject "Cabal unit" $ \o -> do
  ident<-o .: "id"; validPlanText ident
  name<-optionalPlanText "pkg-name" o
  version<-optionalPlanText "pkg-version" o
  kind<-optionalPlanText "type" o
  style<-optionalPlanText "style" o
  component<-optionalPlanText "component-name" o
  dependencies<-planDependencies o
  nested<-o .:? "components" .!= KM.empty
  components<-mapM (\(key,value)->do
    let label=K.toText key
    validPlanText label
    fields<-withObject "Cabal component" planDependencies value
    pure (label,object ("name" .= label:fields))) (KM.toList nested)
  source<-o .:? "pkg-src" >>= maybe (pure Nothing) (withObject "Cabal source" $ \src->do
    sourceType<-src .:? "type" :: Parser (Maybe Text)
    if sourceType==Just "local" then src .:? "path" else pure Nothing)
  let local=style==Just "local"
      references=[object ["unitId" .= ident,"component" .= label] | local,label<-maybe [] (:[]) component++map fst components]
  pure (["id" .= ident,"package" .= name,"version" .= version,"type" .= kind,"style" .= style,
    "local" .= local,"component" .= component,"components" .= map snd components]++dependencies,local,source,references)

planDependencies :: Object -> Parser [Pair]
planDependencies fields=do
  depends<-fields .:? "depends" .!= []
  executables<-fields .:? "exe-depends" .!= []
  mapM_ validPlanText (depends++executables)
  pure ["depends" .= (depends::[Text]),"exeDepends" .= (executables::[Text]),"dependenciesKnown" .= KM.member "depends" fields]

planFreshness :: Desktop -> FilePath -> UTCTime -> [FilePath] -> IO Value
planFreshness desktop root modified sources=do
  let directories=nub (root:sources)
  listings<-forM (take 64 directories) $ \directory -> do
    entries<-either (const []) id <$> tryIOError (listDirectory directory)
    pure ([directory </> name | name<-take 4096 entries,takeExtension name==".cabal" || directory==root && name `elem` ["cabal.project","cabal.project.local","cabal.project.freeze"]],length entries>4096)
  let candidates=concatMap fst listings
  safe<-catMaybes <$> mapM (publicProjectPath desktop root) (take 1024 candidates)
  newer<-filterM (\path->either (const False) (>modified) <$> tryIOError (getModificationTime path)) safe
  dirtyInputs<-fmap catMaybes $ forM (M.toList (buffers desktop)) $ \(ident,doc)->case documentFile doc of
    Just file | dirty (documentBuffer doc),not (privateDocument desktop doc) -> do
      path<-publicProjectPath desktop root (filePath file)
      pure (if maybe False (`elem` safe) path then Just ident else Nothing)
    _ -> pure Nothing
  pure (object ["status" .= (if null newer && null dirtyInputs then "unknown" else "stale"::Text),
    "basis" .= ("Known manifest timestamps and live unsaved manifests; unchanged timestamps do not prove the plan current."::Text),
    "newerInputs" .= map (makeRelative root) (take 128 newer),"unsavedBufferIds" .= take 128 dirtyInputs,
    "checkedInputCount" .= length safe,"inputsTruncated" .= (length directories>64 || any snd listings || length candidates>1024 || length newer>128 || length dirtyInputs>128)])
