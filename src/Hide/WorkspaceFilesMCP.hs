{-# LANGUAGE OverloadedStrings #-}
-- | Workspace search, filesystem operations and exact buffer patches for agents.
--
-- Search overlays eligible open buffers on disk candidates and reports bounded
-- results. Filesystem mutations validate canonical containment, exclude repository
-- metadata/private paths, and reject dirty affected buffers. Unified diffs require
-- exact context and coordinates: no fuzzy matching or external patch command.
-- Search and strict diff preparation run on reply/permission workers. Diff adoption
-- runs in the owning permission tick; filesystem mutations remain initial-phase IO.
module Hide.WorkspaceFilesMCP (fileTools, fileToolNames, fileTool, applyUnifiedDiff, PatchSource, PreparedPatch, capturePatchSource, capturePatchRequest, preparePatch, commitPatch) where

import Hide.Sidebar
import Control.Exception (IOException, bracket, try, evaluate)
import Control.Monad (forM, unless, when)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sort, nub)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO (IOMode(ReadMode), withBinaryFile, hClose)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Process
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Hide.Buffer
import Hide.BufferEdits (PreparedEdit,prepareBufferEdit,commitEdits)
import Hide.Plugin.BufferHost (DiffResult(..),ContentVersion,captureVersion)
import Hide.Build (resolveBuildRoot)
import Hide.Files (FileState(..), saveFile)
import Hide.GuestAccess (protectedBuffer, protectedPath, protectedPathParent)
import Hide.Model
import Hide.Process (processCleanup)

fileToolNames :: [Text]
fileToolNames=["workspace_search","buffer_apply_diff","workspace_files"]

fileTools :: [Value]
fileTools=
  [describe "workspace_search" "Literal line search of ignore-respecting workspace files, or Git tracked files only. Live buffers replace disk contents, including unsaved text; default search also includes untitled buffers. Matches are paged, lines/columns start at 1. Files over 1 MiB and binary files are skipped; total scan is capped at 32 MiB and 10000 files/matches." True ["query"]
    [("query",string),("trackedOnly",boolean),("offset",integer),("limit",integer)]
  ,describe "buffer_apply_diff" "Apply one strict unified diff to a live text buffer at the given revision. Context and hunk positions must match exactly. The entire patch is atomic and undoable; no file is saved. File headers are optional and identify only this buffer, never disk paths." False ["bufferId","revision","diff"]
    [("bufferId",integer),("revision",integer),("diff",string)]
  ,describe "workspace_files" "Create a directory or empty file, delete a file/empty directory, or rename a workspace path. Paths must stay inside the project; Git metadata and symlink endpoints are protected. Refuses overwrite and dirty open descendants. Delete closes clean views; rename updates open buffer paths. No recursive deletion." False ["operation","path"]
    [("operation",object ["type" .= ("string"::Text),"enum" .= (["mkdir","create_file","delete","rename"]::[Text])]),("path",string),("to",string)]]
  where
    string=object ["type" .= ("string"::Text)]
    integer=object ["type" .= ("integer"::Text)]
    boolean=object ["type" .= ("boolean"::Text)]
    describe :: Text -> Text -> Bool -> [Text] -> [(Key,Value)] -> Value
    describe name description readonly required properties=object ["name" .= name,"description" .= description,
      "inputSchema" .= object ["type" .= ("object"::Text),"properties" .= Object (KM.fromList properties),"required" .= required,"additionalProperties" .= False],
      "annotations" .= object ["readOnlyHint" .= readonly,"destructiveHint" .= not readonly,"openWorldHint" .= False]]

type Core = Desktop -> [Effect] -> IO (Bool,Desktop)
type Reply = (Desktop,IO (Either Text Value))

data Request = Search Text Bool Int Int | Files Text FilePath (Maybe FilePath)

-- | Dispatch a validated workspace request; execute its returned search continuation
-- outside the desktop lock. Filesystem operations may perform synchronous IO.
fileTool :: Core -> Desktop -> Text -> Value -> IO Reply
fileTool core desktop name args=case parseEither parse args of
  Left err -> immediate desktop (Left (T.pack err))
  Right (Search query tracked offset count) -> pure (desktop,guardIO (searchWorkspace desktop query tracked offset count))
  Right (Files operation path target) -> do
    result<-try (fileOperation core desktop operation path target)
    case result of Left (err::IOException) -> immediate desktop (Left (T.pack (show err)))
                   Right (updated,value) -> immediate updated value
  where
    parse=withObject "file tool arguments" $ \o -> case name of
      "workspace_search" -> do
        exact o ["query","trackedOnly","offset","limit"]
        query<-o .: "query"
        tracked<-o .:? "trackedOnly" .!= False
        offset<-o .:? "offset" .!= 0
        count<-o .:? "limit" .!= 100
        unless (not (T.null query) && T.length query<=256 && not (T.any (`elem` ['\0','\n','\r']) query)) (fail "query must be 1..256 characters on one line")
        unless (offset>=0 && offset<10000 && count>0 && count<=1000) (fail "offset must be 0..9999 and limit 1..1000")
        pure (Search query tracked offset count)
      "workspace_files" -> do
        exact o ["operation","path","to"]
        operation<-o .: "operation"
        unless (operation `elem` ["mkdir","create_file","delete","rename"]) (fail "Unknown file operation")
        path<-o .: "path"
        target<-o .:? "to"
        unless ((operation=="rename")==isJust target) (fail "Only rename requires a to path")
        pure (Files operation path target)
      _ -> fail "Unknown file tool"
    exact o allowed=unless (all (`elem` allowed) (KM.keys o)) (fail "Unknown argument")

immediate :: Desktop -> Either Text Value -> IO Reply
immediate desktop value=pure (desktop,pure (value >>= bounded))

bounded :: Value -> Either Text Value
bounded value | BL.length (encode value)>=1048576=Left "Result exceeds 1 MiB; request a smaller page."
              | otherwise=Right value

guardIO :: IO (Either Text Value) -> IO (Either Text Value)
guardIO action=do
  result<-try action
  pure (either (Left . T.pack . show) (>>=bounded) (result::Either IOException (Either Text Value)))

-- Strict root containment is checked after resolving symlinks, including the
-- existing ancestors of a new path. Metadata paths and the root itself are never targets.
checkedPath :: FilePath -> FilePath -> IO FilePath
checkedPath root raw=do
  when (null raw || length raw>32768 || '\0' `elem` raw || ".." `elem` splitDirectories raw) (ioError (userError "Invalid workspace path"))
  let joined=if isAbsolute raw then raw else root </> raw
  resolved<-canonicalizePath joined
  unless (within root resolved && resolved/=root && not (metadata raw) && not (metadata (makeRelative root resolved)))
    (ioError (userError "Path must stay inside the workspace and outside repository metadata"))
  pure resolved
  where metadata=any ((`elem` [".git",".hg",".svn"]) . T.toCaseFold . T.pack) . splitDirectories

within :: FilePath -> FilePath -> Bool
within root path=let relative=makeRelative root path in not (isAbsolute relative) && ".." `notElem` splitDirectories relative

operationPath :: FilePath -> FilePath -> IO FilePath
operationPath root raw=do
  path<-checkedPath root raw
  symlink<-catchIOError (pathIsSymbolicLink (if isAbsolute raw then raw else root </> raw))
    (\err -> if isDoesNotExistError err then pure False else ioError err)
  when symlink (ioError (userError "File operations do not follow symlink endpoints"))
  pure path

fileOperation :: Core -> Desktop -> Text -> FilePath -> Maybe FilePath -> IO (Desktop,Either Text Value)
fileOperation core desktop operation raw target=do
  root<-resolveBuildRoot desktop >>= canonicalizePath
  path<-operationPath root raw
  rejectPrivate path
  affected<-fmap catMaybes $ forM (M.toList (buffers desktop)) $ \(bid,doc) -> case documentFile doc of
    Nothing -> pure Nothing
    Just file -> do canonical<-canonicalizePath (filePath file)
                    pure (if within path canonical then Just (bid,doc,canonical) else Nothing)
  when (any (\(_,doc,_)->dirty (documentBuffer doc)) affected) (ioError (userError "Save or close dirty open buffers before changing their paths"))
  exists<-doesPathExist path
  updated<-case operation of
    "mkdir" -> do
      when exists (ioError (userError "Path already exists"))
      createDirectory path
      pure desktop
    "create_file" -> do
      when exists (ioError (userError "Path already exists"))
      saved<-saveFile (FileState path Nothing) (newBuffer "")
      either (ioError . userError) (const (pure desktop)) saved
    "delete" -> do
      unless exists (ioError (userError "Path does not exist"))
      directory<-doesDirectoryExist path
      if directory then removeDirectory path else removeFile path
      let ids=[bid | (bid,_,_)<-affected]
          keepDirectory value=if within path value then root else value
      pure desktop {buffers=foldr M.delete (buffers desktop) ids,windows=filter (maybe True (`notElem` ids) . bufferId) (windows desktop),
        defaultDirectory=fmap keepDirectory (defaultDirectory desktop),sideTree=fmap (\tree->tree {treeRoot=keepDirectory (treeRoot tree)}) (sideTree desktop)}
    _ -> do
      unless exists (ioError (userError "Path does not exist"))
      destination<-maybe (ioError (userError "rename requires to")) (operationPath root) target
      rejectPrivate destination
      destinationExists<-doesPathExist destination
      destinationLink<-catchIOError (pathIsSymbolicLink destination) (\err -> if isDoesNotExistError err then pure False else ioError err)
      when (destinationExists || destinationLink || within path destination) (ioError (userError "Rename destination exists or is inside the source"))
      renamePath path destination
      let remap oldPath | oldPath==path=destination
                        | within path oldPath=destination </> makeRelative path oldPath
                        | otherwise=oldPath
          replacements=M.fromList [(bid,newPath) | (bid,_,oldPath)<-affected,let newPath=if oldPath==path then destination else destination </> makeRelative path oldPath]
      pure $ foldr normalizeDocumentViews desktop {buffers=M.mapWithKey (\bid doc -> case M.lookup bid replacements of
        Nothing -> doc
        Just newPath -> restyle doc {documentFile=fmap (\file -> file {filePath=newPath}) (documentFile doc)}) (buffers desktop),
        defaultDirectory=fmap remap (defaultDirectory desktop),sideTree=fmap (\tree->tree {treeRoot=remap (treeRoot tree)}) (sideTree desktop)} (M.keys replacements)
  refreshed<-case sideTree updated of Nothing -> pure updated; Just tree -> catchIOError (snd <$> core updated [ReadTree (treeRoot tree)]) (\err->pure updated {status="Filesystem operation completed; tree refresh failed: "<>T.pack (show err)})
  pure (refreshed,Right (object ["operation" .= operation,"path" .= path,"to" .= target,"savedBuffers" .= False]))
  where rejectPrivate path=when (protectedPathParent desktop path) (ioError (userError "This path contains private editor configuration or session data"))

-- | A strict diff replacement prepared by a worker, with exact reply metadata.
-- No constructor or structural Eq/Show is exposed; adoption uses BufferEdits.
data PreparedPatch = PreparedPatch !Int Text !Bool PreparedEdit

-- | Narrow original source retained by the request, not an entire Desktop.
-- Each edited attempt uses this same identity/baseline; an equal-revision reload
-- while the approval waits cannot turn the request into authority for new text.
data PatchSource = PatchSource !Int !Buffer !(Maybe FileState)

-- | Capture only original target metadata. Whole-text validation belongs to
-- preparePatch's worker; current editability/privacy is checked again at adoption.
capturePatchSource :: Desktop -> Value -> Either Text PatchSource
capturePatchSource desktop args=do
  (bid,expected,patch)<-patchArguments args
  unless (T.length patch<=1048576) (Left "Diff exceeds 1 MiB characters")
  doc<-maybe (Left "Unknown bufferId") Right (M.lookup bid (buffers desktop))
  unless (not (protectedBuffer desktop bid)) (Left "This buffer contains private user or editor configuration data")
  let old=documentBuffer doc
  unless (revision old==expected) (Left "Buffer revision changed; read the buffer again")
  unless (textBuffer old && documentLabel doc==Nothing) (Left "Diff edits require an editable text buffer")
  pure (PatchSource bid old (documentFile doc))

-- | Validate wire arguments and capture only the exact original content identity.
-- The queued typed service refuses replacement before its source admission.
capturePatchRequest :: Desktop -> Value -> IO (Either Text (Int,ContentVersion,Text))
capturePatchRequest desktop args=case capturePatchSource desktop args of
  Left err->pure (Left err)
  Right (PatchSource bid old _)->case patchArguments args of
    Left err->pure (Left err)
    Right (_,_,patch)->do
      version<-captureVersion old
      pure (Right (bid,version,patch))

preparePatch :: PatchSource -> Maybe Text -> Value -> IO (Either Text PreparedPatch)
preparePatch (PatchSource target old file) original args=case patchArguments args of
  Left err->pure (Left err)
  Right (bid,expected,patch) | bid/=target || expected/=revision old->pure (Left "Diff attempt changed its original target")
                          | otherwise->case applyUnifiedDiff (contents old) patch of
    Left err->pure (Left err)
    Right (_,edits)->do
      prepared<-prepareBufferEdit bid file old edits
      modified<-evaluate (maybe False (/=patch) original)
      pure (PreparedPatch bid patch modified <$> prepared)

patchArguments :: Value -> Either Text (Int,Int,Text)
patchArguments= either (Left . T.pack) Right . parseEither
  (withObject "buffer_apply_diff" $ \o->do
    unless (all (`elem` ["bufferId","revision","diff"]) (KM.keys o)) (fail "Unknown argument")
    (,,) <$> o .: "bufferId" <*> o .: "revision" <*> o .: "diff")

-- | Recheck current editability and install exactly once through the shared
-- all-target owner. The caller has rechecked ticket lifetime, actor and policy.
commitPatch :: PreparedPatch -> Desktop -> IO (Either Text (Desktop,DiffResult))
commitPatch (PreparedPatch bid patch modified prepared) desktop=case M.lookup bid (buffers desktop) of
  Just doc | textBuffer (documentBuffer doc),documentLabel doc==Nothing->do
    adopted<-commitEdits [prepared] desktop
    pure $ do
      (updated,_)<-adopted
      let next=updated {windows=map (\w->if bufferId w==Just (bid) then w {windowHexLow=False} else w) (windows updated)}
      pure (next,DiffResult (revision (documentBuffer (buffers next M.! bid))) patch modified)
  _->pure (Left "Diff target is no longer an editable text buffer")

-- | Validate and apply a single-file unified diff with exact old/new line counts.
-- Return new text and original half-open character edits; reject all bad context
-- before changing any caller-owned state.
applyUnifiedDiff :: Text -> Text -> Either Text (Text,[(Int,Int,Text)])
applyUnifiedDiff original patch=do
  unless (T.length patch<=1048576 && not (T.any (=='\0') patch)) (Left "Diff exceeds 1 MiB characters or contains NUL")
  input<-preamble (T.lines patch)
  hunks<-parseHunks input
  unless (not (null hunks) && length hunks<=1000) (Left "Diff must contain 1..1000 hunks")
  result<-applyHunks 0 0 0 (pieces original) hunks
  unless (length (pieces (fst result))==length (pieces original)+sum [newCount-count | (_,count,_,newCount,_,_)<-hunks])
    (Left "A no-newline marker would join separate diff lines")
  pure result
  where
    preamble (a:b:rest) | "diff --git " `T.isPrefixOf` a = preamble (b:rest)
    preamble (a:rest) | "index " `T.isPrefixOf` a = preamble rest
    preamble (a:b:rest) | "--- " `T.isPrefixOf` a && "+++ " `T.isPrefixOf` b = Right rest
    preamble rest=Right rest
    parseHunks []=Right []
    parseHunks (header:rest)=do
      (start,count,newStart,newCount)<-case T.words header of
        "@@":oldRange:newRange:"@@":_ -> do
          (a,b)<-lineRange '-' oldRange
          (c,d)<-lineRange '+' newRange
          pure (a,b,c,d)
        _ -> Left "Expected a unified diff hunk header; only one file is supported"
      (before,after,remaining)<-hunk count newCount rest
      ((start,count,newStart,newCount,before,after):) <$> parseHunks remaining
    lineRange sign value=case T.uncons value of
      Just (c,body) | c==sign -> case T.splitOn "," body of
        [a] -> (,1) <$> number a
        [a,b] -> (,) <$> number a <*> number b
        _ -> Left "Invalid hunk range"
      _ -> Left "Invalid hunk range"
    number t=case readMaybe (T.unpack t) of Just n | n>=0 && n<=1048576 -> Right n; _ -> Left "Invalid hunk line number"
    hunk oldLeft newLeft rest
      | oldLeft==0 && newLeft==0=Right ([],[],rest)
      | oldLeft<0 || newLeft<0=Left "Hunk line counts do not match"
    hunk oldLeft newLeft (line:rest)=case T.uncons line of
      Just (tag,text) | tag `elem` [' ','-','+'] -> do
        let (piece,remaining)=case rest of "\\ No newline at end of file":xs -> (text,xs); _ -> (text<>"\n",rest)
        (oldLines,newLines,tailLines)<-hunk (oldLeft-if tag=='+' then 0 else 1) (newLeft-if tag=='-' then 0 else 1) remaining
        pure (if tag=='+' then oldLines else piece:oldLines,if tag=='-' then newLines else piece:newLines,tailLines)
      _ -> Left "Invalid unified diff content"
    hunk _ _ []=Left "Incomplete diff hunk"
    applyHunks _ _ _ remaining []=Right (T.concat remaining,[])
    applyHunks oldLine newLine charOffset remaining ((start,count,newStart,newCount,before,after):rest)=do
      let index=if count==0 then start else start-1
          newIndex=if newCount==0 then newStart else newStart-1
          gap=index-oldLine
          unchanged=take gap remaining
          current=drop gap remaining
      unless (gap>=0 && gap<=length remaining && newIndex==newLine+gap && take count current==before && length before==count) (Left "Diff positions or context do not match the current buffer")
      let prefix=T.concat unchanged
          removed=T.concat before
          added=T.concat after
          a=charOffset+T.length prefix
          z=a+T.length removed
      (tailText,edits)<-applyHunks (index+count) (newIndex+newCount) z (drop count current) rest
      pure (prefix<>added<>tailText,(a,z,added):edits)

pieces :: Text -> [Text]
pieces text=case T.splitOn "\n" text of
  [] -> []
  rows -> map (<>"\n") (init rows)++[last rows | not (T.null (last rows))]

searchWorkspace :: Desktop -> Text -> Bool -> Int -> Int -> IO (Either Text Value)
searchWorkspace desktop query tracked offset count=do
  root<-resolveBuildRoot desktop >>= canonicalizePath
  listed<-listFiles root tracked
  case listed of
    Left err -> pure (Left err)
    Right paths -> do
      live<-fmap M.fromList $ fmap catMaybes $ forM (M.toList (buffers desktop)) $ \(bid,doc) -> case documentFile doc of
        Just file -> do
          path<-canonicalizePath (filePath file)
          pure $ if within root path && not (protectedBuffer desktop bid) then Just (path,(bid,doc)) else Nothing
        Nothing -> pure Nothing
      let candidates=sort (nub (paths++[path | path<-M.keys live,not tracked]))
          untitled=[(Nothing,Just (bid,revision (documentBuffer doc)),contents (documentBuffer doc)) | (bid,doc)<-M.toList (buffers desktop),not tracked,documentFile doc==Nothing,textBuffer (documentBuffer doc),documentLabel doc==Nothing]
      (loaded,truncated,skipped)<-loadCandidates desktop root live 33554432 (take 10000 candidates)
      let (inputs,liveTruncated)=boundInputs 33554432 (take 10000 (loaded++untitled))
          allMatches=take 10001 (concatMap matches inputs)
          limited=take 10000 allMatches
      pure (Right (object ["root" .= root,"query" .= query,"trackedOnly" .= tracked,"includesUnsaved" .= True,
        "offset" .= offset,"matches" .= take count (drop offset limited),"total" .= length limited,
        "truncated" .= (truncated || liveTruncated || length loaded+length untitled>10000 || length candidates>10000 || length allMatches>10000),"skippedFiles" .= skipped]))
  where
    boundInputs _ []=([],False)
    boundInputs budget (entry@(_,_,text):rest)
      | T.length text>1048576 = let (others,_)=boundInputs budget rest in (others,True)
      | used>budget = ([],True)
      | otherwise = let (others,truncated)=boundInputs (budget-used) rest in (entry:others,truncated)
      where used=BS.length (TE.encodeUtf8 text)
    matches (path,live,text)=[object ["path" .= path,"bufferId" .= fmap fst live,"revision" .= fmap snd live,
      "line" .= row,"column" .= (T.length prefix+1),"text" .= T.take 1024 line,"textTruncated" .= (T.length line>1024)]
      | (row,line)<-zip [1::Int ..] (T.lines text),let (prefix,suffix)=T.breakOn query line,not (T.null suffix)]

loadCandidates :: Desktop -> FilePath -> M.Map FilePath (Int,Document) -> Int -> [FilePath] -> IO ([(Maybe FilePath,Maybe (Int,Int),Text)],Bool,Int)
loadCandidates _ _ _ _ []=pure ([],False,0)
loadCandidates _ _ _ budget _ | budget<=0=pure ([],True,0)
loadCandidates desktop root live budget (path:rest)=do
  checked<-try (checkedPath root path)
  case checked of
    Left (_::IOException) -> skip
    Right canonical | protectedPath desktop canonical -> skip
    Right canonical -> do
      loaded<-case M.lookup canonical live of
        Just (bid,doc) | textBuffer (documentBuffer doc) -> pure (Just (Just (bid,revision (documentBuffer doc)),contents (documentBuffer doc)))
                      | otherwise -> pure Nothing
        Nothing -> do
          bytes<-(try (withBinaryFile canonical ReadMode (\h -> BS.hGet h 1048577)) :: IO (Either IOException BS.ByteString))
          pure $ case bytes of
            Right value | BS.length value<=1048576,not (BS.elem 0 value),Right text<-TE.decodeUtf8' value -> Just (Nothing,text)
            _ -> Nothing
      case loaded of
        Nothing -> skip
        Just (ident,text) | T.length text>1048576 -> skip
                         | otherwise -> do
          let used=BS.length (TE.encodeUtf8 text)
          if used>budget then pure ([],True,0) else do
            (others,truncated,skipped)<-loadCandidates desktop root live (budget-used) rest
            pure ((Just canonical,ident,text):others,truncated,skipped)
  where skip=do (others,truncated,skipped)<-loadCandidates desktop root live budget rest
                pure (others,truncated,skipped+1)

listFiles :: FilePath -> Bool -> IO (Either Text [FilePath])
listFiles root tracked=do
  ripgrep<-findExecutable "rg"
  let usingGit=tracked || ripgrep==Nothing
      command=if usingGit then proc "git" (["ls-files","--cached"]++(if tracked then [] else ["--others","--exclude-standard"])++["-z","--"])
        else proc "rg" ["--files","--null","--glob","!.git","--"]
  bracket (do
      handles@(_,_,_,process)<-createProcess command {cwd=Just root,std_in=NoStream,std_out=CreatePipe,std_err=NoStream,create_group=True}
      cleanup<-processCleanup process
      pure (handles,cleanup)) (\((_,out,_,_),cleanup)->cleanup >> mapM_ (\h->catchIOError (hClose h) (const (pure ()))) out) $ \((_,stdoutHandle,_,process),_) -> case stdoutHandle of
    Nothing -> pure (Left "Could not read workspace file listing")
    Just handle -> do
      result<-timeout 10000000 $ do
        bytes<-BS.hGet handle 2097153
        if BS.length bytes>2097152 then pure (Left "Workspace file listing exceeds 2 MiB") else do
          code<-waitForProcess process
          pure $ if code/=ExitSuccess && (usingGit || code/=ExitFailure 1) then Left "Workspace listing failed; install ripgrep, or use trackedOnly in a Git repository"
            else case TE.decodeUtf8' bytes of
              Left _ -> Left "Workspace contains non-UTF-8 file names"
              Right text -> Right (take 10001 [root </> T.unpack path | path<-T.splitOn "\0" text,not (T.null path)])
      pure (fromMaybe (Left "Workspace file listing timed out") result)
