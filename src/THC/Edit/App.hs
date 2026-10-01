{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.App (main, demoDesktop, applyEffects) where

import Control.Exception (bracket)
import Control.Monad (foldM, when)
import System.Timeout (timeout)
import System.IO (hFlush, stdout)
import THC.Edit.Debugger
import THC.Edit.Conversation
import THC.Edit.Tooling
import THC.Edit.GitOperations
import qualified Data.Map.Strict as M
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Graphics.Vty.CrossPlatform (mkVty)
import qualified Graphics.Vty as V
import System.Console.GetOpt
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, setCurrentDirectory, listDirectory)
import System.FilePath ((</>), isAbsolute, takeDirectory, takeFileName, takeExtension)
import Control.Exception (try, IOException)
import Paths_thc_edit (getDataFileName)
import THC.Edit.Browser
import THC.Edit.Help
import THC.Edit.Git
import System.Environment (getArgs, lookupEnv)
import THC.Edit.Frontend
import THC.Edit.Window (runWindow)
import System.Exit (die)
import THC.Edit.Buffer
import THC.Edit.Model
import THC.Edit.Unicode (updatePicture)
import THC.Edit.Render
import THC.Edit.Files
import THC.Edit.Reconcile

data Option = Use Backend | Scale String | Size String | Mode String | Demo | CRT | WordStar | Snapshot | Html | Scene String | Usage deriving Eq
options :: [OptDescr Option]
options = [Option [] ["metal"] (NoArg (Use Metal)) "Open a Metal window"
          ,Option [] ["vulkan"] (NoArg (Use Vulkan)) "Open a Vulkan window"
          ,Option [] ["window"] (NoArg (Use Auto)) "Open a window using the platform backend"
          ,Option [] ["terminal"] (NoArg (Use Terminal)) "Use the terminal (override THC_EDIT_BACKEND)"
          ,Option [] ["crt"] (NoArg CRT) "Enable CRT scanlines and vignetting (window only)"
          ,Option [] ["scale"] (ReqArg Scale "FACTOR") "Window pixel scale, 1 to 8 in 1/8 steps (default THC_EDIT_SCALE or display density)"
          ,Option [] ["mode"] (ReqArg Mode "NUMBER") "Window screen mode: 3 (80x25), 259 (80x50); default 3"
          ,Option [] ["vga50"] (NoArg (Mode "259")) "Alias for --mode 259 (window only)"
          ,Option [] ["size"] (ReqArg Size "COLSxROWS") "Initial character dimensions (override --mode dimensions)"
          ,Option [] ["demo"] (NoArg Demo) "Open a sample Haskell buffer"
          ,Option [] ["wordstar"] (NoArg WordStar) "Use WordStar editing keys"
          ,Option [] ["snapshot"] (NoArg Snapshot) "Print an 80x25 text snapshot and exit"
          ,Option [] ["snapshot-html"] (NoArg Html) "Print an HTML preview of actual Vty output and exit"
          ,Option [] ["scene"] (ReqArg Scene "desktop|menu|about|gallery|split|open|tree|help|diff|preferences") "Preview scene (with --demo)"
          ,Option ['h'] ["help"] (NoArg Usage) "Show help"]

main :: IO ()
main = do
  args<-getArgs
  backendDefault<-lookupEnv "THC_EDIT_BACKEND"
  scaleDefault<-lookupEnv "THC_EDIT_SCALE"
  let (flags,paths,errors)=getOpt Permute options args
  if not (null errors) then die (concat errors)
  else if Usage `elem` flags then putStr (usageInfo "Usage: thc-edit [OPTIONS] [--] [FILE.hs ...]\n\nTurbo Haskell source editor.\nF2 Save, F3 Open, F10 Menu, Alt+X Exit.\n" options)
  else do
    backend <- either die pure (chooseBackend backendDefault [b | Use b <- flags])
    screenMode <- case [s | Mode s <- flags] of
      [] -> pure 3
      [s] -> either die pure (parseScreenMode s)
      _ -> die "Specify --mode or --vga50 only once."
    when (backend == Terminal && any isMode flags && Snapshot `notElem` flags && Html `notElem` flags) $
      die "--mode/--vga50 requires --window, --metal or --vulkan; terminal size is controlled by your terminal."
    scale <- either die pure (chooseScale scaleDefault [s | Scale s <- flags])
    dimensions <- case [s | Size s <- flags] of
      [] -> pure (modeSize screenMode)
      [s] -> either die pure (parseWindowSize s)
      _ -> die "Specify --size only once."
    let initial=if Demo `elem` flags then addDocument Nothing (newBuffer (activeText demoDesktop)) (initialDesktop dimensions) else initialDesktop dimensions
        configured=(fst (handleEvent (uncurry V.EvResize dimensions) initial)) {wordStar=WordStar `elem` flags,crtFilter=CRT `elem` flags,videoMode=if backend == Terminal then Nothing else Just screenMode}
    (_,loaded)<-applyEffects configured (map ReadPath paths)
    cwd<-getCurrentDirectory
    base<-packageDirectory cwd
    (_,browsing)<-if sideTree loaded/=Nothing || Demo `elem` flags || Snapshot `elem` flags || Html `elem` flags then pure (False,loaded)
      else applyEffects loaded [if null paths then ReadPath base else ReadTree base]
    let focused=browsing {sideTree=fmap (\tree -> tree {treeFocused=null (windows browsing)}) (sideTree browsing)}
    (_,withGit)<-applyEffects focused [RefreshGit (startingDirectory focused)]
    staged<-foldM stageScene withGit [scene | Scene scene<-flags]
    if Html `elem` flags then TIO.putStr (snapshotHtml staged)
    else if Snapshot `elem` flags then TIO.putStr (snapshot staged)
    else withDebugger $ \debugger -> withConversation $ \conversation -> withTooling $ \tooling -> withGitOperations $ \gitOperations -> withReconciliation $ \reconciliation -> do
      let effects=gitOperationEffects gitOperations (debuggerEffects debugger (conversationEffects conversation (reconciliationEffects reconciliation (toolingEffects tooling applyEffects))))
          tick d=tickGitOperations gitOperations applyEffects d >>= tickTooling tooling applyEffects >>= tickReconciliation reconciliation >>= tickConversation conversation >>= tickDebugger debugger (toolingEffects tooling applyEffects)
      if backend /= Terminal then runWindow backend scale effects tick staged
      else bracket (mkVty V.defaultConfig) (\vty -> V.shutdown vty >> cursorStyle Nothing) $ \vty -> do
        when (V.supportsMode (V.outputIface vty) V.Mouse) (V.setMode (V.outputIface vty) V.Mouse True)
        when (V.supportsMode (V.outputIface vty) V.BracketedPaste) (V.setMode (V.outputIface vty) V.BracketedPaste True)
        size<-V.displayBounds (V.outputIface vty)
        cursorStyle (Just (blinkCursor staged))
        loop effects tick vty (fst (handleEvent (uncurry V.EvResize size) staged))
  where
    isMode Mode{} = True
    isMode _ = False

stageScene :: Desktop -> String -> IO Desktop
stageScene d scene = case lookup scene [("open",Open),("tree",ToggleTree),("help",Help),("diff",GitDiff)] of
  Just cmd -> snd <$> uncurry applyEffects (runCommand cmd d)
  Nothing -> pure (setScene d scene)

setScene :: Desktop -> String -> Desktop
setScene d scene=case scene of
  "desktop" -> d
  "menu" -> d {menu=Just (0,1)}
  "about" -> fst (runCommand About d)
  "gallery" -> fst (runCommand Gallery d)
  "preferences" -> fst (runCommand EditorOptions d)
  "split" -> fst (runCommand SplitHorizontal d)
  _ -> message "Unknown preview scene" [T.pack scene] d

demoDesktop :: Desktop
demoDesktop = addDocument Nothing (newBuffer sample) (initialDesktop (80,25))
  where sample=T.unlines ["module Main where","", "factorial :: Integer -> Integer", "factorial n = product [1 .. n]", "", "main :: IO ()", "main = do", "  putStrLn \"Enter a number:\"", "  input <- getLine", "  print (factorial (read input))"]

loop :: (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> V.Vty -> Desktop -> IO ()
loop effects tick vty d = do
  updatePicture vty (renderDesktop d)
  event<-timeout 100000 (V.nextEvent vty)
  let (next,requests)=maybe (d,[]) (`handleEvent` d) event
  (exit,updated)<-effects next requests
  when (blinkCursor updated /= blinkCursor d) (cursorStyle (Just (blinkCursor updated)))
  if exit then pure () else tick updated >>= loop effects tick vty

-- DECSCUSR leaves the terminal responsible for its cursor cadence.
cursorStyle :: Maybe Bool -> IO ()
cursorStyle blinking = putStr (case blinking of Just True -> "\ESC[3 q"; Just False -> "\ESC[4 q"; Nothing -> "\ESC[0 q") >> hFlush stdout

applyEffects :: Desktop -> [Effect] -> IO (Bool,Desktop)
applyEffects = foldM apply . (False,)
  where
    apply state@(True,_) _=pure state
    apply (_,d) LanguageRequest{}=pure (False,d {status="Language tools are unavailable in this preview."})
    apply (_,d) JumpTo{}=pure (False,d)
    apply (_,d) RunGit{}=pure (False,d {status="Git operations are unavailable in this preview."})
    apply (_,d) ReadMergeBranches=pure (False,d {status="Git operations are unavailable in this preview."})
    apply (_,d) ReviewExternal=pure (False,d {status="Disk change monitoring is unavailable in this preview."})
    apply (_,d) ResolveConflict{}=pure (False,d {status="Disk change monitoring is unavailable in this preview."})
    apply (_,d) DebugAction{}=pure (False,d {status="Debugger unavailable in this preview."})
    apply (_,d) AgentAction{}=pure (False,d {status="Agents are unavailable in this preview."})
    apply (_,d) Exit=pure (True,d)
    apply (_,d) SetScreenMode{}=pure (False,d {status="Screen modes are available in a graphical window."})
    apply (_,d) (ReadPath path)=do
      directory<-doesDirectoryExist path
      if directory then do
        (_,browsed)<-apply (False,d) (ReadTree path)
        package<-packageFile path
        maybe (pure (False,browsed)) (\file -> apply (False,browsed) (ReadPath file)) package
      else do
        result<-loadFile path
        let opened=case result of
              Left err->message "Cannot open file" (wrapMessage (T.pack err)) d
              Right (file,b)->case find (\(_,doc)->fmap filePath (documentFile doc)==Just (filePath file)) (M.toList (buffers d)) of
                Just (bid,_)->maybe d (\w->focusWindow (windowId w) d) (find ((==bid).bufferId) (windows d))
                Nothing->addDocument (Just file) b d
        apply (False,opened) (RefreshGit (startingDirectory opened))
    apply (_,d) (BrowsePath path pattern)=do
      result<-readDirectory path pattern
      pure (False,case result of Left err -> browserError (T.pack err) d; Right (base,entries) -> openBrowser base pattern entries d)
    apply (_,d) (BrowseDirectories path)=do
      result<-readDirectory path "*"
      pure (False,case result of Left err -> browserError (T.pack err) d; Right (base,entries) -> openDirectoryBrowser base entries d)
    apply (_,d) (ChangeDirectory path)=do
      result<-readDirectory path "*"
      case result of
        Left err -> pure (False,browserError (T.pack err) d)
        Right (base,entries) -> do
          changed<-try (setCurrentDirectory base) :: IO (Either IOException ())
          case changed of
            Left err -> pure (False,browserError (T.pack (show err)) d)
            Right () -> apply (False,installTree base entries d {defaultDirectory=Just base,dialog=Nothing,status="Directory changed."}) (RefreshGit base)
    apply (_,d) (OpenChoice base input pattern)=do
      let chosen=if T.null input then pattern else input
          path=if isAbsolute (T.unpack chosen) then T.unpack chosen else base </> T.unpack chosen
      directory<-doesDirectoryExist path
      if directory then apply (False,d) (BrowsePath path pattern)
      else if T.any (`elem` ("*?" :: String)) chosen then apply (False,d) (BrowsePath (takeDirectory path) (T.pack (takeFileName path)))
      else do
        exists<-doesFileExist path
        if exists then apply (False,d {dialog=Nothing}) (ReadPath path)
        else pure (False,browserError "File not found." d)
    apply (_,d) (ReadTree path)=do
      result<-readDirectory path "*"
      case result of
        Left err -> pure (False,message "Cannot browse directory" (wrapMessage (T.pack err)) d)
        Right (base,entries) -> apply (False,installTree base entries d) (RefreshGit base)
    apply (_,d) (ExpandTree index)=case sideTree d of
      Just tree | node:_ <- drop index (treeRows tree) -> do
        result<-readDirectory (nodePath node) "*"
        pure (False,case result of Left err -> d {status=T.pack err}; Right (_,entries) -> expandTree index entries d)
      _ -> pure (False,d)
    apply (_,d) ReadHelp=do
      path<-getDataFileName "README.md"
      result<-try (TIO.readFile path) :: IO (Either IOException T.Text)
      pure (False,case result of Left err -> message "Cannot open Help" (wrapMessage (T.pack (show err))) d; Right text -> addHelp (layoutMarkdown (max 20 (min 76 (fst (screenSize d)-treeWidthOf d-4))) text) d)
    apply (_,d) (RefreshGit path)=do
      repo<-repositoryStatus path
      pure (False,d {branchStatus=maybe "" (\r -> repoBranch r <> if repoDirty r then "*" else "") repo,branchAdded=maybe 0 repoAdded repo,branchDeleted=maybe 0 repoDeleted repo,branchRoot=fmap repoRoot repo,gitReview=case gitReview d of Just review | fmap repoRoot repo == Just (reviewRoot review) -> Just review; _ -> Nothing})
    apply (_,d) ReadGitDiff=do
      reviewed<-reviewRepository (gitDirectory d)
      case reviewed of
        Left err -> pure (False,message "Cannot review changes" (wrapMessage err) d)
        Right review -> apply (False,(addReadOnly "Git diff" (reviewText review) d) {gitReview=Just review,status="Review saved changes; Tools > Approve changes commits them."}) (RefreshGit (reviewRoot review))
    apply (_,d) AskGitCommit
      | any (dirty . documentBuffer) (M.elems (buffers d)) = pure (False,message "Unsaved changes" ["Save changed buffers before approving a commit."] d)
      | Just review <- gitReview d = pure (False,d {dialog=Just (Dialog "Approve changes" Committing [Input "Commit message" "" 0] 0 ["Commit","Cancel"] ["Commit all reviewed saved changes in:",T.pack (reviewRoot review)]),menu=Nothing})
      | otherwise = apply (False,d) ReadGitDiff
    apply (_,d) (WriteGitCommit text)=case gitReview d of
      Nothing -> pure (False,message "Review required" ["Open Tools > Git diff before approving changes."] d)
      Just review -> do
        result<-commitReview review text
        case result of
          Left err -> pure (False,d {status=err,dialog=fmap (\dg -> dg {body=wrapMessage err}) (dialog d)})
          Right summary -> do
            (_,updated)<-apply (False,d {dialog=Nothing}) ReadGitDiff
            pure (False,updated {gitReview=Nothing,status=T.takeWhile (/='\n') summary})
    apply (_,d) (SaveDocument bid target after)=case M.lookup bid (buffers d) of
      Nothing->pure (False,d)
      Just doc->do
        result<-case target of
          Nothing->case documentFile doc of
            Nothing->pure (Left "Choose a filename with Save as.")
            Just file->saveFile file (documentBuffer doc)
          Just path->do
            -- loadFile resolves symbolic links and reports encoding/permission errors.
            loaded<-loadFile path
            case loaded of
              Left err->pure (Left err)
              Right (file,_)->case documentFile doc of
                Just old | filePath old==filePath file->saveFile old (documentBuffer doc)
                _ | diskBytes file/=Nothing->pure (Left "Save as will not overwrite an existing file. Open it first or choose a new name.")
                  | otherwise->saveFile file (documentBuffer doc)
        case result of
          Left err->pure (False,message "Cannot save file" (wrapMessage (T.pack err)) d)
          Right file->do
            let b=documentBuffer doc
                clean=restyle doc {documentFile=Just file,documentBuffer=markSaved b}
                updated=d {buffers=M.insert bid clean (buffers d),status="File saved."}
            (_,refreshed)<-apply (False,updated) (RefreshGit (takeDirectory (filePath file)))
            case after of
              Nothing->pure (False,refreshed)
              Just cmd->uncurry applyEffects (runCommand cmd refreshed)


browserError :: T.Text -> Desktop -> Desktop
browserError err d = case dialog d of
  Just dg -> d {dialog=Just dg {body=take 1 (body dg) ++ [T.take 54 err]},status=err}
  Nothing -> message "Cannot open directory" (wrapMessage err) d

gitDirectory :: Desktop -> FilePath
gitDirectory d = case activeDocument d >>= documentFile of
  Just file -> takeDirectory (filePath file)
  Nothing -> maybe (startingDirectory d) reviewRoot (gitReview d)

-- Start with the nearest enclosing Cabal package, without invoking a build.
packageDirectory :: FilePath -> IO FilePath
packageDirectory start = search start
  where
    search path = do
      entries<-either (const []) id <$> (try (listDirectory path) :: IO (Either IOException [FilePath]))
      if any ((==".cabal") . takeExtension) entries then pure path
        else if takeDirectory path==path then pure start else search (takeDirectory path)
