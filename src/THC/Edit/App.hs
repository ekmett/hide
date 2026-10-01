{-# LANGUAGE CPP, OverloadedStrings #-}
module THC.Edit.App (main, demoDesktop, applyEffects) where

import Control.Exception (bracket, finally, catch, AsyncException(UserInterrupt), throwIO)
import Control.Concurrent (myThreadId, throwTo)
#ifndef mingw32_HOST_OS
import System.Posix.Signals (installHandler, Handler(Catch), sigTERM, sigHUP)
#endif
import Data.Aeson (object, (.=))
import THC.Edit.Protocol (WirePacket(..))
import THC.Edit.EditorMCP (runEditorMCP)
import THC.Edit.Session
import THC.Edit.RemoteTerminal (runRemoteTerminal)
import Text.Read (readMaybe)
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad (foldM, when)
import System.IO (hFlush, stdout, stdin, hIsTerminalDevice)
import THC.Edit.Debugger
import THC.Edit.Conversation
import THC.Edit.Tooling
import THC.Edit.GitOperations
import qualified Data.Map.Strict as M
import Data.List (find, isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Graphics.Vty as V
import System.Console.GetOpt
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getHomeDirectory, setCurrentDirectory, listDirectory)
import System.FilePath ((</>), isAbsolute, takeDirectory, takeFileName, takeExtension)
import Control.Exception (try, IOException)
import Paths_thc_edit (getDataFileName)
import THC.Edit.Browser
import THC.Edit.Markdown (renderMarkdown)
import THC.Edit.Git
import System.Environment (getArgs, lookupEnv, setEnv)
import THC.Edit.Frontend
import THC.Edit.Remote
import THC.Edit.RemoteWindow (runRemoteWindow)
import THC.Edit.RemoteWeb (runRemoteWeb)
import System.Exit (die)
import THC.Edit.Buffer
import THC.Edit.Model
import THC.Edit.Render
import THC.Edit.Files
import THC.Edit.Reconcile

data Option = MCPBridge String | Resume (Maybe String) | SSH String | RemoteSession String | RemoteDaemon String | Use Backend | Scale String | Size String | Mode String | ColorMode String | Demo | CRT | MaterialIcons | WordStar | Snapshot | Html | Scene String | Usage deriving Eq
options :: [OptDescr Option]
options = [Option [] ["appearance"] (ReqArg ColorMode "light|dark|system") "Document/terminal colors (default THC_EDIT_APPEARANCE or system)"
          ,Option [] ["metal"] (NoArg (Use Metal)) "Open a Metal window"
          ,Option [] ["vulkan"] (NoArg (Use Vulkan)) "Open a Vulkan window"
          ,Option [] ["remote"] (NoArg (Use Remote)) "Serve the remote editing protocol on stdin/stdout"
          ,Option [] ["ssh"] (ReqArg SSH "HOST") "Connect to a remote thc-edit (also HOST:PATH)"
          ,Option [] ["resume"] (OptArg Resume "ID") "Resume an unfinished editor session (choose if several exist)"
          ,Option [] ["remote-session"] (ReqArg RemoteSession "ID") "Reattach to an existing remote session"
          ,Option [] ["web"] (NoArg (Use Web)) "Open the local WebGL browser frontend"
          ,Option [] ["window"] (NoArg (Use Auto)) "Open a window using the platform backend"
          ,Option [] ["terminal"] (NoArg (Use Terminal)) "Use the terminal (override THC_EDIT_BACKEND)"
          ,Option [] ["crt"] (NoArg CRT) "Enable CRT scanlines and vignetting (window only)"
          ,Option [] ["material-icons"] (NoArg MaterialIcons) "Use Material folder icons (terminal requires a compatible Nerd Font)"
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
  appearanceDefault<-lookupEnv "THC_EDIT_APPEARANCE"
  terminalColors<-lookupEnv "COLORFGBG"
  let (flags,paths,errors)=getOpt Permute (options++[Option [] ["mcp-editor"] (ReqArg MCPBridge "ID") "Internal editor introspection bridge",Option [] ["remote-daemon"] (ReqArg RemoteDaemon "ID") "Internal remote session process"]) (resumeArguments args)
  if not (null errors) then die (concat errors)
  else if Usage `elem` flags then putStr (usageInfo "Usage: thc-edit [OPTIONS] [--] [FILE.hs ...]\n\nTurbo Haskell source editor.\nF2 Save, F3 Open, F10 Menu, Alt+X Exit.\n" options)
  else if [ident | MCPBridge ident<-flags]/=[] then case flags of
    [MCPBridge ident] | null paths -> runEditorMCP ident
    _ -> die "--mcp-editor accepts only a session ID."
  else do
    daemon <- case [sid | RemoteDaemon sid<-flags] of []->pure Nothing; [sid]->pure (Just sid); _->die "Specify --remote-daemon once."
    resume <- case [ident | Resume ident<-flags] of
      [] -> pure Nothing
      [ident] -> do
        when (not (null paths) || any isSSH flags || any isSession flags || daemon/=Nothing || Use Remote `elem` flags || Snapshot `elem` flags || Html `elem` flags) (die "--resume cannot be combined with paths, --ssh, --remote, or snapshots.")
        Just <$> chooseSession ident
      _ -> die "Specify --resume only once."
    let serving=Use Remote `elem` flags || daemon/=Nothing || (null [b | Use b<-flags] && backendDefault==Just "remote")
    when (resume/=Nothing && serving) (die "--resume requires a display backend, not --remote.")
    target <- if serving then pure Nothing else case ([host | SSH host<-flags],paths) of
      ([],[path]) | Just remote<-parseRemoteTarget path -> pure (Just remote)
      ([],_) | any (maybe False (const True) . parseRemoteTarget) paths -> die "Open one remote project at a time; do not mix local and remote paths."
             | otherwise -> pure Nothing
      ([host],[]) -> pure (Just (host,"."))
      ([host],[path]) -> pure (Just (host,path))
      _ -> die "Specify one SSH host and one remote file or project path."
    backend <- if daemon/=Nothing then pure Remote else either die pure (chooseBackend backendDefault [b | Use b<-flags])
    when (target/=Nothing && (Snapshot `elem` flags || Html `elem` flags)) (die "Snapshots require local paths.")
    when (length [sid | RemoteSession sid<-flags]>1) (die "Specify --remote-session once.")
    when (target==Nothing && any isSession flags) (die "--remote-session requires HOST:PATH or --ssh HOST.")
    when (serving && (Snapshot `elem` flags || Html `elem` flags || any isSSH flags)) (die "--remote cannot be combined with snapshots or --ssh.")
    colorMode <- case [s | ColorMode s<-flags] of
      [] -> parseAppearance (maybe "system" id appearanceDefault)
      [s] -> parseAppearance s
      _ -> die "Specify --appearance only once."
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
    if backend==Remote && daemon==Nothing then runRemoteRelay (remoteArguments flags paths)
    else if daemon==Nothing && Snapshot `notElem` flags && Html `notElem` flags then do
#ifndef WITH_WINDOW
      when (backend `elem` [Auto,Metal,Vulkan]) (die "Graphical support is not built. Rebuild with -fwindow.")
#endif
#ifndef WITH_WEB
      when (backend==Web) (die "Browser support is not built. Rebuild with -fweb.")
#endif
      record <- case resume of
        Just saved -> pure saved
        Nothing -> do
          let host=fmap fst target
              arguments=remoteArguments (filter (not . isSession) flags) (maybe paths (pure . snd) target)
          fresh<-newSessionRecord host arguments
          pure $ case [ident | RemoteSession ident<-flags] of
            [ident] -> fresh {sessionId=ident}
            _ -> fresh
      wasInterrupted<-newIORef False
      let reattach=resume/=Nothing || any isSession flags
          attach=case sessionHost record of
            Nothing -> withLocalPeer (sessionId record) reattach (sessionArguments record)
            Just host -> withSSHSession host (sessionId record) reattach (sessionArguments record)
          display peer = do
            peerSend peer (JsonPacket (object ["type" .= ("frontend"::T.Text),"mode" .= (if backend==Terminal then Nothing else Just screenMode)]))
            case backend of
              Terminal -> runRemoteTerminal peer
              Web -> runRemoteWeb scale (maybe "" id (sessionHost record)) peer
              _ -> runRemoteWindow backend scale dimensions screenMode (maybe "" id (sessionHost record)) peer
          report = do
            saved<-loadSession (sessionId record)
            detached<-readIORef wasInterrupted
            when (saved/=Nothing || detached) $ putStrLn ("Session: "++sessionId record++"\nResume: thc-edit --resume "++sessionId record) >> hFlush stdout
      -- A local session starts beside the project that created it. Reattachment
      -- needs only its endpoint, so a removed/renamed working directory is fine.
      (withDetachSignals (attach display) `catch` (\err -> writeIORef wasInterrupted True >> interrupted err)) `finally` report
    else do
        let initial=if Demo `elem` flags then addDocument Nothing (newBuffer (activeText demoDesktop)) (initialDesktop dimensions) else initialDesktop dimensions
            configured=(fst (handleEvent (uncurry V.EvResize dimensions) initial)) {appearance=colorMode,systemDark=maybe True (not . (`elem` ["7","15"]) . reverse . takeWhile (/=';') . reverse) terminalColors,wordStar=WordStar `elem` flags,crtFilter=CRT `elem` flags,materialIcons=MaterialIcons `elem` flags,videoMode=if backend == Terminal then Nothing else Just screenMode}
        localPaths<-if daemon/=Nothing then mapM expandRemoteHome paths else pure paths
        (_,loaded)<-applyEffects configured (map ReadPath localPaths)
        cwd<-getCurrentDirectory
        base<-packageDirectory cwd
        (_,browsing)<-if sideTree loaded/=Nothing || Demo `elem` flags || Snapshot `elem` flags || Html `elem` flags then pure (False,loaded)
          else applyEffects loaded [if null paths then ReadPath base else ReadTree base]
        let focused=browsing {sideTree=fmap (\tree -> tree {treeFocused=null (windows browsing)}) (sideTree browsing)}
        (_,withGit)<-applyEffects focused [RefreshGit (startingDirectory focused)]
        staged<-foldM stageScene withGit [scene | Scene scene<-flags]
        if Html `elem` flags then TIO.putStr (snapshotHtml staged)
        else if Snapshot `elem` flags then TIO.putStr (snapshot staged)
        else do
          mapM_ (setEnv "THC_EDIT_SESSION") daemon
          withDebugger $ \debugger -> withConversation $ \conversation -> withTooling $ \tooling -> withGitOperations $ \gitOperations -> withReconciliation $ \reconciliation -> do
            let effects=gitOperationEffects gitOperations (debuggerEffects debugger (conversationEffects conversation (reconciliationEffects reconciliation (toolingEffects tooling applyEffects))))
                tick d=tickGitOperations gitOperations applyEffects d >>= tickTooling tooling applyEffects >>= tickReconciliation reconciliation >>= tickConversation conversation >>= tickDebugger debugger (toolingEffects tooling applyEffects)
            case daemon of
              Just sid -> runRemoteDaemon sid scale effects tick staged
              Nothing -> die "Missing session process identity."
  where
    parseAppearance s = maybe (die "Appearance must be light, dark, or system.") pure (lookup s [("light",LightMode),("dark",DarkMode),("system",SystemMode)])
    isMode Mode{} = True
    isMode _ = False
    isSession RemoteSession{} = True
    isSession _ = False
    isSSH SSH{} = True
    isSSH _ = False

-- GetOpt optional arguments normally require '='. Also accept --resume ID.
resumeArguments :: [String] -> [String]
resumeArguments ("--":rest)="--":rest
resumeArguments ("--resume":ident:rest) | not ("-" `isPrefixOf` ident) = ("--resume="++ident):resumeArguments rest
resumeArguments (arg:rest)=arg:resumeArguments rest
resumeArguments []=[]

chooseSession :: Maybe String -> IO SessionRecord
chooseSession wanted = do
  sessions<-listSessions
  case wanted of
    Just ident -> case filter (isPrefixOf ident . sessionId) sessions of
      [record] | not (null ident) -> pure record
      [] -> die ("No unfinished session matches "++ident++".")
      _ -> die "Session ID is ambiguous; use a longer ID."
    Nothing -> case sessions of
      [] -> die "No unfinished editor sessions."
      [record] -> pure record
      _ -> do
        putStrLn "Unfinished editor sessions:"
        mapM_ (\(n,record) -> putStrLn (show n++") "++sessionId record++"  "++maybe "local" id (sessionHost record)++"  "++sessionDirectory record)) (zip [1::Int ..] sessions)
        interactive<-hIsTerminalDevice stdin
        if not interactive then die "Choose one with --resume ID."
        else do
          putStr "Resume session number (empty to cancel): "; hFlush stdout
          answer<-getLine
          case readMaybe answer of
            Just n | n>=1, record:_<-drop (n-1) sessions -> pure record
            _ -> die "No session selected."

interrupted :: AsyncException -> IO ()
interrupted UserInterrupt=pure ()
interrupted other=throwIO other

withDetachSignals :: IO a -> IO a
#ifdef mingw32_HOST_OS
withDetachSignals = id
#else
withDetachSignals action = do
  thread<-myThreadId
  let install signal=installHandler signal (Catch (throwTo thread UserInterrupt)) Nothing
      restore signal handler=installHandler signal handler Nothing >> pure ()
  bracket (install sigTERM) (restore sigTERM) $ \_ ->
    bracket (install sigHUP) (restore sigHUP) (const action)
#endif

-- Rebuild arguments rather than reinterpreting their shell spelling. Paths after
-- -- are passed verbatim to the remote process, including spaces and metacharacters.
remoteArguments :: [Option] -> [FilePath] -> [String]
remoteArguments flags paths = concatMap option flags++["--"]++paths
  where
    option (Scale s)=["--scale",s]
    option (Size s)=["--size",s]
    option (Mode s)=["--mode",s]
    option (ColorMode s)=["--appearance",s]
    option (RemoteSession s)=["--remote-session",s]
    option (Scene s)=["--scene",s]
    option Demo=["--demo"]
    option CRT=["--crt"]
    option MaterialIcons=["--material-icons"]
    option WordStar=["--wordstar"]
    option _=[]

expandRemoteHome :: FilePath -> IO FilePath
expandRemoteHome "~" = getHomeDirectory
expandRemoteHome ('~':'/':path) = (</> path) <$> getHomeDirectory
expandRemoteHome path = pure path

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
    apply (_,d) DownloadDocument{}=pure (False,d)
    apply (_,d) ReadBrowserClipboard=pure (False,d)
    apply (_,d) WriteBrowserClipboard{}=pure (False,d)
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
      pure (False,case result of Left err -> message "Cannot open Help" (wrapMessage (T.pack (show err))) d; Right text -> addHelpStyled (renderMarkdown (max 20 (min 76 (fst (screenSize d)-treeWidthOf d-4))) text) d)
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
