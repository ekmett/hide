{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.App (main, demoDesktop, applyEffects) where

import Control.Exception (bracket)
import Control.Monad (foldM, when)
import qualified Data.Map.Strict as M
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Graphics.Vty.CrossPlatform (mkVty)
import qualified Graphics.Vty as V
import System.Console.GetOpt
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), isAbsolute, takeDirectory, takeFileName)
import Control.Exception (try, IOException)
import Paths_thc_edit (getDataFileName)
import THC.Edit.Browser
import THC.Edit.Help
import THC.Edit.Git
import System.Environment (getArgs, lookupEnv)
import Text.Read (readMaybe)
import THC.Edit.Frontend
import THC.Edit.Window (runWindow)
import System.Exit (die)
import THC.Edit.Buffer
import THC.Edit.Model
import THC.Edit.Render
import THC.Edit.Files

data Option = Use Backend | Scale String | Size String | Demo | WordStar | Snapshot | Html | Scene String | Usage deriving Eq
options :: [OptDescr Option]
options = [Option [] ["metal"] (NoArg (Use Metal)) "Open a Metal window"
          ,Option [] ["vulkan"] (NoArg (Use Vulkan)) "Open a Vulkan window"
          ,Option [] ["window"] (NoArg (Use Auto)) "Open a window using the platform backend"
          ,Option [] ["terminal"] (NoArg (Use Terminal)) "Use the terminal (override THC_EDIT_BACKEND)"
          ,Option [] ["scale"] (ReqArg Scale "N") "Integer window pixel scale, 1 to 8"
          ,Option [] ["size"] (ReqArg Size "COLSxROWS") "Initial window dimensions in characters (default 80x25)"
          ,Option [] ["demo"] (NoArg Demo) "Open a sample Haskell buffer"
          ,Option [] ["wordstar"] (NoArg WordStar) "Use WordStar editing keys"
          ,Option [] ["snapshot"] (NoArg Snapshot) "Print an 80x25 text snapshot and exit"
          ,Option [] ["snapshot-html"] (NoArg Html) "Print an HTML preview of actual Vty output and exit"
          ,Option [] ["scene"] (ReqArg Scene "desktop|menu|about|gallery|split|open|tree|help|diff") "Preview scene (with --demo)"
          ,Option ['h'] ["help"] (NoArg Usage) "Show help"]

main :: IO ()
main = do
  args<-getArgs
  backendDefault<-lookupEnv "THC_EDIT_BACKEND"
  let (flags,paths,errors)=getOpt Permute options args
  if not (null errors) then die (concat errors)
  else if Usage `elem` flags then putStr (usageInfo "Usage: thc-edit [OPTIONS] [--] [FILE.hs ...]\n\nTurbo Haskell source editor.\nF2 Save, F3 Open, F10 Menu, Alt+X Exit.\n" options)
  else do
    backend <- either die pure (chooseBackend backendDefault [b | Use b <- flags])
    scale <- case [s | Scale s <- flags] of
      [] -> pure 0
      [s] | Just n <- readMaybe s, n >= 1, n <= 8 -> pure n
      _ -> die "--scale needs one integer from 1 to 8."
    dimensions <- case [s | Size s <- flags] of
      [] -> pure (80,25)
      [s] -> either die pure (parseWindowSize s)
      _ -> die "Specify --size only once."
    let initial=if Demo `elem` flags then addDocument Nothing (newBuffer (activeText demoDesktop)) (initialDesktop dimensions) else initialDesktop dimensions
        configured=(fst (handleEvent (uncurry V.EvResize dimensions) initial)) {wordStar=WordStar `elem` flags}
    (_,loaded)<-applyEffects configured (map ReadPath paths)
    (_,withGit)<-applyEffects loaded [RefreshGit (startingDirectory loaded)]
    staged<-foldM stageScene withGit [scene | Scene scene<-flags]
    if Html `elem` flags then TIO.putStr (snapshotHtml staged)
    else if Snapshot `elem` flags then TIO.putStr (snapshot staged)
    else if backend /= Terminal then runWindow backend scale applyEffects staged
    else bracket (mkVty V.defaultConfig) V.shutdown $ \vty -> do
      when (V.supportsMode (V.outputIface vty) V.Mouse) (V.setMode (V.outputIface vty) V.Mouse True)
      when (V.supportsMode (V.outputIface vty) V.BracketedPaste) (V.setMode (V.outputIface vty) V.BracketedPaste True)
      size<-V.displayBounds (V.outputIface vty)
      loop vty (fst (handleEvent (uncurry V.EvResize size) staged))

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
  "split" -> fst (runCommand SplitHorizontal d)
  _ -> message "Unknown preview scene" [T.pack scene] d

demoDesktop :: Desktop
demoDesktop = addDocument Nothing (newBuffer sample) (initialDesktop (80,25))
  where sample=T.unlines ["module Main where","", "factorial :: Integer -> Integer", "factorial n = product [1 .. n]", "", "main :: IO ()", "main = do", "  putStrLn \"Enter a number:\"", "  input <- getLine", "  print (factorial (read input))"]

loop :: V.Vty -> Desktop -> IO ()
loop vty d = do
  V.update vty (renderDesktop d)
  event<-V.nextEvent vty
  let (next,effects)=handleEvent event d
  (exit,updated)<-applyEffects next effects
  if exit then pure () else loop vty updated

applyEffects :: Desktop -> [Effect] -> IO (Bool,Desktop)
applyEffects = foldM apply . (False,)
  where
    apply state@(True,_) _=pure state
    apply (_,d) Exit=pure (True,d)
    apply (_,d) (ReadPath path)=do
      directory<-doesDirectoryExist path
      if directory then apply (False,d) (ReadTree path)
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
      pure (False,d {branchStatus=maybe "" (\r -> repoBranch r <> if repoDirty r then "*" else "") repo,gitReview=case gitReview d of Just review | fmap repoRoot repo == Just (reviewRoot review) -> Just review; _ -> Nothing})
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
                clean=doc {documentFile=Just file,documentBuffer=b {saved=contents b}}
                updated=d {buffers=M.insert bid clean (buffers d),status="File saved."}
            (_,refreshed)<-apply (False,updated) (RefreshGit (takeDirectory (filePath file)))
            case after of
              Nothing->pure (False,refreshed)
              Just cmd->uncurry applyEffects (runCommand cmd refreshed)

wrapMessage :: T.Text -> [T.Text]
wrapMessage text | T.null text=[]
wrapMessage text=T.take 54 text:wrapMessage (T.drop 54 text)


browserError :: T.Text -> Desktop -> Desktop
browserError err d = case dialog d of
  Just dg -> d {dialog=Just dg {body=take 1 (body dg) ++ [T.take 54 err]},status=err}
  Nothing -> message "Cannot open directory" (wrapMessage err) d

gitDirectory :: Desktop -> FilePath
gitDirectory d = case activeDocument d >>= documentFile of
  Just file -> takeDirectory (filePath file)
  Nothing -> maybe (startingDirectory d) reviewRoot (gitReview d)
