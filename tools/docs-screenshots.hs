{-# LANGUAGE OverloadedStrings #-}
-- Capture actual editor commands over this checkout through the Metal frontend.
-- Run from the repository root; see docs/contributing.md#documentation-screenshots.
import Control.Monad (forM_)
import qualified Graphics.Vty as V
import System.Directory
import System.Environment (getArgs, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Process (callProcess)
import THC.Edit.App (applyEffects)
import THC.Edit.Conversation (withConversationAt, conversationEffects)
import THC.Edit.Debugger (withDebugger, debuggerEffects)
import THC.Edit.Frontend (Backend(Metal))
import THC.Edit.Model
import THC.Edit.Window (runWindow)

main :: IO ()
main = do
  root <- getCurrentDirectory
  requested <- getArgs
  let scratch = root </> "build/docs-capture"
      output = root </> "docs/site/screenshots"
  createDirectoryIfMissing True scratch
  createDirectoryIfMissing True output
  -- Never read personal provider settings or save an editing session.
  setEnv "XDG_CONFIG_HOME" (scratch </> "config")
  setEnv "thc_edit_datadir" root
  unsetEnv "THC_ROOT"
  setEnv "THC_EDIT_CAPTURE_EXIT" "1"
  withConversationAt root $ \conversation -> withDebugger $ \debugger -> do
    let effects = debuggerEffects debugger (conversationEffects conversation applyEffects)
        command cmd d = let (next, pending) = runCommand cmd d in snd <$> effects next pending
        key k mods d = fst (handleEvent (V.EvKey k mods) d)
        typeText :: String -> Desktop -> Desktop
        typeText text d = foldl (\state c -> key (V.KChar c) [] state) d text
        start = (initialDesktop (100,32))
          {videoMode=Just 3, crtFilter=True, pixelateUnicode=True, blinkCursor=False, streamerMode=True, nativeMac=True}
    (_, loaded) <- applyEffects start [ReadPath root, ReadPath (root </> "src/THC/Edit/Buffer.hs")]
    -- Start on a short source declaration, with the package visible behind it.
    let desktop = modifyActive (\w -> w {scrollRow=23}) loaded
        scenes =
          [ ("desktop", pure)
          , ("file-menu", pure . (\d -> d {menu=Just (0,1)}))
          , ("open-file", \d -> key V.KDown [] . snd <$> applyEffects d [BrowsePath (root </> "src/THC/Edit") "*.hs"])
          , ("save-as", command SaveAs)
          , ("save-changes", command Close . insertText "-- Local editing example\n")
          , ("change-directory", \d -> snd <$> applyEffects d [BrowseDirectories root])
          , ("find", fmap (typeText "LineMeasure") . command Find)
          , ("replace", fmap (typeText "LineStats" . key (V.KChar '\t') [] . typeText "LineMeasure") . command Replace)
          , ("preferences", command EditorOptions)
          , ("build-target", command RunOptions)
          , ("debug-launch", command (DebugCommand "launch"))
          , ("git-commit", \d -> command GitDiff d {streamerMode=False} >>= command GitCommit >>= pure . typeText "Document editor dialogs")
          ]
    forM_ scenes $ \(name, open) -> if not (null requested) && name `notElem` requested then pure () else do
      shown <- open desktop
      let bmp = scratch </> name ++ ".bmp"
          png = output </> name ++ ".png"
      setEnv "THC_EDIT_CAPTURE" bmp
      runWindow Metal 3 effects pure shown
      callProcess "sips" ["-s", "format", "png", bmp, "--out", png]
      putStrLn ("Captured " ++ name)
