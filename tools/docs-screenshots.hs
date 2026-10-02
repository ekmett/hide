{-# LANGUAGE OverloadedStrings #-}
-- Capture actual editor commands over this checkout through the Metal frontend.
-- Run from the repository root; see docs/contributing.md#documentation-screenshots.
import Control.Monad (forM_, when)
import qualified Data.Text as T
import THC.Edit.Buffer (newBuffer, Selection(..))
import qualified Graphics.Vty as V
import System.Directory
import System.Environment (getArgs, lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import THC.Edit.App (applyEffects)
import THC.Edit.Conversation (withConversationAt, conversationEffects, tickConversation)
import THC.Edit.Debugger (withDebugger, debuggerEffects, tickDebugger)
import THC.Edit.Model
import qualified EditorDriver as Driver

main :: IO ()
main = do
  root <- getCurrentDirectory
  requested <- getArgs
  let scratch = root </> "build/docs-capture"
      output = root </> "docs/site/screenshots"
  createDirectoryIfMissing True scratch
  createDirectoryIfMissing True output
  -- Live conversations require an explicit, caller-supplied provider configuration.
  agentConfig <- lookupEnv "THC_DOCS_AGENT_CONFIG"
  when ("conversation" `elem` requested) $ case agentConfig of
    Nothing -> fail "Set THC_DOCS_AGENT_CONFIG to an ACP provider configuration."
    Just path -> do
      createDirectoryIfMissing True (scratch </> "config/thc-edit")
      copyFile path (scratch </> "config/thc-edit/agents.json")
  unsetEnv "THC_EDIT_SESSION"
  setEnv "XDG_DATA_HOME" (scratch </> "data")
  -- Never save an editing session.
  setEnv "XDG_CONFIG_HOME" (scratch </> "config")
  setEnv "thc_edit_datadir" root
  unsetEnv "THC_ROOT"
  setEnv "THC_EDIT_CAPTURE_EXIT" "1"
  withConversationAt root $ \conversation -> withDebugger $ \debugger -> do
    let effects = debuggerEffects debugger (conversationEffects conversation applyEffects)
        command = Driver.command effects
        key k mods = Driver.input effects (V.EvKey k mods)
        typeText = Driver.typeText effects
        await = Driver.await
        chat d = do
          sent <- snd <$> effects d [AgentAction "send" ["0",
            "Read src/THC/Edit/Buffer.hs and explain its finger tree of lines in three short bullets (under 70 words). Read-only, please.",
            "false","false","false"]]
          reply <- await "agent reply" (tickConversation conversation) ((=="Agent: end_turn") . status) sent
          followup <- snd <$> effects reply [AgentAction "send" ["0",
            "Which operations avoid scanning the whole file? Two bullets, under 40 words; use what you just read.",
            "false","false","false"]]
          answered <- await "follow-up reply" (tickConversation conversation) ((=="Agent: end_turn") . status) followup
          wide <- command ToggleTree answered
          full <- command Zoom (fst (handleEvent (V.EvResize 100 24) wide))
          let bottom = case activeDocument full of
                Just doc -> modifyActive (\w -> w {scrollRow=scrollbarLimit full True doc w}) full
                Nothing -> full
              draft = "How would you benchmark edits to a large file?"
          pure bottom {composerFocused=True,composerBuffer=newBuffer draft,composerSelection=Selection (T.length draft) (T.length draft)}
        debug d = do
          port <- maybe (fail "Set THC_DOCS_DAP_PORT to a suspended local THC program.") pure =<< lookupEnv "THC_DOCS_DAP_PORT"
          connected <- snd <$> effects d [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
          let tick=tickDebugger debugger effects
          stopped <- await "initial source stop" tick (T.isPrefixOf "Stopped in " . status) connected
          stepped <- command (DebugCommand "stepIn") stopped
          next <- await "source step" tick (T.isPrefixOf "Stopped in " . status) stepped
          wide <- command ToggleTree next
          full <- command Zoom (fst (handleEvent (V.EvResize 100 24) wide))
          pure full
        start = (initialDesktop (100,32))
          {videoMode=Just 3, crtFilter=True, pixelateUnicode=True, blinkCursor=False, streamerMode=True, nativeMac=True}
    (_, loaded) <- applyEffects start [ReadPath root, ReadPath (root </> "src/THC/Edit/Buffer.hs")]
    -- Start on a short source declaration, with the package visible behind it.
    let desktop = modifyActive (\w -> w {scrollRow=23}) loaded
        scenes =
          [ ("conversation", chat)
          , ("debug-step", debug)
          , ("file-menu", pure . (\d -> d {menu=Just (0,1)}))
          , ("split", \d -> command Zoom d >>= command SplitHorizontal >>= pure . modifyActive (\w -> w {scrollRow=45}))
          , ("preferences", command EditorOptions)
          , ("build-target", \d -> command RunOptions d >>= key (V.KChar '\t') [] >>= typeText "exe:thc-edit")
          , ("debug-launch", command (DebugCommand "launch"))
          , ("debug-menu", pure . (\d -> d {menu=Just (5,6)}))
          , ("toolchain", command ToolchainOptions)
          , ("git-commit", \d -> command GitDiff d {streamerMode=False} >>= command GitCommit >>= typeText "Document editor dialogs")
          ]
    forM_ scenes $ \(name, open) -> if (null requested && name `elem` ["conversation","debug-step"]) || (not (null requested) && name `notElem` requested) then pure () else do
      shown <- open desktop
      capture effects scratch output name shown
      when (name=="debug-step") $ do
        capture effects scratch output "debug-menu" shown {menu=Just (5,6)}
        pending <- command (DebugCommand "stack") shown
        stack <- await "call stack" (tickDebugger debugger effects) (maybe False ((=="Call stack") . dialogTitle) . dialog) pending
        capture effects scratch output "debug-stack" stack
        resumed <- command (DebugCommand "continue") shown
        _ <- await "program termination" (tickDebugger debugger effects) (\d -> T.isPrefixOf "Debug session ended" (status d) && status d/="Debug session ended.") resumed
        pure ()

capture :: (Desktop -> [Effect] -> IO (Bool,Desktop)) -> FilePath -> FilePath -> String -> Desktop -> IO ()
capture effects scratch output name shown = do
  let bmp = scratch </> name ++ ".bmp"
      png = output </> name ++ ".png"
  -- Crop exact rendered pixels, using the same cell rectangles as the UI.
  -- Include the actual shadow and eight pixels of context; menus retain their heading.
  let crop = case dialog shown of
        Just dg -> Just (dialogRect shown dg)
        Nothing | Just (r,_)<-contextMenu shown -> Just r
        Nothing -> case menu shown of
          Just (i,_) -> Just (menuRect shown i)
          Nothing | name `elem` ["conversation","debug-step"] -> bounds <$> activeWindow shown
                  | otherwise -> Nothing
      pixels = case crop of
        Nothing -> Nothing
        Just (Rect x y w h) ->
          let x0=max 0 (x*24-8); y0=if menu shown/=Nothing then 0 else max 0 (y*48-8)
              x1=min (fst (screenSize shown)*24) ((x+w+2)*24+8); y1=min (snd (screenSize shown)*48) ((y+h+1)*48+8)
          in Just (Rect x0 y0 (x1-x0) (y1-y0))
  Driver.captureMetal effects 3 bmp png pixels shown
  putStrLn ("Captured " ++ name)
