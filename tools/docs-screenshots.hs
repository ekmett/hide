{-# LANGUAGE OverloadedStrings #-}
-- Capture actual editor commands over this checkout through the Metal frontend.
-- Run from the repository root; see docs/contributing.md#documentation-screenshots.
import Control.Concurrent.Async (withAsync)
import Control.Monad (forM_, unless, when, (>=>))
import qualified Data.Map.Strict as M
import Data.Aeson (object, (.=))
import Data.List (intersperse)
import Hide.Syntax (Style(Plain))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hide.Buffer (newBuffer, Selection(..), revision, contents, replaceSelection, bufferLineOffset, bufferLineChanges, bufferLineColumn, displayColumn, bufferLineCount, bufferLineAt)
import Hide.BufferView (BufferView(SideBySideView,MarkdownView))
import Hide.Files (FileState(..))
import Hide.TextPresentation (prepareTextPresentations)
import Hide.MCPPermissions (withPermissionsAt, bufferEditor, tickPermissions, policyEffects)
import Hide.WorkspaceFilesMCP (fileTools)
import Hide.BufferDiffCommand (withBufferDiffCommands,bufferDiffTool)
import qualified Graphics.Vty as V
import System.Directory
import System.Environment (getArgs, lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hSetBuffering,stdout,BufferMode(LineBuffering))
import Hide.App (applyEffects)
import Hide.Conversation (withConversationAt, conversationEffects, tickConversation, renderReply)
import Hide.Debugger (withDebugger, debuggerEffects, tickDebugger, hdbOfferDialog, debuggerTool)
import Hide.DebuggerSidebar (withDebuggerSidebar,tickDebuggerSidebar)
import Hide.DocsMCP (withDocsCommands)
import Hide.MenuCommands (withMenuCommands,menuEffects,tickMenus,menuContributions,menuAgentReferences)
import Hide.Sidebar (emptySidebar,treeRows,treeScroll,treeWidth,rowInfo,rowHit)
import Hide.SidebarCommands (withSidebarCommands,sidebarEffects,initializeSidebar,tickSidebar)
import qualified Hide.Compilers as Compilers
import qualified Hide.HdbAcquisition as Hdb
import qualified Hide.Plugin.Window as PluginWindow
import qualified Hide.Plugin.Tree as PluginTree
import qualified Hide.Plugin.Menu as PluginMenu
import Hide.Environment (environmentAction)
import Hide.Model
import qualified EditorDriver as Driver

main :: IO ()
main = PluginWindow.withWindowScope $ \downloadScope -> do
  hSetBuffering stdout LineBuffering
  root <- getCurrentDirectory
  requested <- getArgs
  captureDirectory <- lookupEnv "THC_DOCS_CAPTURE_DIR"
  let scratch = maybe (root </> "build/docs-capture") (</> "scratch") captureDirectory
      output = maybe (root </> "docs/site/screenshots") (</> "screenshots") captureDirectory
  createDirectoryIfMissing True scratch
  createDirectoryIfMissing True output
  hdbPlan<-if any (`elem` requested) ["hdb-download","downloads"] then do
    compiler<-Compilers.compilerInfo root False "ghc" >>= either (fail . T.unpack) pure
    Just <$> (Hdb.prepareHdb compiler >>= either (fail . T.unpack) pure)
    else pure Nothing
  -- Live conversations require an explicit, caller-supplied provider configuration.
  agentConfig <- lookupEnv "THC_DOCS_AGENT_CONFIG"
  when ("conversation" `elem` requested) $ case agentConfig of
    Nothing -> pure ()
    Just path -> do
      createDirectoryIfMissing True (scratch </> "config/thc-edit")
      copyFile path (scratch </> "config/thc-edit/agents.json")
  unsetEnv "THC_EDIT_SESSION"
  setEnv "XDG_DATA_HOME" (scratch </> "data")
  -- Never save an editing session.
  setEnv "XDG_CONFIG_HOME" (scratch </> "config")
  setEnv "hide_datadir" root
  unsetEnv "THC_ROOT"
  setEnv "THC_EDIT_CAPTURE_EXIT" "1"
  withDocsCommands $ \docs -> withMenuCommands docs $ \menuHost -> withSidebarCommands $ \sidebarHost -> withDebugger $ \debugger -> withDebuggerSidebar sidebarHost debugger $ \debugSidebar -> withPermissionsAt (scratch </> "permissions.toml") fileTools $ \permissions -> withBufferDiffCommands $ \diffCommands -> do
    contributions <- PluginMenu.menuSnapshot (menuContributions menuHost)
    let core = policyEffects permissions (debuggerEffects debugger applyEffects)
        effects = sidebarEffects sidebarHost (menuEffects menuHost core)
        tick = tickDebugger debugger >=> tickMenus menuHost core >=> tickDebuggerSidebar debugSidebar sidebarHost debugger >=> tickSidebar sidebarHost core
        command = Driver.command effects
        key k mods = Driver.input effects (V.EvKey k mods)
        typeText = Driver.typeText effects
        await label pump predicate d=do
          putStrLn ("Waiting for "<>label<>"; "<>T.unpack (status d))
          Driver.await label pump predicate d
        chat d = case agentConfig of
          Nothing -> recordedChat d
          Just _ -> withConversationAt root $ \conversation -> do
            let liveEffects=conversationEffects conversation effects
            sent <- snd <$> liveEffects d [AgentAction "send" ["0",
              "Read src/Hide/Buffer.hs and explain its finger tree of lines in three short bullets (under 70 words). Read-only, please.",
              "false","false","false"]]
            reply <- await "agent reply" (tickConversation conversation) ((=="Agent: end_turn") . status) sent
            followup <- snd <$> liveEffects reply [AgentAction "send" ["0",
              "Which operations avoid scanning the whole file? Two bullets, under 40 words; use what you just read.",
              "false","false","false"]]
            answered <- await "follow-up reply" (tickConversation conversation) ((=="Agent: end_turn") . status) followup
            wide <- command ToggleTree answered
            full <- command Zoom (fst (handleEvent (V.EvResize 100 24) wide))
            reflowed <- tickConversation conversation full
            let bottom = case activeDocument reflowed of
                  Just doc -> modifyActive (\w -> w {scrollRow=scrollbarLimit reflowed True doc w}) reflowed
                  Nothing -> reflowed
                draft = "How would you benchmark edits to a large file?"
            pure bottom {composerFocused=True,composerBuffer=newBuffer draft,composerSelection=Selection (T.length draft) (T.length draft)}
        -- Visible messages transcribed from the original real capture's
        -- build/docs-capture/conversation.txt. Tool output was truncated there,
        -- so replay only the complete exchange, model and rounded usage.
        recordedChat d = do
          let resized=fst (handleEvent (V.EvResize 100 21) d {sideTree=Nothing})
              opened=selectConversationView "" "Primary" resized
          full <- command Zoom opened
          case activeWindow full of
            Nothing -> fail "Recorded conversation window is missing"
            Just window -> do
              bid <- maybe (fail "Recorded conversation has no source buffer") pure (bufferId window)
              let replies=
                    [ (False,T.unlines
                        [ "- Each leaf stores one line’s text and length, including its newline; the final leaf has no newline and may be empty."
                        , "- Subtrees cache character and line counts, enabling measured splits to locate offsets, rows, and columns efficiently."
                        , "- Edits rebuild affected lines and concatenate untouched trees. Undo/redo retain persistent trees with structural sharing; full text is flattened lazily and cached."
                        ])
                    , (True,"Which operations avoid scanning the whole file? Two bullets, under 40 words; use what you just read.")
                    , (False,T.unlines
                        [ "- Length and line count use cached measures; offset/row lookup and line access use measured tree searches."
                        , "- Selection reads scan selected lines; replacements rebuild boundary lines plus inserted text. Undo/redo swap persistent trees; full-text flattening is deferred."
                        ])
                    ]
                  contentWidth=width (bounds window)-2
                  bubbles=[renderReply True contentWidth outgoing (T.stripEnd replyText) | (outgoing,replyText)<-replies]
                  styled=concat (intersperse [('\n',Plain),('\n',Plain)] bubbles)
                  text=T.pack (map fst styled)
                  draft="How would you benchmark edits to a large file?"
                  shown=full {buffers=M.adjust (\doc -> doc {documentBuffer=newBuffer text,documentHighlight=styled,documentCursorVisible=False}) bid (buffers full)
                    ,composerFocused=True,composerBuffer=newBuffer draft,composerSelection=Selection (T.length draft) (T.length draft)
                    ,agentSettings=[AgentSetting "model" "Model" "model" "gpt-6-astra" [("gpt-6-astra","gpt-6-astra")],AgentSetting "effort" "Effort" "thought_level" "medium" [("medium","medium")]]
                    ,agentContextUsage=Just (33000,258000),status="Recorded conversation"}
              let rowWidths=[displayColumn row (T.length row) | cells<-bubbles,row<-T.lines (T.pack (map fst cells))]
              unless (all (<=contentWidth) rowWidths && maximum (0:rowWidths)>=contentWidth-6)
                (fail "Recorded conversation does not fit its current window width")
              case activeDocument shown of
                Just doc -> unless (bufferLineCount (documentBuffer doc)<=windowContentRows shown doc window)
                  (fail "Recorded conversation does not fit its capture height")
                Nothing -> fail "Recorded conversation document is missing"
              pure shown
        debug d = do
          port <- maybe (fail "Set THC_DOCS_DAP_PORT to a suspended local THC program.") pure =<< lookupEnv "THC_DOCS_DAP_PORT"
          connected <- snd <$> effects d [DebugAction "connect" ["0","127.0.0.1",T.pack port]]
          stopped <- await "initial source stop" tick (T.isPrefixOf "Stopped in " . status) connected
          stepped <- command (DebugCommand "stepIn") stopped
          next <- await "source step" tick (T.isPrefixOf "Stopped in " . status) stepped
          wide <- command ToggleTree next
          full <- command Zoom (fst (handleEvent (V.EvResize 100 24) wide))
          pure full
        debugWatches = do
          toy <- maybe (fail "Set THC_DOCS_WATCH_ROOT to the live hdb toy project.") pure =<< lookupEnv "THC_DOCS_WATCH_ROOT"
          config <- maybe (fail "Set THC_DOCS_HDB_CONFIG to its existing hdb adapter config.") pure =<< lookupEnv "THC_DOCS_HDB_CONFIG"
          (_,opened) <- applyEffects start {contributedMenus=contributions,agentMenuRefs=menuAgentReferences menuHost,menusActive=True} [ReadPath (toy </> "Main.hs")]
          mountedTree <- initializeSidebar sidebarHost opened {sideTree=Just (emptySidebar toy 38 False)} >>= tick
          mounted <- command Zoom mountedTree
          let rows d=maybe [] (M.elems.treeRows) (sideTree d)
              has text=any (T.isInfixOf text . PluginTree.infoLabel . rowInfo) . rows
              findRow title d=case [r | r<-rows d,title `T.isPrefixOf` PluginTree.infoLabel (rowInfo r)] of
                [r]->pure r
                _->fail ("Missing watch row: "<>T.unpack title)
              invoke title target d=do
                index<-case [i | (i,r)<-zip [0..] (rows d),rowHit r==rowHit target] of
                  i:_->pure i
                  []->fail "Watch target disappeared before its context menu"
                let y=2+index-maybe 0 treeScroll (sideTree d)
                popup<-Driver.input effects (V.EvMouseDown 20 y V.BRight []) d
                case [i | (i,(label,_))<-zip [0..] (contextItemsFor popup),label==title] of
                  i:_->key V.KEnter [] popup {contextMenu=fmap (\(rect,_)->(rect,i)) (contextMenu popup)}
                  _->fail ("Missing watch action: "<>T.unpack title)
              expand title d=case [i | (i,r)<-zip [0..] (rows d),title `T.isPrefixOf` PluginTree.infoLabel (rowInfo r)] of
                i:_->let (next,pending)=activateTree True i d in snd <$> effects next pending
                _->fail ("Missing watch branch: "<>T.unpack title)
              tool name args d=do
                (next,finish)<-debuggerTool debugger d name (object args)
                _<-finish >>= either (fail . T.unpack) pure
                pure next
          window<-maybe (fail "Missing toy source") pure (activeWindow mounted)
          bid<-maybe (fail "Toy source is not an editable file") pure (bufferId window)
          prepared<-tool "debug_set_breakpoints" ["generation" .= (0::Int),"bufferId" .= bid,"lines" .= ([7]::[Int])] mounted
          launched<-tool "debug_launch" ["adapterConfig" .= config] prepared
          stopped<-await "live hdb source stop" tick (T.isPrefixOf "Stopped in " . status) launched
          ready<-expand "Watches" stopped >>= await "Watches root" tick (has "No watches")
          case (activeWindow ready,activeDocument ready) of
            (Just w,Just doc)->do
              let row=6; line=bufferLineAt (documentBuffer doc) row
                  (prefix,suffix)=T.breakOn "counter" line
                  offset=bufferLineOffset (documentBuffer doc) row+T.length prefix
                  selected=modifyActive (\value->value {selection=Selection offset (offset+7),scrollRow=0}) ready
              unless ("counter" `T.isPrefixOf` suffix) (fail "hdb toy source anchor changed")
              popup<-Driver.input effects (V.EvMouseDown (left (bounds w)+1+T.length prefix+1) (top (bounds w)+1+row) V.BRight []) selected
              chosen<-case [i | (i,(title,_))<-zip [0..] (contextItemsFor popup),"Add watch" `T.isPrefixOf` title] of
                i:_->key V.KEnter [] popup {contextMenu=fmap (\(rect,_)->(rect,i)) (contextMenu popup)}
                _->fail "Source Add Watch contribution missing"
              editing<-await "source Add Watch dialog" tick ((/=Nothing).dialog) chosen
              unless (case dialog editing of Just dg->[SelectedInput "Expression" "counter" (Selection 0 7)]==fields dg; _->False)
                (fail "Source selection did not prefill the watch expression")
              filled<-typeText "counter + 1" editing
              capture effects scratch output "debug-add-watch" filled
              saved<-key V.KEnter [] filled >>= await "saved source watch" tick (has "counter + 1")
              watch<-findRow "counter + 1" saved
              evaluated<-invoke "Evaluate watch" watch saved >>= await "live scalar watch result" tick (has "counter + 1 = 42")
              rootRow<-findRow "Watches" evaluated
              adding<-invoke "Add watch…" rootRow evaluated >>= await "list watch dialog" tick ((/=Nothing).dialog)
              listDraft<-typeText "values" adding
              listSaved<-key V.KEnter [] listDraft >>= await "saved list watch" tick (has "values")
              listRow<-findRow "values" listSaved
              list<-invoke "Evaluate watch" listRow listSaved >>= await "live list result" tick (has "values =")
              expanded<-expand "values =" list >>= await "live expanded list" tick (has "tail =")
              target<-findRow "values =" expanded
              index<-case [i | (i,r)<-zip [0..] (rows expanded),rowHit r==rowHit target] of
                i:_->pure i
                []->fail "Evaluated watch disappeared before its menu"
              let y=2+index-maybe 0 treeScroll (sideTree expanded)
              shown<-Driver.input effects (V.EvMouseDown 20 y V.BRight []) expanded
              unless (any ((=="Evaluate watch").fst) (contextItemsFor shown)) (fail "Evaluated watch has no refresh action")
              capture effects scratch output "debug-watches" shown
              resumed<-command (DebugCommand "continue") expanded
              _<-await "hdb toy termination" tick (T.isPrefixOf "Debug session ended" . status) resumed
              pure ()
            _->fail "Live hdb source was not opened"
        permissionDiff d = case (activeWindow d,activeDocument d) of
          (Just w,Just doc) | first:rest<-take 6 (T.lines (contents (documentBuffer doc))) -> do
            let patch=T.unlines (["--- a/src/Hide/Buffer.hs","+++ b/src/Hide/Buffer.hs","@@ -1,6 +1,7 @@"] ++
                  ["-"<>first,"+{-# LANGUAGE MultiParamTypeClasses #-}","+{-# LANGUAGE OverloadedStrings #-}"] ++ map (" "<>) rest)
            (_,reply)<-bufferDiffTool diffCommands (bufferEditor permissions (pure (Right ()))) d "buffer_apply_diff"
              (object ["bufferId" .= bufferId w,"revision" .= revision (documentBuffer doc),"diff" .= patch])
            withAsync reply $ \_->await "typed diff approval" (tickPermissions permissions) ((/=Nothing).dialog) d
          _ -> fail "Permission screenshot requires the open source buffer"
        shellBlockMenu d = do
          opened <- command Help (fst (handleEvent (V.EvResize 80 25) d {sideTree=Nothing}))
          case (activeWindow opened,activeDocument opened) of
            (Just w,Just doc) | Just bid<-bufferId w,block@(blockStart,_,_,raw):_<-documentShellBlocks doc -> do
              unless (raw=="hide .\n") (fail "Help shell screenshot source anchor changed")
              let (row,_)=bufferLineColumn (documentBuffer doc) blockStart
                  scroll=max 0 (row-2)
                  positioned=modifyActive (\window -> window {bounds=Rect 1 1 78 11,scrollRow=scroll}) opened
                  -- Open the real context menu, without invoking its command.
                  (shown,pending)=handleEvent (V.EvMouseDown 26 (2+row-scroll+1) V.BRight []) positioned
                  expected=ExecuteShellBlock bid block
              unless (null pending && contextKind shown==ShellContext expected &&
                contextItems (contextKind shown)==[("Execute in terminal",expected)] && contextMenu shown/=Nothing)
                (fail "Shell block context menu did not open without executing")
              pure shown
            _ -> fail "Help screenshot needs an executable shell fence"
        documentationLinks d = do
          opened <- command Help (fst (handleEvent (V.EvResize 80 25) d {sideTree=Nothing}))
          case (activeWindow opened,activeDocument opened) of
            (Just _,Just doc) | (offset,_,target):_<-filter (\(_,_,url)->url=="docs/install.md") (documentLinks doc) -> do
              let (row,col)=bufferLineColumn (documentBuffer doc) offset
                  scroll=max 0 (row-3)
                  positioned=modifyActive (\w->w {bounds=Rect 1 1 78 12,scrollRow=scroll}) opened
                  x=2+col; y=2+row-scroll
              pressed <- Driver.input effects (V.EvMouseDown x y V.BLeft []) positioned
              followed <- Driver.input effects (V.EvMouseUp x y (Just V.BLeft)) pressed
              unless (maybe False ((==Just (root </> "docs/install.md")).documentMarkdownPath) (activeDocument followed))
                (fail "Installation link did not open in Help")
              let (shown,pending)=handleEvent (V.EvMouseDown x y V.BRight []) positioned
              unless (null pending && contextKind shown==LinkContext (OpenLink (documentMarkdownPath doc) target))
                (fail "Documentation link menu did not open")
              pure shown
            _ -> fail "README Installation link is missing"
        review d = do
          let file=root </> "src/Hide/Frontend.hs"
          source <- TIO.readFile file
          let replace before after buffer =
                let (prefix,suffix)=T.breakOn before (contents buffer)
                    offset=T.length prefix
                in if T.null suffix then error "Review screenshot source anchor is missing"
                   else replaceSelection (Selection offset (offset+T.length before)) after buffer
              changed=replace "    Just \"web\" -> Right Web"
                "    Just \"web\" -> Right Web\n    -- Accept a familiar browser alias.\n    Just \"browser\" -> Right Web"
                (replace "    Just \"auto\" -> Right Auto" "    Just \"auto\" -> Right Metal" (newBuffer source))
              resized=fst (handleEvent (V.EvResize 132 36) d)
              opened=addDocument (Just (FileState file Nothing)) changed resized {sideTree=Nothing}
              styled=opened {buffers=M.map highlightDocument (buffers opened)}
              positioned=modifyActive (\w -> w {bounds=Rect 1 1 130 19,
                selection=Selection (bufferLineOffset changed 29) (bufferLineOffset changed 29)}) styled
          unless (bufferLineChanges changed==(3,1)) (fail "Unexpected review screenshot line counts")
          side <- modifyActive (\w -> w {scrollRow=21}) <$> command (SetBufferView SideBySideView) positioned
          case activeWindow side of
            Nothing -> fail "Missing review screenshot window"
            Just w -> do
              let r=bounds w
                  old=left r+1+fst (reviewPaneWidths w)
                  target=left r+1+(width r-3)*40 `div` 100
                  y=top r+5
              dragged <- Driver.input effects (V.EvMouseDown old y V.BLeft []) side
                >>= Driver.input effects (V.EvMouseDown target y V.BLeft [])
                >>= Driver.input effects (V.EvMouseUp target y (Just V.BLeft))
              unless (maybe False (\window -> reviewSplit window>=38 && reviewSplit window<=41) (activeWindow dragged))
                (fail "Review screenshot divider drag did not apply")
              pure dragged
        markdownPreview d = do
          let file=root </> "docs/editing.md"
          source <- TIO.readFile file
          let resized=fst (handleEvent (V.EvResize 126 29) d)
              opened=addDocument (Just (FileState file Nothing)) (newBuffer source)
                resized {sideTree=Nothing,windows=[],wideSectionTitles=True,branchStatus=""}
              styled=opened {buffers=M.map highlightDocument (buffers opened)}
          split <- command SplitVertical styled
          preview <- command (SetBufferView MarkdownView) split
          prepareTextPresentations preview
        reviewMenu d = do
          shown <- review d
          case [i | (i,(name,_,_))<-zip [0..] menus,name=="Window"] of
            i:_ -> pure shown {menu=Just (i,13)}
            [] -> fail "Missing Window menu"
        start = (initialDesktop (100,32))
          {videoMode=Just 3, crtFilter=True, pixelateUnicode=True, blinkCursor=False, streamerMode=True, nativeMac=True}
    when ("debug-watches" `elem` requested) debugWatches
    (_, loaded) <- applyEffects start [ReadPath root, ReadPath (root </> "src/Hide/Buffer.hs")]
    -- Start on a short source declaration, with the package visible behind it.
    let desktop = modifyActive (\w -> w {scrollRow=23}) loaded
        scenes =
          [ ("hdb-download", \d -> case hdbPlan of
              Just plan->pure d {dialog=Just (hdbOfferDialog 1 plan)}
              Nothing->fail "Request hdb-download explicitly")
          -- Render the runtime's actual Downloads view from a deterministic
          -- progress snapshot; documentation capture never fetches an asset.
          , ("downloads", \d -> case hdbPlan of
              Just plan->do
                details<-PluginWindow.prepareTextWindow "Details" ("Downloading hdb\n5242880 / "<>T.pack (show (Hdb.hdbAssetSize (Hdb.hdbAsset plan)))<>" bytes received")
                ident<-either (fail . T.unpack) pure (PluginTree.nodeId "1")
                prepared<-PluginWindow.prepareRecoverableRowsWindow "hide.downloads" 1 "Downloads" []
                  [PluginWindow.WindowRow ident ("hdb for GHC "<>Compilers.compilerVersion (Hdb.hdbCompiler plan)<>" — Downloading hdb") details] >>= either (fail . T.unpack) pure
                update<-PluginWindow.openTextWindow downloadScope prepared >>= maybe (fail "Downloads scope closed") pure
                value<-PluginWindow.admitWindowUpdate False update >>= maybe (fail "Downloads publication expired") pure
                let shown=uncurry addPluginWindow value d {streamerMode=False}
                pure (case activeWindow shown of Just w->resizeWindowBounds (windowId w) (Rect 8 3 64 19) shown; _->shown)
              Nothing->fail "Request downloads explicitly")
          , ("find-replace", \d -> command Find d >>= typeText "bufferLineAt" >>= key (V.KChar 'h') [V.MCtrl] >>= typeText "lineAt")
          , ("permission-diff", permissionDiff)
          , ("shell-block-menu", shellBlockMenu)
          , ("documentation-links", documentationLinks)
          , ("side-by-side", review)
          , ("window-views-menu", reviewMenu)
          , ("markdown-view", markdownPreview)
          , ("conversation", chat)
          , ("debug-step", debug)
          , ("file-menu", pure . (\d -> d {menu=Just (0,1)}))
          , ("split", \d -> command Zoom d >>= command SplitHorizontal >>= pure . modifyActive (\w -> w {scrollRow=45}))
          , ("environment", \d -> environmentAction "choose" ["1"] d {streamerMode=False} >>= typeText "PKG_CONFIG_PATH" >>= key (V.KChar '\t') [] >>= typeText (root </> ".deps/ghostty/share/pkgconfig"))
          , ("preferences", command EditorOptions)
          , ("build-target", \d -> command RunOptions d >>= key (V.KChar '\t') [] >>= typeText "exe:hide")
          , ("debug-launch", command (DebugCommand "launch"))
          , ("debug-menu", pure . (\d -> d {menu=Just (5,6)}))
          , ("toolchain", command ToolchainOptions)
          , ("git-commit", \d -> command GitDiff d {streamerMode=False} >>= command GitCommit >>= typeText "Document editor dialogs")
          ]
    forM_ scenes $ \(name, open) -> if (null requested && name `elem` ["conversation","debug-step","hdb-download","downloads"]) || (not (null requested) && name `notElem` requested) then pure () else do
      shown <- open desktop
      capture effects scratch output name shown
      when (name=="debug-step") $ do
        capture effects scratch output "debug-menu" shown {menu=Just (5,6)}
        pending <- command (DebugCommand "stack") shown
        stack <- await "call stack" tick (maybe False ((=="Call stack") . dialogTitle) . dialog) pending
        capture effects scratch output "debug-stack" stack
        resumed <- command (DebugCommand "continue") shown
        _ <- await "program termination" tick (\d -> T.isPrefixOf "Debug session ended" (status d) && status d/="Debug session ended.") resumed
        pure ()

capture :: (Desktop -> [Effect] -> IO (Bool,Desktop)) -> FilePath -> FilePath -> String -> Desktop -> IO ()
capture effects scratch output name shown = do
  let bmp = scratch </> name ++ ".bmp"
      png = output </> name ++ ".png"
  -- Crop exact rendered pixels, using the same cell rectangles as the UI.
  -- Include the actual shadow and eight pixels of context; menus retain their heading.
  let crop = case dialog shown of
        _ | name `elem` ["shell-block-menu","documentation-links"] -> bounds <$> activeWindow shown
        _ | name=="debug-watches",Just tree<-sideTree shown ->
          let Rect x y w h=maybe (Rect 0 1 (treeWidth tree) (snd (screenSize shown)-2)) fst (contextMenu shown)
              first=case [i | (i,row)<-zip [0..] (M.elems (treeRows tree)),PluginTree.infoLabel (rowInfo row)=="Watches"] of i:_->max 1 (2+i-treeScroll tree); []->1
              bottom=max (y+h+1) (2+M.size (treeRows tree)-treeScroll tree)
          in Just (Rect 0 first (max (treeWidth tree) (x+w)) (bottom-first))
        Just dg -> Just (dialogRect shown dg)
        Nothing | Just (r,_)<-contextMenu shown -> Just r
        Nothing -> case menu shown of
          Just (i,_) -> Just (menuRect shown i)
          Nothing | name `elem` ["conversation","debug-step","side-by-side","downloads"] -> bounds <$> activeWindow shown
                  | otherwise -> Nothing
      pixels = case crop of
        Nothing -> Nothing
        Just (Rect x y w h) | name `elem` ["side-by-side","shell-block-menu","documentation-links"] ->
          Just (Rect (max 0 (x*24-8)) (max 0 (y*48-8)) (w*24+16) (h*48+16))
        Just (Rect x y w h) ->
          let x0=max 0 (x*24-8); y0=if menu shown/=Nothing then 0 else max 0 (y*48-8)
              x1=min (fst (screenSize shown)*24) ((x+w+2)*24+8); y1=min (snd (screenSize shown)*48) ((y+h+1)*48+8)
          in Just (Rect x0 y0 (x1-x0) (y1-y0))
  Driver.captureMetal effects 3 bmp png pixels shown
  putStrLn ("Captured " ++ name)
