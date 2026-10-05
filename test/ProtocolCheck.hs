{-# LANGUAGE OverloadedStrings #-}
module ProtocolCheck (checks) where
import Control.Exception (SomeException, bracket, try, evaluate, displayException)
import Control.DeepSeq (force)
import GHC.Conc (getAllocationCounter)
import Hide.Sidebar (emptySidebar)
import Control.Monad (unless, forM_)
import Data.Aeson
import Data.Aeson.Types (parseEither, parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.Zlib.Raw as Z
import qualified Data.Text as T
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO
import qualified Hide.Commands as Commands
import qualified Hide.Bindings as Bindings
import Data.List (nub, findIndex, isInfixOf)
import qualified Data.Map.Strict as M
import qualified Graphics.Vty as V
import Hide.Protocol
import Hide.Model
import Hide.Window (nativeCommands, nativeMenuEvent)
import Hide.GuestAccess (beginGuestInput)
import Hide.Files (FileState(..))
import Hide.Buffer
import Hide.Render (snapshotHtml)

checks :: IO ()
checks = do
  let check name ok=unless ok (error name)
      rejects name action=do
        result<-try action :: IO (Either SomeException ())
        check name (either (const True) (const False) result)
  dir<-getTemporaryDirectory
  bracket (openBinaryTempFile dir "thc-wire") (\(path,h)->hClose h >> removeFile path) $ \(_,h)->do
    let packets=[JsonPacket (object ["text" .= ("λ 👩🏽\x200d\&💻"::T.Text)]),BinaryPacket (BS.pack [0,1,2,255])]
    mapM_ (writePacket h) packets
    hSeek h AbsoluteSeek 0
    actual<-sequence [readPacket h,readPacket h,readPacket h]
    check "binary and Unicode packets round trip with clean EOF" (actual==map Just packets++[Nothing])
    forM_ [BS.pack [0],BS.pack [0,0,0,3,0,123],BS.pack [255,255,255,255],BS.pack [0,0,0,1,9]] $ \bad->do
      hSetFileSize h 0; hSeek h AbsoluteSeek 0; BS.hPut h bad; hSeek h AbsoluteSeek 0
      rejects "truncated, oversized and unknown-kind packets fail" (readPacket h >> pure ())
  let source=T.replicate 200 "main = putStrLn \"hello 👩🏽\x200d\&💻\"\n"
      opened=addDocument (Just (FileState "/project/Main.hs" Nothing)) (newBuffer source) (initialDesktop (180,55))
      docked=opened {sideTree=Just (emptySidebar "/project" 24 False),
        windows=[w {bounds=Rect 23 1 157 53} | w<-windows opened],buffers=M.map highlightDocument (buffers opened)}
  _<-evaluate (sum [documentWidth doc+maybe 0 length (documentSourceRows doc) | doc<-M.elems (buffers docked)])
  -- Drag starts from an already presented source. Do not include lazy initial
  -- syntax preparation in this compositor allocation receipt.
  _<-evaluate (force (frameRows docked))
  let resized=fst (handleEvent (V.EvMouseDown 31 20 V.BLeft []) docked {drag=Just DockSizing})
  before<-getAllocationCounter
  _<-evaluate (force (frameRows resized))
  after<-getAllocationCounter
  -- Keep a prepared redraw within its allocation budget. A larger allowance
  -- requires measured attribution and a feature benefit, not a silent rebaseline.
  check "prepared dock-neighbor frame export stays within 6 MB" (before-after<6000000)
  let d=addDocument Nothing (newBuffer "λ\nhello") (initialDesktop (80,25))
      screens=map frameRows [d,insertText "world " d,d {screenSize=(100,30)}]
  check "frame exposes editor window metadata for the real native session frontend"
    (parseMaybe (withObject "metadata" (.: "editorWindows")) (object (frameMetadata "." d))==Just [object ["id" .= (1::Int),"title" .= ("NONAME1.HS"::T.Text),"selected" .= True,"enabled" .= True]])
  let second=addDocument Nothing (newBuffer "second") d
      focus=FocusWindow 1
      focused=fst (applyInput focus second)
      closed=closeActive (focusWindow 1 second)
      blocked=fst (runCommand About second)
      opaque=second {buffers=M.map (\doc->doc {documentBuffer=error "Dock metadata forced buffer"}) (buffers second)}
  check "stable Dock focus selects a live target without effects" (fmap windowId (activeWindow focused)==Just 1 && null (snd (applyInput focus second)))
  check "closed Dock target refuses focus without redirecting" (fmap windowId (activeWindow (fst (applyInput focus closed)))==fmap windowId (activeWindow closed))
  check "Dock focus cannot switch a modal owner" (fmap windowId (activeWindow (fst (applyInput focus blocked)))==fmap windowId (activeWindow blocked) && dialog (fst (applyInput focus blocked))==dialog blocked)
  guestFocus<-applyGuestInput focus (beginGuestInput second)
  check "agent input cannot invoke native Dock focus" (case guestFocus of Left _->True; _->False)
  let oddName=d {buffers=M.adjust (\doc->doc {documentFile=Just (FileState "/project/odd\nname\t.hs" Nothing)}) 1 (buffers d)}
  check "legal control characters in filenames are safe display metadata"
    (case editorWindowEntries oddName of [(1,title,_,_)]->title=="odd·name·.hs"; _->False)
  check "window title projection and focus never force buffers" (length (editorWindowEntries opaque)==2 && fmap windowId (activeWindow (fst (applyInput focus opaque)))==Just 1)
  check "Dock focus input uses bounded stable IDs" (parseEither parseInput (object ["type" .= ("focus-window"::T.Text),"id" .= (1::Int)])==Right focus && case parseEither parseInput (object ["type" .= ("focus-window"::T.Text),"id" .= (0::Int)]) of Left _->True; _->False)
  let menu fields=parseEither parseInput (object (["type" .= ("menu"::T.Text)]++fields))
  let canonical=map Commands.builtinIdentifier Commands.builtinCommands
  check "public command identities are unique" (length canonical==length (nub canonical))
  check "canonical names do not depend on constructor spelling" (all (T.isPrefixOf "hide.") canonical)
  forM_ protocolCommands $ \cmd -> case cmd of
    Disabled{} -> check "separators expose no command" (Commands.commandIdentifier cmd==Nothing)
    Help -> check "contributed Help requires an exact menu lifetime" (either (const True) (const False) (menu ["command" .= ("hide.help.contents"::T.Text)]))
    _ -> do
      check "every menu action has a stable identity" (Commands.commandIdentifier cmd/=Nothing)
      forM_ (Commands.commandIdentifier cmd) $ \name ->
        check "every public name resolves to its exact action" (menu ["command" .= name]==Right (MenuCommand cmd))
  check "parameterized command injection has no public name" (Commands.commandIdentifier (DebugCommand "unregistered")==Nothing)
  check "stable namespaced menu ID resolves independently of constructor spelling" (menu ["command" .= ("hide.file.new"::T.Text)]==Right (MenuCommand New))
  check "parameterized commands use explicit namespaced IDs" (menu ["command" .= ("hide.debug.step-into"::T.Text)]==Right (MenuCommand (DebugCommand "stepIn")))
  check "named menu ignores a stale positional index" (menu ["command" .= ("hide.file.new"::T.Text),"index" .= (999::Int)]==Right (MenuCommand New))
  check "unknown named menu is rejected" (either (const True) (const False) (menu ["command" .= ("future-command"::T.Text)]))
  check "menu input requires a command identity" (either (const True) (const False) (menu ["index" .= (0::Int)]))
  let platformMaps=either (error . show) id (Commands.configuredBindings [] (M.singleton "macos" (M.singleton "source" (M.fromList [("hide.edit.copy",["Cmd+Shift+J"]),("hide.edit.paste",["Cmd+Shift+K"])]))))
      browser=modifyActive (\w->w {selection=Selection 0 5}) (addDocument Nothing (newBuffer "hello") d) {browserFrontend=True,nativeMac=True,videoMode=Just 3,keyBindings=platformMaps}
      parsedKey=parseEither parseInput (object ["type" .= ("key"::T.Text),"key" .= ("j"::T.Text),"mods" .= (["cmd","shift"]::[T.Text])])
  check "Command wire input resolves effective clipboard copy" (case parsedKey of Right input->snd (applyInput input browser)==[WriteBrowserClipboard "hello"]; _->False)
  check "Command clipboard remap requests platform paste and disables the old shortcut" (snd (applyInput (Key "k" [V.MMeta,V.MShift]) browser)==[ReadBrowserClipboard] && null (snd (applyInput (Key "v" [V.MMeta]) browser)))
  let projected=parseMaybe (withObject "metadata" (.: "bindings")) (object (frameMetadata "/" browser))::Maybe [(T.Text,T.Text)]
  check "focused frame advertises effective clipboard chords only" (maybe False (\entries->lookup "Cmd+Shift+J" entries==Just "hide.edit.copy" && lookup "Cmd+C" entries==Nothing) projected)
  let modalMaps=either (error . show) id (Commands.configuredBindings [] (M.singleton "macos" (M.singleton "dialog" (M.fromList [("hide.edit.copy",["Cmd+Shift+J"]),("hide.edit.paste",["Cmd+Shift+K"])]))))
      modalBrowser=prompt "Edit" Information [TextArea "Text" True (newBuffer "field") (Selection 0 5) 0 0] browser {keyBindings=modalMaps}
  check "browser remapped modal Copy exports the field selection" (snd (applyInput (Key "j" [V.MMeta,V.MShift]) modalBrowser)==[WriteBrowserClipboard "field"])
  check "browser remapped modal Paste requests clipboard from its field owner" (snd (applyInput (Key "k" [V.MMeta,V.MShift]) modalBrowser)==[ReadBrowserClipboard] && null (snd (applyInput (Key "v" [V.MMeta]) modalBrowser)))
  let modalProjection=parseMaybe (withObject "metadata" (.: "bindings")) (object (frameMetadata "/" modalBrowser))::Maybe [(T.Text,T.Text)]
  check "browser modal projection uses only its allowed effective chords" (maybe False (\entries->lookup "Cmd+Shift+J" entries==Just "hide.edit.copy" && lookup "Cmd+C" entries==Nothing && all ((/= "hide.file.save").snd) entries) modalProjection)
  let bindings=either (error . show) id (Commands.platformBindings [] Bindings.TerminalPlatform (M.singleton "source" (M.singleton "hide.file.save" ["Ctrl+Shift+S"])))
      daemon=(addDocument (Just (FileState "/project/Main.hs" Nothing)) (newBuffer "hello") d) {browserFrontend=True,videoMode=Just 3,keyBindings=bindings}
      terminal=fst (applyInput (Frontend Nothing False) daemon)
  check "terminal attachment enables custom source keys without losing clipboard transport" (case snd (applyInput (Key "s" [V.MCtrl,V.MShift]) terminal) of [SaveDocument{}]->browserFrontend terminal; _->False)

  let review=d {dialog=Just (Dialog "Review" (PermissionDialog "approve:1") [TextArea "diff" True (newBuffer "private diff") (Selection 0 7) 0 0] 0 ["Allow once","Deny"] [])}
      selected=parseMaybe (withObject "metadata" (.: "selection")) (object (frameMetadata "/" review))::Maybe T.Text
      (copied,copyEffects)=applyInput (BrowserCommand Copy) review
      (cut,cutEffects)=applyInput (BrowserCommand Cut) review
      text desktop=case editableDialogField desktop of Just (TextArea _ _ b _ _ _) -> contents b; _ -> ""
  check "human browser clipboard reads only focused review selection" (selected==Just "private" && clipboard copied=="private" && copyEffects==[WriteBrowserClipboard "private"])
  check "human browser cut changes only review text" (text cut==" diff" && activeText cut==activeText d && cutEffects==[WriteBrowserClipboard "private"])
  check "browser paste requests the system clipboard for the review" (applyInput (BrowserCommand Hide.Model.Paste) review==(review,[ReadBrowserClipboard]))
  reviewInputs<-mapM (\input->applyGuestInput input review) [BrowserCommand Copy,BrowserCommand Cut,BrowserCommand SelectAll,Hide.Protocol.Paste "bad"]
  check "agent browser commands cannot inspect or edit human review" (all (either (const True) (const False)) reviewInputs)
  let search=fst (runCommand Find d)
      typed=fst (applyInput (Hide.Protocol.Paste "hello") search)
      replacement=fst (applyInput (BrowserCommand Replace) typed)
      filled=fst (applyInput (Hide.Protocol.Paste "world") replacement)
      back=fst (applyInput (BrowserCommand Find) filled)
      again=fst (applyInput (BrowserCommand Replace) back)
  check "browser Find and Replace commands switch tabs without losing text" (case dialog again of Just dg -> [value | Input _ value _<-fields dg]==["hello","world"]; _->False)
  check "browser understands modern search and conversation commands" (and [parseEither parseInput (object ["type" .= ("command"::T.Text),"command" .= name])==Right (BrowserCommand cmd) | (name,cmd)<-[("hide.search.replace"::T.Text,Replace),("hide.agents.conversation",Conversation),("hide.agents.new",AgentNew)]])
  check "browser shortcut aliases are rejected" (all (either (const True) (const False) . parseEither parseInput . (\name->object ["type" .= ("command"::T.Text),"command" .= name])) (["copy","find","newConversation"]::[T.Text]))
  let unavailable=initialDesktop (80,25)
  check "session Options actions remain available before opening a document" (all (menuCommandAvailable unavailable) [EditorOptions,EnvironmentOptions,ChatInputOptions,AgentOptions,AgentPermissions,AgentGuidance,AutocompleteCommand "settings",ReloadBindings,InspectBindings])
  let optionsMenu command=case [(i,j) | (i,(_,_,items))<-zip [0..] menus,(j,MenuItem _ _ action)<-zip [0..] items,action==command] of
        position:_ -> unavailable {menu=Just position}
        [] -> error "binding command missing from Options menu"
      enter command=snd (handleEvent (V.EvKey V.KEnter []) (optionsMenu command))
  check "empty desktop Options menu actually requests binding reload" (enter ReloadBindings==[ReloadKeyBindings (startingDirectory unavailable)])
  check "empty desktop Options menu actually requests effective binding inspection" (enter InspectBindings==[InspectKeyBindings (Just (Bindings.TerminalPlatform,Bindings.SourceKeys)) Nothing])
  check "empty desktop native/browser named menu route admits binding tasks" (snd (applyInput (MenuCommand ReloadBindings) unavailable)==enter ReloadBindings && snd (applyInput (MenuCommand InspectBindings) unavailable)==enter InspectBindings)
  forM_ [ReloadBindings,InspectBindings] $ \command -> do
    let native=do
          token<-findIndex (==command) nativeCommands
          nativeMenuEvent 9 [11,token,9]
    check "empty desktop stamped native binding menu action resolves and dispatches" (native==Just command && maybe [] (\action->snd (applyInput (MenuCommand action) unavailable)) native==enter command)
  check "queued native and browser menu checks share current availability" (not (menuCommandAvailable unavailable SplitVertical) && null (snd (applyInput (MenuCommand SplitVertical) unavailable)))
  let emptySaveMenu=snapshotHtml unavailable {menu=Just (0,2)}
  check "main menu renders unavailable Save with disabled foreground" ("color:rgb(85,85,85);background:rgb(0,170,0)" `T.isInfixOf` emptySaveMenu)
  let primary=selectConversationView "" "Primary" (initialDesktop (80,25))
      drafted=primary {composerBuffer=newBuffer "unsent",composerSelection=Selection 6 6}
      child=selectConversationView "child" "Worker" drafted
  check "browser exit protects an inactive conversation draft" (webDirty child)
  _<-foldFrames check [] screens
  rejects "unknown display encoding rejected" (decodeFrame [] (BS.pack [9]) >> pure ())
  rejects "bad compressed stream rejected" (decodeFrame [] (BS.pack [0,255,255]) >> pure ())
  let compressed bytes=BS.cons 0 (BL.toStrict (Z.compress bytes))
      rejectsWith reason bytes=do
        result<-try (decodeFrame [] (compressed bytes)) :: IO (Either SomeException (Value,[Value]))
        check reason (case result of Left err->reason `isInfixOf` displayException err; Right _->False)
  rejectsWith "Invalid display JSON" "{not json}"
  rejectsWith "Invalid display JSON" "{\"rows\":[[0,[]]]} null"
  rejectsWith "Invalid display row indices" "{\"rows\":[[0,[]],[0,[]]]}"
  rejectsWith "Incomplete display frame" "{\"rows\":[[1,[]]]}"
  rejectsWith "Display frame exceeds 64 MiB" (BL.replicate 67108865 32)
  putStrLn "protocol checks passed"
  where
    foldFrames _ old []=pure old
    foldFrames check old (rows:rest)=do
      let reset=null old || length old/=length rows
      (_,actual)<-decodeFrame old (BL.toStrict (framePacket reset old rows ["size" .= (80::Int,length rows)]))
      _<-check "display reset and dictionary patches reconstruct identical cells" (actual==rows)
      foldFrames check actual rest
