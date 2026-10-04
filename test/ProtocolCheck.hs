{-# LANGUAGE OverloadedStrings #-}
module ProtocolCheck (checks) where
import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless, forM_)
import Data.Aeson
import Data.Aeson.Types (parseEither, parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO
import qualified Hide.Commands as Commands
import Data.List (nub, findIndex)
import qualified Data.Map.Strict as M
import qualified Graphics.Vty as V
import Hide.Protocol
import Hide.Model
import Hide.Window (nativeCommands, nativeMenuEvent)
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
  let d=addDocument Nothing (newBuffer "λ\nhello") (initialDesktop (80,25))
      screens=map frameRows [d,insertText "world " d,d {screenSize=(100,30)}]
  let menu fields=parseEither parseInput (object (["type" .= ("menu"::T.Text)]++fields))
  let canonical=map Commands.builtinIdentifier Commands.builtinCommands
  check "public command identities are unique" (length canonical==length (nub canonical))
  check "canonical names do not depend on constructor spelling" (all (T.isPrefixOf "hide.") canonical)
  forM_ protocolCommands $ \cmd -> case cmd of
    Disabled{} -> check "separators expose no command" (Commands.commandIdentifier cmd==Nothing)
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
  let bindings=either (error . show) id (Commands.terminalBindings (M.singleton "source" (M.singleton "hide.file.save" ["Ctrl+Shift+S"])))
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
  check "agent browser commands cannot inspect or edit human review" (all (\input->case applyGuestInput input review of Left _ -> True; _ -> False) [BrowserCommand Copy,BrowserCommand Cut,BrowserCommand SelectAll,Hide.Protocol.Paste "bad"])
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
  check "empty desktop Options menu actually requests effective binding inspection" (enter InspectBindings==[InspectKeyBindings Nothing Nothing])
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
  putStrLn "protocol checks passed"
  where
    foldFrames _ old []=pure old
    foldFrames check old (rows:rest)=do
      let reset=null old || length old/=length rows
      (_,actual)<-decodeFrame old (BL.toStrict (framePacket reset old rows ["size" .= (80::Int,length rows)]))
      _<-check "display reset and dictionary patches reconstruct identical cells" (actual==rows)
      foldFrames check actual rest
