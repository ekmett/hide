{-# LANGUAGE CPP, OverloadedStrings #-}
module MenuCommandsCheck (checks) where
import Control.Concurrent (threadDelay)
import Control.Exception (bracket,finally)
import Control.Concurrent.MVar
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv,setEnv,unsetEnv)
import Control.Monad (unless)
import Data.Aeson
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Data.List (findIndex)
import System.Timeout (timeout)
import Hide.Buffer (contents)
import Hide.DocsMCP
import Hide.MenuCommands
import Hide.Model
import Hide.Plugin.Command (withRegistry,registerCommand,CommandDef(..),Codec(..))
import qualified Hide.Plugin.Menu as Plugin
import Hide.Links (prepareMarkdown)
import MenuExtension
#if defined(WITH_WEB) || defined(WITH_REMOTE)
import Data.Aeson.Types (parseEither)
import Hide.GuestAccess (beginGuestInput)
import Hide.Protocol
import Hide.RemoteWindow
import Hide.Window (nativeCommands, nativeCommandsFor, nativeMenuEventFor)
#endif

checks :: IO ()
checks=bracket (lookupEnv "hide_datadir") (maybe (unsetEnv "hide_datadir") (setEnv "hide_datadir")) $ \_->do
  getCurrentDirectory >>= setEnv "hide_datadir"
  runChecks

runChecks :: IO ()
runChecks=withDocsCommands $ \docs->withRegistry $ \registry->withMenuCommands docs $ \host->do
  extension<-either (error . show) pure =<< registerExtension registry (menuContributions host)
    (\context text->prepareMarkdown (Plugin.invocationColumns context) "/tmp/README.md" "" text)
  metadata<-Plugin.menuSnapshot (menuContributions host)
  let initial=(initialDesktop (80,25)) {contributedMenus=metadata,agentMenuRefs=menuAgentReferences host,menusActive=True}
      helpRef=case [Plugin.menuReference entry | entry<-metadata,Plugin.menuName (Plugin.menuReference entry)=="hide.help.contents"] of ref:_->ref; _->error "missing help contribution"
      check label condition=unless condition (error label)
      waitDoc desktop=do
        updated<-tickMenus host desktop
        case activeDocument updated of
          Just _->pure updated
          _ | "failed" `T.isInfixOf` status updated || "expired" `T.isInfixOf` status updated->error (T.unpack (status updated))
            | otherwise->threadDelay 5000 >> waitDoc updated
      run desktop effects=do
        (_,queued)<-menuEffects host (\_ _->error "registered action missed menu worker") desktop effects
        timeout 5000000 (waitDoc queued) >>= maybe (error "menu worker did not complete") pure
  let (fromF1,effects)=handleEvent (V.EvKey (V.KFun 1) []) initial
  check "F1 resolves live contributed Help lifetime" (effects==[InvokeMenu helpRef Plugin.HumanMenu])
  help<-run fromF1 effects
  check "registered help prepares styled document with relative links" (maybe False (\doc->documentMarkdownPath doc/=Nothing && not (null (documentLinks doc)) && documentLabel doc==Just "Haskell Help") (activeDocument help))
  let helpIndex=case findIndex (\(title,_,_)->title=="Help") menus of Just index->index; _->error "missing help slot"
      popup=initial {menu=Just (helpIndex,0)}
      (chosen,popupEffects)=handleEvent (V.EvKey V.KEnter []) popup
  check "Help popup invokes same exact contribution as F1" (popupEffects==effects)
  _<-run chosen popupEffects
#if defined(WITH_WEB) || defined(WITH_REMOTE)
  packet<-either error pure (parseEither parseInput (object ["type" .= ("menu"::T.Text),"command" .= Plugin.menuName extension,"registry" .= Plugin.menuEpoch extension,"generation" .= Plugin.menuGeneration extension]))
  let transport=object (frameMetadata "/tmp" initial++["menuCommands" .= map fst protocolMenuCommands])
  frame<-either error pure (parseRemoteFrame transport (replicate 25 (toJSON ([]::[Value]))))
  let extensionIndex=case findIndex ((==extension) . Plugin.menuReference) metadata of Just index->index; _->error "missing extension snapshot"
      nativeToken=length nativeCommands+extensionIndex
  nativePacket<-maybe (error "native contribution failed to resolve") pure (remoteNativeMenuInput frame 19 [11,nativeToken,19])
  check "native contribution transports exact ID and generation" (nativePacket==object ["type" .= ("menu"::T.Text),"command" .= Plugin.menuName extension,"registry" .= Plugin.menuEpoch extension,"generation" .= Plugin.menuGeneration extension])
  check "native old menu incarnation refuses extension event" (remoteNativeMenuInput frame 20 [11,nativeToken,19]==Nothing)
  check "local native catalogue retains exact registered action" (nativeMenuEventFor (nativeCommandsFor initial) 19 [11,nativeToken,19]==Just (contributionCommand initial (metadata !! extensionIndex)))
  check "native Help row carries its registered token and extension joins Help menu" (case lookup "Help" (remoteMenuLayout frame) of Just rows->any (\(title,_,token)->title=="Extension manual" && token==nativeToken) rows && any (\(title,_,token)->title=="Contents" && token>=length nativeCommands) rows; _->False)
  nativeInput<-either error pure (parseEither parseInput nativePacket)
  check "native emitted packet reaches same actual host invocation" (snd (applyInput nativeInput initial)==[InvokeMenu extension Plugin.HumanMenu])
  let (invoked,extensionEffects)=applyInput packet initial
  check "extension declaration reaches actual transported route" (extensionEffects==[InvokeMenu extension Plugin.HumanMenu])
  opened<-run invoked extensionEffects
  check "transported extension installs independent Markdown and links" (maybe False (T.isInfixOf "Independent extension" . contents . documentBuffer) (activeDocument opened) && maybe False (not . null . documentLinks) (activeDocument opened))
  check "extension metadata cannot grant agent invocation" (case applyGuestInput packet (beginGuestInput initial) of Left _->True; _->False)
  helpPacket<-either error pure (parseEither parseInput (object ["type" .= ("menu"::T.Text),"command" .= Plugin.menuName helpRef,"registry" .= Plugin.menuEpoch helpRef,"generation" .= Plugin.menuGeneration helpRef]))
  withMenuCommands docs $ \nextHost->do
    nextMetadata<-Plugin.menuSnapshot (menuContributions nextHost)
    let nextDesktop=initial {contributedMenus=nextMetadata,agentMenuRefs=menuAgentReferences nextHost}
    check "prior scope same name/generation cannot target a new session registry" (null (snd (applyInput helpPacket nextDesktop)))
  check "host-permitted help preserves agent origin" (case applyGuestInput helpPacket (beginGuestInput initial) of Right (_,requests)->requests==[InvokeMenu helpRef Plugin.AgentMenu]; _->False)
  stalePacket<-either error pure (parseEither parseInput (object ["type" .= ("menu"::T.Text),"command" .= Plugin.menuName extension,"registry" .= Plugin.menuEpoch extension,"generation" .= (Plugin.menuGeneration extension+100)]))
  let oversized=object ["type" .= ("menu"::T.Text),"command" .= Plugin.menuName extension,"registry" .= Plugin.menuEpoch extension,"generation" .= (9007199254740992::Integer)]
  check "contributed wire generations are bounded" (case parseEither parseInput oversized of Left _->True; _->False)
  check "stale transported generation cannot invoke extension" (null (snd (applyInput stalePacket initial)))
#endif
  -- Retire through the host owner while an admitted result is outstanding.
  (_,pending)<-menuEffects host (\_ _->error "missing host") initial [InvokeMenu extension Plugin.HumanMenu]
  retireMenuFromHost host extension
  threadDelay 100000
  refused<-tickMenus host pending
  check "ordered host retirement precedes late reply adoption" (activeDocument refused==Nothing && all ((/=extension) . Plugin.menuReference) (contributedMenus refused))
  (_,queuedOld)<-menuEffects host (\_ _->error "missing host") refused [InvokeMenu extension Plugin.HumanMenu]
  check "queued retired action has no document and explicit refusal" (activeDocument queuedOld==Nothing && "stale" `T.isInfixOf` status queuedOld)
  entered<-newEmptyMVar
  finished<-newEmptyMVar
  blocked<-newEmptyMVar
  withMenuCommands docs $ \closingHost->do
    let unit=Codec Null (const (Right ())) (const Null)
        textCodec=Codec Null (\value->case value of String valueText->Right valueText; _->Left "Expected text") String
        slow=CommandDef "example.cancel-on-close" "Closing" unit textCodec
          (\_ ()->(putMVar entered () >> takeMVar blocked >> pure (Right "# Closed")) `finally` putMVar finished ())
    command<-either (error . show) pure =<< registerCommand registry slow
    reference<-either (error . show) pure =<< Plugin.contributeMenu (menuContributions closingHost)
      (Plugin.MenuDef "example.cancel-on-close" "help" "extensions" 20 "Closing" "" False
        (Plugin.menuAction registry command () (\context valueText->prepareMarkdown (Plugin.invocationColumns context) "/tmp/README.md" "" valueText)))
    closingMetadata<-Plugin.menuSnapshot (menuContributions closingHost)
    let closingDesktop=initial {contributedMenus=closingMetadata,agentMenuRefs=menuAgentReferences closingHost}
    _<-menuEffects closingHost (\_ _->error "missing closing worker") closingDesktop [InvokeMenu reference Plugin.HumanMenu]
    began<-timeout 1000000 (takeMVar entered)
    check "closing worker starts" (began==Just ())
  joined<-tryTakeMVar finished
  check "session scope joins cancelled worker before registry closure" (joined==Just ())
  putStrLn "live menu command checks passed"
