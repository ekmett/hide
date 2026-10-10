-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : WebCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module WebCheck (checks) where
import EditorFixture (withEditorFixture, sameBufferVersions)
import SourceWindowFixture (sourceFixtureBuffer)
import Data.Bits ((.&.), shiftR)
import Control.Monad (unless,forM_)
import Data.Aeson
import qualified Codec.Compression.Zlib.Raw as Z
import qualified Data.Map.Strict as M
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Hide.Model as Model
import qualified Graphics.Vty as V
import Hide.App (applyEffects)
import Hide.SidebarCommands (withSidebarCommands,sidebarEffects,awaitFileOpening)
import Hide.Web
import Hide.Model hiding (Paste)
import Hide.Buffer
import Hide.Frontend
import qualified Hide.Protocol as P
import Hide.Plugin.Canvas
import Codec.Picture (generateImage,encodePng,PixelRGBA8(..))

checks :: IO ()
checks = withEditorFixture "" (initialDesktop (80,25)) $ \chatBase->do
  canvasChecks
  let check name ok=unless ok (error name)
      base=addDocument Nothing (newBuffer "") (initialDesktop (80,25))
      typed=fst (applyInput (Key "λ" []) base)
      pasted=fst (applyInput (Paste "\n👩🏽\x200d\&💻") typed)
      modified=pasted {heldModifiers=[V.MCtrl],drag=Just DockSizing}
      released=fst (applyInput Blur modified)
  check "leave guard covers modified buffers and unsent conversation drafts"
    (not (webDirty base) && webDirty typed && webDirty (setComposerInput (newBuffer "unsent thought") (Selection 0 0) True chatBase))
  check "web backend selection" (chooseBackend (Just "web") []==Right Web && chooseBackend Nothing [Web,Terminal]/=Right Web)
  let platform=object ["type" .= ("frontend"::T.Text),"mode" .= (3::Int),"mac" .= True]
      macFrontend=parseMaybe parseInput platform
  check "client platform survives remote transport for native key labels"
    (maybe False (nativeMac . fst . (`applyInput` base)) macFrontend &&
     not (nativeMac (fst (applyInput (Frontend Nothing False) base {nativeMac=True}))))
  check "browser text and paste preserve Unicode" (activeText pasted=="λ\n👩🏽\x200d\&💻")
  check "browser blur releases drag and modifiers" (null (heldModifiers released) && drag released==Nothing)
  let (browserSaved,browserSaveEffects)=applyInput (Key "F2" []) pasted
      (terminalSaved,terminalSaveEffects)=handleEvent (V.EvKey (V.KFun 2) []) pasted
  sameSaved<-sameBufferVersions browserSaved terminalSaved
  check "browser F2 matches existing Save event" (sameSaved && browserSaveEffects==terminalSaveEffects && dialog browserSaved==dialog terminalSaved)
  let (browserTab,browserTabEffects)=applyInput (Key "Tab" [V.MShift]) base
      (terminalTab,terminalTabEffects)=handleEvent (V.EvKey V.KBackTab [V.MShift]) base
  check "Shift+Tab is backward traversal" ((windowId <$> activeWindow browserTab)==(windowId <$> activeWindow terminalTab) && browserTabEffects==terminalTabEffects && menu browserTab==menu terminalTab)
  check "ordinary screen text uses compact UTF-8 runs" (BL.length (encode (frameRows base))<8192)
  check "frame row count follows grid" (length (frameRows base)==25)
  check "origin must match exact loopback host" (allowedOrigin "127.0.0.1:123" "127.0.0.1:123" (Just "http://127.0.0.1:123") &&
    not (allowedOrigin "127.0.0.1:123" "evil.example" (Just "http://evil.example")) && not (allowedOrigin "127.0.0.1:123" "127.0.0.1:123" Nothing))
  forM_ [object ["type" .= ("resize"::T.Text),"width" .= (0::Int),"height" .= (25::Int)],
         object ["type" .= ("mouse"::T.Text),"action" .= ("down"::T.Text),"x" .= (999999::Int),"y" .= (0::Int)],
         object ["type" .= ("key"::T.Text),"key" .= ("F2"::T.Text),"mods" .= ["bad"::T.Text]],
         object ["type" .= ("unknown"::T.Text)]] $ \bad ->
    check "invalid browser events rejected" (parseMaybe parseInput bad==Nothing)
  check "keyboard event parses" (parseMaybe parseInput (object ["type" .= ("key"::T.Text),"key" .= ("F2"::T.Text),"mods" .= ([]::[T.Text])])==Just (Key "F2" []))
  let browser=pasted {browserFrontend=True}
      selected=fst (runCommand SelectAll browser)
      (copied,copyEffects)=runCommand Copy selected
      menuCommands d=[cmd | MenuItem _ _ cmd<-menuItemsFor d 0]
  imported<-withSidebarCommands $ \host->uncurry (sidebarEffects host applyEffects) (applyInput (UploadFile "sample.bin" (BS.pack [0,255,65])) browser) >>= awaitFileOpening host . snd
  textImport<-withSidebarCommands $ \host->uncurry (sidebarEffects host applyEffects) (applyInput (UploadFile "demo.cabal" "name: demo") browser) >>= awaitFileOpening host . snd
  check "Download only in browser File menu" (Download `elem` menuCommands browser && Download `notElem` menuCommands base)
  check "menu Copy always exports system clipboard, including repeated copies"
    (copyEffects==[WriteBrowserClipboard (activeText pasted)] && snd (runCommand Copy copied)==copyEffects)
  check "menu Paste requests browser clipboard" (snd (runCommand Model.Paste browser)==[ReadBrowserClipboard])
  check "binary drop preserves bytes in a new unsaved-path window"
    (length (windows imported)==length (windows browser)+1 &&
     fmap (bufferBytes . documentBuffer) (activeDocument imported)==Just (BS.pack [0,255,65]) &&
     (activeDocument imported >>= documentFile)==Nothing &&
     (activeDocument imported >>= documentSuggestedName)==Just "sample.bin")
  check "text drop remains editable and retains suggested filename"
    (activeText textImport=="name: demo" && activeText (insertText "x" textImport)/=activeText textImport && currentPath textImport=="demo.cabal")
  check "Download refers to exact active buffer"
    (snd (runCommand Download imported)==[DownloadDocument (sourceFixtureBuffer w) | w<-take 1 (windows imported)])
  let found=findText "one" (addDocument Nothing (newBuffer "one two one") (initialDesktop (80,25)))
      next=fst (applyInput (BrowserCommand FindNext) found)
      previous=fst (applyInput (BrowserCommand FindPrevious) next)
  check "browser find next and previous navigate matches" (fmap selection (activeWindow previous)==fmap selection (activeWindow found))
  forM_ ["../escape","a/b","a\\b",".","..",""] $ \name ->
    check "upload names cannot become server paths" (parseMaybe parseInput (object ["type" .= ("upload"::T.Text),"name" .= (name::T.Text)])==Nothing)
  let draft=setComposerInput (newBuffer "draft") (Selection 0 0) True chatBase
  check "Exit asks before discarding a conversation draft" (null (snd (runCommand Quit draft)) && fmap purpose (dialog (fst (runCommand Quit draft)))==Just DiscardDraft)
  let modal=fst (runCommand SaveAs selected)
  forM_ [SelectAll,Copy,Cut,Undo,Redo,Find,FindNext] $ \cmd ->do
    let (blocked,blockedEffects)=applyInput (BrowserCommand cmd) modal
    sameVersions<-sameBufferVersions blocked modal
    check "browser menu commands cannot edit behind a modal dialog"
      (sameVersions && fmap selection (activeWindow blocked)==fmap selection (activeWindow modal) &&
       blockedEffects==[WriteBrowserClipboard "" | cmd `elem` [Copy,Cut]] && dialog blocked==dialog modal)
  let terminal=addDocument Nothing (newBuffer "") base
      terminalView=terminal {buffers=M.adjust (\doc->doc {documentLabel=Just "Terminal 1"}) (nextId base) (buffers terminal)}
  check "terminal Ctrl+C reaches the PTY" (snd (applyInput (Key "c" [V.MCtrl]) terminalView)==[ServiceAction "terminal-input" ["1","\ETX"]])
  let originals=map frameRows [base,pasted,pasted {screenSize=(240,80)},base]
      decodePacket old packet = do
        let tag=BL.head packet
            dictionary=if tag==0 then BS.empty else frameDictionary old
            n=BS.length dictionary
            prefix=BL.pack (map fromIntegral [0,n .&. 255,n `shiftR` 8,(65535-n) .&. 255,(65535-n) `shiftR` 8])<>BL.fromStrict dictionary
        value<-decode (BL.drop (fromIntegral n) (Z.decompress (prefix<>BL.tail packet)))
        rowPairs<-parseMaybe (withObject "frame" (\o->o .: "rows")) value :: Maybe [(Int,Value)]
        pure (M.elems (M.union (M.fromList rowPairs) (if tag==2 then M.fromList (zip [0..] old) else M.empty)))
  forM_ (zip ([]:originals) originals) $ \(old,rows) -> do
    let reset=null old || length old/=length rows
        candidates=frameCandidates reset old rows ["size" .= (80::Int, length rows)]
        chosen=framePacket reset old rows ["size" .= (80::Int,length rows)]
    check "adaptive frame chooses actual minimum byte count" (BL.length chosen==minimum (map BL.length candidates))
    forM_ candidates $ \packet -> check "full and row packets reconstruct identical Unicode screens" (decodePacket old packet==Just rows)
  let rows=frameRows pasted
      unchanged=framePacket False rows rows []
  check "metadata-only update chooses row encoding" (BL.head unchanged==2 && decodePacket rows unchanged==Just rows)
  check "browser system theme preserves explicit appearance" (darkAppearance (fst (applyInput (SystemTheme True) base)) && not (darkAppearance (fst (applyInput (SystemTheme True) base {appearance=LightMode}))))
  putStrLn "web protocol checks passed"

-- Resource residency changes independently of the cell-frame dictionary.
canvasChecks :: IO ()
canvasChecks=do
  let check label good=unless good (error label)
      png=BL.toStrict (encodePng (generateImage (\x y->PixelRGBA8 (fromIntegral x) (fromIntegral y) 117 192) 300 300))
      epoch=T.replicate 48 "a"
  image<-prepareImage png >>= either (error . T.unpack) pure
  let surface=CanvasSurface 1 1 image (0,0,10,10) (0,0,10,10) "image" "300 × 300" "1" True
      scene=CanvasScene [surface] (BS.replicate 200 0)
      (first,a,more)=P.canvasTransfer (P.CanvasSender epoch M.empty) scene
      (done,b,finished)=P.canvasTransfer first scene
      (_,steady,steadyPending)=P.canvasTransfer done scene
      moved=scene {canvasSurfaces=[surface {canvasTarget=(2,3,8,8)}]}
      (_,panned,_)=P.canvasTransfer done moved
      (closed,released,_)=P.canvasTransfer done (CanvasScene [] (BS.replicate 200 0))
      (_,reopened,_)=P.canvasTransfer closed scene
      kind (P.JsonPacket value)=parseMaybe (withObject "control" (.: "type")) value :: Maybe T.Text
      kind _=Nothing
      chunks=[bytes | P.BinaryPacket bytes<-a++b]
  check "image transfer uses bounded chunks and exact source bytes"
    (more && not finished && map BS.length chunks==[262144,97856] && BS.concat chunks==imageRGBA image)
  check "only first chunk starts immutable image residency"
    (map kind a==map Just ["canvas-resource","canvas-chunk"]++[Nothing] && map kind b==[Just "canvas-chunk",Nothing])
  check "pan and occlusion do not retransmit image payloads" (null steady && not steadyPending && null panned)
  check "closing releases resources and fresh admission restarts at the header"
    (map kind released==[Just "canvas-release"] && take 1 (map kind reopened)==[Just "canvas-resource"])
  let (_,interrupted,_)=P.canvasTransfer first (CanvasScene [] BS.empty)
  check "closing during an upload emits no late pixel chunk" (map kind interrupted==[Just "canvas-release"])
  other<-prepareImage png >>= either (error . T.unpack) pure
  let (earlier,later)=if imageResourceId image<imageResourceId other then (image,other) else (other,image)
      activeScene=scene {canvasSurfaces=[surface {canvasImage=later}]}
      (active,_,_)=P.canvasTransfer (P.CanvasSender epoch M.empty) activeScene
      added=activeScene {canvasSurfaces=[surface {canvasImage=later},surface {canvasId=2,canvasSlot=2,canvasImage=earlier}]}
      (_,during,_) = P.canvasTransfer active added
  check "newly admitted image cannot interrupt an existing upload cursor"
    (map kind during==[Just "canvas-chunk",Nothing])
