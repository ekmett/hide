{-# LANGUAGE CPP, OverloadedStrings #-}
module RemoteWindowCheck (checks) where
import Control.Monad (unless)
import Data.List (elemIndex)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import Hide.RemoteWindow
import Hide.Accessibility (SemanticAudience(..),dialogSemantics)
import Hide.FrameTiming
import Hide.Window (nativeMenuEvent,nativeCommands)
import Hide.Buffer (newBuffer,Selection(..))
import qualified Hide.Model as Model
import qualified Data.Map.Strict
#ifdef WITH_REMOTE
import Data.Aeson.Types (parseEither)
import qualified Hide.Protocol as P
import Hide.Model (initialDesktop, addDocument, Desktop(..))
#endif

checks :: IO ()
checks = do
  let check name good = unless good (error name)
      (first,one)=requestFrame 100 emptyFrameTiming
      (second,two)=requestFrame 150 one
      (third,three)=requestFrame 300 two
      (coalesced,remaining)=settleFrame second three
      (lastStart,empty)=settleFrame third remaining
      (duplicate,_)=settleFrame third empty
      (_,afterNoop)=settleFrame first three
      (afterNoopStart,_)=settleFrame third afterNoop
  check "coalesced frame counts oldest represented demand and retains later input" (coalesced==Just 100 && lastStart==Just 300)
  check "no-op receipt clears its demand without poisoning next frame timing" (afterNoopStart==Just 150)
  check "replayed frame receipt cannot produce another sample" (duplicate==Nothing)
  let meta = object ["size" .= ([80,25]::[Int]), "bindings" .= ([]::[(T.Text,T.Text)]),"mode" .= (3::Int)]
      row = toJSON [(0::Int,0xffffff::Int,0::Int,0::Int,[String "abc",toJSON ("界"::T.Text,2::Int,False,0::Int,2::Int)])]
      rows = row : replicate 24 (toJSON ([]::[Value]))
      valid = either (const False) (const True) . parseRemoteFrame meta
  let epoch=T.replicate 48 "a"
      otherEpoch=T.replicate 48 "b"
      resource=T.replicate 48 "c"
      reset=object ["type" .= ("canvas-reset"::T.Text),"epoch" .= epoch]
      begin w h bytes=object ["type" .= ("canvas-resource"::T.Text),"epoch" .= epoch,"id" .= resource,"width" .= (w::Int),"height" .= (h::Int),"bytes" .= (bytes::Int)]
      chunk owner offset bytes=object ["type" .= ("canvas-chunk"::T.Text),"epoch" .= owner,"id" .= resource,"offset" .= (offset::Int),"length" .= (bytes::Int)]
      release=object ["type" .= ("canvas-release"::T.Text),"epoch" .= epoch,"id" .= resource]
      step state control=either error fst (admitCanvasControl state control)
      state0=step emptyCanvasReceiveState reset
      uploading=step state0 (begin 2 2 16)
      partial=step uploading (chunk epoch 0 3)
      retired=step partial release
      rejects state=either (const True) (const False) . admitCanvasControl state
  check "canvas receive cursor rejects absent/old epoch and malformed dimensions"
    (rejects emptyCanvasReceiveState (begin 2 2 16) && rejects uploading (chunk otherEpoch 0 3) && rejects state0 (begin 4096 4096 67108864) && rejects state0 (begin 2 2 15))
  check "canvas receive cursor rejects duplicate live begins and noncontiguous chunks"
    (rejects uploading (begin 2 2 16) && rejects partial (chunk epoch 2 4) && rejects partial (chunk epoch 3 14) && rejects partial (chunk epoch 3 0))
  check "canvas release cancels cursor, duplicate release is harmless and late chunks cannot resurrect"
    (rejects retired (chunk epoch 3 13) && not (rejects retired release) && rejects (step partial reset) (chunk epoch 3 13))
  let finished=step partial (chunk epoch 3 13)
      (_,header)=either error id (admitCanvasControl uploading (chunk epoch 0 3))
  check "canvas exact binary pairs cannot become ordinary cell frames"
    (validateCanvasChunk header (BS.pack [0,255,1])==Right () && either (const True) (const False) (validateCanvasChunk header (BS.pack [0,255])) && rejects finished (chunk epoch 16 1))
  let mask=BS.pack ([1,0,1,128,0,0]++replicate (80*25*2-6) 0)
      surface slot viewport=object ["id" .= (91::Int),"resource" .= resource,"slot" .= (slot::Int),"rect" .= (viewport::[Int]),
        "target" .= ([0,0,2,2]::[Double]),"name" .= ("safe λ <script>.png"::T.Text),"description" .= ("PNG, 2 by 2 pixels"::T.Text)]
      scene :: [Value] -> BS.ByteString -> Value
      scene surfaces bytes=object ["epoch" .= epoch,"surfaces" .= surfaces,"mask" .= TE.decodeUtf8 (B64.encode bytes)]
      frame value=parseRemoteFrame (object ["size" .= ([80,25]::[Int]),"bindings" .= ([]::[(T.Text,T.Text)]),"canvas" .= value]) rows
      validScene=scene [surface 1 [0,0,2,2]] mask
      canvasOf=either (const Nothing) remoteCanvas . frame
  check "canvas frame retains a complete bounded scene and exact little endian dim mask"
    (maybe False (\scene'->canvasMask scene'==mask && map canvasWindow (canvasSurfaces scene')==[91]) (canvasOf validScene))
  check "canvas semantics omit opaque resource IDs and wholly covered surfaces"
    (maybe False (\scene'->not (TE.encodeUtf8 resource `BS.isInfixOf` canvasAccessibility scene') && TE.encodeUtf8 "safe λ <script>.png" `BS.isInfixOf` canvasAccessibility scene') (canvasOf validScene))
  check "canvas empty scene shorthand clears ownership without a zero grid"
    (maybe False (\scene'->BS.null (canvasMask scene') && null (canvasSurfaces scene')) (canvasOf (scene [] BS.empty)) &&
     either (const True) (const False) (frame (scene [surface 1 [0,0,2,2]] BS.empty)))
  check "canvas scene rejects malformed mask bytes, absent owners and duplicate slots"
    (all (either (const True) (const False) . frame)
      [scene [surface 1 [0,0,2,2]] (BS.drop 1 mask),scene [] mask,scene [surface 1 [1,0,2,2]] mask,scene [surface 1 [0,0,2,2],surface 1 [0,0,2,2]] mask])
  check "canvas occlusion retains resource surface but omits image AX element"
    (maybe False (\scene'->not ("91" `BS.isInfixOf` canvasAccessibility scene') && length (canvasSurfaces scene')==1) (canvasOf (scene [surface 1 [0,0,2,2]] (BS.replicate (80*25*2) 0))))
  let windowMeta=object ["size" .= ([80,25]::[Int]),"bindings" .= ([]::[(T.Text,T.Text)]),"editorWindows" .= [object ["id" .= (71::Int),"title" .= ("Main.hs"::T.Text),"selected" .= True,"enabled" .= True]]]
      dockFrame=either error id (parseRemoteFrame windowMeta rows)
  check "actual remote native Dock event carries stable host target" (remoteDockWindowInput dockFrame 8 [16,71,8]==Just (object ["type" .= ("focus-window"::T.Text),"id" .= (71::Int)]))
  check "remote Dock refuses closed stale and disabled targets" (all ((==Nothing) . remoteDockWindowInput dockFrame 8) [[16,0,8],[16,71,7],[16,93,8]] && remoteDockWindowInput dockFrame {remoteWindows=[(71,"Main.hs",True,False)]} 8 [16,71,8]==Nothing)
  let duplicateWindows=object ["size" .= ([80,25]::[Int]),"bindings" .= ([]::[(T.Text,T.Text)]),"editorWindows" .= [object ["id" .= (71::Int),"title" .= ("Main.hs"::T.Text),"selected" .= True,"enabled" .= True],object ["id" .= (71::Int),"title" .= ("Other.hs"::T.Text),"selected" .= False,"enabled" .= True]]]
  check "remote rejects duplicate editor window identities" (either (const True) (const False) (parseRemoteFrame duplicateWindows rows))
#ifdef WITH_REMOTE
  let named=Model.addDocument Nothing (newBuffer "payload") (Model.initialDesktop (80,25))
      odd=named {Model.buffers=Data.Map.Strict.map (\doc->doc {Model.documentLabel=Just "Odd\nlabel\t"}) (Model.buffers named)}
  check "sanitized host labels round trip through actual remote metadata"
    (case parseRemoteFrame (object (P.frameMetadata "." odd)) (P.frameRows odd) of Right decoded->remoteWindows decoded==[(1,"Odd·label·",True,True)]; _->False)
#endif
  check "drag and wheel updates wait for the new frame instead of repainting stale content"
    (not (nativeRepaint [3,10,4,0,0,1]) && not (nativeRepaint [9,10,4,-1,0]))
  check "press and release repaint the local pointer visibility"
    (nativeRepaint [3,10,4,1,0,1] && nativeRepaint [4,10,4])
  check "remote Unicode rows validate" (valid rows)
  let exportMetadata receipt=object ["size" .= ([80,25]::[Int]),"bindings" .= ([]::[(T.Text,T.Text)]),"fileExportView" .= receipt]
  check "export gesture metadata rejects unbounded receipts"
    (either (const True) (const False) (parseRemoteFrame (exportMetadata (replicate 14 (1::Integer))) rows))
  check "export gesture metadata excludes negative identities"
    (either (const True) (const False) (parseRemoteFrame (exportMetadata ([-1]::[Integer])) rows))
#ifdef WITH_REMOTE
  let offered=named {pendingFileExport=(9,Just (Model.ExportFileCopy "saved.hs" (BS.pack [0,255]) (Model.Rect 1 2 20 1) []))}
      detached=fst (P.applyInput P.Blur offered)
  check "detaching invalidates prepared saved exports without touching buffers"
    (fst (pendingFileExport detached)==10 && snd (pendingFileExport detached)==Nothing && Model.fileExportView detached/=Model.fileExportView offered)
#endif

  let sidebar=object ["readOnly" .= True,"revision" .= (1::Int),"nodes" .= ([]::[Value])]
      sidebarFrame=object ["size" .= ([80,25]::[Int]),"bindings" .= ([]::[(T.Text,T.Text)]),"semanticSidebar" .= sidebar]
  check "native receiver retains the exact bounded semantic snapshot"
    (case parseRemoteFrame sidebarFrame rows of Right value->eitherDecodeStrict' (remoteSidebar value)==Right sidebar; _->False)
  check "missing native semantics clears earlier accessibility state"
    (case parseRemoteFrame meta rows of Right value->BS.null (remoteSidebar value); _->False)
  check "native semantic transport rejects oversized and nonobject snapshots"
    (all (either (const True) (const False) . (\value->parseRemoteFrame (object ["size" .= ([80,25]::[Int]),"bindings" .= ([]::[(T.Text,T.Text)]),"semanticSidebar" .= value]) rows))
      [String "bad",object ["name" .= T.replicate 2097153 "a"]])

  let dialogId=["dialog"]::[T.Text]
      modalNode :: [T.Text] -> Maybe [T.Text] -> T.Text -> T.Text -> Maybe T.Text -> Value
      modalNode ident parent role name value=object
        ["id" .= ident,"parent" .= parent,"role" .= role,"name" .= name,"value" .= value,
         "bounds" .= ([1,2,20,1]::[Int]),"focused" .= True,"checked" .= (Nothing::Maybe Bool),
         "selected" .= (Nothing::Maybe Bool),"expanded" .= (Nothing::Maybe Bool),"multiline" .= False]
      modalRoot=modalNode dialogId (Nothing::Maybe [T.Text]) ("dialog"::T.Text) ("Options"::T.Text) (Nothing::Maybe T.Text)
      modalInput=modalNode ["dialog","field","0"] (Just dialogId) "textbox" "Name" (Just "safe <script>")
      dialogValue :: Bool -> [Value] -> Value
      dialogValue present nodes=object ["present" .= present,"readOnly" .= True,"truncated" .= False,"nodes" .= nodes]
      dialogFrame value=parseRemoteFrame (object ["size" .= ([80,25]::[Int]),"bindings" .= ([]::[(T.Text,T.Text)]),"semanticDialog" .= value]) rows
      modal=dialogValue True [modalRoot,modalInput]
      wrapper value=object ["dialog" .= value,"size" .= ([80,25]::[Int])]
  check "native receiver validates and prepares the complete modal snapshot"
    (case dialogFrame modal of Right value->remoteDialog value==Just (BL.toStrict (encode (wrapper modal))); _->False)
  check "privacy-hidden modal remains present without exposing labels or controls"
    (case dialogFrame (dialogValue True ([]::[Value])) of Right value->remoteDialog value/=Nothing; _->False)
  check "missing and dismissed dialog metadata retire native modal state"
    (all (\result->case result of Right value->remoteDialog value==Nothing; _->False)
      [parseRemoteFrame meta rows,dialogFrame (dialogValue False ([]::[Value]))])
  let actualModal=Model.prompt "Native modal" Model.Widgets
        [Model.Input "Name" "visible" 2,Model.CheckBox "Enabled" True,Model.Radio "Choice" ["A","B"] 1,
         Model.ComboBox "Combo" ["A","B"] 0 (Just 1),Model.TextArea "Notes" False (newBuffer "first\nsecond") (Selection 0 0) 0 0]
        (Model.initialDesktop (80,25))
      projected=dialogSemantics OwnerSemantics actualModal
  check "actual host modal projection passes the production native receiver"
    (case dialogFrame projected of Right value->remoteDialog value==Just (BL.toStrict (encode (wrapper projected))); _->False)
  let editNode updates=case modalInput of Object fields->Object (KM.union (KM.fromList updates) fields); _->error "modal fixture"
      badNodes=[editNode ["role" .= ("action"::T.Text)],editNode ["parent" .= (["dialog","field","0"]::[T.Text])],
        editNode ["value" .= T.replicate 2049 "x"],editNode ["focused" .= (1::Int)],editNode ["bounds" .= ([80,2,1,1]::[Int])],
        editNode ["id" .= (["dialog","field",T.replicate 25 "0"]::[T.Text])],editNode ["name" .= ("bad\0name"::T.Text)]]
  check "native modal parser rejects malformed roles states geometry identities and cycles"
    (all (either (const True) (const False) . dialogFrame . dialogValue True . (modalRoot:) . (:[])) badNodes)
  check "native modal parser rejects duplicate nodes nonempty dismissals and node overflow"
    (all (either (const True) (const False) . dialogFrame)
      [dialogValue True [modalRoot,modalInput,modalInput],dialogValue False [modalRoot],dialogValue True (replicate 257 modalRoot)])
  let budgetNodes=[modalNode ["dialog","body",T.pack (show i)] (Just dialogId) "text" (T.replicate 256 "x") (Just (T.replicate 2048 "y")) | i<-[0..14::Int]]
  check "native modal parser bounds total text independently of node count"
    (either (const True) (const False) (dialogFrame (dialogValue True (modalRoot:budgetNodes))))

  let menuMeta fields=object (["size" .= ([80,25]::[Int]), "bindings" .= ([]::[(T.Text,T.Text)]) ]++fields)
      states metadata=either (const []) remoteMenus (parseRemoteFrame metadata rows)
      commandToken command=maybe (error "missing native command") id (elemIndex command nativeCommands)
      newToken=commandToken Model.New
      quitToken=commandToken Model.Quit
  check "menus are disabled without named metadata" (not (or (states (menuMeta []))))
  check "menu state requires advertised command capability" (not (or (states (menuMeta ["menuState" .= [("hide.file.new"::T.Text,True)]]))))
  let reordered=states (menuMeta ["menuCommands" .= (["hide.app.quit","hide.file.new"]::[T.Text]),"menuState" .= [("hide.app.quit"::T.Text,False),("hide.file.new",True)]])
  check "menu enable state maps by command name across reordered layouts" (take 1 (drop newToken reordered)==[True] && take 1 (drop quitToken reordered)==[False])
  let remoteMenu metadata index= either (const Nothing) (\frame -> remoteMenuInput frame index) (parseRemoteFrame metadata rows)
      enabledMenu=menuMeta ["menuCommands" .= (["hide.file.new"]::[T.Text]),"menuState" .= [("hide.file.new"::T.Text,True)]]
  check "menu invocation uses its public identity" (remoteMenu enabledMenu newToken==Just (object ["type" .= ("menu"::T.Text),"command" .= ("hide.file.new"::T.Text)]))
  check "invalid menu positions cannot alias the first command" (remoteMenu enabledMenu (-1)==Nothing && remoteMenu enabledMenu 10000==Nothing)
  check "disabled menu actions cannot be invoked" (remoteMenu (menuMeta ["menuCommands" .= (["hide.file.new"]::[T.Text]),"menuState" .= [("hide.file.new"::T.Text,False)]]) newToken==Nothing)
  check "native menus resolve command tokens only in their current incarnation" (nativeMenuEvent 7 [11,newToken,7]==Just Model.New)
  check "stale, unstamped and unknown native menu events cannot invoke" (all ((==Nothing) . nativeMenuEvent 8) [[11,newToken,7],[11,newToken],[11,-1,8],[11,10000,8]] && nativeEventInput [11,newToken,8]==Nothing)
  check "remote rows must match height" (not (valid (take 24 rows)))
  check "remote span overflow rejected" (not (valid (toJSON [(79::Int,0::Int,0::Int,0::Int,[String "ab"])] : drop 1 rows)))
  check "unknown remote font flags rejected" (not (valid (toJSON [(0::Int,0::Int,0::Int,4::Int,[String "a"])] : drop 1 rows)))
  check "explicit width cannot enter decorated span paint" (not (valid (toJSON [(0::Int,0::Int,0::Int,12::Int,[String "a"])] : drop 1 rows)))
  check "unknown high font flags rejected" (not (valid (toJSON [(0::Int,0::Int,0::Int,32::Int,[String "a"])] : drop 1 rows)))
  check "remote colors bounded" (not (valid (toJSON [(0::Int,-1::Int,0::Int,0::Int,[String "a"])] : drop 1 rows)))
  check "remote clusters cannot contain NUL" (not (valid (toJSON [(0::Int,0::Int,0::Int,0::Int,[toJSON ("a\0"::T.Text,1::Int,False,0::Int,1::Int)])] : drop 1 rows)))
  check "remote invalid dimensions rejected" (either (const True) (const False) (parseRemoteFrame (object ["size" .= ([999999,25]::[Int]),"bindings" .= ([]::[(T.Text,T.Text)])]) rows))
  check "Control bracket detaches locally" (remoteDetachShortcut [1,fromEnum ']',2] && nativeEventInput [1,fromEnum ']',2]==Nothing)
  check "other bracket shortcuts remain editor input" (all (not . remoteDetachShortcut . (\mods -> [1,fromEnum ']',mods])) [0,1,3,6,8])
  check "native close requests checked remote quit" (nativeEventInput [6] == Just (object ["type" .= ("command"::T.Text),"command" .= ("hide.app.quit"::T.Text)]))
  check "offline closes detach without queuing remote quit" (remoteCloseDetaches False [6] && not (remoteCloseDetaches True [6]))
  check "offline user input is ignored" (all (not . remoteInputAllowed False) [[1,97,0],[2],[3,1,1,1,0,1],[11,0],[14]])
  check "offline zoom remains local" (remoteInputAllowed False [1,fromEnum '+',2] && remoteInputAllowed True [1,97,0])
  check "native blur releases remote state" (nativeEventInput [7] == Just (object ["type" .= ("blur"::T.Text)]))
  check "remote key mapping preserves shift tab" (nativeKeyInput (-9) 1 == Just (object ["type" .= ("key"::T.Text),"key" .= ("Tab"::T.Text),"mods" .= (["shift"]::[T.Text])]))
  check "native Command preserves wire modifier" (nativeKeyInput (fromEnum 'v') 8==Just (object ["type" .= ("key"::T.Text),"key" .= ("v"::T.Text),"mods" .= (["cmd"]::[T.Text])]))
  check "download names stay single safe components" (sanitizeDownloadName "../../secret" == "secret" && sanitizeDownloadName "..\\..\\secret" == "secret" && sanitizeDownloadName ".." == "download" && not (T.any (<' ') (sanitizeDownloadName "bad\0name")))

  check "download filenames respect UTF8 filesystem limits" (BS.length (TE.encodeUtf8 (sanitizeDownloadName (T.replicate 180 "界")))<=180)
#ifdef WITH_REMOTE
  check "coalesced native wheel travel survives the wire" (case nativeEventInput [9,10,4,-125,0] of
    Just value -> parseEither P.parseInput value==Right (P.Wheel 10 4 (-125) [])
    _ -> False)
  let desktop = (addDocument Nothing (newBuffer "λ界 é\n🐈") (initialDesktop (80,25))) {videoMode=Just 3,browserFrontend=True}
      actualRows = P.frameRows desktop
  (decoded,reconstructed) <- P.decodeFrame [] (BL.toStrict (P.framePacket True [] actualRows (P.frameMetadata "/remote/project" desktop)))
  check "compressed editor Unicode frame validates for native rendering" (either (const False) ((==(80,25)).remoteSize) (parseRemoteFrame decoded reconstructed))
  check "validated native menu route uses shared protocol" (case remoteMenu enabledMenu newToken of Just value -> parseEither P.parseInput value==Right (P.MenuCommand Model.New); _ -> False)
  check "native events use shared protocol" (all (maybe False (either (const False) (const True) . parseEither P.parseInput) . nativeEventInput)
    [[1,-9,1],[3,10,4,2,2,1],[3,11,4,0,1,1],[4,11,4],[5,80,25],[6],[7],[9,10,4,-1,4],[12,-1,-1],[13,15]])
#endif
