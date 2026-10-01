{-# LANGUAGE OverloadedStrings #-}
module HexCheck (checks) where

import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import System.Directory
import System.IO (openBinaryTempFile,hClose)
import THC.Edit.Buffer
import THC.Edit.Files
import THC.Edit.Hex
import THC.Edit.Model
import THC.Edit.Render (snapshot, snapshotHtml)
import qualified THC.Edit.AgentFiles as A

checks :: IO ()
checks = bracket temporary removeFile $ \path -> do
  let original=BS.pack [0,255,65,10,128,195,169]
      check name ok=unless ok (error name)
      buffer d=maybe (error "no buffer") documentBuffer (activeDocument d)
      key k=fst . handleEvent (V.EvKey k [])
      command c=fst . runCommand c
      desktop b=addDocument (Just (FileState path (Just original))) b (initialDesktop (80,25))
  BS.writeFile path original
  (file,b)<-loadFile path >>= either error pure
  check "byte representation rejects out-of-range characters" (bufferBytes (replaceBuffer True "λ" b)==original && bufferBytes (replaceSelection (Selection 0 1) "λ" b)==original)
  check "binary load is lossless and clean" (byteMode b && bufferBytes b==original && not (dirty b))
  let opened=desktop b
      high=key (V.KChar 'a') opened
      low=key (V.KChar '5') high
      ascii=key (V.KChar 'Z') (key (V.KChar '\t') low)
      undone=command Undo ascii
      redone=command Redo undone
  check "hex nibbles replace one byte and advance" (bufferBytes (buffer low)==BS.cons 165 (BS.tail original) && fmap (caret . selection) (activeWindow low)==Just 1)
  let range=modifyActive (\w -> w {selection=Selection 0 2}) opened
  check "hex edit replaces selection at first byte" (BS.take 2 (bufferBytes (buffer (key (V.KChar 'a') range)))==BS.pack [160,65])
  check "ASCII edits one byte" (BS.take 3 (bufferBytes (buffer ascii))==BS.pack [165,90,65])
  check "byte edit undo redo are exact" (bufferBytes (buffer undone)==bufferBytes (buffer low) && bufferBytes (buffer redone)==bufferBytes (buffer ascii))
  let inserted=key V.KIns redone
      deleted=key V.KDel inserted
  check "byte insert delete are reversible" (bufferBytes (buffer deleted)==bufferBytes (buffer redone) && bufferBytes (buffer (command Undo deleted))==bufferBytes (buffer inserted))
  latest<-saveFile file (buffer redone) >>= either error pure
  disk<-BS.readFile path
  check "binary save exact and updates baseline" (disk==bufferBytes (buffer redone) && diskBytes latest==Just disk)
  BS.writeFile path "external"
  refused<-saveFile latest (buffer redone)
  after<-BS.readFile path
  check "binary save preserves concurrent disk changes" (case refused of Left _ -> after=="external"; _ -> False)
  check "invalid bytes cannot toggle to text" (activeHex (command ToggleHex opened) && bufferBytes (buffer (command ToggleHex opened))==original)
  let unicode="λ😀\r\nx"; text=desktop (newBuffer unicode)
      hex=command ToggleHex (moveTo False 2 text)
      restored=command ToggleHex hex
  check "Unicode toggle preserves exact UTF8 and clean baseline" (bufferBytes (buffer hex)==TE.encodeUtf8 unicode && not (dirty (buffer hex)) && contents (buffer restored)==unicode && not (dirty (buffer restored)))
  check "Unicode cursor maps through byte offsets" (fmap (caret . selection) (activeWindow hex)==Just 6 && fmap (caret . selection) (activeWindow restored)==Just 2)
  let savedHex=markSaved (buffer hex)
  check "undo across representation preserves saved-byte identity" (bufferBytes (undo savedHex)==TE.encodeUtf8 unicode && not (dirty (undo savedHex)))
  check "hex is excluded from ACP source and prompt context" (M.null (A.sourceSnapshots opened) && T.null (A.contextText True True False opened))
  root<-getTemporaryDirectory
  capture<-A.captureFile root path hex
  check "ACP rejects even valid text opened in hex mode" (case capture of Left _ -> True; _ -> False)
  let image=snapshot opened
  check "grid includes offset bytes and ASCII without a HEX title tag" (all (`T.isInfixOf` image) ["00000000","00 FF 41 0A 80 C3 A9","..A...."] && not ("[HEX]" `T.isInfixOf` image))
  let named=addDocument (Just (FileState "/tmp/firmware.bin" Nothing)) b (initialDesktop (80,25))
      titleLine d=T.lines (snapshot d) !! 1
      sized n=modifyActive (\w -> w {bounds=(bounds w) {width=n}}) named
      title=" firmware.bin "
  check "hex filename is centered between value-pane dividers in both layouts"
    (and [T.take (T.length title) (T.drop (10+(hexAsciiColumn count-10-T.length title) `div` 2) (titleLine (sized n)))==title
          | (n,count)<-[(80,16),(56,8)]] &&
     all (\n -> T.index (titleLine (sized n)) 9=='╤') [80,56])
  let cellAt d x y=T.index (T.lines (snapshot d) !! y) x
      narrow=modifyActive (\w -> w {bounds=(bounds w) {width=42}}) opened
      narrowDoc=maybe (error "no document") id (activeDocument narrow)
      narrowWindow=maybe (error "no window") id (activeWindow narrow)
      bar=scrollbarRect narrow False narrowDoc narrowWindow
      scrolled=fst (handleEvent (V.EvMouseDown (left bar+width bar-1) (top bar) V.BLeft []) narrow)
      widened=key (V.KFun 5) scrolled
  let compact=modifyActive (\w -> w {bounds=(bounds w) {width=56}}) (desktop (newByteBuffer (BS.pack (take 512 (cycle [0..255])))))
      compactWindow=maybe (error "no compact window") id (activeWindow compact)
      compactDoc=maybe (error "no compact document") id (activeDocument compact)
  check "eight-byte rows fit beside Files with both panes visible"
    ("00000008" `T.isInfixOf` snapshot compact && "│........" `T.isInfixOf` snapshot compact &&
     scrollbarLimit compact False compactDoc compactWindow==0 && not ("◄" `T.isInfixOf` snapshot compact))
  check "hex dividers join both borders and continue below EOF"
    (and [cellAt opened x 1=='╤' && cellAt opened x 23=='╧' && all (\y -> cellAt opened x y=='│') [2..22] | x<-[9,58]])
  check "no divider beside the scrollbar" (all (\y -> cellAt opened 78 y/='│') [2..22])
  let dots=snapshotHtml (desktop (newByteBuffer (BS.pack [65,0,46,255,66])))
  check "placeholder dots are gray but literal periods stay yellow"
    (all (`T.isInfixOf` dots) ["color:rgb(170,170,170);background:rgb(0,0,170)'>.</span>","color:rgb(255,255,85);background:rgb(0,0,170)'>.</span>"])
  check "full width hex has no horizontal scrollbar or scrollable blank column"
    (not ("◄" `T.isInfixOf` image) && maybe False (\w -> scrollbarLimit narrow False narrowDoc w==0 && width (scrollbarRect narrow False narrowDoc w)==0) (activeWindow opened))
  check "narrow hex retains working horizontal scrollbar"
    ("◄" `T.isInfixOf` snapshot narrow && fmap scrollColumn (activeWindow scrolled)==Just 1)
  check "widening hex resets horizontal scrolling and hides scrollbar"
    (fmap scrollColumn (activeWindow widened)==Just 0 && not ("◄" `T.isInfixOf` snapshot widened) && cellAt widened 58 2=='│')
  check "hex digits touch their dividers without spare padding"
    ("│00 FF 41 0A 80 C3 A9" `T.isInfixOf` image && hexWidth 16==74 && hexWidth 8==41)
  check "hex cells match hit geometry" (and [hexHit 16 (hexColumn i)==(i,False,False) && hexHit 16 (hexColumn i+1)==(i,False,True) && hexHit 16 (58+i)==(i,True,False) | i<-[0..15]])
  check "both layouts share exact row and hit geometry"
    (and [length (hexRow count 0 (contents (buffer compact)))==hexWidth count &&
      and [hexHit count (hexColumn i)==(i,False,False) && hexHit count (hexColumn i+1)==(i,False,True) &&
           hexHit count (hexAsciiColumn count+i)==(i,True,False) | i<-[0..count-1]] | count<-[8,16]])
  check "sixteen-byte layout switches only when the entire row fits"
    (windowHexBytes compactWindow {bounds=(bounds compactWindow) {width=75}}==8 &&
     windowHexBytes compactWindow {bounds=(bounds compactWindow) {width=76}}==16)
  let at10=moveTo False 10 compact
      position=fmap (caret.selection) . activeWindow
      clickCompact=fst (handleEvent (V.EvMouseDown 41 3 V.BLeft []) compact)
      editCompact=key (V.KChar 'Z') clickCompact
      shiftDown=fst (handleEvent (V.EvKey V.KDown [V.MShift]) at10)
  check "compact navigation moves by eight-byte rows"
    (map (position . (`key` at10)) [V.KUp,V.KDown,V.KHome,V.KEnd,V.KPageDown]==map Just [2,18,8,15,170] &&
     fmap selection (activeWindow shiftDown)==Just (Selection 10 18))
  check "compact ASCII click and edit address the correct byte"
    (position clickCompact==Just 15 && BS.index (bufferBytes (buffer editCompact)) 15==90 && position editCompact==Just 16)
  check "compact vertical extent includes every byte and EOF insertion row"
    (documentRows compactDoc compactWindow==65 && scrollbarLimit compact True compactDoc compactWindow==44)
  let wide=moveTo False 300 (desktop (buffer compact))
      docked=installTree "/tmp" [] wide
      undocked=setTree Nothing docked
      cursorVisible d=case activeWindow d of
        Just w -> let (r,c)=windowCursorCell (buffer d) w in r>=scrollRow w && r<scrollRow w+height (bounds w)-2 && c>=scrollColumn w && c<scrollColumn w+width (bounds w)-2
        Nothing -> False
      split=command SplitVertical wide
  check "docking reflows without moving the byte caret or losing its viewport"
    (map position [wide,docked,undocked]==replicate 3 (Just 300) && all cursorVisible [wide,docked,undocked] &&
     fmap windowHexBytes (activeWindow docked)==Just 8 && fmap windowHexBytes (activeWindow undocked)==Just 16 &&
     buffers docked==buffers wide && buffers undocked==buffers wide)
  check "split views choose their own hex row size without changing bytes"
    (all ((==8).windowHexBytes) (windows split) && bufferBytes (buffer split)==bufferBytes (buffer wide))
  check "hex paste accepts only complete byte pairs" (parseHex "00 FF\n41"==Right "\0\255A" && case parseHex "0xFF" of Left _ -> True; _ -> False)
  let pasted=fst (handleEvent (V.EvPaste "FE 00") (moveTo False 0 opened))
  check "external hex paste inserts exact bytes" (BS.take 3 (bufferBytes (buffer pasted))==BS.pack [254,0,0])
  let clicked=fst (handleEvent (V.EvMouseDown 59 2 V.BLeft []) opened)
  check "mouse chooses ASCII byte" (fmap (\w -> (caret (selection w),windowHexAscii w)) (activeWindow clicked)==Just (0,True))
  BS.writeFile path (TE.encodeUtf8 "λ")
  (unicodeFile,unicodeBuffer)<-loadFile path >>= either error pure
  asHex<-either (error.T.unpack) pure (toggleByteMode unicodeBuffer)
  savedFile<-saveFile unicodeFile asHex >>= either error pure
  asText<-either (error.T.unpack) pure (toggleByteMode (markSaved asHex))
  let textDesktop=addDocument (Just savedFile) asText (initialDesktop (80,25))
  beforeWrite<-A.captureFile root path textDesktop >>= either (error.T.unpack) pure
  afterWrite<-A.acceptWrite beforeWrite "μ" textDesktop >>= either (error.T.unpack) pure
  check "ACP save after a hex roundtrip marks the text baseline clean" (not (dirty (buffer afterWrite)) && bufferBytes (buffer afterWrite)==TE.encodeUtf8 "μ")
  putStrLn "hex editor checks passed"
  where
    temporary=do
      root<-getTemporaryDirectory
      (path,h)<-openBinaryTempFile root "thc-hex-test"
      hClose h
      canonicalizePath path
