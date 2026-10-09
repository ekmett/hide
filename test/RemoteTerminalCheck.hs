{-# LANGUAGE CPP, OverloadedStrings #-}
module RemoteTerminalCheck (checks,packetChecks) where
import AllocationProfile (AllocationProfile, withinBudget)
import Control.Monad (unless)
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import GHC.Conc (getAllocationCounter)
import Data.Aeson
import Data.IORef (newIORef,readIORef,writeIORef)
import qualified Data.ByteString.Char8 as BSC
import Blaze.ByteString.Builder.ByteString (writeByteString)
import Graphics.Vty.Output (Output(..),DisplayContext(..))
import Graphics.Vty.Output.Mock (mockTerminal)
import Hide.Unicode (Script(..), updateDisplayOps)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vec
import Graphics.Vty.Span (SpanOp(..))
import qualified Graphics.Vty as V
import Hide.TextStyle (textForeground,textFlags)
import Hide.RemoteTerminal
import Hide.RemoteWindow (parseRemoteFrame, RemoteFrame(..), RemoteCell(..))
import qualified Hide.Protocol as P
#ifdef WITH_REMOTE
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically,newTBQueueIO,flushTBQueue)
import Control.Exception (bracket)
import Data.IORef (atomicModifyIORef')
import Hide.FileExport (withFileExports)
import Hide.Remote (RemotePeer(..))
import System.Directory
import System.Environment (lookupEnv,setEnv,unsetEnv)
import System.FilePath ((</>),takeFileName,takeExtension)
import System.Info (os)
import System.IO (hClose,openTempFile)
import System.Timeout (timeout)
import qualified Data.ByteString.Lazy as BL
#endif

checks :: AllocationProfile -> IO ()
checks profile = do
  packetChecks
  let check name good=unless good (error name)
      parsed event=terminalEventInput event >>= either (const Nothing) Just . parseEither P.parseInput
  check "terminal shift tab uses shared key protocol" (parsed (V.EvKey V.KBackTab [])==Just (P.Key "Tab" [V.MShift]))
  check "terminal Ctrl+C stays remote editor input" (parsed (V.EvKey (V.KChar 'c') [V.MCtrl])==Just (P.Key "c" [V.MCtrl]))
  check "terminal bracketed Unicode paste stays one input" (parsed (V.EvPaste (TE.encodeUtf8 "λ\n界"))==Just (P.Paste "λ\n界"))
  check "terminal invalid UTF8 paste rejected" (terminalEventInput (V.EvPaste (BS.pack [255]))==Nothing)
  check "terminal paste size is bounded" (terminalEventInput (V.EvPaste (BS.replicate 1048577 97))==Nothing)
  check "terminal resize clamps protocol bounds" (parsed (V.EvResize 9999 1)==Just (P.Resize 512 12))
  check "terminal mouse coordinates and modifiers preserved" (parsed (V.EvMouseDown 8 3 V.BRight [V.MShift])==Just (P.Mouse "down" 8 3 2 1 [V.MShift]))
  check "terminal wheel preserves direction" (parsed (V.EvMouseDown 4 5 V.BScrollUp [])==Just (P.Mouse "wheel-up" 4 5 0 1 []))
  check "terminal focus loss releases remote modifiers" (parsed V.EvLostFocus==Just P.Blur)
  check "terminal OSC52 uses UTF8 base64" (terminalClipboard "λ"=="\ESC]52;c;zrs=\BEL")
  check "terminal OSC52 handles base64 padding" (map terminalClipboard ["","f","fo","foo","foobar"]==map (\s -> "\ESC]52;c;"<>s<>"\BEL") ["","Zg==","Zm8=","Zm9v","Zm9vYmFy"])
  let metadata=object ["size" .= ([40,12]::[Int]),"bindings" .= ([]::[(T.Text,T.Text)]),"cursor" .= ([5,0]::[Int])]
      row=toJSON [(2::Int,0x123456::Int,0xffffff::Int,0::Int,[String "a",toJSON ("界"::T.Text,2::Int,False,0::Int,2::Int),toJSON ("é"::T.Text,1::Int,False,0::Int,1::Int)])]
  frame <- either error pure (parseRemoteFrame metadata (row:replicate 11 (toJSON ([]::[Value]))))
  let runText=T.dropEnd 6 (T.drop 6 ("prefixAéδ░│𝄞Zsuffix"::T.Text))
      textRow text=toJSON [(0::Int,0x123456::Int,0xffffff::Int,0::Int,[String text])]
      parseText text=parseRemoteFrame metadata (textRow text:replicate 11 (toJSON ([]::[Value])))
  underlyingRun<-either error pure (parseText runText)
  mapM_ (\text->check "character runs reject non-cell scalars and controls" (case parseText text of Left _->True; _->False))
    ["界","e\x301","\x200d","\xfe0f","\x20e3","🇨🇦","⌘","\xf024b","\n","\DEL",T.replicate 41 "a",T.replicate 513 "a"]
  check "receiver preserves each validated text run rather than allocating per character" (length (remoteCells underlyingRun)==1)
  check "borrowed nonzero-base run keeps scalar count and paint" (case remoteCells underlyingRun of
    [RemoteText 0 0 paint text 7]->text==runText && textForeground paint==0x123456
    _->False)
  let (_,croppedRun)=remoteTerminalDisplay (4,1) (Just underlyingRun) ""
  check "terminal clips text runs by cells without splitting UTF8" (T.concat [TL.toStrict text | TextSpan _ _ _ text<-Vec.toList (Vec.head croppedRun)]=="Aéδ░")
  let denseMetadata=object ["size" .= ([180,55]::[Int]),"bindings" .= ([]::[(T.Text,T.Text)])]
      denseRows=replicate 55 (textRow ("║"<>T.replicate 178 " "<>"║"))
  _<-evaluate (force (denseMetadata,denseRows))
  before<-getAllocationCounter
  dense<-either error pure (parseRemoteFrame denseMetadata denseRows)
  occupied<-evaluate (sum [case cell of
    RemoteText x y paint text n->x+y+textFlags paint+T.length text+n
    RemoteGlyph x y paint text full start shown->x+y+textFlags paint+T.length text+full+start+shown
    RemoteScript x y paint text natural _->x+y+textFlags paint+T.length text+natural | cell<-remoteCells dense])
  after<-getAllocationCounter
  check "dense receiver retains 55 runs within a 1.5 MB allocation budget"
    (length (remoteCells dense)==55 && occupied==21285 && withinBudget profile (before-after) (1500000))
  let (cursor,ops)=remoteTerminalDisplay (40,12) (Just frame) ""
      rowWidth values=sum [n | TextSpan _ n _ _<-Vec.toList values]
      rowText values=T.concat [TL.toStrict text | TextSpan _ _ _ text<-Vec.toList values]
  let scripted x runs=toJSON [(x::Int,0x123456::Int,0xffffff::Int,27::Int,runs)]
      script text natural mode=toJSON (text::T.Text,natural::Int,mode::T.Text)
      parseScript value=parseRemoteFrame metadata (value:replicate 11 (toJSON ([]::[Value])))
  scripts<-either error pure (parseScript (scripted 0 [script "A" 1 "sup",script "界" 2 "sub",script "é" 1 "sub",script "👩🏽\x200d\&💻" 2 "sup",String "Z"]))
  check "script receiver preserves semantic graphemes, natural width, paint and one-cell placement" (case remoteCells scripts of
    [RemoteScript 0 0 paint "A" 1 Superscript,RemoteScript 1 0 _ "界" 2 Subscript,RemoteScript 2 0 _ "é" 1 Subscript,RemoteScript 3 0 _ "👩🏽\x200d\&💻" 2 Superscript,RemoteText 4 0 _ "Z" 1]->textFlags paint==27
    _->False)
  let (_,scriptOps)=remoteTerminalDisplay (5,1) (Just scripts) ""
      (_,scriptBanner)=remoteTerminalDisplay (5,1) (Just scripts) "Hi"
  check "script terminal projection keeps narrow text and substitutes wide text without moving the suffix"
    (rowText (Vec.head scriptOps)=="A\xfffd\&é\xfffd\&Z" && rowWidth (Vec.head scriptOps)==5)
  check "banner clips scripted cells using allocated width" (rowText (Vec.head scriptBanner)=="Hié\xfffd\&Z")
  mapM_ (\value->check "invalid script payloads rejected at the receiver" (case parseScript value of Left _->True; _->False))
    ([scripted 0 [script text natural mode] | (text,natural,mode)<-
      [("",1,"sup"),("AB",1,"sup"),("界",1,"sup"),("A",2,"sub"),("A",0,"sup"),("A",3,"sup"),("A",1,"bad"),("\n",1,"sup"),("́",1,"sub")]]++
     [scripted 40 [script "A" 1 "sup"],scripted 39 [script "A" 1 "sup",String "Z"]])
  check "direct terminal spans fill frame dimensions with sparse Unicode cells" (Vec.length ops==12 && Vec.all ((==40).rowWidth) ops)
  check "direct terminal spans preserve remote cursor" (cursor==V.Cursor 5 0)
  let (_,cropped)=remoteTerminalDisplay (4,2) (Just frame) ""
      (_,expanded)=remoteTerminalDisplay (43,14) (Just frame) ""
      (hidden,notice)=remoteTerminalDisplay (6,2) (Just frame) "Hi"
  check "terminal resize clips wide glyph at actual right edge" (Vec.length cropped==2 && Vec.all ((==4).rowWidth) cropped && rowText (Vec.head cropped)=="  a ")
  check "terminal larger bounds clear newly exposed rows and columns" (Vec.length expanded==14 && Vec.all ((==43).rowWidth) expanded && rowText (Vec.last expanded)==T.replicate 43 " ")
  check "terminal banner uses actual bottom row and hides cursor" (hidden==V.NoCursor && rowText (notice Vec.! 1)=="Hi    " && rowText (Vec.head notice)=="  a界é")
  let bannerRow=toJSON [(0::Int,0x123456::Int,0xffffff::Int,0::Int,[toJSON ("界"::T.Text,2::Int,False,0::Int,2::Int),String "tail"])]
  underlying<-either error pure (parseRemoteFrame metadata (replicate 1 (toJSON ([]::[Value]))++[bannerRow]++replicate 10 (toJSON ([]::[Value]))))
  let (_,covered)=remoteTerminalDisplay (6,2) (Just underlying) "X"
      (_,clippedBanner)=remoteTerminalDisplay (1,1) Nothing "界"
  check "banner preserves suffix and blanks partially covered underlying glyph" (rowText (covered Vec.! 1)=="X tail")
  check "banner partial glyph occupies blank rather than overflow" (rowText (Vec.head clippedBanner)==" " && rowWidth (Vec.head clippedBanner)==1)
  (_,mock)<-mockTerminal (6,2)
  captured<-newIORef BS.empty
  let output=mock {outputByteBuffer=writeIORef captured,mkDisplayContext = \device size->do
        dc<-mkDisplayContext mock device size
        pure dc {writeMoveCursor = \x y->writeByteString (BSC.pack ("<"<>show x<>","<>show y<>">"))}}
      (visibleCursor,visible)=remoteTerminalDisplay (6,2) (Just frame) ""
  updateDisplayOps output (6,2) visibleCursor visible
  emitted<-readIORef captured
  check "direct writer keeps explicit two-cell cursor correction" (TE.encodeUtf8 "  <3,0>界<5,0>" `BS.isInfixOf` emitted)
  updateDisplayOps output (6,2) visibleCursor visible
  unchanged<-readIORef captured
  check "direct writer unchanged rows emit only cursor operations" (unchanged=="HS<5,0>")
  updateDisplayOps output (6,2) hidden notice
  overlay<-readIORef captured
  check "direct writer banner changes only bottom row and suppresses cursor" ("Hi" `BS.isInfixOf` overlay && not (TE.encodeUtf8 "界" `BS.isInfixOf` overlay) && not ("S" `BS.isInfixOf` overlay))

-- | Exercise the production terminal receiver with actual binary payload pairs,
-- local staging and a live helper. No terminal or editor UI is needed.
packetChecks :: IO ()
#ifdef WITH_REMOTE
packetChecks=unless (os=="mingw32") $ bracket temporary removePathForcibly $ \root->
  bracket (mapM (\name->do value<-lookupEnv name; pure (name,value)) names) (mapM_ restore) $ \_->do
    let check label ok=unless ok (error label)
        helper=root </> "helper"
        copied=root </> "snapshot"
        arguments=root </> "arguments"
        saved=BS.pack [0,255,13,10,128,1]
        ordinary=BS.pack [255,0,128]
        header name= P.JsonPacket (object ["type" .= ("download"::T.Text),"purpose" .= ("file-export"::T.Text),
          "name" .= (name::T.Text),"row" .= ([2,3,20,1]::[Int]),"view" .= ([1,2,3]::[Int])])
        rows=replicate 12 (toJSON ([]::[Value]))
        frame=P.BinaryPacket (BL.toStrict (P.framePacket True [] rows ["size" .= ([40,12]::[Int]),"bindings" .= ([]::[(T.Text,T.Text)])]))
    writeFile helper "#!/bin/sh\nprintf '%s\\000' \"$@\" > \"$THC_TERMINAL_EXPORT_ARGS\"\n/bin/cp \"$2\" \"$THC_TERMINAL_EXPORT_SNAPSHOT\"\nexec /bin/sleep 60\n"
    permissions<-getPermissions helper
    setPermissions helper permissions {executable=True}
    setEnv "HOME" root
    setEnv "THC_EDIT_FILE_DRAG_HELPER" helper
    setEnv "THC_TERMINAL_EXPORT_ARGS" arguments
    setEnv "THC_TERMINAL_EXPORT_SNAPSHOT" copied
    packets<-newIORef [header "../invalid.bin",P.BinaryPacket saved,header "savedλ.bin",P.BinaryPacket saved,
      P.JsonPacket (object ["type" .= ("download"::T.Text),"name" .= ("../../ordinary.bin"::T.Text)]),P.BinaryPacket ordinary,
      header "busy.bin",P.BinaryPacket saved,
      P.JsonPacket (object ["type" .= ("canvas-resource"::T.Text)]),
      P.JsonPacket (object ["type" .= ("canvas-chunk"::T.Text),"length" .= (6::Int)]),P.BinaryPacket saved,frame]
    let peer=RemotePeer
          { peerSend = \_->error "terminal receiver sent a transport packet"
          , peerSendBatch = \_->error "terminal receiver sent a transport batch"
          , peerAttachment=pure 0
          , peerSession=pure ""
          , peerReceive=atomicModifyIORef' packets (\pending->case pending of []->([],Nothing); packet:rest->(rest,Just packet))
          }
    withFileExports $ \exports->do
      queue<-newTBQueueIO 8
      received<-timeout 2000000 (receiveTerminalFrames exports peer queue)
      check "file drag helper cannot block subsequent terminal packets" (received==Just ())
      ready<-timeout 2000000 (awaitFile copied)
      check "file-export payload reaches the terminal host helper" (ready==Just ())
      snapshot<-BS.readFile copied
      check "terminal export preserves exact saved binary bytes" (snapshot==saved)
      argv<-BS.readFile arguments
      path<-case BS.split 0 argv of
        ["--and-exit",staged,""] | takeFileName (T.unpack (TE.decodeUtf8 staged))=="savedλ.bin"->pure (T.unpack (TE.decodeUtf8 staged))
        _->error "terminal helper did not receive one local saved basename via argv"
      retained<-BS.readFile path
      check "staged copy stays available while the helper is open" (retained==saved)
      downloaded<-listDirectory (root </> "Downloads")
      downloadedPath<-case downloaded of
        [name] | "ordinary" `T.isPrefixOf` T.pack name, takeExtension name==".bin"->pure (root </> "Downloads" </> name)
        _->error "ordinary download did not remain a separate Downloads file"
      downloadedBytes<-BS.readFile downloadedPath
      check "ordinary download preserves its existing binary behavior" (downloadedBytes==ordinary)
      incoming<-atomically (flushTBQueue queue)
      let notices=[message | Control value<-incoming,Right message<-[parseEither (withObject "notice" (.: "message")) value]]
      check "invalid exported basenames are reported explicitly" (any (T.isInfixOf "valid basename") notices)
      check "busy helper refusal is visible" (any (T.isInfixOf "busy") notices)
      check "frames continue through the production receiver with an open helper" (length [() | Frame _<-incoming]==1)
      check "receiver completes through the existing control path" (any isClosed incoming)
    putStrLn "terminal file export packet checks passed"
  where
    names=["HOME","THC_EDIT_FILE_DRAG_HELPER","THC_TERMINAL_EXPORT_ARGS","THC_TERMINAL_EXPORT_SNAPSHOT"]
    restore (name,value)=maybe (unsetEnv name) (setEnv name) value
    awaitFile path=do exists<-doesFileExist path; unless exists (threadDelay 10000 >> awaitFile path)
    isClosed (Control value)=parseEither (withObject "control" (.: "type")) value==Right ("closed"::T.Text)
    isClosed _=False
    temporary=do
      base<-getTemporaryDirectory
      (path,handle)<-openTempFile base "hide-terminal-export-check"
      hClose handle
      removeFile path
      createDirectory path
      canonicalizePath path
#else
packetChecks=pure ()
#endif
