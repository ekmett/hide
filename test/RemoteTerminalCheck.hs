{-# LANGUAGE OverloadedStrings #-}
module RemoteTerminalCheck (checks) where
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
import Hide.Unicode (updateDisplayOps)
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

checks :: IO ()
checks = do
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
    RemoteGlyph x y paint text full start shown->x+y+textFlags paint+T.length text+full+start+shown | cell<-remoteCells dense])
  after<-getAllocationCounter
  check "dense receiver retains 55 runs within a 1.5 MB allocation budget"
    (length (remoteCells dense)==55 && occupied==21285 && before-after<1500000)
  let (cursor,ops)=remoteTerminalDisplay (40,12) (Just frame) ""
      rowWidth values=sum [n | TextSpan _ n _ _<-Vec.toList values]
      rowText values=T.concat [TL.toStrict text | TextSpan _ _ _ text<-Vec.toList values]
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
