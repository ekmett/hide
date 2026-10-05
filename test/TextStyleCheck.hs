{-# LANGUAGE OverloadedStrings #-}
module TextStyleCheck (checks) where

import Control.Monad (unless)
import Data.Bits ((.&.))
import Data.Aeson (Value(..),parseJSON)
import qualified Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Graphics.Vty.Span (SpanOp(..))
import Hide.Buffer (newBuffer,Selection(..))
import Hide.Model
import Hide.Protocol (frameRows,framePacket,frameMetadata,decodeFrame)
import Hide.Syntax (Style(..),prepareSourceRow,fontTraits,linkSpans)
import Hide.TextStyle
import Hide.Markdown (renderMarkdown)
import Hide.Render (snapshotHtml,renderCellRows)
import Hide.Unicode (CellSpan(..))
import qualified Graphics.Vty as VT
import Hide.RemoteWindow (parseRemoteFrame,RemoteFrame(..),RemoteCell(..))
import Hide.RemoteTerminal (remoteTerminalDisplay)

checks :: IO ()
checks=do
  let check label ok=unless ok (fail label)
      foreground=0x123456
      row=[(c,TerminalStyle foreground 0x654321 flags) | (c,flags)<-[('B',1),('I',2),('X',3),('U',4),('S',8),('A',15),('D',16),('R',0)]]
      original=addDocument Nothing (newBuffer "BIXUSADR") (initialDesktop (80,25))
      desktop=original {buffers=M.adjust (\doc->doc {documentSourceRows=Just (V.singleton (prepareSourceRow "BIXUSADR" row))}) 1 (buffers original)}
      parsed=traverse (parseEither parseJSON) (frameRows desktop)::Either String [[(Int,Int,Int,Int,[Value])]]
      traits ch=[flags | spans<-either (const []) id parsed,(_,fg,_,flags,runs)<-spans,fg==fromIntegral foreground,String text<-runs,ch `T.isInfixOf` text]
  check "real frame carries terminal bold trait" (traits "B"==[1])
  check "real frame carries terminal italic trait" (traits "I"==[2])
  check "real frame carries combined traits" (traits "X"==[3])
  check "real frame converts terminal underline4 to wire8" (traits "U"==[8])
  check "real frame converts terminal strike8 to wire16" (traits "S"==[16])
  check "real frame composes all four traits" (traits "A"==[27])
  check "terminal dim stays outside the font paint mask" (traits "D"==[0])
  check "regular negative control remains regular" (traits "R"==[0])
  mapM_ (\style->check "paint/Attr round-trip law" (textStyleFromAttr (textStyleAttr style)==style))
    [TextStyle fg bg flags | fg<-[0,0x123456,0xffffff],bg<-[0,0x654321],flags<-[0,1,2,3,8,9,10,11,16,17,18,19,24,25,26,27]]
  let nested=renderMarkdown 80 "***both*** [**link**](https://example.test)"
  check "nested Markdown bold and italic compose" (all (\(_,style)->let (_,b,i)=fontTraits style in b && i) (take 4 nested))
  check "styled links retain destinations" (linkSpans nested==[(5,9,"https://example.test")])
  let metadata=frameMetadata "." desktop
      rows=frameRows desktop
      regular=desktop {buffers=M.adjust (\doc->doc {documentSourceRows=Just (V.singleton (prepareSourceRow "BIXUSADR" [(c,TerminalStyle foreground 0x654321 0) | c<-"BIXUSADR"]))}) 1 (buffers desktop)}
  (_,restored)<-decodeFrame rows (BL.toStrict (framePacket False rows (frameRows regular) (frameMetadata "." regular)))
  check "trait-only changes survive real compressed frame reconstruction" (restored==frameRows regular && restored/=rows)
  frame<-either fail pure (parseRemoteFrame (Data.Aeson.object metadata) rows)
  check "native receiver retains all terminal traits" (all (\(char,flags)->any (\cell->case cell of RemoteText _ _ paint text _->textForeground paint==fromIntegral foreground && char `T.isInfixOf` text && textFlags paint==flags; _->False) (remoteCells frame)) [("B",1),("I",2),("X",3),("U",8),("S",16),("A",27),("D",0),("R",0)])
  let (_,terminal)=remoteTerminalDisplay (remoteSize frame) (Just frame) ""
      terminalTraits=[(TL.toStrict text,textFlags (textStyleFromAttr attr)) | ops<-toList terminal, TextSpan {textSpanAttr=attr,textSpanText=text}<-toList ops]
  check "remote TUI restores all four font attributes" (all (\(char,flags)->any (\(text,actual)->char `T.isInfixOf` text && actual==flags) terminalTraits) [("B",1),("I",2),("X",3),("U",8),("S",16),("A",27)])
  check "remote TUI carries the actual Vty underline and strike bits"
    (all (\(character,trait)->any (\ops->any (\span->case span of
      TextSpan attr _ _ text->character `T.isInfixOf` TL.toStrict text && VT.styleMask attr .&. trait/=0
      _->False) (toList ops)) (toList terminal)) [("U",VT.underline),("S",VT.strikethrough)])
  let allPaint=TextStyle (fromIntegral foreground) 0x654321 27
  check "packed trait accessors expose all four traits"
    (textBold allPaint && textItalic allPaint && textUnderline allPaint && textStrikethrough allPaint)
  let asciiText="abc\tde\r\DEL\SOH"
      asciiBase=addDocument Nothing (newBuffer asciiText) (initialDesktop (80,25))
      asciiDesktop=modifyActive (\w->w {selection=Selection 1 5}) asciiBase
        {buffers=M.adjust (\doc->doc {documentSourceRows=Just (V.singleton (prepareSourceRow asciiText [(c,TerminalStyle foreground 0x654321 3) | c<-T.unpack asciiText]))}) 1 (buffers asciiBase)}
      Just asciiWindow=activeWindow asciiDesktop
      Rect ax ay _ _=bounds asciiWindow
      cells=concatMap (\span->case span of
        CellText paint text->[(c,paint) | c<-T.unpack text]
        CellGlyph paint _ _ _ shown->replicate shown (' ',paint))
        (toList (renderCellRows asciiDesktop V.! (ay+1)))
      displayed=take 12 (drop (ax+1) cells)
      normal=textStyleAttr (TextStyle (fromIntegral foreground) 0x654321 3)
      chosen=normal `VT.withForeColor` VT.RGBColor 0 0 170 `VT.withBackColor` VT.RGBColor 170 170 170
  check "ASCII styled runs retain tab, control, selection and font geometry"
    (map fst displayed=="abc     de··" && map snd displayed==[if i>=1 && i<=8 then chosen else normal | i<-[0..11]])
  let markdown=addHelpStyled (renderMarkdown 80 "# Heading\n\n***both*** regular") (initialDesktop (80,25))
      selected=modifyActive (\w->w {selection=Selection 9 13}) markdown
      selectedFrame=frameRows selected
      selectedFlags=[flags | spans<-either (const []) id (traverse (parseEither parseJSON) selectedFrame::Either String [[(Int,Int,Int,Int,[Value])]]),(_,_,_,flags,runs)<-spans,String text<-runs,"both" `T.isInfixOf` text]
  check "selection keeps composed traits" (3 `elem` selectedFlags)
  check "styling keeps copied semantic text" (clipboard (fst (runCommand Copy selected))=="both")
  check "HTML snapshots carry both font traits" ("font-weight:bold;font-style:italic" `T.isInfixOf` snapshotHtml markdown)
  check "direct terminal projection keeps dimensions" (V.length terminal==25 && V.all ((==80).sum.map textSpanOutputWidth.toList) terminal)
  putStrLn "text style checks passed"
