{-# LANGUAGE OverloadedStrings #-}
module TextStyleCheck (checks) where

import Control.Monad (unless)
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
import Hide.Syntax (Style(..),fontTraits,linkSpans)
import Hide.TextStyle
import Hide.Markdown (renderMarkdown)
import Hide.Render (snapshotHtml)
import Hide.RemoteWindow (parseRemoteFrame,RemoteFrame(..),RemoteCell(..))
import Hide.RemoteTerminal (remoteTerminalDisplay)

checks :: IO ()
checks=do
  let check label ok=unless ok (fail label)
      foreground=0x123456
      row=[(c,TerminalStyle foreground 0x654321 flags) | (c,flags)<-[('B',1),('I',2),('X',3),('R',0)]]
      original=addDocument Nothing (newBuffer "BIXR") (initialDesktop (80,25))
      desktop=original {buffers=M.adjust (\doc->doc {documentSourceRows=Just (V.singleton row)}) 1 (buffers original)}
      parsed=traverse (parseEither parseJSON) (frameRows desktop)::Either String [[(Int,Int,Int,Int,[Value])]]
      traits ch=[flags | spans<-either (const []) id parsed,(_,fg,_,flags,runs)<-spans,fg==fromIntegral foreground,String text<-runs,ch `T.isInfixOf` text]
  check "real frame carries terminal bold trait" (traits "B"==[1])
  check "real frame carries terminal italic trait" (traits "I"==[2])
  check "real frame carries combined traits" (traits "X"==[3])
  check "regular negative control remains regular" (traits "R"==[0])
  mapM_ (\style->check "paint/Attr round-trip law" (textStyleFromAttr (textStyleAttr style)==style))
    [TextStyle fg bg bold italic | fg<-[0,0x123456,0xffffff],bg<-[0,0x654321],bold<-[False,True],italic<-[False,True]]
  let nested=renderMarkdown 80 "***both*** [**link**](https://example.test)"
  check "nested Markdown bold and italic compose" (all (\(_,style)->let (_,b,i)=fontTraits style in b && i) (take 4 nested))
  check "styled links retain destinations" (linkSpans nested==[(5,9,"https://example.test")])
  let metadata=frameMetadata "." desktop
      rows=frameRows desktop
      regular=desktop {buffers=M.adjust (\doc->doc {documentSourceRows=Just (V.singleton [(c,TerminalStyle foreground 0x654321 0) | c<-"BIXR"])}) 1 (buffers desktop)}
  (_,restored)<-decodeFrame rows (BL.toStrict (framePacket False rows (frameRows regular) (frameMetadata "." regular)))
  check "trait-only changes survive real compressed frame reconstruction" (restored==frameRows regular && restored/=rows)
  frame<-either fail pure (parseRemoteFrame (Data.Aeson.object metadata) rows)
  check "native receiver retains all terminal traits" ([textFlags paint | RemoteCell _ _ paint text _ _ _<-remoteCells frame,textForeground paint==fromIntegral foreground,text `elem` ["B","I","X","R"]]==[1,2,3,0])
  let (_,terminal)=remoteTerminalDisplay (remoteSize frame) (Just frame) ""
      terminalTraits=[(TL.toStrict text,textFlags (textStyleFromAttr attr)) | ops<-toList terminal, TextSpan {textSpanAttr=attr,textSpanText=text}<-toList ops]
  check "remote TUI restores bold and italic attributes" (all (\(char,flags)->any (\(text,actual)->char `T.isInfixOf` text && actual==flags) terminalTraits) [("B",1),("I",2),("X",3)])
  let markdown=addHelpStyled (renderMarkdown 80 "# Heading\n\n***both*** regular") (initialDesktop (80,25))
      selected=modifyActive (\w->w {selection=Selection 9 13}) markdown
      selectedFrame=frameRows selected
      selectedFlags=[flags | spans<-either (const []) id (traverse (parseEither parseJSON) selectedFrame::Either String [[(Int,Int,Int,Int,[Value])]]),(_,_,_,flags,runs)<-spans,String text<-runs,"both" `T.isInfixOf` text]
  check "selection keeps composed traits" (3 `elem` selectedFlags)
  check "styling keeps copied semantic text" (clipboard (fst (runCommand Copy selected))=="both")
  check "HTML snapshots carry both font traits" ("font-weight:bold;font-style:italic" `T.isInfixOf` snapshotHtml markdown)
  check "direct terminal projection keeps dimensions" (V.length terminal==25 && V.all ((==80).sum.map textSpanOutputWidth.toList) terminal)
  putStrLn "text style checks passed"
