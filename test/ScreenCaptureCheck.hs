{-# LANGUAGE OverloadedStrings #-}
module ScreenCaptureCheck (checks) where

import Codec.Picture (Image, PixelRGB8(..), convertRGB8, decodePng, imageHeight, imageWidth, pixelAt)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Base64 as B64
import Data.List (nub)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hide.Browser (Entry(..))
import Hide.Buffer (newBuffer, Selection(..))
import Hide.GuestAccess (CellAccess(..))
import Hide.Font (loadFont)
import SidebarFixture
import Hide.Sidebar
import Hide.Model
import Hide.Render (snapshot)
import Hide.ScreenCapture
import Hide.Unicode (clusterWidth, graphemes)

checks :: IO ()
checks=do
  font<-loadFont
  let desktop=addDocument Nothing (newBuffer "  λ 中 ▙ é\n") (initialDesktop (80,25))
      takeCapture d image=capture font d image >>= either (error . T.unpack) pure
  let privateMessages=setProblemsVisible True desktop {guestPrivatePaths=["/authority"],
        diagnostics=[Diagnostic "/authority/secret.hs" Nothing 0 0 1 "secret-diagnostic-payload"]}
  safeMessages<-takeCapture privateMessages True
  check "logical screen omits protected diagnostic content"
    (not ("secret-diagnostic-payload" `T.isInfixOf` fromMaybe "" (field "text" (textMetadata safeMessages))))
  behindDialog<-takeCapture privateMessages {dialog=Just (Dialog "Public dialog" Widgets [] 0 ["OK"] [])} False
  check "opening a public dialog does not reveal protected Messages behind it"
    (not ("secret-diagnostic-payload" `T.isInfixOf` fromMaybe "" (field "text" (textMetadata behindDialog))))
  textOnly<-takeCapture desktop False
  let metadata=textMetadata textOnly
  check "screen text is the complete colorless rendered frame" (field "text" metadata==Just (snapshot desktop))
  check "text-only capture has no image block" (length (blocks textOnly)==1)
  check "screen text preserves Unicode and grapheme clusters" (all (`T.isInfixOf` snapshot desktop) ["λ","中","▙","é"])
  withImage<-takeCapture desktop True
  image<-pngImage withImage
  check "PNG and colorless text use the same frame" ((field "text" (textMetadata withImage)::Maybe T.Text)==field "text" metadata)
  check "mode 3 uses 8x16 bitmap cells" (imageWidth image==640 && imageHeight image==400)
  check "image metadata declares bitmap approximation" (maybe False (T.isInfixOf "approximation") (field "renderer" metadata))
  let cursor=fromMaybe (error "missing screen cursor") (field "cursor" metadata)
      cx=fromMaybe (error "missing cursor x") (field "x" cursor)
      cy=fromMaybe (error "missing cursor y") (field "y" cursor)
      inverse (PixelRGB8 r g b)=PixelRGB8 (255-r) (255-g) (255-b)
  check "visible cursor inverts bottom two pixel rows of its blank cell"
    (pixelAt image (cx*8) (cy*16+15)==inverse (pixelAt image (cx*8) (cy*16+12)))
  let location target=fromMaybe (error "missing screen glyph") $ listToMaybe
        [(sum (map clusterWidth (graphemes prefix)),y) | (y,line)<-zip [0..] (T.lines (snapshot desktop)),
          let (prefix,suffix)=T.breakOn target line,not (T.null suffix)]
      (wx,wy)=location "中"
      distinct x width=length (nub [pixelAt image px py | px<-[x*8..x*8+width-1],py<-[wy*16..wy*16+15]])
  check "wide Unicode glyph draws ink in both cells" (distinct wx 8>1 && distinct (wx+1) 8>1)
  let (qx,qy)=location "▙"
  check "quarter block retains filled and empty quadrants"
    (pixelAt image (qx*8+1) (qy*16+2)/=pixelAt image (qx*8+6) (qy*16+2) &&
     pixelAt image (qx*8+1) (qy*16+13)==pixelAt image (qx*8+6) (qy*16+13))
  compact<-takeCapture desktop {videoMode=Just 259,screenSize=(80,50)} True
  compactImage<-pngImage compact
  check "mode 259 uses 8x8 cells and preserves 80x50 aspect" (imageWidth compactImage==640 && imageHeight compactImage==400 && field "cellHeight" (textMetadata compact)==Just (8::Int))
  stable<-takeCapture desktop {blinkCursor=not (blinkCursor desktop),crtFilter=not (crtFilter desktop)} True
  check "capture is independent of cursor blink phase and CRT effects" (withImage==stable)
  let conversation=(addReadOnly "Conversation" "Session: provider-secret\nPublic assistant response 中" (initialDesktop (80,25)))
        {composerBuffer=newBuffer "private draft 中é",composerSelection=Selection 7 7,composerFocused=True}
  privateCapture<-takeCapture conversation True
  privateImage<-pngImage privateCapture
  let privateMetadata=textMetadata privateCapture
      privateText=fromMaybe "" (field "text" privateMetadata)
      accesses=accessCells privateMetadata
      privateCells=[(x,y) | (x,y,readable,_)<-accesses,not readable]
  check "screen capture keeps public conversation but redacts draft and session key" ("Public assistant response" `T.isInfixOf` privateText && not ("provider-secret" `T.isInfixOf` privateText) && not ("private draft" `T.isInfixOf` privateText))
  check "private screen text and PNG share the same cell mask" (not (null privateCells) && all (\(x,y)->all (==PixelRGB8 0 0 0) [pixelAt privateImage px py | px<-[x*8..x*8+7],py<-[y*16..y*16+15]]) privateCells)
  check "cursor coordinates inside private input are hidden" (field "cursor" privateMetadata==Just Null)
  check "screen cell masks cover the complete grid" (length accesses==80*25)
  check "readable conversation cells can still be unclickable" (any (\(_,_,readable,clickable)->readable && not clickable) accesses)
  check "screen exposes blocked conversation command permissions" (case field "commandPermissions" privateMetadata::Maybe [Value] of
    Just commands->any (\entry->field "command" entry==Just ("hide.options.agent-permissions"::T.Text) && field "allowed" entry==Just False) commands
    _->False)
  let config=desktop {dialog=Just (Dialog "Agents" (AgentDialog "configure")
        [Input "Executable" "public-command" 0,Input "Environment (JSON object)" "private-env-token" 0] 0 ["OK","Cancel"] [])}
  configCapture<-takeCapture config False
  let configText=fromMaybe "" (field "text" (textMetadata configCapture))
  check "agent settings are readable while environment values are private" ("public-command" `T.isInfixOf` configText && not ("private-env-token" `T.isInfixOf` configText))
  let sessionDialog=desktop {dialog=Just (Dialog "Resume session" (AgentDialog "load")
        [Input "Session ID" "private-session-value" 0] 0 ["OK","Cancel"] [])}
  sessionCapture<-takeCapture sessionDialog False
  let sessionText=fromMaybe "" (field "text" (textMetadata sessionCapture))
  check "session fields keep labels readable but redact values" ("Session ID" `T.isInfixOf` sessionText && not ("private-session-value" `T.isInfixOf` sessionText))
  statusCapture<-takeCapture desktop {status="Session private-footer-value"} False
  check "session status messages do not leak identifiers" (not ("private-footer-value" `T.isInfixOf` fromMaybe "" (field "text" (textMetadata statusCapture))))
  check "key permission metadata blocks typing in conversation" (case field "keyPermissions" privateMetadata::Maybe [Value] of
    Just keys->any (\entry->field "key" entry==Just ("Enter"::T.Text) && field "mods" entry==Just ([]::[T.Text]) && field "allowed" entry==Just False) keys
    _->False)
  check "key permission metadata allows ordinary source typing" (case field "keyPermissions" metadata::Maybe [Value] of
    Just keys->any (\entry->field "key" entry==Just ("Enter"::T.Text) && field "mods" entry==Just ([]::[T.Text]) && field "allowed" entry==Just True) keys
    _->False)
  let privateFile="/authority/secret-session.json"
      browser=openBrowser "/authority" "*" [Entry "secret-session.json" False Nothing Nothing,Entry "public.hs" False Nothing Nothing] (initialDesktop (80,25)) {guestPrivatePaths=[privateFile]}
  listed<-sidebarFixture "/authority" [("secret-session.json",privateFile),("public.hs","/authority/public.hs")] (initialDesktop (80,25)) {guestPrivatePaths=[privateFile]}
  listingCapture<-takeCapture listed False
  browserCapture<-takeCapture browser False
  let publicListing value=let output=fromMaybe "" (field "text" (textMetadata value)) in not ("secret-session" `T.isInfixOf` output) && "public.hs" `T.isInfixOf` output
  check "screen capture redacts private filenames in Files and Open while preserving ordinary names" (publicListing listingCapture && publicListing browserCapture)
  let ordinary=addDocument Nothing (newBuffer "Session: public source example\nEnvironment: source content") (initialDesktop (80,25))
  ordinaryCapture<-takeCapture ordinary False
  check "screen redaction does not scan unrelated source text" (field "text" (textMetadata ordinaryCapture)==Just (snapshot ordinary))
  let readable=CellAccess True True
      private=CellAccess False False
      (wideText,wideAccess)=redactCluster "中" [readable,private]
      (emojiText,emojiAccess)=redactCluster "👩\x200d\&💻" [private,readable]
  check "one private wide-glyph cell redacts the entire grapheme" (wideText=="  " && all (not . cellReadable) wideAccess && map cellClickable wideAccess==[True,False])
  check "multi-codepoint grapheme redaction preserves cell width" (emojiText=="  " && all (not . cellReadable) emojiAccess)
  bounded<-capture font desktop {screenSize=(maxBound,2)} True
  check "oversized capture fails before rendering or overflow" (case bounded of Left _ -> True; _ -> False)
  invalid<-capture font desktop {screenSize=(0,25)} False
  check "invalid grid dimensions are rejected" (case invalid of Left _ -> True; _ -> False)
  putStrLn "screen capture checks passed"

blocks :: Value -> [Value]
blocks=fromMaybe [] . field "content"

textMetadata :: Value -> Value
textMetadata value=fromMaybe (error "missing screen metadata") $ do
  block<-listToMaybe (blocks value)
  content<-field "text" block
  decodeStrictText content

pngImage :: Value -> IO (Image PixelRGB8)
pngImage value=case [encoded | block<-blocks value,field "type" block==Just ("image"::T.Text),Just encoded<-[field "data" block]] of
  [encoded] -> either error (pure . convertRGB8) (B64.decode (TE.encodeUtf8 encoded) >>= decodePng)
  _ -> error "missing PNG content block"

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "screen object" (.:key))

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)

-- Expand the public run-length mask so each image cell can be checked.
accessCells :: Value -> [(Int,Int,Bool,Bool)]
accessCells metadata=concatMap row (fromMaybe [] (field "accessRows" metadata))
  where
    row value=let y=fromMaybe (-1) (field "y" value)
              in concatMap (run y) (fromMaybe [] (field "runs" value))
    run y value=let x=fromMaybe (-1) (field "x" value)
                    count=fromMaybe 0 (field "length" value)
                    readable=fromMaybe False (field "readable" value)
                    clickable=fromMaybe False (field "clickable" value)
                in [(column,y,readable,clickable) | column<-[x..x+count-1]]
