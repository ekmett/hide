{-# LANGUAGE OverloadedStrings #-}
module ScreenCaptureCheck (checks) where

import EditorFixture (withEditorBodyFixture)
import Codec.Picture (Image, PixelRGB8(..), convertRGB8, decodePng, imageHeight, imageWidth, pixelAt)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hide.Accessibility
import Hide.Browser (Entry(..))
import Hide.Buffer (newBuffer, Selection(..))
import qualified Hide.Plugin.Window as W
import qualified Data.Vector as Vec
import Hide.TextLayout (prepareTextLayout)
import Hide.GuestAccess (CellAccess(..), cellAccess)
import Hide.Markdown (renderMarkdown)
import Hide.TextPresentation (prepareTextPresentations)
import Hide.Font (loadFont)
import SidebarFixture
import Hide.Model
import Hide.Render (snapshot)
import Hide.ScreenCapture
import Hide.Unicode (clusterWidth, graphemes, Script(..))
import Hide.Syntax (Style(..))

checks :: IO ()
checks=do
  let text="Session: provider-secret\nPublic assistant response 中"
      hidden=Vec.singleton (0,T.length "Session: provider-secret")
      semantics=W.TextSemantics W.CopyText Nothing Vec.empty Vec.empty W.ReadableWindow hidden hidden Vec.empty
  body<-W.prepareSemanticTextWindow "Conversation" [(text,Plain)] semantics
    >>= either (error . T.unpack) pure
  withEditorBodyFixture "" body (initialDesktop (80,25)) checksWithBody

checksWithBody :: Desktop -> IO ()
checksWithBody chatBase=do
  font<-loadFont
  let desktop=addDocument Nothing (newBuffer "  λ 中 ▙ é 👩🏽\x200d\&💻 ❤️\n") (initialDesktop (80,25))
      takeCapture d image=capture font d image >>= either (error . T.unpack) pure
  let overflow="a"<>T.replicate 70 "\x301"<>"Z"
      overflowView=addDocument Nothing (newBuffer overflow) (initialDesktop (80,25))
      fallbackView=addDocument Nothing (newBuffer "���Z") (initialDesktop (80,25))
      view=fromMaybe (error "missing overflow view") (activeWindow overflowView)
      ox=left (bounds view)+1; oy=top (bounds view)+1
  overflowCapture<-takeCapture overflowView True
  fallbackCapture<-takeCapture fallbackView True
  overflowImage<-pngImage overflowCapture
  fallbackImage<-pngImage fallbackCapture
  check "overflow capture text and pixel geometry agree with bounded visible fragments"
    (maybe False (T.isInfixOf "���Z") (field "text" (textMetadata overflowCapture)) &&
     all (\(x,y)->pixelAt overflowImage x y==pixelAt fallbackImage x y) [(x,y) | x<-[ox*8..(ox+4)*8-1],y<-[oy*16..(oy+1)*16-1]])
  let privateOverflow=overflowView {guestPrivatePaths=["/authority"],buffers=M.map (\doc->doc {documentOrigin=Just "/authority/overflow.txt"}) (buffers overflowView)}
  hiddenOverflow<-takeCapture privateOverflow True
  hiddenOverflowImage<-pngImage hiddenOverflow
  check "overflow source privacy masks every fragment and its following cell"
    (all (==PixelRGB8 0 0 0) [pixelAt hiddenOverflowImage x y | x<-[ox*8..(ox+4)*8-1],y<-[oy*16..(oy+1)*16-1]] &&
     all (\x->not (cellReadable (cellAccess privateOverflow x oy))) [ox..ox+3])
  let privateMessages=setProblemsVisible True desktop {guestPrivatePaths=["/authority"],
        diagnostics=[Diagnostic "/authority/secret.hs" Nothing 0 0 1 "secret-diagnostic-payload"]}
  safeMessages<-takeCapture privateMessages True
  check "screen metadata uses the shared guest-safe semantic sidebar"
    ((field "semanticSidebar" (textMetadata safeMessages)::Maybe Value)==Just (sidebarSemantics GuestSemantics privateMessages))
  check "logical screen omits protected diagnostic content"
    (not ("secret-diagnostic-payload" `T.isInfixOf` fromMaybe "" (field "text" (textMetadata safeMessages))))
  behindDialog<-takeCapture privateMessages {dialog=Just (Dialog "Public dialog" Widgets [] 0 ["OK"] [])} False
  check "opening a public dialog does not reveal protected Messages behind it"
    (not ("secret-diagnostic-payload" `T.isInfixOf` fromMaybe "" (field "text" (textMetadata behindDialog))))
  textOnly<-takeCapture desktop False
  let metadata=textMetadata textOnly
  check "screen text is the complete colorless rendered frame" (field "text" metadata==Just (snapshot desktop))
  check "text-only capture has no image block" (length (blocks textOnly)==1)
  check "screen text preserves Unicode and grapheme clusters" (all (`T.isInfixOf` snapshot desktop) ["λ","中","▙","é","👩🏽\x200d\&💻","❤️"])
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
  let scriptInitial=(addHelpStyled [("█",ScriptStyle Superscript Plain),("中",ScriptStyle Subscript Plain),("X",Plain)] (initialDesktop (80,25)))
  scriptReady<-prepareTextPresentations scriptInitial
  scriptCapture<-takeCapture scriptReady True
  scriptImage<-pngImage scriptCapture
  let scriptView=fromMaybe (error "missing script source view") (activeWindow scriptReady)
      sx=left (bounds scriptView)+1; sy=top (bounds scriptView)+1
      scriptPixel column px py=pixelAt scriptImage ((sx+column)*8+px) (sy*16+py)
      scriptBackground=PixelRGB8 0 0 170
  check "script screen text uses one-cell projections with the following source sentinel"
    (maybe False (T.isInfixOf "█\xfffdX") (field "text" (textMetadata scriptCapture)))
  check "narrow superscript samples the normal tile in the upper-left quarter"
    (scriptPixel 0 1 1/=scriptBackground && scriptPixel 0 5 1==scriptBackground && scriptPixel 0 1 12==scriptBackground)
  check "wide subscript samples natural tile ink only in the lower half of one cell"
    (all (==scriptBackground) [scriptPixel 1 px py | px<-[0..7],py<-[0..7]] &&
     any (/=scriptBackground) [scriptPixel 1 px py | px<-[0..7],py<-[8..15]])
  let privateScript=scriptReady {guestPrivatePaths=["/authority"],buffers=M.map (\doc->doc {documentOrigin=Just "/authority/script-secret.hs"}) (buffers scriptReady)}
  hiddenScript<-takeCapture privateScript True
  hiddenScriptImage<-pngImage hiddenScript
  check "script privacy removes semantic text and all ink in the allocated cell"
    (not (maybe False (T.isInfixOf "█") (field "text" (textMetadata hiddenScript))) &&
     all (==PixelRGB8 0 0 0) [pixelAt hiddenScriptImage ((sx+column)*8+px) (sy*16+py) | column<-[0,1],px<-[0..7],py<-[0..15]])
  compact<-takeCapture desktop {videoMode=Just 259,screenSize=(80,50)} True
  compactImage<-pngImage compact
  check "mode 259 uses 8x8 cells and preserves 80x50 aspect" (imageWidth compactImage==640 && imageHeight compactImage==400 && field "cellHeight" (textMetadata compact)==Just (8::Int))
  stable<-takeCapture desktop {blinkCursor=not (blinkCursor desktop),crtFilter=not (crtFilter desktop)} True
  check "capture is independent of cursor blink phase and CRT effects" (withImage==stable)
  let conversation=setComposerInput (newBuffer "private draft 中é") (Selection 7 7) True chatBase
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
  -- Capture an actual prepared heading, not an invented span. The second
  -- semantic character owns two private cells; public glyphs and the border
  -- after it must retain the same positions in text, PNG and access metadata.
  -- CommonMark normalizes accents; retain a decomposed combining cluster in
  -- this styled payload to exercise capture's actual grapheme path as well.
  let styled=map (\(text,style)->(T.replace "é" "e\x0301" text,style))
        (renderMarkdown 40 "# ABC界é👩🏽\x200d\&💻❤️\n\npublic")
      withHeading hidden wide run=do
        let semantics=W.TextSemantics W.CopyText Nothing Vec.empty Vec.empty W.ReadableWindow
              (Vec.fromList hidden) Vec.empty Vec.empty
        body<-W.prepareSemanticTextWindow "Conversation" styled semantics >>= either (error . T.unpack) pure
        withEditorBodyFixture "" body (initialDesktop (80,25)) $ \mounted->do
          let headingView=fromMaybe (error "missing heading window") (activeWindow mounted)
              rows=case W.preparedWindowRows body of W.StyledRows preparedRows->preparedRows; _->error "heading is not styled"
          layout<-prepareTextLayout wide 10 (W.preparedWindowText body) rows
          let receipt reference=InstalledBody reference (Just (BodyControlReceipt body 10 wide (Just layout) (HostBodyControls Nothing Nothing Nothing [])))
              heading=mounted {wideSectionTitles=wide,windows=[headingView {bounds=Rect 2 2 12 12}],
                conversationViews=M.adjust (\value->case conversationBodyRef value of
                  Just reference->value {conversationBody=receipt reference}
                  Nothing->error "missing heading body reference") "" (conversationViews mounted)}
          run heading
  withHeading [(1,2),(6,10)] True $ \guarded->do
    wideCapture<-takeCapture guarded True
    ordinaryCapture<-withHeading [] False (\plainHeading->takeCapture plainHeading True)
    wideImage<-pngImage wideCapture
    ordinaryImage<-pngImage ordinaryCapture
    let wideMetadata=textMetadata wideCapture
        wideRows=T.lines (fromMaybe "" (field "text" wideMetadata))
        wideAccess=accessCells wideMetadata
        atCell x y=[(readable,clickable) | (cx',cy',readable,clickable)<-wideAccess,cx'==x,cy'==y]
        cellPixels picture x y=[pixelAt picture px py | px<-[x*8..x*8+7],py<-[y*16..y*16+15]]
    check ("capture text preserves explicit heading advances around private cells: "++show (take 2 (drop 3 wideRows)))
      ("Ａ  Ｃ界ｅ́" `T.isInfixOf` (wideRows!!3) && "  ❤️" `T.isInfixOf` (wideRows!!4) && not ("👩🏽" `T.isInfixOf` T.unlines wideRows) && all ((==80) . sum . map clusterWidth . graphemes) wideRows)
    check "capture masks agree with prepared source positions in both heading cells"
      (all (\x->atCell x 3==[(False,False)] && not (cellReadable (cellAccess guarded x 3))) [5,6] &&
       all (\x->atCell x 3==[(True,False)] && cellReadable (cellAccess guarded x 3)) [3,4,7,8,9,10])
    check "private stretched glyph is black in both PNG cells"
      (all (all (==PixelRGB8 0 0 0) . (\x->cellPixels wideImage x 3)) [5,6])
    check "natural ZWJ emoji is wholly redacted while the following variation-selector emoji keeps its cells"
      (all (\x->atCell x 4==[(False,False)] && all (==PixelRGB8 0 0 0) (cellPixels wideImage x 4)) [3,4] &&
       all (\x->atCell x 4==[(True,False)] && cellReadable (cellAccess guarded x 4)) [5,6])
    check "combining heading grapheme and natural CJK retain their complete two-cell spans"
      (all (\x->atCell x 3==[(True,False)]) [9,10,11,12])
    check "public stretched glyph draws in both cells and leaves following border fixed"
      (all (\x->length (nub (cellPixels wideImage x 3))>1) [7,8] &&
        cellPixels wideImage 13 3==cellPixels ordinaryImage 13 3)
    -- An interior modifier/selector interval must redact the entire grapheme.
    naturalCapture<-withHeading [(3,4),(7,8),(11,12)] False (\heading->takeCapture heading True)
    naturalImage<-pngImage naturalCapture
    let naturalMetadata=textMetadata naturalCapture
        naturalCells=accessCells naturalMetadata
        naturalText=fromMaybe "" (field "text" naturalMetadata)
    check "ordinary natural CJK and ZWJ/variation-selector emoji redact both occupied cells"
      (not (any (`T.isInfixOf` naturalText) ["界","👩🏽","❤️"]) &&
        all (\x->(x,3,False,False) `elem` naturalCells && all (==PixelRGB8 0 0 0) (cellPixels naturalImage x 3)) [6,7,9,10,11,12])
  let config=desktop {dialog=Just (Dialog "Agents" (AgentDialog "configure")
        [Input "Executable" "public-command" 0,Input "Environment (JSON object)" "private-env-token" 0] 0 ["OK","Cancel"] [])}
  configCapture<-takeCapture config False
  let configText=fromMaybe "" (field "text" (textMetadata configCapture))
  check "agent settings are readable while environment values are private" ("public-command" `T.isInfixOf` configText && not ("private-env-token" `T.isInfixOf` configText))
  let dialogMetadata=field "semanticDialog" (textMetadata configCapture)::Maybe Value
      encodedConfig=TE.decodeUtf8 (BL.toStrict (encode (textMetadata configCapture)))
  check "actual screen response carries read-only dialog semantics without private environment values"
    (maybe False (\value->field "present" value==Just True && field "readOnly" value==Just True) dialogMetadata &&
      "public-command" `T.isInfixOf` encodedConfig && not ("private-env-token" `T.isInfixOf` encodedConfig))
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
      (stretchedText,stretchedAccess)=redactCluster "A" [readable,private]
      (emojiText,emojiAccess)=redactCluster "👩\x200d\&💻" [private,readable]
  check "one private wide-glyph cell redacts the entire grapheme" (wideText=="  " && all (not . cellReadable) wideAccess && map cellClickable wideAccess==[True,False])
  check "one private stretched ASCII cell redacts its full explicit advance" (stretchedText=="  " && all (not . cellReadable) stretchedAccess && map cellClickable stretchedAccess==[True,False])
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
