{-# LANGUAGE OverloadedStrings #-}
module WideTextCheck (checks) where
import Control.Monad (unless,forM_)
import Control.Concurrent (threadDelay)
import System.Timeout (timeout)
import Data.Maybe (fromJust,isNothing)
import qualified Data.Map.Strict as Map
import qualified Hide.Model as M
import qualified Hide.BufferView as M
import Hide.TextPresentation
import Hide.Markdown (renderMarkdown)
import qualified Hide.Links as Links
import Hide.Render (snapshot,snapshotHtml,renderKey)
import qualified Hide.Plugin.Window as W
import Hide.GuestAccess (readableAt,streamerReadableAt)
import Data.Aeson (Value(..),object,toJSON,(.=))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Graphics.Vty.Span (SpanOp(..))
import Blaze.ByteString.Builder (writeToByteString)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Hide.Buffer (Buffer(undoStack),Selection(..),newBuffer,bufferContent,contentSlice,contentLength,contents)
import Hide.Syntax (Style(..),fontTraits)
import Hide.TextLayout
import Hide.Unicode
import qualified Hide.Protocol as Protocol
import Hide.TextStyle (textBold,textItalic)
import Hide.RemoteWindow
import Hide.RemoteTerminal (remoteTerminalDisplay)

checks :: IO ()
checks=do
 let check name ok=unless ok (fail name)
     ops image=Vec.toList (displayOpsForPic (V.picForImage image) (8,1) Vec.! 0)
     glyphs image=[(TL.toStrict text,width) | TextSpan _ width _ text<-ops image]
     original=wideTextImage V.defAttr "A"
 check ("compositor retains original narrow text and two-cell advance: "++show (glyphs original)) (take 1 (glyphs original)==[("A",2)])
 check "flattening preserves explicit advances" (displayOpsForPic (flattenPicture (8,1) (V.picForImage original)) (8,1)==displayOpsForPic (V.picForImage original) (8,1))
 check "partial wide crop becomes blank" (all (not . T.isInfixOf "A" . fst) (glyphs (V.cropRight 1 original)))
 let covered=V.picForLayers [V.translateX 1 (textImage V.defAttr "X"),original]
 check "covering either wide half clears the full glyph" (all (not . T.isInfixOf "A") [TL.toStrict text | row<-Vec.toList (displayOpsForPic covered (8,1)),TextSpan _ _ _ text<-Vec.toList row])
 forM_ [("A","Ａ"),(" ","　"),("é","ｅ́"),("é","é "),("́"," ́ "),("界","界"),("👩🏽\x200d\&💻","👩🏽\x200d\&💻")] $ \(semantic,projected)->do
   check "all title graphemes occupy two cells" (V.imageWidth (wideTextImage V.defAttr semantic)==2)
   let (bytes,end)=terminalSpan (const mempty) 0 2 semantic
   check "terminal projection reserves two cells and keeps semantic text separate" (end==2 && TE.encodeUtf8 projected `BS.isInfixOf` writeToByteString bytes)
 let metadata=object ["size" .= ([40,12]::[Int]),"bindings" .= ([]::[(T.Text,T.Text)])]
     row=toJSON [(0::Int,0xffffff::Int,0::Int,3::Int,[toJSON ("A"::T.Text,2::Int,True,0::Int,2::Int)])]
 frame<-either fail pure (parseRemoteFrame metadata (row:replicate 11 (toJSON ([]::[Value]))))
 check "native receiver keeps stretched semantic glyph width" (case remoteCells frame of [RemoteCell 0 0 _ "A" 2 0 2]->True; _->False)
 check "remote TUI retains the same original glyph/advance before output projection" (case [ (TL.toStrict text,width) | rowOps<-Vec.toList (snd (remoteTerminalDisplay (40,12) (Just frame) "")),TextSpan _ width _ text<-Vec.toList rowOps,text=="A"] of [("A",2)]->True; _->False)
 let clipped start shown=toJSON [(0::Int,0xffffff::Int,0::Int,3::Int,[toJSON ("A"::T.Text,2::Int,True,start::Int,shown::Int)])]
 forM_ [0,1] $ \start->do
   partial<-either fail pure (parseRemoteFrame metadata (clipped start 1:replicate 11 (toJSON ([]::[Value]))))
   check "native wire preserves partial glyph origin and full allocated width" (case remoteCells partial of [RemoteCell 0 0 _ "A" 2 offset 1]->offset==start; _->False)
   check "text mode suppresses a partial glyph without shifting following cells"
     (all (not . T.isInfixOf "A") [TL.toStrict text | rowOps<-Vec.toList (snd (remoteTerminalDisplay (40,12) (Just partial) "")),TextSpan _ _ _ text<-Vec.toList rowOps])
 check "clipped wire rejects impossible extents" (all (either (const True) (const False) . parseRemoteFrame metadata . (:replicate 11 (toJSON ([]::[Value])))) [clipped (-1) 1,clipped 2 1,clipped 1 2,clipped 0 0,clipped maxBound 1,clipped 1 maxBound])
 let bad flag advance text=toJSON [(0::Int,0::Int,0::Int,0::Int,[toJSON (text::T.Text,advance::Int,flag::Bool,0::Int,advance::Int)])]
 check "invalid stretched wire glyphs refuse instead of changing geometry" (all (either (const True) (const False) . parseRemoteFrame metadata . (:replicate 11 (toJSON ([]::[Value])))) [bad True 1 "A",bad True 2 "界",bad True 2 "ab",bad False 2 "A"])
 let heading="ABé界é👩🏽\x200d\&💻"
     semantic=heading<>"\nplain"
     source=bufferContent (newBuffer semantic)
     styles=Vec.fromList [[(c,SectionStyle 1 (BoldStyle (ItalicStyle (Heading 1)))) | c<-T.unpack heading],[(c,Plain) | c<-"plain"]]
 layout<-prepareTextLayout True 5 source styles
 ordinary<-prepareTextLayout False 5 source styles
 check "wide layout wraps without changing semantic source" (Vec.length (layoutRows layout)==4 && contentSlice source 0 (contentLength source)==semantic && Vec.length (layoutRows ordinary)==2)
 forM_ (zip [0..] (Vec.toList (layoutRows layout))) $ \(rowNumber,visual)->
   forM_ (Vec.toList (layoutGlyphs visual)) $ \glyph->do
     check "prepared source ranges retain complete original graphemes" (contentSlice source (layoutStart glyph) (layoutEnd glyph-layoutStart glyph)==layoutText glyph)
     check "render and navigation share the prepared position map" (layoutPosition layout (layoutStart glyph)==(rowNumber,layoutColumn glyph))
     forM_ [0..layoutAdvance glyph-1] $ \cell->check "all glyph cells hit one original source range" (layoutOffset layout rowNumber (layoutColumn glyph+cell)==layoutStart glyph)
 check "widening removes bold while retaining italic and heading semantics" (all (\glyph->let (_,bold,italic)=fontTraits (layoutStyle glyph) in not bold && italic) [glyph | row<-Vec.toList (layoutRows layout),glyph<-Vec.toList (layoutGlyphs row),layoutStart glyph<T.length heading])
 check "ordinary heading styles retain bold and italic" (all (\glyph->let (_,bold,italic)=fontTraits (layoutStyle glyph) in bold && italic) (Vec.toList (layoutGlyphs (Vec.head (layoutRows ordinary)))))
 check "ordinary rows do not acquire title geometry" (layoutRowWidth (Vec.last (layoutRows layout))==5)
 let controls="A\ESC#6B\r"
     controlSource=bufferContent (newBuffer controls)
     controlStyles=Vec.singleton [(c,SectionStyle 1 (Heading 1)) | c<-T.unpack controls]
 controlLayout<-prepareTextLayout True 40 controlSource controlStyles
 check "prepared rendering sanitizes controls before measuring glyphs" (all (\glyph->not (T.any (\c->c<' ') (layoutDisplayText glyph))) (Vec.toList (layoutGlyphs (Vec.head (layoutRows controlLayout)))) && contentSlice controlSource 0 (contentLength controlSource)==controls)
 withTextPresentation $ \owner->do
   let opened=M.addHelpStyled (renderMarkdown 40 "# *ABCDEF*\n\n[link](file.md)") (M.initialDesktop (80,25))
       view=fromJust (M.activeWindow opened)
       initial=opened {M.wideSectionTitles=True,M.windows=[view {M.bounds=M.Rect 2 2 8 12}]}
       settle desktop=do
         next<-tickTextPresentation owner desktop
         if maybe False (\w->M.windowPresentation next w/=Nothing) (M.activeWindow next) then pure next
         else threadDelay 1000 >> settle next
       prepare desktop=timeout 5000000 (settle desktop) >>= maybe (fail "wide heading owner did not adopt") pure
   let browsing=initial {M.windows=[(head (M.windows initial)) {M.scrollRow=1}]}
       originalText=bufferContent (M.documentBuffer (fromJust (M.activeDocument initial)))
       topOffset desktop=let current=fromJust (M.activeWindow desktop) in M.windowTextOffset desktop current originalText (M.scrollRow current) (M.scrollColumn current)
   preparedBrowsing<-prepareTextPresentations browsing
   check "adoption preserves the semantic viewport top" (topOffset preparedBrowsing==topOffset browsing)
   let shownPreferences=fst (M.runCommand M.EditorOptions preparedBrowsing)
       offDialog=(fromJust (M.dialog shownPreferences)) {M.fields=map (\field->case field of M.CheckBox "Wide section titles" _->M.CheckBox "Wide section titles" False; _->field) (M.fields (fromJust (M.dialog shownPreferences)))}
       returned=fst (M.submitDialog 0 offDialog shownPreferences)
   check "disabling wide titles preserves semantic manual browsing" (topOffset returned==topOffset preparedBrowsing)
   queued<-tickTextPresentation owner initial
   let queuedResize=queued {M.windows=[view {M.bounds=M.Rect 2 2 10 12}]}
   pendingReady<-prepare queuedResize
   check "pending resize cannot adopt the old worker width" (case Map.lookup (M.windowId view) (M.windowPresentations pendingReady) of Just (M.WindowPresentation _ columns _)->columns==8; _->False)
   ready<-prepare initial
   let w=fromJust (M.activeWindow ready)
       doc=fromJust (M.activeDocument ready)
       text=bufferContent (M.documentBuffer doc)
       layout=fromJust (M.windowPresentation ready w)
       headingStart=layoutStart (Vec.head (layoutGlyphs (Vec.head (layoutRows layout))))
       click col=M.selectAt False (M.left (M.bounds w)+1+col) (M.top (M.bounds w)+1) ready
       selected desktop=caret (M.selection (fromJust (M.activeWindow desktop)))
       atStart=M.moveTo False headingStart ready
       down=M.editorKey V.KDown [] atStart
       lastColumn=M.editorKey V.KEnd [] atStart
   check "live headings wrap in the shared prepared owner" (Vec.length (layoutRows layout)>M.windowTextRows (ready {M.wideSectionTitles=False}) w text)
   check "both stretched cells select the same original character" (selected (click 0)==headingStart && selected (click 1)==headingStart)
   check "Down and End use visual row edges" (selected down==layoutOffset layout 1 0 && selected lastColumn==layoutOffset layout 0 maxBound)
   let allText=fst (M.runCommand M.SelectAll ready)
       copied=fst (M.runCommand M.Copy allText)
   check "copy retains original ASCII without fullwidth substitution or wrap newlines" (M.clipboard copied==contents (M.documentBuffer doc) && not ("Ａ" `T.isInfixOf` M.clipboard copied))
   check "plain snapshot projects terminal fullwidth headings" ("ＡＢＣ" `T.isInfixOf` snapshot ready)
   check "HTML snapshots preserve stretch geometry and font traits" (all (`T.isInfixOf` snapshotHtml ready) ["width:2ch","scaleX(2)","font-style:italic"])
   native<-either fail pure (parseRemoteFrame (object (Protocol.frameMetadata "." ready)) (Protocol.frameRows ready))
   check "actual native widened heading retains span and italic without bold" (any (\cell->case cell of RemoteCell _ _ paint "A" 2 0 2->not (textBold paint) && textItalic paint; _->False) (remoteCells native))
   normalNative<-either fail pure (parseRemoteFrame (object (Protocol.frameMetadata "." (ready {M.wideSectionTitles=False}))) (Protocol.frameRows (ready {M.wideSectionTitles=False})))
   check "actual native normal-width heading remains bold and italic" (any (\cell->case cell of RemoteCell _ _ paint "A" 1 0 1->textBold paint && textItalic paint; _->False) (remoteCells normalNative))
   let originalRows=Protocol.frameRows (ready {M.wideSectionTitles=False})
   (_,rebuilt)<-Protocol.decodeFrame originalRows (BL.toStrict (Protocol.framePacket False originalRows (Protocol.frameRows ready) (Protocol.frameMetadata "." ready)))
   check "actual geometry change survives production compressed patch reconstruction" (rebuilt==Protocol.frameRows ready)
   let (a,z,_) = head (M.documentLinks doc)
       (linkRow,linkColumn)=layoutPosition layout a
       lx=M.left (M.bounds w)+1+linkColumn
       ly=M.top (M.bounds w)+1+linkRow
   check "links below wrapped headings retain their original target" (M.linkAt lx ly ready==Just (M.OpenLink Nothing "file.md") && z>a)
   let resized=ready {M.windows=[w {M.bounds=M.Rect 2 2 10 12}]}
       disabled=ready {M.wideSectionTitles=False}
   let review=ready {M.windows=[w {M.bufferView=M.SideBySideView}]}
   check "review views retain their original coordinate owner" (M.windowPresentation review (head (M.windows review))==Nothing)
   check "resize and disabling refuse stale layout immediately" (M.windowPresentation resized (head (M.windows resized))==Nothing && M.windowPresentation disabled w==Nothing)
   let preferences=fst (M.runCommand M.EditorOptions disabled)
       dg=fromJust (M.dialog preferences)
       changed=dg {M.fields=map (\field->case field of M.CheckBox "Wide section titles" _->M.CheckBox "Wide section titles" True; _->field) (M.fields dg)}
       (saved,effects)=M.submitDialog 0 changed preferences
   check "Preferences adopts and persists the Display option" (M.wideSectionTitles saved && M.SaveWideSectionTitles True `elem` effects)
   readyKey<-renderKey ready
   disabledKey<-renderKey disabled
   check "preference changes invalidate the shallow render key" (readyKey/=disabledKey)
   resizedReady<-prepare resized
   check "resized layout adopts a fresh measured identity" (M.windowPresentation resizedReady (head (M.windows resizedReady))/=Just layout)
   result<-Links.prepareMarkdown 40 "/tmp/notes.md" "" "# New document"
   let linkReplaced=fst (Links.applyLink result ready)
   check "prepared Markdown replacement cannot reuse an old source-map version" (M.windowPresentation linkReplaced (fromJust (M.activeWindow linkReplaced))==Nothing)
   let replaced=M.addHelpStyled (renderMarkdown 40 "# Replacement\n\nnext") ready
   check "same read-only document replacement retires old presentation" (maybe False (\current->M.windowPresentation replaced current==Nothing) (M.activeWindow replaced))
   _<-prepare replaced
   let poisoned=ready {M.buffers=Map.map (\document->document {M.documentBuffer=(M.documentBuffer document) {undoStack=error "wide heading inspected Undo"}}) (M.buffers ready)}
   _<-tickTextPresentation owner poisoned
   check "render/layout metadata never inspect Undo" ("Ａ" `T.isInfixOf` snapshot poisoned)
   let terminal=M.addReadOnly "Terminal test" "title" ready
       terminalStyled=terminal {M.buffers=Map.adjust (\document->document {M.documentHighlight=[('t',TerminalStyle 0xffffff 0 1)]}) (M.nextId terminal) (M.buffers terminal)}
   check "terminal ownership never enters Markdown preparation" (isNothing (M.windowPresentationTarget terminalStyled (fromJust (M.activeWindow terminalStyled))))
 W.withWindowScope $ \scope->withTextPresentation $ \owner->do
   prepared<-W.prepareMarkdownWindow 40 "Private notes" "# ABCDEF\n\nbody"
   update<-W.openTextWindow scope prepared >>= maybe (fail "missing plugin open") pure
   (reference,payload)<-W.admitWindowUpdate False update >>= maybe (fail "missing plugin admission") pure
   let opened=M.addPluginWindow reference payload (M.initialDesktop (80,25))
       w=fromJust (M.activeWindow opened)
       initial=opened {M.wideSectionTitles=True,M.windows=[w {M.bounds=M.Rect 2 2 8 12}]}
       settle desktop=do
         next<-tickTextPresentation owner desktop
         if M.windowPresentation next (head (M.windows next))/=Nothing then pure next else threadDelay 1000 >> settle next
   ready<-timeout 5000000 (settle initial) >>= maybe (fail "plugin heading layout not adopted") pure
   let view=head (M.windows ready)
       x=M.left (M.bounds view)+1;y=M.top (M.bounds view)+1
       copied=fst (M.runCommand M.Copy (fst (M.runCommand M.SelectAll ready)))
   check "plugin Markdown uses same wide layout and original copy" ("ＡＢＣ" `T.isInfixOf` snapshot ready && M.clipboard copied==contentSlice (W.preparedWindowText prepared) 0 (contentLength (W.preparedWindowText prepared)))
   check "wide plugin geometry does not bypass privacy" (not (readableAt ready x y) && not (streamerReadableAt (ready {M.streamerMode=True}) x y))
   fresh<-W.prepareMarkdownWindow 40 "Private notes" "# new"
   let refreshed=ready {M.pluginWindows=Map.insert reference fresh (M.pluginWindows ready)}
   check "plugin payload identity change refuses stale layout" (M.windowPresentation refreshed view==Nothing)
   plain<-W.prepareTextWindow "Plain" "# literal"
   check "plain plugin content is not mistaken for Markdown" (not (W.preparedWindowHasSections plain))
 putStrLn "wide semantic glyph compositor, prepared map and live frontend checks passed"
