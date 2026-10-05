{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
-- | Shared grapheme segmentation, cell widths and picture composition.
--
-- utf8proc supplies segmentation with an ASCII fast path; source offsets remain
-- Unicode characters while display widths follow graphemes and editor overrides.
-- Clipped GPU cells retain their full semantic glyph. Text-mode partial clusters
-- become blanks. Terminal output advances
-- explicitly past two-cell clusters even when the user's font draws them narrowly.
module Hide.Unicode (graphemes, clusterWidth, textImage, wideTextImage, displayClusters, terminalProjection, terminalSpan, CellSpan(..), CellLayer(..), cellRowsForLayers, cellRowsForPic, cellDisplayOps, flattenPicture, displayOpsForPic, updatePicture, terminalText, textInputChar) where

import Control.Monad (forM_, when)
import Data.Char (isPrint)
import Data.IORef (readIORef, writeIORef)
import Blaze.ByteString.Builder (Write, writeToByteString)
import Blaze.ByteString.Builder.ByteString (writeByteString)
import Graphics.Vty.Output
import Graphics.Vty.Attributes (FixedAttr(..), defaultStyleMask)
import Graphics.Vty.DisplayAttributes (fixDisplayAttr, displayAttrDiffs)
import Graphics.Vty.Span (SpanOp(..), DisplayOps)
import Control.Monad.ST (runST)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MV
import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Image.Internal as I

foreign import ccall unsafe "utf8proc_charwidth" c_width :: CInt -> CInt
foreign import ccall unsafe "thc_graphemes" c_graphemes :: CString -> CInt -> Ptr CInt -> IO CInt

-- | Segment extended graphemes, preserving CRLF as one cluster on the ASCII fast path.
graphemes :: T.Text -> [T.Text]
graphemes t
  | T.all (<'\128') t = ascii t
  | otherwise = unsafePerformIO $ BS.useAsCStringLen (TE.encodeUtf8 t) $ \(s,n) ->
      allocaArray (T.length t+1) $ \p -> do
        count <- fromIntegral <$> c_graphemes s (fromIntegral n) p
        offsets <- map fromIntegral <$> peekArray count p
        pure [T.take (z-a) (T.drop a t) | (a,z)<-zip offsets (drop 1 offsets)]
  where
    ascii s = case T.uncons s of
      Nothing -> []
      Just ('\r',rest) | Just ('\n',after)<-T.uncons rest -> "\r\n":ascii after
      Just (c,rest) -> T.singleton c:ascii rest
{-# NOINLINE graphemes #-}

clusterWidth :: T.Text -> Int
clusterWidth t
  | T.any (`elem` ['⌥','⌘']) t = 2 -- Mac key legends reserve a two-cell tile in every frontend.
  | T.any (`elem` ['\xf024b','\xf0770']) t = 2 -- Two-cell Material folder tiles, including terminal cursor correction.
  | T.any (`elem` ['\xfe0f','\x20e3']) t = 2
  | T.any (\c -> c>='\x1f1e6' && c<='\x1f1ff') t = 2
  | otherwise = maximum (0:map (max 0 . fromIntegral . c_width . fromIntegral . fromEnum) (T.unpack t))

textImage :: V.Attr -> T.Text -> V.Image
textImage _ t | T.null t = V.emptyImage
textImage a t = I.HorizText a (TL.fromStrict t) (sum (map clusterWidth (graphemes t))) (T.length t)

-- | Render each semantic grapheme in exactly two cells. Naturally wide text
-- remains two cells. The original text remains in HorizText, whose explicit
-- advance survives the compositor; no Unicode substitution or attribute marker.
wideTextImage :: V.Attr -> T.Text -> V.Image
wideTextImage a=unmergedImages . map (\g->I.HorizText a (TL.fromStrict g) 2 (T.length g)) . graphemes

-- Vty merges adjacent HorizText values by attribute. That optimization assumes
-- natural advances, so an explicit-width primitive retains a join boundary.
unmergedImages :: [V.Image] -> V.Image
unmergedImages=foldr (\image rest->I.HorizJoin image rest
  (V.imageWidth image+V.imageWidth rest) (max (V.imageHeight image) (V.imageHeight rest))) V.emptyImage

-- | Read a text primitive's explicit advance. A deliberately widened primitive
-- contains one grapheme; ordinary runs retain natural widths.
displayClusters :: Int -> T.Text -> [(T.Text,Int)]
displayClusters width text=case graphemes text of
  [g] | width==2 -> [(g,2)]
  gs -> [(g,clusterWidth g) | g<-gs]

-- | The final visible row representation shared by terminal and GPU frontends.
-- CellText is a complete ASCII run. CellGlyph retains one semantic grapheme,
-- its full allocated cell width, visible start within that glyph, and visible
-- width. Its glyph origin is the current row position minus the clip start.
-- Backend projection happens after composition; privacy masks replace the
-- semantic glyph before these rows can leave the capture owner.
data CellSpan = CellText !V.Attr !T.Text | CellGlyph !V.Attr !T.Text !Int !Int !Int
  deriving (Eq,Show)

-- Every occupied cell retains the glyph and its position within that glyph.
-- Overwriting one cell preserves the visible portion of an underlying glyph.
data Cell = Cell !V.Attr !T.Text !Int !Int | AsciiCell !V.Attr !Char | Unfilled !(Maybe V.Attr)

-- | Ordered opaque images and small style-only halo regions, front to back.
-- Halo processing preserves glyph origin, identity and allocated width.
data CellLayer = CellImage !V.Image | CellHalo !V.Attr ![(Int,Int,Int,Int)]
               | CellMask !V.Attr ![(Int,Int,Int)]

-- | Compose layers once into bounded mutable storage, retaining partial glyphs.
-- Cost depends on the visible scene, never on Document or Buffer equality.
cellRowsForPic :: V.Picture -> (Int,Int) -> Vec.Vector (Vec.Vector CellSpan)
cellRowsForPic picture=cellRowsForLayers (map CellImage (V.picLayers picture))

-- | Compose image and halo layers in one visible grid. Halo work touches only
-- its bounded exposed bands after occlusion. Front-to-back traversal skips
-- writes to already occupied cells; only unfilled cells accept a halo style.
cellRowsForLayers :: [CellLayer] -> (Int,Int) -> Vec.Vector (Vec.Vector CellSpan)
cellRowsForLayers layers (w,h)=Vec.generate h (\y->Vec.fromList (runs (Vec.toList (Vec.slice (y*w) w cells))))
  where
    cells=runST $ do
      grid<-MV.replicate (w*h) (Unfilled Nothing)
      let put at cell=do
            original<-MV.read grid at
            case original of
              Unfilled paint->MV.write grid at (maybe cell (dim cell) paint)
              _->pure ()
          dim (AsciiCell old c) paint=AsciiCell paint {V.attrStyle=V.attrStyle old} c
          dim (Cell old text width offset) paint=Cell paint {V.attrStyle=V.attrStyle old} text width offset
          dim cell _=cell
      let draw (l,top,r,b) x y img=case img of
            I.HorizText a text advance _ | y>=top && y<b -> do
              let strict=TL.toStrict text
              if advance==T.length strict && T.all (< '\128') strict
                then forM_ [max l x..min r (x+advance)-1] $ \i->
                  put (y*w+i) (AsciiCell a (T.index strict (i-x)))
                else if advance==T.length strict && not (T.null strict) && T.all (==T.head strict) strict && clusterWidth (T.take 1 strict)==1
                  then let glyph=T.take 1 strict in forM_ [max l x..min r (x+advance)-1] $ \i->put (y*w+i) (Cell a glyph 1 0)
                else do
                  let chunks=displayClusters advance strict
                  forM_ (zip (scanl (+) x (map snd chunks)) chunks) $ \(cx,(t,n))->do
                    let lo=max l cx; hi=min r (cx+n)
                    forM_ [lo..hi-1] $ \i->put (y*w+i)
                      (if n==1 && T.length t==1 && T.head t<'\128' then AsciiCell a (T.head t) else Cell a t n (i-cx))
            I.HorizJoin left right _ _->draw (l,top,r,b) x y left >> draw (l,top,r,b) (x+V.imageWidth left) y right
            I.VertJoin above below _ _->draw (l,top,r,b) x y above >> draw (l,top,r,b) x (y+V.imageHeight above) below
            I.Crop inside dx dy cw ch->
              let clip=(max l x,max top y,min r (x+cw),min b (y+ch))
              in when (max l x<min r (x+cw) && max top y<min b (y+ch)) (draw clip (x-dx) (y-dy) inside)
            _->pure ()
      let layer (CellImage image)=draw (0,0,w,h) 0 0 image
          layer (CellHalo paint regions)=forM_ regions $ \(x,y,columns,rows)->
            forM_ [max 0 y..min h (y+rows)-1] $ \cy->
              forM_ [max 0 x..min w (x+columns)-1] $ \cx->do
                original<-MV.read grid (cy*w+cx)
                case original of
                  Unfilled Nothing->MV.write grid (cy*w+cx) (Unfilled (Just paint))
                  _->pure ()
          layer CellMask{}=pure ()
          mask paint regions=forM_ regions $ \(x,y,columns)->when (y>=0 && y<h) $
            forM_ [max 0 x..min w (x+columns)-1] $ \cx->do
              original<-MV.read grid (y*w+cx)
              case original of
                Cell _ text width offset->forM_ [max 0 (cx-offset)..min w (cx-offset+width)-1] $ \i->do
                  visible<-MV.read grid (y*w+i)
                  case visible of
                    Cell _ glyph full part | glyph==text && full==width && i-part==cx-offset->
                      MV.write grid (y*w+i) (AsciiCell paint '*')
                    _->pure ()
                _->MV.write grid (y*w+cx) (AsciiCell paint '*')
      mapM_ layer layers
      mapM_ (uncurry mask) [(paint,regions) | CellMask paint regions<-layers]
      Vec.freeze grid
    asciiCell (AsciiCell a c)=Just (a,c)
    asciiCell (Unfilled paint)=Just (maybe V.defAttr id paint,' ')
    asciiCell _=Nothing
    runs []=[]
    runs (AsciiCell a c:rest)=asciiRun a c rest
    runs (Unfilled paint:rest)=asciiRun (maybe V.defAttr id paint) ' ' rest
    runs (Cell a t n offset:rest)=let (count,after)=follow (offset+1) rest
                                in CellGlyph a t n offset (count+1):runs after
      where
        follow expected (Cell b g width part:more)
          | expected<n && a==b && t==g && n==width && part==expected=
            let (count,after)=follow (expected+1) more in (count+1,after)
        follow _ more=(0,more)

    asciiRun a c rest=
      let (same,after)=span (\cell->case asciiCell cell of Just (b,_)->a==b; _->False) rest
      in CellText a (T.pack (c:[ch | Just (_,ch)<-map asciiCell same])):runs after

-- | Text-mode projection suppresses partial graphemes with occupied-cell blanks.
-- A complete explicit-width glyph retains its advance for terminal correction.
displayOpsForPic :: V.Picture -> (Int,Int) -> DisplayOps
displayOpsForPic picture size=cellDisplayOps (cellRowsForPic picture size)

-- | Project an already composed common grid for text-mode output. Partially
-- visible graphemes occupy blanks; complete glyphs retain their explicit width.
cellDisplayOps :: Vec.Vector (Vec.Vector CellSpan) -> DisplayOps
cellDisplayOps=Vec.map (Vec.map terminal)
  where
    terminal (CellText a text)=TextSpan a (T.length text) (T.length text) (TL.fromStrict text)
    terminal (CellGlyph a text full start width)
      | start==0 && width==full=TextSpan a full (T.length text) (TL.fromStrict text)
      | otherwise=TextSpan a width width (TL.fromStrict (T.replicate width " "))

-- | Explicit text-mode picture projection. Display frontends consume cell rows
-- directly; this helper is for callers needing an ordinary clipped Vty image.
flattenPicture :: (Int,Int) -> V.Picture -> V.Picture
flattenPicture size picture=picture {V.picLayers=[V.vertCat (map row (Vec.toList (displayOpsForPic picture size)))]}
  where row=unmergedImages . map image . Vec.toList
        image (TextSpan a tWidth chars text)=I.HorizText a text tWidth chars
        image _=V.emptyImage

-- | Emit grapheme-aware spans through Vty capabilities and its row-diff cache.
updatePicture :: V.Vty -> V.Picture -> IO ()
updatePicture vty picture = do
  let output=V.outputIface vty
  size@(w,h) <- displayBounds output
  dc <- displayContext output size
  previous <- readIORef (assumedStateRef output)
  urls <- getModeStatus output Hyperlink
  let ops=displayOpsForPic picture size
      initial=FixedAttr defaultStyleMask Nothing Nothing Nothing
      changed y row=case prevOutputOps previous of
        Just old | Vec.length old==Vec.length ops -> old Vec.! y/=row
        _ -> True
      emit y (prefix,old,x) (TextSpan a advance _ t) =
        let limited=limitAttrForDisplay output a
            fixed=fixDisplayAttr old limited
            (text,end)=terminalSpan (\col -> writeMoveCursor dc (min (w-1) col) y) x advance (TL.toStrict t)
        in (prefix <> writeSetAttr dc urls old limited (displayAttrDiffs old fixed) <> text,fixed,end)
      emit _ state _=state
      rowBytes y row=let (text,_,_)=foldl' (emit y) (mempty,initial,0) (Vec.toList row)
                     in writeMoveCursor dc 0 y <> writeDefaultAttr dc urls <> text
      cursor=case V.picCursor picture of
        V.Cursor x y -> at x y
        V.AbsoluteCursor x y -> at x y
        _ -> mempty
      at x y=writeShowCursor dc <> writeMoveCursor dc (max 0 (min (w-1) x)) (max 0 (min (h-1) y))
      bytes=writeHideCursor dc <> mconcat [rowBytes y row | (y,row)<-zip [0..] (Vec.toList ops),changed y row] <> cursor
  outputByteBuffer output (writeToByteString bytes)
  writeIORef (assumedStateRef output) previous {prevOutputOps=Just ops}

-- | Accept printable input and the joiner/tag characters needed by complex graphemes.
textInputChar :: Char -> Bool
textInputChar c=isPrint c || c `elem` ['\x200c','\x200d'] || c>='\xe0020' && c<='\xe007f'

-- | Encode positioned terminal text with explicit advancement across two-cell clusters.
terminalText :: (Int -> Write) -> Int -> T.Text -> (Write,Int)
terminalText move start text = foldl' emit (mempty,start) (graphemes text)
  where
    emit (bytes,x) g =
      let n=clusterWidth g
          raw=writeByteString (TE.encodeUtf8 g)
          drawn=if n==2 then writeByteString "  " <> move x <> raw <> move (x+2) else raw
      in (bytes<>drawn,x+n)

-- | Project explicit advances only at terminal output. Fullwidth ASCII and
-- ideographic spaces preserve two-cell geometry; other narrow graphemes use
-- their original text plus a padding cell. Semantic text remains unchanged.
terminalSpan :: (Int -> Write) -> Int -> Int -> T.Text -> (Write,Int)
terminalSpan move start advance text=terminalText move start (terminalProjection advance text)

-- | Display-only fullwidth/padding projection, also used by plain grid snapshots.
terminalProjection :: Int -> T.Text -> T.Text
terminalProjection advance text=case displayClusters advance text of
  [(g,2)] | clusterWidth g<2 -> fullwidth g
  _->text
  where
    fullwidth g | clusterWidth g==0 = " "<>g<>" "
                | Just (base,rest)<-T.uncons g,base==' ' = "\x3000"<>rest
                | Just (base,rest)<-T.uncons g,base>='!' && base<='~' = T.cons (toEnum (fromEnum base+0xfee0)) rest
                | otherwise=g<>" "
