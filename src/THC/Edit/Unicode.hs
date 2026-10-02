{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module THC.Edit.Unicode (graphemes, clusterWidth, textImage, flattenPicture, displayOpsForPic, updatePicture, terminalText, textInputChar) where

import Control.Monad (forM_, when)
import Data.Char (isPrint)
import Data.List (groupBy)
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

-- utf8proc owns the grapheme rules (including emoji ZWJ,
-- flags and combining sequences); source offsets remain Unicode code points.
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

-- Vty's layout is useful, but its clipping splits individual code points.
-- Compose its image tree on a cell grid first, so partial clusters become
-- blanks, including when another window covers one half of a wide glyph.
data Cell = Cell V.Attr T.Text Int | Tail V.Attr Int

flattenPicture :: (Int,Int) -> V.Picture -> V.Picture
flattenPicture size picture = picture {V.picLayers=[V.vertCat (pictureRows size picture)]}

pictureRows :: (Int,Int) -> V.Picture -> [V.Image]
pictureRows (w,h) picture = images
  where
    cells = runST $ do
      grid <- MV.replicate (w*h) (Cell V.defAttr " " 1)
      let clear x y = when (x>=0 && x<w && y>=0 && y<h) $ do
            cell <- MV.read grid (y*w+x)
            let start=case cell of Tail _ n -> x-n; _ -> x
            first <- MV.read grid (y*w+start)
            case first of
              Cell a _ n -> forM_ [start..min (w-1) (start+n-1)] $ \i -> MV.write grid (y*w+i) (Cell a " " 1)
              _ -> pure ()
          put x y a t n = do
            forM_ [x..x+n-1] $ \i -> clear i y
            MV.write grid (y*w+x) (Cell a t n)
            forM_ [1..n-1] $ \i -> MV.write grid (y*w+x+i) (Tail a i)
          draw (l,top,r,b) x y img = case img of
            I.HorizText a text _ _ | y>=top && y<b -> do
              let chunks=graphemes (TL.toStrict text)
              forM_ (zip (scanl (+) x (map clusterWidth chunks)) chunks) $ \(cx,t) -> do
                let n=clusterWidth t; lo=max l cx; hi=min r (cx+n)
                when (lo<hi) $ if cx>=l && cx+n<=r then put cx y a t n
                  else forM_ [lo..hi-1] $ \i -> put i y a " " 1
            I.HorizJoin left right _ _ -> draw (l,top,r,b) x y left >> draw (l,top,r,b) (x+V.imageWidth left) y right
            I.VertJoin above below _ _ -> draw (l,top,r,b) x y above >> draw (l,top,r,b) x (y+V.imageHeight above) below
            I.Crop inside dx dy cw ch ->
              let clip=(max l x,max top y,min r (x+cw),min b (y+ch))
              in when (max l x<min r (x+cw) && max top y<min b (y+ch)) (draw clip (x-dx) (y-dy) inside)
            _ -> pure () -- BGFill is transparent to lower layers.
      mapM_ (draw (0,0,w,h) 0 0) (reverse (V.picLayers picture))
      Vec.freeze grid
    images=[V.horizCat [textImage a (T.concat (map snd group)) | group@((a,_):_)<-groupBy (\a b -> fst a==fst b) [(a,t) | Cell a t _<-Vec.toList (Vec.slice (y*w) w cells)]] | y<-[0..h-1]]

-- The same cluster widths must reach both frontends. Vty's stock span builder
-- remeasures text by code point, undoing the image widths for ZWJ sequences.
displayOpsForPic :: V.Picture -> (Int,Int) -> DisplayOps
displayOpsForPic picture size = Vec.fromList (map (Vec.fromList . spans) (pictureRows size picture))
  where
    spans (I.HorizText a t w n)=[TextSpan a w n t]
    spans (I.HorizJoin a b _ _) = spans a ++ spans b
    spans _=[]

-- Reuse Vty's terminal capabilities and attribute writers, supplying our spans.
-- Keep its per-row diff cache; input, resize and terminal lifecycle stay in Vty.
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
      emit y (prefix,old,x) (TextSpan a _ _ t) =
        let limited=limitAttrForDisplay output a
            fixed=fixDisplayAttr old limited
            (text,end)=terminalText (\col -> writeMoveCursor dc (min (w-1) col) y) x (TL.toStrict t)
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

-- Format code points are essential components of emoji and Indic text.
textInputChar :: Char -> Bool
textInputChar c=isPrint c || c `elem` ['\x200c','\x200d'] || c>='\xe0020' && c<='\xe007f'

-- Reserve both cells even when the terminal's font draws a wide grapheme in
-- one. Explicit positioning also repairs the following text's column.
terminalText :: (Int -> Write) -> Int -> T.Text -> (Write,Int)
terminalText move start text = go (graphemes text) start
  where
    go clusters start = foldl' emit (mempty,start) clusters
    emit (bytes,x) g =
      let n=clusterWidth g
          raw=writeByteString (TE.encodeUtf8 g)
          drawn=if n==2 then writeByteString "  " <> move x <> raw <> move (x+2) else raw
      in (bytes<>drawn,x+n)
