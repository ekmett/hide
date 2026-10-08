{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Immutable bounded PNG resources and the host-composed image scene.
-- Preparation runs on a worker. IDs identify bytes, never permission or input
-- authority; a surface may retain a resource while every cell is occluded.
module Hide.Plugin.Canvas
  ( PreparedImage, preparePNG, imageResourceId, imageWidth, imageHeight, imageRGBA, imagePNG
  , CanvasView(..), fitCanvasView, canvasImageTarget
  , CanvasSurface(..), CanvasScene(..), canvasOwnerAt, canvasPixel
  ) where

import Codec.Picture (decodePng, convertRGBA8, imageData)
import qualified Codec.Picture as P
import Control.Exception (evaluate)
import Data.Bits ((.|.),(.&.),shiftL)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Storable as VS
import Hide.RemoteEndpoint (randomIdentity)

-- | Exact resource identity. Equality never examines the pixel or PNG payload.
data PreparedImage = PreparedImage !Text !Int !Int !BS.ByteString !BS.ByteString
instance Eq PreparedImage where
  a==b=imageResourceId a==imageResourceId b
instance Show PreparedImage where
  show image="PreparedImage "++show (imageResourceId image,imageWidth image,imageHeight image)
-- | O(1). Fresh opaque ID for this immutable decoded resource.
imageResourceId :: PreparedImage -> Text
imageResourceId (PreparedImage ident _ _ _ _)=ident
-- | O(1). Positive source dimensions, each at most 4096; area at most 4 Mi pixels.
imageWidth, imageHeight :: PreparedImage -> Int
imageWidth (PreparedImage _ width _ _ _)=width
imageHeight (PreparedImage _ _ height _ _)=height
-- | O(1). Strict row-major straight RGBA8; length is exactly width * height * 4.
imageRGBA :: PreparedImage -> BS.ByteString
imageRGBA (PreparedImage _ _ _ bytes _)=bytes
-- | O(1). Original bounded PNG bytes, suitable for explicit external opening.
imagePNG :: PreparedImage -> BS.ByteString
imagePNG (PreparedImage _ _ _ _ bytes)=bytes

-- | Decode on a worker after checking the signature and IHDR allocation bounds.
-- No successful resource exceeds 4 Mi pixels or retains over 16 MiB input bytes.
preparePNG :: BS.ByteString -> IO (Either Text PreparedImage)
preparePNG bytes
  | BS.length bytes>16777216=pure (Left "PNG exceeds the 16 MiB file limit.")
  | BS.length bytes<33 || BS.take 8 bytes/=BS.pack [137,80,78,71,13,10,26,10] || word 8/=13 || BS.take 4 (BS.drop 12 bytes)/="IHDR"=
      pure (Left "Invalid PNG header.")
  | width<=0 || height<=0 || width>4096 || height>4096 || toInteger width*toInteger height>4194304=
      pure (Left "PNG exceeds 4096 pixels per side or the 4-megapixel limit.")
  | otherwise=case decodePng bytes of
      Left _->pure (Left "Cannot decode PNG.")
      Right decoded->do
        let image=convertRGBA8 decoded
            (pointer,offset,count)=VS.unsafeToForeignPtr (imageData image)
            rgba=BSI.fromForeignPtr pointer offset count
        if P.imageWidth image/=width || P.imageHeight image/=height || BS.length rgba/=width*height*4
          then pure (Left "PNG decoded dimensions do not match its header.") else do
            owned<-evaluate (BS.copy bytes)
            _<-evaluate (BS.length rgba)
            ident<-T.pack <$> randomIdentity
            pure (Right (PreparedImage ident width height rgba owned))
  where
    word at=BS.foldl' (\n byte->n*256+fromIntegral byte) (0::Integer) (BS.take 4 (BS.drop at bytes))
    width=fromInteger (word 16); height=fromInteger (word 20)

-- | Host interaction in editor logical pixels. Nothing means fit the viewport;
-- explicit zoom is source pixels to logical pixels. Pan offsets the centered image.
data CanvasView = CanvasView !(Maybe Double) !Double !Double deriving (Eq,Show)
-- | Fit without pan. Window resizing recomputes fit without touching the resource.
fitCanvasView :: CanvasView
fitCanvasView=CanvasView Nothing 0 0

-- | Destination in fractional cells. Source aspect is preserved at both font
-- heights. Zoom and pan are clamped before producing finite bounded coordinates.
canvasImageTarget :: Int -> (Int,Int,Int,Int) -> CanvasView -> PreparedImage -> (Double,Double,Double,Double)
canvasImageTarget cellHeight (x,y,width,height) (CanvasView chosen dx dy) image=(fromIntegral x+left/8,fromIntegral y+top/ch,iw*scale/8,ih*scale/ch)
  where
    ch=fromIntegral (max 1 cellHeight)
    vw=fromIntegral (max 0 width)*8; vh=fromIntegral (max 0 height)*ch
    iw=fromIntegral (imageWidth image); ih=fromIntegral (imageHeight image)
    fit=min (vw/iw) (vh/ih)
    scale=case chosen of Nothing->fit; Just value->if isNaN value || isInfinite value then fit else max (1/64) (min 64 value)
    bound extent value | isNaN value || isInfinite value=0
                       | otherwise=max (-extent) (min extent value)
    left=(vw-iw*scale)/2+bound ((vw+iw*scale)/2) dx
    top=(vh-ih*scale)/2+bound ((vh+ih*scale)/2) dy

-- | A complete immutable image placement. Slots 1..64 belong only to this frame;
-- host window IDs and resource IDs survive ordinary pan/zoom and occlusion.
data CanvasSurface = CanvasSurface
  { canvasId :: !Int, canvasSlot :: !Int, canvasImage :: !PreparedImage
  , canvasRect :: !(Int,Int,Int,Int), canvasTarget :: !(Double,Double,Double,Double)
  , canvasName :: !Text, canvasDescription :: !Text
  } deriving (Eq,Show)
-- | Cells and this scene must come from the same composition. Mask bytes are
-- little-endian uint16 per cell: low 15 bits slot, high bit half-intensity shadow.
data CanvasScene = CanvasScene {canvasSurfaces :: ![CanvasSurface], canvasMask :: !BS.ByteString} deriving (Eq,Show)

-- | O(1). Read a valid cell index; absent/out-of-range ownership is background.
canvasOwnerAt :: CanvasScene -> Int -> Int
canvasOwnerAt scene index
  | index<0 || index>=BS.length bytes `div` 2=0
  | otherwise=fromIntegral (BS.index bytes (2*index)) .|. (fromIntegral (BS.index bytes (2*index+1)) `shiftL` 8)
  where bytes=canvasMask scene

-- | Sample one cell-local point through the same image transform as the GPU.
-- The image composites straight alpha onto black; uncovered viewport pixels are
-- black. Nothing means this cell is not owned by this surface. Coordinates are
-- fractional screen cells, independent of Retina scale and capture resolution.
canvasPixel :: CanvasScene -> Int -> Double -> Double -> Maybe (Int,Int,Int)
canvasPixel scene columns x y=do
  let owner=canvasOwnerAt scene (floor y*columns+floor x)
      slot=owner .&. 32767
  surface<-findSlot slot (canvasSurfaces scene)
  let (left,top,width,height)=canvasTarget surface
      image=canvasImage surface
      ix=floor ((x-left)*fromIntegral (imageWidth image)/width)
      iy=floor ((y-top)*fromIntegral (imageHeight image)/height)
      inside=width>0 && height>0 && x>=left && y>=top && x<left+width && y<top+height
      bytes=imageRGBA image
      at=4*(iy*imageWidth image+ix)
      channel offset=fromIntegral (BS.index bytes (at+offset))::Int
      dim=if owner .&. 32768/=0 then 2 else 1
      paint offset=channel offset*channel 3 `div` (255*dim)
  pure (if inside then (paint 0,paint 1,paint 2) else (0,0,0))
  where
    findSlot _ []=Nothing
    findSlot slot (surface:rest) | slot/=0 && canvasSlot surface==slot=Just surface
                               | otherwise=findSlot slot rest
