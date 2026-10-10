{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Immutable bounded PNG/JPEG resources and the host-composed image scene.
-- Preparation runs on a worker. IDs identify bytes, never permission or input
-- authority; a surface may retain a resource while every cell is occluded.
module Hide.Plugin.Canvas
  ( PreparedImage, imageContentFormat, isImageContent, prepareImage, imageResourceId, imageFormat, imageWidth, imageHeight, imageRGBA, imageEncoded
  , ImageAction(..), ImageControl(..), imageActionPacket, parseImageActionPacket
  , CanvasView(..), fitCanvasView, canvasImageTarget
  , canvasVisibleCells, canvasControls
  , CanvasSurface(..), CanvasScene(..), canvasOwnerAt, canvasPixel
  ) where

import Codec.Picture (decodePng, decodeJpeg, convertRGBA8, imageData)
import qualified Codec.Picture as P
import Control.Exception (SomeException,SomeAsyncException,evaluate,fromException,throwIO,try)
import Control.Monad (unless,guard)
import Data.Aeson (Value,FromJSON(..),ToJSON(..),object,withObject,(.:),(.=))
import Data.Aeson.Types (Parser)
import qualified Data.IntMap.Strict as IM
import Data.Bits ((.|.),(.&.),shiftL)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Hide.RemoteEndpoint (randomIdentity)

-- | Exact resource identity. Equality never examines pixels or encoded bytes.
data PreparedImage = PreparedImage !Text !Text !Int !Int !BS.ByteString !BS.ByteString
instance Eq PreparedImage where
  a==b=imageResourceId a==imageResourceId b
instance Show PreparedImage where
  show image="PreparedImage "++show (imageResourceId image,imageFormat image,imageWidth image,imageHeight image)
-- | O(1). Fresh opaque ID for this immutable decoded resource.
imageResourceId :: PreparedImage -> Text
imageResourceId (PreparedImage ident _ _ _ _ _)=ident
-- | O(1). Source format, either @PNG@ or @JPEG@; encoded bytes retain that format.
imageFormat :: PreparedImage -> Text
imageFormat (PreparedImage _ format _ _ _ _)=format
-- | O(1). Positive oriented dimensions, each at most 4096; area at most 4 Mi pixels.
imageWidth, imageHeight :: PreparedImage -> Int
imageWidth (PreparedImage _ _ width _ _ _)=width
imageHeight (PreparedImage _ _ _ height _ _)=height
-- | O(1). Strict row-major straight RGBA8; length is exactly width * height * 4.
-- JPEG EXIF orientations 1..8 are normalized here, without changing source bytes.
imageRGBA :: PreparedImage -> BS.ByteString
imageRGBA (PreparedImage _ _ _ _ bytes _)=bytes
-- | O(1). Exact original bounded source bytes, including original metadata.
imageEncoded :: PreparedImage -> BS.ByteString
imageEncoded (PreparedImage _ _ _ _ _ bytes)=bytes

-- | O(1). Recognize a supported source signature independently of its filename.
-- A format label grants no decode/allocation authority; malformed images may match.
imageContentFormat :: BS.ByteString -> Maybe Text
imageContentFormat bytes
  | BS.take 8 bytes==BS.pack [137,80,78,71,13,10,26,10]=Just "PNG"
  | BS.take 3 bytes==BS.pack [255,216,255]=Just "JPEG"
  | otherwise=Nothing

-- | O(1). Supported content signature; full preparation rechecks allocation bounds.
isImageContent :: BS.ByteString -> Bool
isImageContent=maybe False (const True) . imageContentFormat

-- | Decode on a worker after checking source and every possible frame's bounds.
-- PNG uses IHDR. JPEG admits one bounded 8-bit baseline/progressive Huffman frame;
-- dynamic-height DNL and additional frames are refused before decoding. Missing or
-- malformed EXIF orientation defaults to 1; only a bounded IFD0 scalar is read.
--
-- Success retains at most 16 MiB source and 4 Mi pixels. Original bytes are exact;
-- only RGBA is oriented. Decoder failures return Left; cancellation still propagates.
prepareImage :: BS.ByteString -> IO (Either Text PreparedImage)
prepareImage bytes
  | BS.length bytes>16777216=pure (Left "Image exceeds the 16 MiB file limit.")
  | otherwise=case imageContentFormat bytes of
      Just "PNG"->case pngHeader bytes of
        Left err->pure (Left err)
        Right (width,height)->decode "PNG" width height 1 bytes decodePng
      Just "JPEG"->case jpegHeader bytes of
        Left err->pure (Left err)
        Right (width,height,orientation,source)->decode "JPEG" width height orientation source decodeJpeg
      _->pure (Left "Unsupported image signature.")
  where
    decode format width height orientation source decoder=do
      result<-try $ case decoder source of
        Left _->pure (Left ("Cannot decode "<>format<>"."))
        Right decoded->do
          let original=convertRGBA8 decoded
          if P.imageWidth original/=width || P.imageHeight original/=height
            then pure (Left (format<>" decoded dimensions do not match its header.")) else do
              let image=orientImage orientation original
                  (pointer,offset,count)=VS.unsafeToForeignPtr (imageData image)
                  rgba=BSI.fromForeignPtr pointer offset count
                  iw=P.imageWidth image; ih=P.imageHeight image
              if BS.length rgba/=iw*ih*4 then pure (Left "Invalid decoded RGBA image.") else do
                owned<-evaluate (BS.copy bytes)
                _<-evaluate (BS.length rgba)
                ident<-T.pack <$> randomIdentity
                pure (Right (PreparedImage ident format iw ih rgba owned))
      case (result :: Either SomeException (Either Text PreparedImage)) of
        Left err | Just async<-(fromException err :: Maybe SomeAsyncException)->throwIO async
                 | otherwise->pure (Left ("Cannot decode "<>format<>"."))
        Right prepared->pure prepared

boundedDimensions :: Int -> Int -> Either Text ()
boundedDimensions width height=unless
  (width>0 && height>0 && width<=4096 && height<=4096 && toInteger width*toInteger height<=4194304)
  (Left "Image exceeds 4096 pixels per side or the 4-megapixel limit.")

pngHeader :: BS.ByteString -> Either Text (Int,Int)
pngHeader bytes=do
  unless (BS.length bytes>=33 && word 8==13 && BS.take 4 (BS.drop 12 bytes)=="IHDR") (Left "Invalid PNG header.")
  boundedDimensions width height
  pure (width,height)
  where
    word at=BS.foldl' (\n byte->n*256+fromIntegral byte) (0::Integer) (BS.take 4 (BS.drop at bytes))
    width=fromInteger (word 16); height=fromInteger (word 20)

-- Walk all markers, including those between progressive scans. The only decoded
-- header fields are allocation bounds and orientation; JuicyPixels owns the codec.
-- EXIF APP1 is omitted from decoder input: its generic TIFF parser allocates vectors
-- from untrusted tag counts. The original source remains untouched and retained.
jpegHeader :: BS.ByteString -> Either Text (Int,Int,Int,BS.ByteString)
jpegHeader bytes=scan 2 Nothing Nothing False []
  where
    size=BS.length bytes
    invalid=Left "Invalid JPEG marker structure."
    word at | at>=0 && at<=size-2=Right (fromIntegral (BS.index bytes at)*256+fromIntegral (BS.index bytes (at+1)))
            | otherwise=invalid
    marker !at
      | at>=size=invalid
      | BS.index bytes at==255=marker (at+1)
      | otherwise=Right (at,BS.index bytes at)
    scan !at header orientation seenScan omitted=do
      unless (at<size && BS.index bytes at==255) invalid
      (codeAt,code)<-marker at
      case code of
        217->case header of
          Just (width,height) | seenScan->Right (width,height,maybe 1 id orientation,withoutExif (reverse omitted))
          _->invalid
        216->invalid
        220->Left "JPEG dynamic-height DNL frames are unsupported."
        0->invalid
        1->scan (codeAt+1) header orientation seenScan omitted
        _ | code>=208 && code<=215->scan (codeAt+1) header orientation seenScan omitted
          | otherwise->do
            count<-word (codeAt+1)
            unless (count>=2 && count<=size-codeAt-1) invalid
            let payload=codeAt+3
                end=codeAt+1+count
                sof=code>=192 && code<=207 && code `notElem` [196,200,204]
                exif=code==225 && BS.take 6 (BS.drop payload bytes)=="Exif\0\0" && count>=8
            nextHeader<-if not sof then pure header else do
              unless (count>=8) invalid
              height<-word (payload+1)
              width<-word (payload+3)
              boundedDimensions width height
              unless (code==192 || code==194) (Left "Unsupported JPEG frame kind.")
              unless (header==Nothing) (Left "JPEG contains multiple image frames.")
              let components=fromIntegral (BS.index bytes (payload+5))
                  sample i=BS.index bytes (payload+7+3*i)
                  boundedSample i=let value=sample i; x=value `div` 16; y=value .&. 15 in x>=1 && x<=4 && y>=1 && y<=4
              unless (BS.index bytes payload==8 && components>=1 && components<=4 && count>=8+3*components && all boundedSample [0..components-1])
                (Left "Unsupported JPEG component allocation bounds.")
              pure (Just (width,height))
            let nextOrientation=case orientation of
                  Just value->Just value
                  Nothing | exif->exifOrientation (BS.take (count-2) (BS.drop payload bytes))
                          | otherwise->Nothing
                nextOmitted=if exif then (at,end):omitted else omitted
            if code/=218 then scan end nextHeader nextOrientation seenScan nextOmitted else do
              unless (nextHeader/=Nothing) invalid
              next<-entropyEnd end
              scan next nextHeader nextOrientation True nextOmitted
    entropyEnd !at=case BS.elemIndex 255 (BS.drop at bytes) of
      Nothing->invalid
      Just offset->do
        let start=at+offset
        (codeAt,code)<-marker start
        if code==0 || code>=208 && code<=215 then entropyEnd (codeAt+1) else pure start
    withoutExif []=bytes
    withoutExif ranges=BS.concat (pieces 0 ranges)
      where pieces at []=[BS.drop at bytes]
            pieces at ((first,lastOffset):rest)=BS.take (first-at) (BS.drop at bytes):pieces lastOffset rest

-- Read only orientation's inline SHORT from TIFF IFD0. Every offset/table fits
-- the one <=64 KiB APP1 segment; no next/sub-IFD or variable-count value is followed.
exifOrientation :: BS.ByteString -> Maybe Int
exifOrientation payload=do
  guard (BS.take 6 payload=="Exif\0\0")
  let bytes=BS.drop 6 payload
      size=BS.length bytes
  little<-case BS.take 2 bytes of "II"->Just True; "MM"->Just False; _->Nothing
  let word count at=do
        guard (at>=0 && count<=size-at)
        let part=BS.take count (BS.drop at bytes)
        pure (BS.foldl' (\n byte->n*256+fromIntegral byte) (0::Integer) (if little then BS.reverse part else part))
  magic<-word 2 2
  guard (magic==42)
  start<-word 4 4
  guard (start>=8 && start<=toInteger (size-6))
  let first=fromInteger start
  count<-word 2 first
  guard (count<=toInteger ((size-first-6) `div` 12))
  let find !index
        | index>=fromInteger count=Nothing
        | otherwise=do
            let at=first+2+12*index
            tag<-word 2 at
            if tag/=274 then find (index+1) else do
              kind<-word 2 (at+2)
              entries<-word 4 (at+4)
              guard (kind==3 && entries==1)
              value<-word 2 (at+8)
              guard (value>=1 && value<=8)
              pure (fromInteger value)
  find 0

orientImage :: Int -> P.Image P.PixelRGBA8 -> P.Image P.PixelRGBA8
orientImage orientation source
  | orientation==1=source
  | otherwise=P.generateImage pixel width height
  where
    w=P.imageWidth source; h=P.imageHeight source
    swapped=orientation>=5 && orientation<=8
    width=if swapped then h else w; height=if swapped then w else h
    pixel x y=uncurry (P.pixelAt source) $ case orientation of
      2->(w-1-x,y)
      3->(w-1-x,h-1-y)
      4->(x,h-1-y)
      5->(y,x)
      6->(y,h-1-x)
      7->(w-1-y,h-1-x)
      8->(w-1-y,x)
      _->(x,y)

-- | Closed image-view operations. They grant no file, source-editor or shell access.
data ImageAction = FitImage | ActualImageSize | ZoomImageIn | ZoomImageOut deriving (Eq,Show)

-- | An exact displayed image instance and one visible cell. This is a stale-action
-- receipt, not authority. The host checks current geometry, visibility and origin.
data ImageControl = ImageControl
  { controlWindow :: !Int, controlView :: !Text, controlResource :: !Text
  , controlAnchor :: !(Int,Int)
  } deriving (Eq,Show)

instance ToJSON ImageControl where
  toJSON target=object ["id" .= controlWindow target,"view" .= controlView target,
    "resource" .= controlResource target,"anchor" .= controlAnchor target]
instance FromJSON ImageControl where
  parseJSON=withObject "image control" $ \o->do
    ident<-o .: "id"; view<-o .: "view"; resource<-o .: "resource"; anchor@(x,y)<-o .: "anchor"
    unless (ident>0 && ident<=2147483647 && not (T.null view) && T.length view<=20 &&
      T.all (\c->c>='0' && c<='9') view && T.head view/='0' &&
      T.length resource==48 && T.all (`elem` ("0123456789abcdef"::String)) resource &&
      x>=0 && x<512 && y>=0 && y<256) (fail "Invalid image control target")
    pure (ImageControl ident view resource anchor)

-- | The same bounded packet travels through browser, native and SSH input.
imageActionPacket :: ImageControl -> ImageAction -> Value
imageActionPacket target action=object ["type" .= ("canvas-action"::Text),"target" .= target,"action" .= name]
  where name=case action of FitImage->"fit"; ActualImageSize->"actual-size"; ZoomImageIn->"zoom-in"; ZoomImageOut->"zoom-out" :: Text

-- | Decode only the four viewport actions. No generic command dispatch is exposed.
parseImageActionPacket :: Value -> Parser (ImageControl,ImageAction)
parseImageActionPacket=withObject "image action" $ \o->do
  kind<-o .: "type"
  unless (kind==("canvas-action"::Text)) (fail "Invalid image action type")
  target<-o .: "target"
  name<-o .: "action"
  action<-case (name::Text) of
    "fit"->pure FitImage; "actual-size"->pure ActualImageSize
    "zoom-in"->pure ZoomImageIn; "zoom-out"->pure ZoomImageOut
    _->fail "Unknown image action"
  pure (target,action)

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
  , canvasViewIdentity :: !Text, canvasInteractive :: !Bool
  } deriving (Eq,Show)
-- | Cells and this scene must come from the same composition. Mask bytes are
-- little-endian uint16 per cell: low 15 bits slot, high bit half-intensity shadow.
data CanvasScene = CanvasScene {canvasSurfaces :: ![CanvasSurface], canvasMask :: !BS.ByteString} deriving (Eq,Show)

-- | /O(visible grid cells)/, at most 64 retained anchors. The same ownership mask
-- drives paint and controls; covered surfaces have no action target.
canvasVisibleCells :: CanvasScene -> Int -> IM.IntMap (Int,Int)
canvasVisibleCells scene columns
  | columns<=0=IM.empty
  | otherwise=go 0 IM.empty
  where
    count=BS.length (canvasMask scene) `div` 2
    go !index !found
      | index>=count=found
      | otherwise=let slot=canvasOwnerAt scene index .&. 32767
        in go (index+1) (if slot==0 || IM.member slot found then found else IM.insert slot (index `mod` columns,index `div` columns) found)

-- | Constant-size action receipt for a visible, currently interactive surface.
canvasControls :: IM.IntMap (Int,Int) -> CanvasSurface -> Maybe ImageControl
canvasControls visible surface=do
  guard (canvasInteractive surface)
  anchor<-IM.lookup (canvasSlot surface) visible
  pure (ImageControl (canvasId surface) (canvasViewIdentity surface) (imageResourceId (canvasImage surface)) anchor)

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
canvasPixel scene columns=sample
  where
    -- Prepare the bounded slot table once when this sampler is captured, not
    -- once per output pixel. Scene constructors need not order their surfaces.
    bySlot=V.accum (\_ surface->Just surface) (V.replicate 65 Nothing)
      [(canvasSlot surface,surface) | surface<-canvasSurfaces scene,canvasSlot surface>=1,canvasSlot surface<=64]
    sample x y
      | columns<=0 || isNaN x || isInfinite x || isNaN y || isInfinite y || x<0 || x>=fromIntegral columns || y<0=Nothing
      | otherwise=do
          let owner=canvasOwnerAt scene (floor y*columns+floor x)
              slot=owner .&. 32767
          surface<-if slot<=64 then bySlot V.! slot else Nothing
          let (left,top,width,height)=canvasTarget surface
              image=canvasImage surface
              ix=max 0 (min (imageWidth image-1) (floor ((x-left)*fromIntegral (imageWidth image)/width)))
              iy=max 0 (min (imageHeight image-1) (floor ((y-top)*fromIntegral (imageHeight image)/height)))
              inside=width>0 && height>0 && x>=left && y>=top && x<left+width && y<top+height
              bytes=imageRGBA image
              at=4*(iy*imageWidth image+ix)
              channel offset=fromIntegral (BS.index bytes (at+offset))::Int
              dim=if owner .&. 32768/=0 then 2 else 1
              paint offset=channel offset*channel 3 `div` (255*dim)
          pure (if inside then (paint 0,paint 1,paint 2) else (0,0,0))
