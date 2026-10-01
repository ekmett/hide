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
import THC.Edit.Buffer (newBuffer)
import THC.Edit.Font (loadFont)
import THC.Edit.Model
import THC.Edit.Render (snapshot)
import THC.Edit.ScreenCapture
import THC.Edit.Unicode (clusterWidth, graphemes)

checks :: IO ()
checks=do
  font<-loadFont
  let desktop=addDocument Nothing (newBuffer "  λ 中 ▙ é\n") (initialDesktop (80,25))
      takeCapture d image=capture font d image >>= either (error . T.unpack) pure
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
