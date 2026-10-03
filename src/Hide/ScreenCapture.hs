{-# LANGUAGE OverloadedStrings #-}
-- | Logical framebuffer capture for agent tools, not an OS screenshot.
--
-- Text, PNG and cell permissions derive from one grapheme-aware picture. A cluster
-- is redacted wholly if any occupied cell is unreadable, preserving coordinates.
-- The bitmap PNG approximates complex graphemes and uses a fixed cursor phase;
-- it does not include OS chrome, native shaping or the CRT shader.
module Hide.ScreenCapture (capture, screenTool, redactCluster) where

import Codec.Picture (PixelRGB8(..), encodePng, generateImage)
import Data.Aeson
import Data.Bits (testBit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.List (mapAccumL, groupBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Graphics.Vty.Span (SpanOp(..))
import Hide.Font
import Hide.Frontend (modeHeight)
import Hide.Model (Desktop(..), MenuItem(..), menus, commandEnabled)
import Hide.GuestAccess (CellAccess(..), cellAccess, guestKeyboardAllowed, beginGuestInput, guestKeyCombinations)
import qualified Hide.Protocol as P
import Hide.Render (renderDesktop)
import Hide.Unicode (clusterWidth, displayOpsForPic, graphemes)

screenTool :: Value
screenTool=object
  ["name" .= ("editor_screen"::Text),
   "description" .= ("Capture the guest-readable editor frame as colorless text and optional PNG. Private cells are redacted in both outputs. Includes independent readable/clickable cell masks, key combinations and command permissions; excludes OS chrome, CRT effects and native Unicode shaping."::Text),
   "inputSchema" .= object ["type" .= ("object"::Text),"properties" .= object
     ["image" .= object ["type" .= ("boolean"::Text),"default" .= False]],"additionalProperties" .= False],
   "annotations" .= object ["readOnlyHint" .= True,"destructiveHint" .= False,"openWorldHint" .= False]]

-- | Return bounded text/permission metadata and optionally a bitmap PNG.
-- Apply agent read masks before producing either representation.
capture :: Font -> Desktop -> Bool -> IO (Either Text Value)
capture font desktop includeImage
  | cols<=0 || rows<=0 || toInteger cols*toInteger rows>32768 = pure (Left "Editor screen exceeds the 32768-cell capture limit.")
  | includeImage && toInteger cols*8*toInteger rows*toInteger cellHeight>4194304 = pure (Left "Editor image exceeds the 4-megapixel capture limit.")
  | BS.length (TE.encodeUtf8 plain)>1048576 = pure (Left "Editor screen text exceeds 1 MiB.")
  | includeImage && BL.length png>2097152 = pure (Left "Editor PNG exceeds 2 MiB; request text only or reduce the editor size.")
  | BL.length (encode result)>4194304 = pure (Left "Editor screen result exceeds 4 MiB.")
  | otherwise = pure (Right result)
  where
    (cols,rows)=screenSize desktop
    cellHeight=modeHeight (fromMaybe 3 (videoMode desktop))
    picture=renderDesktop desktop
    spans=map toList (toList (displayOpsForPic picture (cols,rows)))
    maskedRows=[maskRow y line | (y,line)<-zip [0..] spans]
    maskRow y line=snd (mapAccumL (maskCluster y) 0 padded)
      where
        chunks=concatMap spanClusters line
        occupied=sum [clusterWidth text | (text,_)<-chunks]
        padded=chunks++replicate (max 0 (cols-occupied)) (" ",V.defAttr)
    spanClusters TextSpan{textSpanAttr=attr,textSpanText=t}=[(text,attr) | text<-graphemes (TL.toStrict t)]
    spanClusters (Skip n)=replicate n (" ",V.defAttr)
    spanClusters (RowEnd n)=replicate n (" ",V.defAttr)
    maskCluster y x (text,attr)=
      let width=clusterWidth text
          access=[cellAccess desktop column y | column<-[x..x+width-1]]
          (shown,safeAccess)=redactCluster text access
          readable=all cellReadable safeAccess
          shownAttr=if readable then attr else V.defAttr `V.withForeColor` V.RGBColor 0 0 0 `V.withBackColor` V.RGBColor 0 0 0
      in (x+width,(shown,shownAttr,safeAccess))
    plain=T.unlines [T.concat [text | (text,_,_)<-line] | line<-maskedRows]
    accessGrid=Vec.fromList (concatMap (concatMap (\(_,_,access)->access)) maskedRows)
    readableCell x y=cellReadable (accessGrid Vec.! (y*cols+x))
    accessRows=[object ["y" .= y,"runs" .= runs (concatMap (\(_,_,access)->access) line)] | (y,line)<-zip [0::Int ..] maskedRows]
    runs entries=snd (mapAccumL run 0 (groupBy same entries))
      where
        same a b=cellReadable a==cellReadable b && cellClickable a==cellClickable b
        run x group=let n=length group
                        access=case group of a:_->a; _->CellAccess False False
                    in (x+n,object ["x" .= x,"length" .= n,"readable" .= cellReadable access,"clickable" .= cellClickable access])
    cursor=case V.picCursor picture of
      V.Cursor x y | x>=0 && x<cols && y>=0 && y<rows && readableCell x y -> Just (x,y)
      _ -> Nothing
    inputAllowed input=case P.applyGuestInput input (beginGuestInput desktop) of
      Left _->False
      Right _->True
    modifierName modifier=case modifier of
      V.MCtrl->"ctrl"::Text
      V.MAlt->"alt"
      V.MShift->"shift"
      _->T.pack (show modifier)
    metadata=object ["cols" .= cols,"rows" .= rows,"text" .= plain,
      "cursor" .= fmap (\(x,y) -> object ["x" .= x,"y" .= y]) cursor,
      "cursorPolicy" .= ("visible underline; blink phase fixed on; hidden in private cells"::Text),
      "accessRows" .= accessRows,"keyboardAllowed" .= guestKeyboardAllowed desktop,
      "keyPermissions" .= [object ["key" .= key,"mods" .= map modifierName modifiers,
        "allowed" .= inputAllowed (P.Key key modifiers)] | (key,modifiers)<-guestKeyCombinations],
      "commandPermissions" .= [object ["menu" .= menuName,"label" .= label,"command" .= show command,
        "allowed" .= inputAllowed (P.MenuCommand (Just command)),"enabled" .= commandEnabled desktop command]
        | (menuName,_,items)<-menus,MenuItem label _ command<-items],
      "redactedCells" .= Vec.length (Vec.filter (not . cellReadable) accessGrid),
      "redaction" .= ("Unreadable graphemes become spaces and solid black pixels; whole wide graphemes are hidden if any covered cell is private. Cell coordinates are preserved."::Text),
      "cellWidth" .= (8::Int),"cellHeight" .= cellHeight,"pixelWidth" .= (cols*8),"pixelHeight" .= (rows*cellHeight),
      "videoMode" .= fromMaybe 3 (videoMode desktop),"imageIncluded" .= includeImage,
      "renderer" .= ("canonical IBM/Unicode bitmap approximation; multi-codepoint graphemes use their first code point"::Text),
      "excluded" .= (["OS chrome","CRT effects","native font shaping","mouse pointer"]::[Text])]
    result=object ["isError" .= False,"content" .=
      (object ["type" .= ("text"::Text),"text" .= TE.decodeUtf8 (BL.toStrict (encode metadata))] :
        [object ["type" .= ("image"::Text),"mimeType" .= ("image/png"::Text),
          "data" .= TE.decodeUtf8 (B64.encode (BL.toStrict png))] | includeImage])]
    blank=(glyph font ' ',PixelRGB8 0 0 0,PixelRGB8 0 0 0,0)
    cells=Vec.fromList (concatMap (take cols . (++repeat blank) . concatMap (\(text,attr,_)->concatMap (cluster attr) (graphemes text))) maskedRows)
    cluster attr text=case T.uncons text of
      Nothing -> []
      Just (c,_) -> let tile=glyph font c
                   in [(tile,color (V.attrForeColor attr),color (V.attrBackColor attr),offset*8) | offset<-[0..clusterWidth text-1]]
    png=encodePng (generateImage pixel (cols*8) (rows*cellHeight))
    pixel x y=let (tile,fg,bg,offset)=cells Vec.! ((y `div` cellHeight)*cols+x `div` 8)
                  gx=offset+x `mod` 8
                  gy=(y `mod` cellHeight)*16 `div` cellHeight
                  ink=gx<glyphWidth tile && gx<16 && testBit (glyphRows tile !! gy) (15-gx)
                  base=if ink then fg else bg
              in if cursor==Just (x `div` 8,y `div` cellHeight) && y `mod` cellHeight>=cellHeight*14 `div` 16
                   then invert base else base
    invert (PixelRGB8 r g b)=PixelRGB8 (255-r) (255-g) (255-b)

-- | Blank a whole grapheme if any covered cell is unreadable; preserve position
-- and keep clickability independent from readability.
redactCluster :: Text -> [CellAccess] -> (Text,[CellAccess])
redactCluster text access
  | all cellReadable access=(text,access)
  | otherwise=(T.replicate (clusterWidth text) " ",[entry {cellReadable=False} | entry<-access])

color :: V.MaybeDefault V.Color -> PixelRGB8
color (V.SetTo (V.RGBColor r g b))=PixelRGB8 r g b
color (V.SetTo (V.ISOColor n))=palette !! (fromIntegral n `mod` 16)
  where palette=[PixelRGB8 0 0 0,PixelRGB8 170 0 0,PixelRGB8 0 170 0,PixelRGB8 170 85 0,
          PixelRGB8 0 0 170,PixelRGB8 170 0 170,PixelRGB8 0 170 170,PixelRGB8 170 170 170,
          PixelRGB8 85 85 85,PixelRGB8 255 85 85,PixelRGB8 85 255 85,PixelRGB8 255 255 85,
          PixelRGB8 85 85 255,PixelRGB8 255 85 255,PixelRGB8 85 255 255,PixelRGB8 255 255 255]
color _=PixelRGB8 0 0 0
