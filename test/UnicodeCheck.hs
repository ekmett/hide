{-# LANGUAGE OverloadedStrings #-}
module UnicodeCheck (checks) where
import Control.Monad (unless, forM_)
import Blaze.ByteString.Builder (writeToByteString)
import Blaze.ByteString.Builder.ByteString (writeByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import Data.Foldable (toList)
import qualified Graphics.Vty as V
import Graphics.Vty.Span (SpanOp(..))
import THC.Edit.Buffer
import THC.Edit.Model
import THC.Edit.Unicode

checks :: IO ()
checks = do
  let check name ok=unless ok (error name)
      clusters=["👩🏽\x200d\&💻","👨\x200d\&👩\x200d\&👧\x200d\&👦","🏳️\x200d\&🌈","🇯🇵","❤️","1️⃣","e\x301","क्\x200d\&ष"]
      wide=take 6 clusters
  forM_ clusters $ \g -> do
    check "platform segments a complete grapheme" (graphemes g==[g])
    check "cursor crosses a complete grapheme" (nextCharacter (g<>"x") 0==T.length g && previousCharacter ("x"<>g) (1+T.length g)==1)
    let base=addDocument Nothing (newBuffer (g<>"x")) (initialDesktop (80,25))
        deleted=fst (handleEvent (V.EvKey V.KDel []) base)
    check "paste preserves complete emoji sequences" (activeText (fst (handleEvent (V.EvPaste (TE.encodeUtf8 g)) (addDocument Nothing (newBuffer "") (initialDesktop (80,25)))))==g)
    check "Delete removes one cluster, undo restores bytes" (activeText deleted=="x" && activeText (fst (runCommand Undo deleted))==g<>"x")
  forM_ wide $ \g -> check "emoji occupies two cells with no internal mouse offsets"
    (displayColumn (g<>"x") (T.length g+1)==3 && columnOffset (g<>"x") 1==0 && columnOffset (g<>"x") 2==T.length g)
  check "combining and tabs use grapheme columns" (displayColumn "e\x301\t🇯🇵" 6==10)
  let plain pic size=T.concat [TL.toStrict t | row<-toList (displayOpsForPic (flattenPicture size pic) size),TextSpan{textSpanText=t}<-toList row]
      image=textImage V.defAttr "A👩🏽\x200d\&💻B"
      crop n=plain (V.picForImage (V.cropRight n image)) (n,1)
  check "wide clusters are blanked at the right edge" (crop 2=="A " && crop 3=="A👩🏽\x200d\&💻")
  check "wide clusters are blanked at the left edge" (plain (V.picForImage (V.translateX (-2) image)) (2,1)==" B")
  check "a window covering half a glyph blanks the exposed half"
    (plain (V.picForLayers [V.translateX 2 (textImage V.defAttr "│"),image]) (4,1)=="A │B")
  let settings=fst (runCommand EditorOptions (initialDesktop (80,25)) {videoMode=Just 3})
      checked=settings {dialog=fmap (\d -> d {fields=[CheckBox "Pixelate Unicode" True]}) (dialog settings)}
  check "preferences apply Unicode pixelation" (pixelateUnicode (fst (handleEvent (V.EvKey V.KEnter []) checked)))
  let position x=writeByteString (BS.pack ("<"++show x++">"))
      (output,end)=terminalText position 3 "A🇯🇵B"
  check "terminal reserves two cells and corrects its cursor after wide glyphs"
    (writeToByteString output==TE.encodeUtf8 "A  <4>🇯🇵<6>B" && end==7)
  putStrLn "Unicode grapheme/layout checks passed"
