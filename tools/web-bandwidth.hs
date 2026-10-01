{-# LANGUAGE OverloadedStrings #-}
-- Run with cabal exec -- runghc -package=thc-edit tools/web-bandwidth.hs
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.IO as T
import Data.List (mapAccumL)
import qualified Graphics.Vty as V
import THC.Edit.Model
import THC.Edit.Buffer
import THC.Edit.Web
import THC.Edit.Render (renderDesktop)

main :: IO ()
main = do
  source <- T.readFile "src/THC/Edit/Model.hs"
  mapM_ (trial source) [(80,25),(128,50),(240,80)]
  where
    trial source size = do
      let start=addDocument Nothing (newBuffer source) (initialDesktop size) {videoMode=Just 3,browserFrontend=True}
          key k d=fst (handleEvent (V.EvKey k []) d)
          steps=[("typing",key (V.KChar c)) | c<-"-- bandwidth trial: λ é 界\n"] ++
                replicate 12 ("cursor",key V.KRight) ++
                [("scroll",modifyActive (\w -> w {scrollRow=n})) | n<-[1..40]] ++
                [("menu",\d -> d {menu=if even n then Just (n `mod` 10,0) else Nothing}) | n<-[0..11]] ++
                [("scroll back",modifyActive (\w -> w {scrollRow=n})) | n<-[39,38..0]]
          snapshots=("initial",start):snd (mapAccumL (\d (label,f) -> let next=f d in (next,(label,next))) start steps)
      mapM_ (\(label,d) -> BL.putStrLn (encode (object ["screen" .= size,"phase" .= (label::String),"frame" .= object
        ["type" .= ("frame"::String),"reset" .= (label=="initial"),"rows" .= zip [0::Int ..] (frameRows d),
         "size" .= size,"mode" .= (3::Int),"dirty" .= webDirty d,"cursor" .= cursor d,
         "blink" .= True,"crt" .= True,"pixelated" .= False,"selection" .= (""::String),"wordstar" .= False]]))) snapshots
    cursor d = case V.picCursor (renderDesktop d) of V.Cursor x y -> Just (x,y); _ -> Nothing
