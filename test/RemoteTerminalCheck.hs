{-# LANGUAGE OverloadedStrings #-}
module RemoteTerminalCheck (checks) where
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import Hide.RemoteTerminal
import Hide.RemoteWindow (parseRemoteFrame)
import qualified Hide.Protocol as P

checks :: IO ()
checks = do
  let check name good=unless good (error name)
      parsed event=terminalEventInput event >>= either (const Nothing) Just . parseEither P.parseInput
  check "terminal shift tab uses shared key protocol" (parsed (V.EvKey V.KBackTab [])==Just (P.Key "Tab" [V.MShift]))
  check "terminal Ctrl+C stays remote editor input" (parsed (V.EvKey (V.KChar 'c') [V.MCtrl])==Just (P.Key "c" [V.MCtrl]))
  check "terminal bracketed Unicode paste stays one input" (parsed (V.EvPaste (TE.encodeUtf8 "λ\n界"))==Just (P.Paste "λ\n界"))
  check "terminal invalid UTF8 paste rejected" (terminalEventInput (V.EvPaste (BS.pack [255]))==Nothing)
  check "terminal paste size is bounded" (terminalEventInput (V.EvPaste (BS.replicate 1048577 97))==Nothing)
  check "terminal resize clamps protocol bounds" (parsed (V.EvResize 9999 1)==Just (P.Resize 512 12))
  check "terminal mouse coordinates and modifiers preserved" (parsed (V.EvMouseDown 8 3 V.BRight [V.MShift])==Just (P.Mouse "down" 8 3 2 1 [V.MShift]))
  check "terminal wheel preserves direction" (parsed (V.EvMouseDown 4 5 V.BScrollUp [])==Just (P.Mouse "wheel-up" 4 5 0 1 []))
  check "terminal focus loss releases remote modifiers" (parsed V.EvLostFocus==Just P.Blur)
  check "terminal OSC52 uses UTF8 base64" (terminalClipboard "λ"=="\ESC]52;c;zrs=\BEL")
  check "terminal OSC52 handles base64 padding" (map terminalClipboard ["","f","fo","foo","foobar"]==map (\s -> "\ESC]52;c;"<>s<>"\BEL") ["","Zg==","Zm8=","Zm9v","Zm9vYmFy"])
  let metadata=object ["size" .= ([40,12]::[Int]),"bindings" .= ([]::[(T.Text,T.Text)]),"cursor" .= ([5,0]::[Int])]
      row=toJSON [(2::Int,0x123456::Int,0xffffff::Int,[String "a",toJSON ("界"::T.Text,2::Int),toJSON ("é"::T.Text,1::Int)])]
  frame <- either error pure (parseRemoteFrame metadata (row:replicate 11 (toJSON ([]::[Value]))))
  let picture=remoteTerminalPicture frame
  check "terminal picture preserves frame dimensions with sparse Unicode cells" (case V.picLayers picture of [image] -> V.imageWidth image==40 && V.imageHeight image==12; _ -> False)
  check "terminal picture preserves remote cursor" (V.picCursor picture==V.Cursor 5 0)
