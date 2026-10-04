{-# LANGUAGE CPP, OverloadedStrings #-}
module RemoteWindowCheck (checks) where
import Control.Monad (unless)
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Hide.RemoteWindow
#ifdef WITH_REMOTE
import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Types (parseEither)
import qualified Hide.Protocol as P
import Hide.Buffer (newBuffer)
import Hide.Model (initialDesktop, addDocument, Desktop(..))
#endif

checks :: IO ()
checks = do
  let check name good = unless good (error name)
      meta = object ["size" .= ([80,25]::[Int]),"mode" .= (3::Int)]
      row = toJSON [(0::Int,0xffffff::Int,0::Int,[String "abc",toJSON ("界"::T.Text,2::Int)])]
      rows = row : replicate 24 (toJSON ([]::[Value]))
      valid = either (const False) (const True) . parseRemoteFrame meta
  check "drag and wheel updates wait for the new frame instead of repainting stale content"
    (not (nativeRepaint [3,10,4,0,0,1]) && not (nativeRepaint [9,10,4,-1,0]))
  check "press and release repaint the local pointer visibility"
    (nativeRepaint [3,10,4,1,0,1] && nativeRepaint [4,10,4])
  check "remote Unicode rows validate" (valid rows)
  let menuMeta fields=object (["size" .= ([80,25]::[Int]) ]++fields)
      states metadata=either (const []) remoteMenus (parseRemoteFrame metadata rows)
  check "menus are disabled without named metadata" (not (or (states (menuMeta []))))
  check "menu state requires advertised command capability" (not (or (states (menuMeta ["menuState" .= [("hide.file.new"::T.Text,True)]]))))
  check "menu enable state maps by command name across reordered layouts" (take 1 (states (menuMeta ["menuCommands" .= (["hide.app.quit","hide.file.new"]::[T.Text]),"menuState" .= [("hide.app.quit"::T.Text,False),("hide.file.new",True)]]))==[True])
  let remoteMenu metadata index= either (const Nothing) (\frame -> remoteMenuInput frame index) (parseRemoteFrame metadata rows)
      enabledMenu=menuMeta ["menuCommands" .= (["hide.file.new"]::[T.Text]),"menuState" .= [("hide.file.new"::T.Text,True)]]
  check "menu invocation uses its public identity" (remoteMenu enabledMenu 0==Just (object ["type" .= ("menu"::T.Text),"command" .= ("hide.file.new"::T.Text)]))
  check "invalid menu positions cannot alias the first command" (remoteMenu enabledMenu (-1)==Nothing && remoteMenu enabledMenu 10000==Nothing)
  check "disabled menu actions cannot be invoked" (remoteMenu (menuMeta ["menuCommands" .= (["hide.file.new"]::[T.Text]),"menuState" .= [("hide.file.new"::T.Text,False)]]) 0==Nothing)
  check "native menus send names rather than positions" (nativeEventInput [11,0]==Just (object ["type" .= ("menu"::T.Text),"command" .= ("hide.file.new"::T.Text)]))
  check "remote rows must match height" (not (valid (take 24 rows)))
  check "remote span overflow rejected" (not (valid (toJSON [(79::Int,0::Int,0::Int,[String "ab"])] : drop 1 rows)))
  check "remote colors bounded" (not (valid (toJSON [(0::Int,-1::Int,0::Int,[String "a"])] : drop 1 rows)))
  check "remote clusters cannot contain NUL" (not (valid (toJSON [(0::Int,0::Int,0::Int,[toJSON ("a\0"::T.Text,1::Int)])] : drop 1 rows)))
  check "remote invalid dimensions rejected" (either (const True) (const False) (parseRemoteFrame (object ["size" .= ([999999,25]::[Int])]) rows))
  check "Control bracket detaches locally" (remoteDetachShortcut [1,fromEnum ']',2] && nativeEventInput [1,fromEnum ']',2]==Nothing)
  check "other bracket shortcuts remain editor input" (all (not . remoteDetachShortcut . (\mods -> [1,fromEnum ']',mods])) [0,1,3,6,8])
  check "native close requests checked remote quit" (nativeEventInput [6] == Just (object ["type" .= ("command"::T.Text),"command" .= ("quit"::T.Text)]))
  check "offline closes detach without queuing remote quit" (remoteCloseDetaches False [6] && not (remoteCloseDetaches True [6]))
  check "offline user input is ignored" (all (not . remoteInputAllowed False) [[1,97,0],[2],[3,1,1,1,0,1],[11,0],[14]])
  check "offline zoom remains local" (remoteInputAllowed False [1,fromEnum '+',2] && remoteInputAllowed True [1,97,0])
  check "native blur releases remote state" (nativeEventInput [7] == Just (object ["type" .= ("blur"::T.Text)]))
  check "remote key mapping preserves shift tab" (nativeKeyInput (-9) 1 == Just (object ["type" .= ("key"::T.Text),"key" .= ("Tab"::T.Text),"mods" .= (["shift"]::[T.Text])]))
  check "terminal control v stays terminal input" (not (pasteShortcut True False (fromEnum 'v') 2) && pasteShortcut True False (fromEnum 'v') 3 && pasteShortcut True True (fromEnum 'v') 3)
  check "WordStar control v stays editor input" (not (pasteShortcut False True (fromEnum 'v') 2) && pasteShortcut False True (fromEnum 'v') 8)
  check "download names stay single safe components" (sanitizeDownloadName "../../secret" == "secret" && sanitizeDownloadName "..\\..\\secret" == "secret" && sanitizeDownloadName ".." == "download" && not (T.any (<' ') (sanitizeDownloadName "bad\0name")))

  check "download filenames respect UTF8 filesystem limits" (BS.length (TE.encodeUtf8 (sanitizeDownloadName (T.replicate 180 "界")))<=180)
#ifdef WITH_REMOTE
  check "coalesced native wheel travel survives the wire" (case nativeEventInput [9,10,4,-125,0] of
    Just value -> parseEither P.parseInput value==Right (P.Wheel 10 4 (-125) [])
    _ -> False)
  let desktop = (addDocument Nothing (newBuffer "λ界 é\n🐈") (initialDesktop (80,25))) {videoMode=Just 3,browserFrontend=True}
      actualRows = P.frameRows desktop
  (decoded,reconstructed) <- P.decodeFrame [] (BL.toStrict (P.framePacket True [] actualRows (P.frameMetadata "/remote/project" desktop)))
  check "compressed editor Unicode frame validates for native rendering" (either (const False) ((==(80,25)).remoteSize) (parseRemoteFrame decoded reconstructed))
  check "native events use shared protocol" (all (maybe False (either (const False) (const True) . parseEither P.parseInput) . nativeEventInput)
    [[1,-9,1],[3,10,4,2,2,1],[3,11,4,0,1,1],[4,11,4],[5,80,25],[6],[7],[9,10,4,-1,4],[11,0],[12,-1,-1],[13,15]])
#endif
