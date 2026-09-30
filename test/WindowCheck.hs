{-# LANGUAGE OverloadedStrings #-}
module WindowCheck (checks) where
import Control.Monad (unless)
import THC.Edit.Frontend
import qualified Graphics.Vty as V
checks :: IO ()
checks = do
  let check name ok = unless ok (error name)
  check "terminal remains default" (chooseBackend Nothing [] == Right Terminal)
  check "environment selects metal" (chooseBackend (Just "metal") [] == Right Metal)
  check "explicit terminal beats environment" (chooseBackend (Just "vulkan") [Terminal] == Right Terminal)
  check "explicit backend overrides invalid environment" (chooseBackend (Just "typo") [Metal] == Right Metal)
  check "invalid default rejected" (case chooseBackend (Just "typo") [] of Left _ -> True; _ -> False)
  check "conflicting backend flags rejected" (case chooseBackend Nothing [Metal,Vulkan] of Left _ -> True; _ -> False)
  check "shift-tab" (decodeKey (-9) 1 == Just (V.EvKey V.KBackTab [V.MShift]))
  check "command maps to control" (decodeKey 115 8 == Just (V.EvKey (V.KChar 's') [V.MCtrl]))
  check "unknown key ignored" (decodeKey (-999) 0 == Nothing)
  putStrLn "window input checks passed"
