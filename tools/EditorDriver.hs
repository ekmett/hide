{-# LANGUAGE OverloadedStrings #-}
-- Trusted local automation over the real editor model, effects and Metal renderer.
-- Agent-facing control continues to use ControlMCP and its input restrictions.
module EditorDriver (command, input, typeText, await, captureMetal) where

import Control.Concurrent (threadDelay)
import Control.Monad (foldM)
import qualified Data.Text.IO as TIO
import qualified Graphics.Vty as V
import System.Environment (setEnv)
import System.FilePath (replaceExtension)
import System.Process (callProcess)
import System.Timeout (timeout)
import THC.Edit.Frontend (Backend(Metal))
import THC.Edit.Model
import THC.Edit.Render (snapshot)
import THC.Edit.Window (runWindow)

type Effects = Desktop -> [Effect] -> IO (Bool,Desktop)

command :: Effects -> Command -> Desktop -> IO Desktop
command effects cmd d = let (next,pending)=runCommand cmd d in snd <$> effects next pending

-- Unlike a pure model transition, input also performs the effects it requests.
input :: Effects -> V.Event -> Desktop -> IO Desktop
input effects event d = let (next,pending)=handleEvent event d in snd <$> effects next pending

typeText :: Effects -> String -> Desktop -> IO Desktop
typeText effects text d = foldM (\state c -> input effects (V.EvKey (V.KChar c) []) state) d text

-- Wait on an observable condition, pumping the caller's real runtime events.
await :: String -> (Desktop -> IO Desktop) -> (Desktop -> Bool) -> Desktop -> IO Desktop
await label tick predicate d = do
  result <- timeout 180000000 (loop d)
  maybe (fail ("Timed out waiting for " ++ label)) pure result
  where loop current = do
          next <- tick current
          if predicate next then pure next else threadDelay 20000 >> loop next

-- Pixel rectangles use the native capture resolution. Nothing captures the desktop.
-- Keep a colorless text companion for assertions and low-bandwidth inspection.
captureMetal :: Effects -> Double -> FilePath -> FilePath -> Maybe Rect -> Desktop -> IO ()
captureMetal effects scale bmp png crop shown = do
  TIO.writeFile (replaceExtension bmp "txt") (snapshot shown)
  setEnv "THC_EDIT_CAPTURE_EXIT" "1"
  setEnv "THC_EDIT_CAPTURE" bmp
  runWindow Metal scale effects pure shown
  let cropArgs = case crop of
        Nothing -> []
        Just (Rect x y w h) -> ["--cropOffset",show y,show x,"--cropToHeightWidth",show h,show w]
  callProcess "sips" (["-s","format","png"] ++ cropArgs ++ [bmp,"--out",png])
