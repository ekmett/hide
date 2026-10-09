module Main (main) where
import qualified Hide.App
import qualified Hide.AgentUI
main :: IO ()
main = Hide.App.main [Hide.AgentUI.plugin]
