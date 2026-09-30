{-# LANGUAGE OverloadedStrings #-}
module BrowserCheck (checks) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (hClose, openBinaryTempFile)
import Data.Maybe (isJust)
import qualified Graphics.Vty as V
import qualified THC.Edit.App as App
import THC.Edit.Model
import THC.Edit.Files (filePath)
import THC.Edit.Browser

checks :: IO ()
checks = do
  forM_ [("*.hs", "Main.hs", True), ("*.hs", "Main.HS", False),
         ("", ".hidden", True), ("*", "", True), ("?", "", False),
         ("a?c", "aλc", True), ("a?c", "ac", False),
         ("a**b*c", "axbybc", True), ("a*b", "ac", False),
         ("a.b", "axb", False), ("[a]", "[a]", True),
         ("file λ.*", "file λ.hs", True)] $ \(patternText, name, expected) ->
    check ("wildcard match " ++ show (patternText, name)) (matchPattern patternText name == expected)
  bracket makeDirectory removePathForcibly $ \dir -> do
    before <- getCurrentDirectory
    createDirectory (dir </> "nested")
    createDirectory (dir </> "Zoo")
    forM_ ["B.hs", "a.hs", "file λ.hs", ".hidden.hs", "read me.txt"] $ \name ->
      BS.writeFile (dir </> name) "abc"
    createDirectoryLink (dir </> "nested") (dir </> "linked")
    createFileLink (dir </> "gone") (dir </> "dangling.hs")
    (resolved, entries) <- readDirectory (dir </> ".") "*.hs" >>= right
    canonical <- canonicalizePath dir
    check "listing returns canonical directory" (resolved == canonical)
    check "directories precede case-insensitively sorted matching files"
      (map entryName entries == ["..", "linked", "nested", "Zoo", ".hidden.hs", "a.hs", "B.hs", "dangling.hs", "file λ.hs"])
    check "directory symlinks are browsable" (any (\e -> entryName e=="linked" && entryDirectory e) entries)
    check "file byte size is reported" (any (\e -> entryName e=="file λ.hs" && entryBytes e==Just 3 && isJust (entryModified e)) entries)
    check "unavailable child size does not prevent listing" (any (\e -> entryName e=="dangling.hs" && entryBytes e==Nothing && entryModified e==Nothing) entries)
    (_, allEntries) <- readDirectory dir "" >>= right
    check "empty filter includes filenames with spaces" (T.pack "read me.txt" `elem` map entryName allEntries)
    (_, filtered) <- readDirectory dir "no-match" >>= right
    check "filter always retains directories" (map entryName filtered == ["..", "linked", "nested", "Zoo"])
    (linked, _) <- readDirectory (dir </> "linked") "*" >>= right
    check "entering symlink canonicalizes location" (linked == canonical </> "nested")
    let root = until (\p -> takeDirectory p == p) takeDirectory canonical
    (_, rootEntries) <- readDirectory root "no-match" >>= right
    check "filesystem root has no parent entry" (".." `notElem` map entryName rootEntries)
    missing <- readDirectory (dir </> "missing") "*"
    check "missing directory returns an error" (isLeft missing)
    file <- readDirectory (dir </> "a.hs") "*"
    check "file is not a directory" (isLeft file)
    permissions <- getPermissions (dir </> "nested")
    bracket (setPermissions (dir </> "nested") (permissions { readable = False }))
            (const (setPermissions (dir </> "nested") permissions)) $ \_ -> do
      denied <- readDirectory (dir </> "nested") "*"
      check "unreadable directory returns an error" (isLeft denied)
    let package=canonical </> "sample.cabal"
    BS.writeFile package "cabal-version: 3.0\nname: sample\nversion: 0.1\n"
    chosenPackage<-packageFile dir
    check "package entrypoint finds the Cabal file" (chosenPackage==Just package)
    (_,project)<-App.applyEffects (initialDesktop (80,25)) [ReadPath dir]
    check "opening a package directory opens its Cabal file and explorer"
      (fmap filePath (activeDocument project >>= documentFile)==Just package && isJust (sideTree project))
    BS.writeFile (canonical </> "aaa.cabal") "name: aaa\n"
    multiple<-packageFile dir
    check "multiple Cabal files choose deterministically" (multiple==Just (canonical </> "aaa.cabal"))
    let namesake=canonical </> takeFileName canonical ++ ".cabal"
    BS.writeFile namesake "name: namesake\n"
    preferred<-packageFile dir
    check "directory namesake Cabal file takes precedence" (preferred==Just namesake)
    (_,chooser)<-App.applyEffects project (snd (runCommand ChangeDir project))
    check "Change dir lists directories only"
      (case dialog chooser of Just Dialog{purpose=ChangingDirectory _ ds} -> all entryDirectory ds; _ -> False)
    let cancelled=fst (handleEvent (V.EvKey V.KEsc []) chooser)
    check "Change dir cancel preserves explorer and buffers" (sideTree cancelled==sideTree project && buffers cancelled==buffers project)
    bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
      (_,changed)<-App.applyEffects chooser [ChangeDirectory (canonical </> "nested")]
      cwd<-getCurrentDirectory
      check "Change dir updates default directory and explorer without replacing buffers"
        (cwd==canonical </> "nested" && startingDirectory changed==cwd && fmap treeRoot (sideTree changed)==Just cwd && buffers changed==buffers project && dialog changed==Nothing)
      (_,failed)<-App.applyEffects changed [ChangeDirectory (canonical </> "missing")]
      still<-getCurrentDirectory
      check "failed directory change preserves context" (still==cwd && defaultDirectory failed==defaultDirectory changed && sideTree failed==sideTree changed)
    after <- getCurrentDirectory
    check "browsing does not change working directory" (before == after)
  putStrLn "browser checks passed"
  where
    check name ok = unless ok (error name)
    right = either error pure
    isLeft (Left _) = True
    isLeft _ = False
    makeDirectory = do
      base <- getTemporaryDirectory
      (path, handle) <- openBinaryTempFile base "thc-edit-browser-check"
      hClose handle
      removeFile path
      createDirectory path
      pure path
