-- SPDX-FileCopyrightText: 2026 Edward Kmett
-- SPDX-License-Identifier: UPL-1.0 AND BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Copyright   : (C) 2026 Edward Kmett
-- License     : UPL-1.0 AND BSD-3-Clause
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : Native GHC; host filesystem/process services
--
-- Build and validate the public documentation site and revision-pinned source links.
module Main (main) where

import Control.Monad (forM, forM_, unless, when)
import Data.Char (chr, digitToInt, isHexDigit, ord)
import Data.List (isPrefixOf, isSuffixOf, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath
import System.Process (readProcess)
import Text.HTML.TagSoup
import Numeric (showHex)

data Guide = Guide FilePath String String

-- An allowlist, not a recursive copy of docs/ (which also holds large evidence).
guides :: [Guide]
guides =
  [ Guide "docs/README.md" "index" "Guide index"
  , Guide "docs/install.md" "install" "Installation"
  , Guide "docs/editing.md" "editing" "Editing"
  , Guide "docs/sessions.md" "sessions" "Sessions"
  , Guide "docs/display.md" "display" "Display and frontends"
  , Guide "docs/configuration.md" "configuration" "Configuration"
  , Guide "docs/hex.md" "hex" "Hex editing"
  , Guide "docs/haskell.md" "haskell" "Haskell language tools"
  , Guide "docs/running.md" "running" "Running and debugging"
  , Guide "docs/git.md" "git" "Git"
  , Guide "docs/session-tools.md" "session-tools" "Session tools"
  , Guide "docs/agent-skills.md" "agent-skills" "Agent skills"
  , Guide "docs/agent-tools.md" "agent-tools" "Agent operation reference"
  , Guide "docs/conversations.md" "conversations" "Conversations"
  , Guide "docs/remote.md" "remote" "Remote editing"
  , Guide "docs/contributing.md" "contributing" "Development"
  , Guide "docs/architecture.md" "architecture" "Architecture"
  , Guide "README.md" "quick-reference" "F1 quick reference"
  , Guide "docs/site/build.md" "build-site" "Build this site"
  ]

site :: FilePath
site = "build/site"

-- These are generated from the live dialog definitions through Metal.
-- Keep this list in sync with tools/docs-screenshots.hs.
screenshotNames :: [FilePath]
screenshotNames = map (<.> "png")
  ["find-replace","hdb-download","downloads", "permission-diff", "file-menu", "split", "preferences", "build-target", "debug-launch", "git-commit",
   "conversation", "debug-step", "debug-menu", "debug-stack", "side-by-side", "window-views-menu"]

repo :: String
repo = "https://github.com/ekmett/thc-edit"

guidePath :: Guide -> FilePath
guidePath (Guide _ slug _) = "guides" </> slug <.> "html"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["build", revision, pandoc] -> do
      checkRevision revision
      checkSiteRoot
      buildSite revision pandoc
      checkSite revision
    ["check", revision] -> checkRevision revision >> checkSite revision
    _ -> die "Usage: thc-edit-docs (build REVISION PANDOC | check REVISION)"

checkRevision :: String -> IO ()
checkRevision revision = do
  unless (length revision == 40 && all (`elem` ("0123456789abcdef" :: String)) revision) $
    die "Documentation requires a full lowercase Git commit ID"
  current <- Text.strip . Text.pack <$> readProcess "git" ["rev-parse", "HEAD"] ""
  unless (current == Text.pack revision) $ die "Documentation revision must match this checkout's HEAD"

-- Check the ancestor too: checking only build/site would permit a symlinked
-- build directory to redirect recursive replacement outside the checkout.
checkSiteRoot :: IO ()
checkSiteRoot = do
  root <- getCurrentDirectory >>= canonicalizePath
  createDirectoryIfMissing False "build"
  redirected <- pathIsSymbolicLink "build"
  actual <- canonicalizePath "build"
  unless (not redirected && actual == root </> "build") $
    die "Refusing redirected build directory; documentation output must stay in this checkout"

buildSite :: String -> FilePath -> IO ()
buildSite revision pandoc = do
  -- Only this generated directory is replaced. Refuse a redirected destination.
  exists <- doesPathExist site
  when exists $ do
    symbolic <- pathIsSymbolicLink site
    when symbolic $ die "Refusing to replace a symlink at build/site"
    removeDirectoryRecursive site
  createDirectoryIfMissing True (site </> "assets")
  copyFile "docs/site/site.css" (site </> "assets/site.css")
  copyFile "docs/site/site.js" (site </> "assets/site.js")
  copyFile "docs/site/theme.js" (site </> "assets/theme.js")
  forM_ screenshotNames $ \name -> do
    createDirectoryIfMissing True (site </> "assets/screenshots")
    copyFile ("docs/site/screenshots" </> name) (site </> "assets/screenshots" </> name)
  forM_ guides $ \guide@(Guide source _ title) -> renderGuide revision pandoc source (guidePath guide) title
  renderGuide revision pandoc "docs/site/index.md" "home.html" "Turbo Haskell editor"
  renderShell revision ("home.html" : map guidePath guides)
  Text.writeFile (site </> "revision.txt") (Text.pack (revision ++ "\n"))
  Text.writeFile (site </> ".nojekyll") ""

filesBelow :: FilePath -> IO [FilePath]
filesBelow root = do
  names <- sort <$> listDirectory root
  fmap concat $ forM names $ \name -> do
    let path = root </> name
    symbolic <- pathIsSymbolicLink path
    when symbolic $ die ("Unexpected symlink in documentation: " ++ path)
    directory <- doesDirectoryExist path
    if directory then filesBelow path else pure [path]

-- Always relative to the generated site root: file://, / and /thc-edit/ all agree.
fromPage :: FilePath -> FilePath -> String
fromPage page target = concat (replicate depth "../") ++ target
  where depth = length (filter (/= ".") (splitDirectories (takeDirectory page)))

escape :: String -> String
escape value = renderTags [TagText value]

link :: String -> String -> String
link url title = "<a href=\"" ++ escape url ++ "\">" ++ escape title ++ "</a>"

-- Guide pages also remain usable independently of the navigation shell.
stylePage :: String -> FilePath -> Text.Text -> IO Text.Text
stylePage revision page html = do
  let stylesheet = "<link rel=\"stylesheet\" href=\"" ++ fromPage page "assets/site.css" ++ "\">"
      theme = "<script src=\"" ++ fromPage page "assets/theme.js" ++ "\"></script>"
      metadata = "<meta name=\"thc-revision\" content=\"" ++ revision ++ "\">"
      (beforeBody, body) = Text.breakOn "<body" html
      (opening, remainder) = Text.breakOn ">" body
  unless (not (Text.null body) && not (Text.null remainder) && "</head>" `Text.isInfixOf` html) $
    die ("Expected an HTML document: " ++ page)
  pure $ Text.replace "</head>" (Text.pack (metadata ++ theme ++ stylesheet) <> "</head>") $
    beforeBody <> opening <> " data-thc-section=\"guide\">" <> Text.drop 1 remainder

jsonString :: String -> String
jsonString value = '"' : concatMap encode value ++ "\""
  where
    encode '"' = "\\\""
    encode '\\' = "\\\\"
    encode '<' = "\\u003c"
    encode c | ord c < 32 = "\\u" ++ replicate (4 - length hex) '0' ++ hex
      where hex = showHex (ord c) ""
    encode c = [c]

renderShell :: String -> [FilePath] -> IO ()
renderShell revision pages = do
  let item page label = "<a class=\"thc-nav-link\" data-page=\"" ++ escape page ++
        "\" href=\"" ++ escape page ++ "\" target=\"thc-content\">" ++ escape label ++ "</a>"
      guideItem guide@(Guide _ _ title) = item (guidePath guide) title
      manifest = "<script type=\"application/json\" id=\"thc-pages\">[" ++
        concat (zipWith (++) ("" : repeat ",") (map jsonString pages)) ++ " ]</script>"
      html = "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">" ++
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">" ++
        "<meta name=\"thc-revision\" content=\"" ++ revision ++ "\">" ++
        "<title>thc-edit documentation</title><script src=\"assets/theme.js\"></script>" ++
        "<link rel=\"stylesheet\" href=\"assets/site.css\"></head>" ++
        "<body class=\"thc-shell\"><button class=\"thc-menu\" type=\"button\" aria-expanded=\"false\" " ++
        "aria-controls=\"thc-rail\">Documentation menu</button><div class=\"thc-shell-layout\">" ++
        "<aside class=\"thc-rail\" id=\"thc-rail\"><a class=\"thc-brand\" href=\"home.html\" " ++
        "data-page=\"home.html\" target=\"thc-content\">thc-edit<span> / docs</span></a>" ++
        "<fieldset class=\"thc-appearance\" id=\"thc-appearance\"><legend>Appearance</legend>" ++
        "<div class=\"thc-theme-options\">" ++
        "<button type=\"button\" data-thc-appearance=\"light\" aria-pressed=\"false\">Light</button>" ++
        "<button type=\"button\" data-thc-appearance=\"dark\" aria-pressed=\"false\">Dark</button>" ++
        "<button type=\"button\" data-thc-appearance=\"system\" aria-pressed=\"true\">Follow OS</button>" ++
        "</div></fieldset>" ++
        "<nav aria-label=\"thc-edit documentation\"><p class=\"thc-nav-label\">Start</p>" ++
        item "home.html" "Overview" ++
        "<a class=\"thc-nav-link\" href=\"" ++ repo ++
        "\" target=\"_blank\" rel=\"noopener noreferrer\">GitHub ↗</a>" ++
        "<p class=\"thc-nav-label\">Guides</p>" ++
        concatMap guideItem guides ++
        "</nav><div class=\"thc-rail-footer\">" ++
        "<p>Terminal · Native · Browser</p>" ++
        link (repo ++ "/tree/" ++ revision) "Source ↗" ++ " · " ++
        link (repo ++ "/commit/" ++ revision) (take 12 revision) ++ "</div></aside>" ++
        "<iframe id=\"thc-content\" name=\"thc-content\" title=\"thc-edit documentation content\" " ++
        "src=\"home.html\"></iframe></div>" ++ manifest ++
        "<script src=\"assets/site.js\" defer></script></body></html>"
  Text.writeFile (site </> "index.html") (Text.pack html)

renderGuide :: String -> FilePath -> FilePath -> FilePath -> String -> IO ()
renderGuide revision pandoc source output title = do
  fragment <- readProcess pandoc ["--from=gfm", "--to=html5", "--wrap=none", source] ""
  tags <- mapM rewriteTag (parseTags fragment)
  let content = "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">" ++
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"><title>" ++ escape title ++
        " · thc-edit</title></head><body class=\"thc-guide\">" ++
        "<main class=\"thc-prose\" id=\"main\">" ++ renderTags tags ++
        "<footer class=\"thc-footer\">" ++ link (repo ++ "/blob/" ++ revision ++ "/" ++ source) "View this page's source" ++
        "</footer></main></body></html>"
  html <- stylePage revision output (Text.pack content)
  createDirectoryIfMissing True (takeDirectory (site </> output))
  Text.writeFile (site </> output) html
  where
    rewriteTag (TagOpen tag attrs) = TagOpen tag <$> mapM rewriteAttr attrs
    rewriteTag tag = pure tag
    rewriteAttr (key, value) | key `elem` ["href", "src"] = do
      url <- guideURL revision source output value
      pure (key, url)
    rewriteAttr attribute = pure attribute

external :: String -> Bool
external url = "//" `isPrefixOf` url || ':' `elem` takeWhile (`notElem` ("/?#" :: String)) url

-- Resolve repository-relative Markdown links before moving a guide. Omitted
-- reports, source and evidence remain pinned repository links, never site copies.
guideURL :: String -> FilePath -> FilePath -> String -> IO String
guideURL revision source output url
  | null url || "#" `isPrefixOf` url || external url = pure url
  | "/" `isPrefixOf` url = die ("Root-relative guide link: " ++ source ++ ": " ++ url)
  | otherwise = do
      let (path, suffix) = break (`elem` ("?#" :: String)) url
          resolved = collapse (takeDirectory source </> decodeURL path)
          pages = ("docs/site/index.md", "home.html") : [(src, guidePath g) | g@(Guide src _ _) <- guides]
            ++ [("docs/site/screenshots" </> name, "assets/screenshots" </> name) | name <- screenshotNames]
      case lookup resolved pages of
        Just target -> pure (fromPage output target ++ suffix)
        Nothing -> do
          exists <- doesPathExist resolved
          unless (exists && not (".." `isPrefixOf` resolved)) $
            die ("Missing repository link in " ++ source ++ ": " ++ url)
          directory <- doesDirectoryExist resolved
          pure (repo ++ (if directory then "/tree/" else "/blob/") ++ revision ++ "/" ++ resolved ++ suffix)

collapse :: FilePath -> FilePath
collapse = joinPath . foldl step [] . splitDirectories
  where
    step xs "." = xs
    step [] ".." = [".."]
    step xs ".." | last xs /= ".." = init xs
    step xs x = xs ++ [x]

decodeURL :: String -> String
decodeURL ('%':a:b:rest) | isHexDigit a && isHexDigit b = chr (16 * digitToInt a + digitToInt b) : decodeURL rest
decodeURL (c:rest) = c : decodeURL rest
decodeURL [] = []

checkSite :: String -> IO ()
checkSite revision = do
  built <- Text.strip <$> Text.readFile (site </> "revision.txt")
  unless (built == Text.pack revision) $ die "The assembled site has a different revision"
  files <- filesBelow site
  let htmlFiles = filter ((== ".html") . takeExtension) files
      inventory = Set.fromList (map (makeRelative site) files)
  pages <- fmap Map.fromList $ forM htmlFiles $ \file -> do
    html <- Text.readFile file
    let tags = parseTags (Text.unpack html)
        anchors = Set.fromList [value | TagOpen tag attrs <- tags, (key,value) <- attrs,
          key == "id" || (tag == "a" && key == "name")]
        urls = [value | TagOpen _ attrs <- tags, (key,value) <- attrs, key `elem` ["href", "src"]]
        path = makeRelative site file
    unless ("name=\"thc-revision\"" `Text.isInfixOf` html &&
            "assets/site.css" `Text.isInfixOf` html &&
            "assets/theme.js" `Text.isInfixOf` html && Text.pack revision `Text.isInfixOf` html) $
      die ("Missing shared theme/revision in " ++ path)
    pure (path, (anchors, urls))
  let failures = concat [checkURL inventory pages page url | (page, (_,urls)) <- Map.toList pages, url <- urls]
      required = ["index.html", "home.html", "assets/site.css", "assets/site.js", "assets/theme.js"] ++ map guidePath guides
      excluded = [path | path <- Set.toList inventory, any (`isSuffixOf` path) [".bgv", ".log", ".zip", ".tar.xz"]]
      missing = [path | path <- required, Set.notMember path inventory]
  unless (null (failures ++ missing ++ excluded)) $
    die (unlines (take 50 (failures ++ map ("Missing page: " ++) missing ++ map ("Unexpected evidence: " ++) excluded)))
  shell <- Text.readFile (site </> "index.html")
  unless ("id=\"thc-pages\"" `Text.isInfixOf` shell &&
          "id=\"thc-content\"" `Text.isInfixOf` shell &&
          "id=\"thc-appearance\"" `Text.isInfixOf` shell) $
    die "Missing site route inventory, content frame, or appearance control"
  putStrLn ("Documentation checked: " ++ show (length guides) ++ " guides, " ++ show (Map.size pages) ++
    " HTML pages, " ++ show (sum [length urls | (_,urls) <- Map.elems pages]) ++
    " links/assets; relative paths also work below /thc-edit/. Revision " ++ revision)

checkURL :: Set.Set FilePath -> Map.Map FilePath (Set.Set String, [String]) -> FilePath -> String -> [String]
checkURL inventory pages page url
  | null url || external url = [problem "Local filesystem URL" | "file:" `isPrefixOf` url]
  | "/" `isPrefixOf` url = [problem "Root-relative URL breaks project Pages"]
  | otherwise =
      let (rawPath, remainder) = break (`elem` ("?#" :: String)) url
          path = if null rawPath then page else collapse (takeDirectory page </> decodeURL rawPath)
          target = if "/" `isSuffixOf` path then path ++ "index.html" else path
          fragment = case dropWhile (/= '#') remainder of [] -> ""; (_:rest) -> decodeURL rest
      in if Set.notMember target inventory then [problem ("Missing target " ++ target)]
         else case Map.lookup target pages of
           Just (anchors, _) | not (null fragment) && Set.notMember fragment anchors -> [problem ("Missing fragment " ++ fragment)]
           _ -> []
  where problem why = why ++ " in " ++ page ++ ": " ++ url
