{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : MarkdownCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module MarkdownCheck (checks) where

import Control.Monad (forM_, unless)
import Control.Exception (evaluate)
import System.Timeout (timeout)
import qualified Data.Text as T
import Hide.Buffer (displayColumn)
import Hide.Model
import Hide.Render (snapshotHtml)
import Hide.Markdown
import Hide.Syntax (Style(..),linkSpans,styledContents,styledLength,splitStyledAt)

checks :: IO ()
checks = do
  let text width = styledContents . renderMarkdown width
      styled = renderMarkdown 80 "# Heading\n\nSome *emphasis* and **strong** with `code`."
  check "headings and inline markup render with styles" (text 80 "# Heading" == "Heading" && contains 'H' (SectionStyle 1 (BoldStyle (Heading 1))) styled && contains 'e' (ItalicStyle Constructor) styled && contains 's' (BoldStyle Keyword) styled && contains 'c' Literal styled)
  check "links retain destination metadata and decode entities" (text 80 "[docs][ref] &amp; &#955;\n\n[ref]: https://example.test/a" == "docs & λ" && linkSpans (renderMarkdown 80 "[docs](https://example.test/a)")==[(0,4,"https://example.test/a")])
  check "escapes are parsed by CommonMark" (text 80 "\\*literal\\*" == "*literal*")
  check "ordered, nested lists and quotes render" ("3. one\n4. two\n   • nested" `T.isInfixOf` text 80 "3. one\n4. two\n   - nested" && text 80 "> quote" == "> quote")
  let haskell = renderMarkdown 80 "```haskell\nmodule X where\nx = 42\n```"
      python = renderMarkdown 80 "```python\ndef answer():\n    return 42\n```"
  check "fenced languages use Skylighting styles" (contains 'm' (CodeStyle False Keyword) haskell && contains '4' (CodeStyle False Number) haskell && contains 'd' (CodeStyle False Keyword) python)
  check "unknown code preserves whitespace" ("     a < b" `T.isInfixOf` text 80 "```unknown\n  a < b\n```")
  check "unfinished fence stays readable" ("   x = 1" `T.isInfixOf` text 80 "```haskell\nx = 1")
  check "unfinished inline markup stays readable" (text 80 "hello **unfinished" == "hello **unfinished")
  check "hard and soft breaks differ" (text 80 "a\nb  \nc" == "a b\nc")
  let sample = "# Wide 界 and é\n\n• words 界界 éé and [link](https://example.test).\n\n- nested words\n\n```text\n\tx界é\n```"
  forM_ [1,2,3,8,20] $ \width -> do
    let rendered = text width sample
    forM_ (T.lines rendered) $ \line ->
      check "wrapping uses display width" (displayColumn line (T.length line) <= width || width == 1 && displayColumn line (T.length line) == 2)
    check "combining characters stay attached" (not ("\ń" `T.isInfixOf` rendered))
  check "zero width progresses" (not (T.null (text 0 "abc")))
  let table="| Name | Count |\n| :--- | ---: |\n| alpha | 42 |\n| **beta** | 7 |"
  check "pipe tables render box borders and preserve inline markup"
    ("┌" `T.isInfixOf` text 40 table && "│" `T.isInfixOf` text 40 table && "alpha" `T.isInfixOf` text 40 table && not ("**" `T.isInfixOf` text 40 table))
  forM_ [4,8,12,40] $ \width -> check "tables fit narrow windows"
    (all (\line -> displayColumn line (T.length line)<=width) (T.lines (text width table)))
  check "heading levels retain hierarchy" (contains 'S' (SectionStyle 2 (BoldStyle (Heading 2))) (renderMarkdown 40 "## Section") && contains 'T' (SectionStyle 3 (BoldStyle (Heading 3))) (renderMarkdown 40 "### Topic"))
  check "code backgrounds include padding" (contains ' ' (CodeStyle False Plain) haskell)
  let shell=renderMarkdown 20 "```sh\necho hello\n```"
      shellLines=T.lines (styledContents shell)
  check "shell panels use their own background style" (any (\(_,style)->case style of CodeStyle True _->True; _->False) shell)
  check "code panel includes top bottom and side padding" ((case shellLines of [first,_,final]->all (T.null . T.strip) [first,final]; _->False) && "   echo hello " `T.isInfixOf` styledContents shell)
  check "list continuation hangs below content" (text 12 "- one two three four" == "• one two\n  three four")
  let help=addHelpStyled (renderMarkdown 40 "# Title\n\nText\n\n```sh\necho hi\n```") (initialDesktop (80,25))
      light=snapshotHtml help {appearance=LightMode}
      dark=snapshotHtml help {appearance=DarkMode}
  check "appearance resolves explicit and OS choices" (not (darkAppearance help {appearance=LightMode,systemDark=True}) && darkAppearance help {appearance=DarkMode,systemDark=False} && darkAppearance help {appearance=SystemMode,systemDark=True})
  check "Help backgrounds follow light and dark appearance" ("background:rgb(0,170,170)" `T.isInfixOf` light && "background:rgb(0,0,0)" `T.isInfixOf` dark && light/=dark)
  let rawShell="printf '%s\\n' 'literal ; λ'\n\tprintf 'tail  '  \n"
      shellSource="before\n\n```bash\n"<>rawShell<>"```\n\nafter\n\n```console\n$ echo ignored\nignored\n```\n\n```zsh\necho second\n```"
  forM_ [7,20,80] $ \width -> do
    let (cells,blocks)=renderMarkdownWithShellBlocks width shellSource
    check "execution metadata preserves whole original shell body"
      (map (\(_,_,dialect,body)->(dialect,body)) blocks==[("bash",rawShell),("zsh","echo second\n")])
    check "execution spans cover shell panels only and exclude surrounding prose"
      (all (\(start,end,_,_)->start>=0 && end>start && end<=styledLength cells &&
        any (\(_,style)->case style of CodeStyle True _->True; _->False) (fst (splitStyledAt (end-start) (snd (splitStyledAt start cells)))) &&
        not ("before" `T.isInfixOf` styledContents (fst (splitStyledAt (end-start) (snd (splitStyledAt start cells)))))) blocks)
    check "metadata does not change Markdown rendering" (cells==renderMarkdown width shellSource)
  check "nested fenced code retains commands without Markdown list markers"
    (map (\(_,_,dialect,body)->(dialect,body)) (snd (renderMarkdownWithShellBlocks 15 "- example\n\n  ```sh\n  echo nested\n  ```"))==[("sh","echo nested\n")])
  check "empty shell block metadata is retained for clear execution errors"
    (map (\(_,_,_,body)->body) (snd (renderMarkdownWithShellBlocks 40 "```sh\n```"))==[""])
  check "empty input" (null (renderMarkdown 80 ""))
  let logical source=T.concat (map markdownBlockText (markdownBlocks (parseMarkdown source)))
  check "logical copy retains hard breaks and block separation without list furniture"
    (logical "a **b**\nc  \nd\n\n- one\n- two\n\n```sh\n\tx\n```"=="a b c\nd\n\none\ntwo\n\n\tx\n")
  check "parser EOF terminator stays out of exact literal bodies"
    (map (\(_,_,_,body)->body) (snd (renderMarkdownWithShellBlocks 40 "```sh\necho hi"))==["echo hi"] &&
     map (\(_,_,_,body)->body) (snd (renderMarkdownWithShellBlocks 40 "- example\n\n  ```sh\n  echo nested"))==["echo nested"] &&
     logical "<div>raw</div>"=="<div>raw</div>")
  let linkedTable=markdownBlocks (parseMarkdown "| H | V |\n|---|---|\n| [alpha](target.md) | 42 |")
  check "table logical copy and link offsets share the parser's canonical cells"
    (T.concat (map markdownBlockText linkedTable)=="H\tV\nalpha\t42" &&
      concatMap markdownBlockLinks linkedTable==[(4,9,"target.md")])
  let paragraph=T.replicate 2000 "Ordinary message with some **bold** and code `abc`.\n"
      long=renderMarkdown 73 paragraph
  rendered<-timeout 2000000 (evaluate (styledLength long))
  check "large streamed paragraph avoids quadratic inline concatenation" (maybe False (>90000) rendered)
  check "large paragraph retains text order and inline styles"
    (T.take 8 (styledContents long)=="Ordinary" && sum [T.count "b" run | (run,BoldStyle Keyword)<-long]==2000 && sum [T.count "a" run | (run,Literal)<-long]==2000)

  putStrLn "Markdown checks passed"
  where check label ok = unless ok (error label)
        contains character style=any (\(text,actual)->actual==style && T.any (==character) text)
