{-# LANGUAGE OverloadedStrings #-}
module MarkdownCheck (checks) where

import Control.Monad (forM_, unless)
import qualified Data.Text as T
import THC.Edit.Buffer (displayColumn)
import THC.Edit.Markdown
import THC.Edit.Syntax (Style(..))

checks :: IO ()
checks = do
  let text width = T.pack . map fst . renderMarkdown width
      styled = renderMarkdown 80 "# Heading\n\nSome *emphasis* and **strong** with `code`."
  check "headings and inline markup render with styles" (text 80 "# Heading" == "Heading" && ('H',Keyword) `elem` styled && ('e',Constructor) `elem` styled && ('s',Keyword) `elem` styled && ('c',Literal) `elem` styled)
  check "links retain destination and decode entities" (text 80 "[docs][ref] &amp; &#955;\n\n[ref]: https://example.test/a" == "docs (https://example.test/a) & λ")
  check "escapes are parsed by CommonMark" (text 80 "\\*literal\\*" == "*literal*")
  check "ordered, nested lists and quotes render" ("3. one\n4. two\n   • nested" `T.isInfixOf` text 80 "3. one\n4. two\n   - nested" && text 80 "> quote" == "> quote")
  let haskell = renderMarkdown 80 "```haskell\nmodule X where\nx = 42\n```"
      python = renderMarkdown 80 "```python\ndef answer():\n    return 42\n```"
  check "fenced languages use Skylighting styles" (('m',Keyword) `elem` haskell && ('4',Number) `elem` haskell && ('d',Keyword) `elem` python)
  check "unknown code preserves whitespace" (text 80 "```unknown\n  a < b\n```" == "  a < b")
  check "unfinished fence stays readable" (text 80 "```haskell\nx = 1" == "x = 1")
  check "unfinished inline markup stays readable" (text 80 "hello **unfinished" == "hello **unfinished")
  check "hard and soft breaks differ" (text 80 "a\nb  \nc" == "a b\nc")
  let sample = "# Wide 界 and é\n\n• words 界界 éé and [link](https://example.test).\n\n- nested words\n\n```text\n\tx界é\n```"
  forM_ [1,2,3,8,20] $ \width -> do
    let rendered = text width sample
    forM_ (T.lines rendered) $ \line ->
      check "wrapping uses display width" (displayColumn line (T.length line) <= width || width == 1 && displayColumn line (T.length line) == 2)
    check "combining characters stay attached" (not ("\ń" `T.isInfixOf` rendered))
  check "zero width progresses" (not (T.null (text 0 "abc")))
  check "empty input" (null (renderMarkdown 80 ""))
  putStrLn "Markdown checks passed"
  where check label ok = unless ok (error label)
