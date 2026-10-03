{-# LANGUAGE OverloadedStrings #-}
module HelpCheck (checks) where

import Control.Monad (forM_, unless)
import qualified Data.Text as T
import Hide.Buffer (displayColumn)
import Hide.Help (layoutMarkdown)

checks :: IO ()
checks = do
  check "headings lose Markdown markers" (layoutMarkdown 40 "## Getting started" == "GETTING STARTED")
  check "code keeps literal punctuation and indentation"
    (layoutMarkdown 8 "```hs\n  x = **value**\n# literal\n```" == "  x = **value**\n# literal")
  check "inline code and emphasis lose decoration"
    (layoutMarkdown 50 "Use `file_name` and **bold** or *italic*." == "Use file_name and bold or italic.")
  check "links retain readable labels"
    (layoutMarkdown 40 "See [the manual](https://example.com/help)." == "See the manual.")
  check "blank lines separate paragraphs" (layoutMarkdown 40 "one\n\ntwo" == "one\n\ntwo")
  check "source paragraph line breaks reflow" (layoutMarkdown 40 "one\ntwo" == "one two")
  check "list wraps retain continuation indentation"
    (layoutMarkdown 13 "- one two three four\n  five" == "- one two\n  three four\n  five")
  check "numbered lists retain number and continuation indentation"
    (layoutMarkdown 13 "12. one two three" == "12. one two\n    three")
  check "unmatched delimiters and identifiers survive"
    (layoutMarkdown 40 "some_name and *literal" == "some_name and *literal")
  let sourceTable = "| Action | Keys |\n| --- | --- |\n| Open file | F3 |\n| Save | F2 |"
      table = layoutMarkdown 22 sourceTable
      tableLines = T.lines table
  check "table has aligned columns" (all ((== 1) . T.count " | ") tableLines)
  check "table removes separator syntax and preserves cells"
    (not ("---" `T.isInfixOf` table) && "Action" `T.isInfixOf` table && "F3" `T.isInfixOf` table)
  let samples = ["paragraph with several words and more", "界界界界界界 abc λλ", "abcdefghijklmnopqrstuv",
                 "- a very long list containing words", "[long](https://example.com)", sourceTable]
  forM_ [8,13,22] $ \width -> forM_ samples $ \sample ->
    check "normal text stays within display columns"
      (all (\line -> displayColumn line (T.length line) <= width) (T.lines (layoutMarkdown width sample)))
  check "splitting long tokens keeps all text"
    (T.concat (T.lines (layoutMarkdown 8 "界界界界界界")) == "界界界界界界")
  putStrLn "help checks passed"
  where
    check name ok = unless ok (error name)
