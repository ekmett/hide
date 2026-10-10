{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.DocsTools
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- The linked plugin's offline documentation tools. Requests use the same checked
-- public codecs and session-owned operations as Help, without filesystem access.
module Hide.DocsTools
  ( tools
  ) where

import Hide.Plugin.Command (CommandDef(..))
import Hide.Plugin.Documentation
import Hide.Plugin.Tool (Tool(..))

-- | Explicit read-only declarations for the editor endpoint. Metadata discovery
-- performs no IO. Execution remains subject to host policy and scoped services.
tools :: [Tool DocsServices]
tools=[Tool "docs_list" True (CommandDef "hide.docs.list"
    "List offline documentation with titles and Markdown headings. Paths are relative to the selected corpus; default corpus is editor."
    listInput listOutput docsList)
  ,Tool "docs_search" True (CommandDef "hide.docs.search"
    "Search literal text in offline documentation, with line numbers and enclosing headings. Searches at most 256 files and 16 MiB; partial results are marked. No regex or network access."
    searchInput searchOutput docsSearch)
  ,Tool "docs_read" True (CommandDef "hide.docs.read"
    "Read a line range from an offline document. Files must be UTF-8 and at most 1 MiB. Responses are capped at 128 Ki characters. Lines start at 1."
    readInput readOutput docsRead)]
