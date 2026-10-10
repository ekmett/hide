-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : ClipboardMCPCheck
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings

module ClipboardMCPCheck (checks) where
import Control.Monad (unless)
import Data.Aeson
import qualified Data.Text as T
import Hide.ClipboardMCP
import Hide.Model

checks :: IO ()
checks=do
  let initial=(initialDesktop (80,25)) {clipboard="private old clipboard",clipboardCode=Just "private old clipboard"}
      check name good=unless good (error name)
  (written,finish)<-clipboardTool initial (object ["text" .= ("guest supplied text"::T.Text)])
  reply<-finish
  check "clipboard write uses supplied text and sequences export" (clipboard written=="guest supplied text" && clipboardCode written==Nothing && clipboardExport written==(1,Just "guest supplied text") && either (const False) (const True) reply)
  (again,_)<-clipboardTool written (object ["text" .= ("guest supplied text"::T.Text)])
  check "equal text writes receive distinct export sequence" (clipboardExport again==(2,Just "guest supplied text"))
  (cleared,_)<-clipboardTool again (object ["text" .= (""::T.Text)])
  check "explicit empty clipboard write allowed" (clipboard cleared=="" && clipboardExport cleared==(3,Just ""))
  (unchanged,bad)<-clipboardTool initial (object ["text" .= T.replicate 600000 "é"])
  failure<-bad
  check "clipboard UTF-8 bound leaves original untouched" (unchanged==initial && either (const True) (const False) failure)
  (_,unexpected)<-clipboardTool initial (object ["text" .= ("x"::T.Text),"read" .= True])
  errorReply<-unexpected
  check "no clipboard read option" (either (const True) (const False) errorReply)
  putStrLn "clipboard MCP checks passed"
