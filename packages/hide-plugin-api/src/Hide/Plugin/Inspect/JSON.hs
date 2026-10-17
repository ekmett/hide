-- SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- | A second interpreter for the same prepared semantic documents as the UI.
-- The caller must authorize disclosure before exporting a page. This module
-- neither exposes private windows nor registers an agent tool. Action numbers
-- are local to the returned page; retain that exact page when following one.
-- Subjects stay typed in memory: no deserialization, string dispatch or casts.
module Hide.Plugin.Inspect.JSON
  ( JSONPage, renderJSON, jsonValue, jsonTarget, openJSON ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Hide.Plugin.Inspect

data JSONPage = JSONPage !Value !(M.Map Int Target)

jsonValue :: JSONPage -> Value
jsonValue (JSONPage value _)=value

-- | A page-local action request, not permission to execute it. The host must
-- re-resolve Inspect targets and apply its normal admission to OpenFile paths.
jsonTarget :: JSONPage -> Int -> Maybe Target
jsonTarget (JSONPage _ targets) ident=M.lookup ident targets

renderJSON :: PreparedDoc -> JSONPage
renderJSON prepared=JSONPage value targets
  where
    (content,targets)=case preparedDoc prepared of
      Text blocks->(object ["kind" .= ("text" :: Text), "blocks" .= map block blocks],M.empty)
      List title items->(object ["kind" .= ("list" :: Text), "title" .= title,
        "items" .= [item n row | (n,row)<-zip [0::Int ..] items]],
        M.fromList [(n,target) | (n,Item _ _ (Just target))<-zip [0..] items])
    value=object ["document" .= content,"hasMore" .= preparedHasMore prepared]
    block (Heading text)=object ["kind" .= ("heading" :: Text),"text" .= text]
    block (Prose text)=object ["kind" .= ("prose" :: Text),"text" .= text]
    block (Facts facts)=object ["kind" .= ("facts" :: Text),"facts" .=
      [object ["label" .= label,"value" .= text] | (label,text)<-facts]]
    item n (Item label detail target)=object
      ["label" .= label,"detail" .= detail,"action" .= fmap (action n) target]
    action n target=case target of
      Inspect subject->object ["id" .= n,"kind" .= ("inspect" :: Text),
        "type" .= T.pack (show (subjectType subject))]
      PageAt _ _ _->object ["id" .= n,"kind" .= ("page" :: Text)]
      OpenFile _->object ["id" .= n,"kind" .= ("open-file" :: Text)]
      OpenAt path row column _->object ["id" .= n,"kind" .= ("open-source" :: Text),"path" .= path,"line" .= row,"column" .= column]

-- | Render an authorized subject's current pure view on the caller's worker.
openJSON :: Inspectors -> Subject -> ViewId -> IO (Either Text JSONPage)
openJSON inspectors subject wanted=fmap (either (Left . T.pack . show) (Right . renderJSON)) (openPage inspectors subject wanted 0)
