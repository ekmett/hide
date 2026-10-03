{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Bounded background source highlighting with identity-checked adoption.
--
-- One worker initializes Skylighting and evaluates rows/widths before publication.
-- The desktop tick coalesces pending visible-buffer requests and installs only
-- results matching buffer identity, revision and syntax path. Failed or timed-out
-- work leaves plain text rather than moving tokenization onto the UI thread.
module Hide.Highlighting
  ( Highlighting, withHighlighting, withHighlightingUsing, tickHighlighting ) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Control.Exception (SomeException, SomeAsyncException, fromException, throwIO, try, evaluate)
import Control.Monad (forever)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Skylighting as Syntax
import System.Mem.StableName
import System.Timeout (timeout)
import Hide.Buffer
import Hide.Model
import Hide.Syntax (Style,highlightFor)

-- Stable identity distinguishes replacements/reloads with equal revisions.
data Key = Key Int Int FilePath (StableName Buffer) deriving Eq
data Request = Request Key Buffer
type Result = (V.Vector [(Char,Style)],Int)
data Work = Work [Request] (Maybe Key) (M.Map Int (Key,Maybe Result))
newtype Highlighting = Highlighting (TVar Work)

-- | Own one worker for the session. Its cancellation/join happens outside the
-- desktop lock when the session closes.
withHighlighting :: (Highlighting -> IO a) -> IO a
withHighlighting = withHighlightingUsing initializeSyntax (\path text -> pure (highlightFor path text))
  where
    -- Filename lookup traverses the shared catalog and decodes its embedded
    -- grammars. Pay that one-time cost on the owned worker, before applying the
    -- per-file tokenizer budget. Keep Skylighting's canonical map, not a cache.
    initializeSyntax = evaluate (M.foldl' (\() syntax ->
      foldl' (\() extension -> length extension `seq` ()) () (Syntax.sExtensions syntax))
      () Syntax.defaultSyntaxMap)

-- | Alternate initializer and tokenizer for deterministic lifecycle tests.
-- Initialization has its own bounded worker phase; pending requests coalesce
-- while it runs. The worker forces every source cell before publishing it.
withHighlightingUsing :: IO () -> (FilePath -> Text -> IO [(Char,Style)]) -> (Highlighting -> IO a) -> IO a
withHighlightingUsing initialize tokenize action = do
  state<-newTVarIO (Work [] Nothing M.empty)
  withAsync (do
    initialized<-bounded 10000000 initialize
    forever (work state initialized)) (\_ -> action (Highlighting state))
  where
    -- Failed initialization leaves sources plain for this session, just as a
    -- failed file tokenizer leaves that source plain until an edit. Neither
    -- failure is retried from the desktop tick.
    work state initialized=do
      Request key@(Key ident _ path _) buffer<-atomically $ do
        Work pending _ done<-readTVar state
        case pending of
          [] -> retry
          request@(Request key _):rest -> writeTVar state (Work rest (Just key) done) >> pure request
      result<-case initialized of
        Nothing -> pure Nothing
        Just () -> bounded 2000000 $ do
          let text=contents buffer
          tokens<-tokenize path text
          let rows=indexedHighlightRows tokens
              width=measureDocumentWidth text
          _<-evaluate (V.foldl' (\() row->foldl' (\() (c,style)->c `seq` style `seq` ()) () row) () rows)
          _<-evaluate width
          pure (rows,width)
      atomically $ do
        Work pending _ done<-readTVar state
        writeTVar state (Work pending Nothing (M.insert ident (key,result) done))

-- Preserve session cancellation; only synchronous failures and the operation's
-- own timeout become a plain-text result.
bounded :: Int -> IO a -> IO (Maybe a)
bounded micros action = do
  outcome<-try @SomeException (timeout micros action)
  case outcome of
    Right value -> pure value
    Left exception | Just async<-(fromException exception :: Maybe SomeAsyncException) -> throwIO async
                   | otherwise -> pure Nothing

-- | Coalesce pending work to the newest visible buffers. At most eight requests
-- wait behind one running tokenizer; timeouts leave plain text until an edit.
-- Call while holding the desktop lock; no tokenizer or whole-buffer scan runs here.
tickHighlighting :: Highlighting -> Desktop -> IO Desktop
tickHighlighting (Highlighting state) desktop = do
  requests<-mapM request candidates
  ready<-atomically $ do
    Work _ running done<-readTVar state
    let current=M.filterWithKey (\ident _->M.member ident (buffers desktop)) done
        pending=take 8 [r | r@(Request key@(Key ident _ _ _) _)<-requests,
          Just key/=running,maybe True ((/=key).fst) (M.lookup ident current)]
    writeTVar state (Work pending running current)
    pure current
  pure desktop {buffers=foldl' (install ready) (buffers desktop) requests}
  where
    (_,candidates)=foldl' choose (S.empty,[]) (filter (windowVisible desktop) (windows desktop))
    choose (seen,docs) window
      | S.member ident seen = (seen,docs)
      | Just doc<-M.lookup ident (buffers desktop),syntaxDocument doc = (S.insert ident seen,docs++[(ident,doc)])
      | otherwise = (seen,docs)
      where ident=bufferId window
    request (ident,doc)=do
      buffer<-evaluate (documentBuffer doc)
      identity<-makeStableName buffer
      pure (Request (Key ident (revision buffer) (documentSyntaxPath doc) identity) buffer)
    install ready docs (Request key@(Key ident _ _ _) _) = case M.lookup ident ready of
      Just (completed,Just (rows,width))
        | key==completed, Just doc<-M.lookup ident docs, Nothing<-documentSourceRows doc ->
            M.insert ident doc {documentSourceRows=Just rows,documentWidth=width} docs
      _ -> docs
