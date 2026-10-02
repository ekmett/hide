{-# LANGUAGE OverloadedStrings #-}
module HighlightingCheck (checks) where

import Control.Concurrent
import Control.Exception (evaluate,finally)
import Control.Monad (unless,foldM)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust,isJust)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Timeout (timeout)
import System.Mem.StableName (makeStableName)
import THC.Edit.Buffer
import THC.Edit.Files
import THC.Edit.Highlighting
import THC.Edit.Model
import THC.Edit.Render (snapshot)
import THC.Edit.Syntax

checks :: IO ()
checks = do
  let initial=addDocument (Just (FileState "example.py" Nothing)) (newBuffer "def old():\n    return 1\n") (initialDesktop (80,25))
      doc d=fromJust (activeDocument d)
      replace text d=d {buffers=M.adjust (\old->restyle old {documentBuffer=newBuffer text}) 1 (buffers d)}
      ready=isJust . documentSourceRows . doc
  check "new source has no lazy tokenizer to force on the display thread" (null (documentHighlight (doc initial)) && not (ready initial))
  firstStarted<-newEmptyMVar
  nextStarted<-newEmptyMVar
  releaseFirst<-newEmptyMVar
  releaseNext<-newEmptyMVar
  calls<-newIORef ([]::[T.Text])
  let tokenizer path text=do
        index<-atomicModifyIORef' calls (\old->(old++[text],length old))
        if index==0 then putMVar firstStarted () >> takeMVar releaseFirst
                    else putMVar nextStarted () >> takeMVar releaseNext
        pure (highlightFor path text)
  withHighlightingUsing tokenizer $ \worker -> do
    first<-tickHighlighting worker initial
    bounded "first tokenizer starts" (takeMVar firstStarted)
    edited<-bounded "rapid edits never wait for the blocked tokenizer" $ foldM
      (\d n->tickHighlighting worker (replace ("def newest"<>T.pack (show n)<>"():\n    return 2\n") d)) first [1::Int ..20]
    check "replacement has same revision but new content" (revision (documentBuffer (doc edited))==revision (documentBuffer (doc initial)))
    check "pending view immediately renders current plain text" ("def newest20" `T.isInfixOf` snapshot edited && not (ready edited))
    putMVar releaseFirst ()
    bounded "only latest queued revision begins next" (takeMVar nextStarted)
    stale<-tickHighlighting worker edited
    check "completed old identity cannot color a replacement with the same revision" (not (ready stale))
    putMVar releaseNext ()
    colored<-await worker ready stale
    requested<-readIORef calls
    check "rapid edits coalesce to first and newest requests" (length requested==2 && last requested==contents (documentBuffer (doc edited)))
    let rows=fromJust (documentSourceRows (doc colored))
    check "accepted rows contain current source and language styles" (map fst (rows V.! 0)=="def newest20():" && take 3 (map snd (rows V.! 0))==replicate 3 Keyword)
    before<-evaluate (buffers colored) >>= makeStableName
    unchanged<-tickHighlighting worker colored
    after<-evaluate (buffers unchanged) >>= makeStableName
    check "completed highlighting preserves unchanged document sharing" (before==after)
    let typed=insertText "x" colored
    check "editing invalidates accepted source rows immediately" (not (ready typed) && null (documentHighlight (doc typed)))
  withHighlighting $ \worker -> do
    colored<-await worker ready initial
    let renamed=colored {buffers=M.map (\d->restyle d {documentFile=Just (FileState "example.unknown" Nothing)}) (buffers colored)}
    plain<-await worker ready renamed
    check "filename changes invalidate syntax selection" (all ((==Plain).snd) (V.concatMap V.fromList (fromJust (documentSourceRows (doc plain)))))
  attempts<-newIORef (0::Int)
  timedOut<-newEmptyMVar
  withHighlightingUsing (\path text->do
    attempt<-atomicModifyIORef' attempts (\n->(n+1,n))
    if attempt==0 then (threadDelay 3000000 >> pure []) `finally` putMVar timedOut ()
      else pure (highlightFor path text)) $ \worker -> do
    started<-tickHighlighting worker initial
    expired<-timeout 4000000 (takeMVar timedOut)
    check "background tokenizer has a bounded runtime" (isJust expired)
    mapM_ (\_ -> tickHighlighting worker started >> threadDelay 1000) [1::Int ..30]
    count<-readIORef attempts
    check "timed out unchanged source is not rescheduled every tick" (count==1)
    retried<-await worker ready (replace "def retry(): return 3" started)
    retries<-readIORef attempts
    check "an edit retries highlighting after timeout" (ready retried && retries==2)
  entered<-newEmptyMVar
  stopped<-newEmptyMVar
  never<-newEmptyMVar
  bounded "closing highlighting cancels its blocked worker" $ withHighlightingUsing
    (\_ _->(putMVar entered () >> takeMVar never) `finally` putMVar stopped ())
    (\worker->tickHighlighting worker initial >> takeMVar entered)
  bounded "worker cancellation cleanup completes" (takeMVar stopped)
  let lineCount=100000
      text=T.replicate lineCount "prefix\n"<>"TAIL"
      deep=addDocument Nothing (newBuffer text) (initialDesktop (80,25))
      indexed=deep {buffers=M.map (\d->d {documentHighlight=error "flat syntax traversed",documentWidth=8,
        documentSourceRows=Just (V.generate (lineCount+1) (\n->if n==lineCount then [('T',Keyword),('A',Keyword),('I',Keyword),('L',Keyword)] else error "offscreen syntax forced"))}) (buffers deep),
        windows=map (\w->w {scrollRow=lineCount}) (windows deep)}
  check "deep source scroll indexes only the visible highlighted row" ("TAIL" `T.isInfixOf` snapshot indexed)
  let pending=indexed {buffers=M.map (\d->d {documentSourceRows=Nothing}) (buffers indexed)}
  check "deep pending source scroll reads visible buffer rows without forcing syntax" ("TAIL" `T.isInfixOf` snapshot pending)
  let narrow=modifyActive (\w->w {bounds=Rect 0 1 20 10}) (addDocument Nothing (newBuffer (T.replicate 100 "x")) (initialDesktop (80,25)))
      end=moveTo False 100 narrow
  check "pending width still permits scrolling to the caret" (maybe False (\w->scrollColumn w>0 && windowDocumentWidth (doc end) w>=100) (activeWindow end))
  _<-evaluate (T.length (snapshot end))
  putStrLn "highlighting checks passed"

bounded :: String -> IO a -> IO a
bounded label action=timeout 1000000 action >>= maybe (error label) pure

await :: Highlighting -> (Desktop -> Bool) -> Desktop -> IO Desktop
await worker done start=timeout 4000000 (loop start) >>= maybe (error "highlighting result timed out") pure
  where loop d=do
          next<-tickHighlighting worker d
          if done next then pure next else threadDelay 10000 >> loop next

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)
