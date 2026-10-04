{-# LANGUAGE OverloadedStrings #-}
module BufferEditsCheck (checks) where

import Control.Exception (evaluate)
import Control.Monad (unless, forM_)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Hide.Buffer
import Hide.Files
import Hide.Model
import qualified Hide.BufferEdits as Edits

checks :: IO ()
checks=do
  let file=FileState "/project/Main.hs" (Just "first\nsecond\n")
      b=newBuffer "first\nsecond\n"
      d=addDocument (Just file) b (initialDesktop (80,25))
      bid=maybe (error "missing window") bufferId (activeWindow d)
  patch<-Edits.prepareEdit (Just bid) file b [(0,5,"changed")] >>= right
  duplicate<-Edits.commitEdits [patch,patch] d
  check "host rejects duplicate live targets before adopting any edit" (isLeft duplicate)
  let otherFile=FileState "/project/Other.hs" (Just "other\n")
      other=newBuffer "other\n"
      both=addDocument (Just otherFile) other d
      otherId=maybe (error "missing other window") bufferId (activeWindow both)
  second<-Edits.prepareEdit (Just otherId) otherFile other [(0,5,"updated")] >>= right
  (applied,changes)<-Edits.commitEdits [patch,second] both >>= right
  check "each target receives one ordinary Undo and remains unsaved"
    (all (\(ident,old)->let current=bufferAt ident applied in
      contents (undo current)==old && length (undoStack current)==1 && dirty current)
      [(bid,"first\nsecond\n"),(otherId,"other\n")] && length changes==2)
  let moved=both {windows=map (\w->w {selection=Selection 1 5,bounds=Rect 3 4 20 10}) (windows both)}
  (rebased,_)<-Edits.commitEdits [patch,second] moved >>= right
  check "selection rebasing uses current navigation and preserves geometry"
    (all (\w->bounds w==Rect 3 4 20 10 && selection w==Selection 7 7) (windows rebased))
  forM_ [ ("replacement with equal revision and contents",both {buffers=M.adjust (\doc->doc {documentBuffer=(newBuffer (T.copy (contents b))) {revision=revision b}}) bid (buffers both)})
           , ("edited target",both {buffers=M.adjust (\doc->doc {documentBuffer=replaceSelection (Selection 0 0) "x" other}) otherId (buffers both)})
           , ("closed target",both {buffers=M.delete otherId (buffers both)})
           , ("private target",both {guestPrivatePaths=[filePath otherFile]})
           , ("ambiguous target",addDocument (Just file) (newBuffer "duplicate") both)
           , ("changed file baseline",both {buffers=M.adjust (\doc->doc {documentFile=Just (FileState (filePath file) (Just "external"))}) bid (buffers both)})
           ] $ \(label,current)->do
    rejected<-Edits.commitEdits [patch,second] current
    check ("host batch rejects "++label++" atomically") (isLeft rejected)
  let closedFile=FileState "/project/Closed.hs" (Just "closed\n")
  closed<-Edits.prepareEdit Nothing closedFile (newBuffer "closed\n") [(0,6,"opened")] >>= right
  (opened,_)<-Edits.commitEdits [closed] both >>= right
  check "closed-file preparation opens one ordinary unsaved Undo buffer"
    (let current=bufferAt (nextId both) opened in dirty current && contents (undo current)=="closed\n")
  nowOpen<-Edits.commitEdits [closed] (addDocument (Just closedFile) (newBuffer "closed\n") both)
  check "opening a closed target invalidates its prepared edit" (isLeft nowOpen)
  closedDuplicate<-Edits.commitEdits [closed,closed] both
  check "host rejects duplicate closed-file targets" (isLeft closedDuplicate)
  overlapping<-Edits.prepareEdit (Just bid) file b [(0,4,"a"),(3,5,"b")]
  check "worker preparation rejects overlapping ranges" (isLeft overlapping)
  sameStart<-Edits.prepareEdit (Just bid) file b [(0,0,"a"),(0,0,"b")]
  check "worker preparation rejects duplicate edit starts" (isLeft sameStart)
  empty<-Edits.prepareEdit (Just bid) file b [] >>= right
  (unchanged,none)<-Edits.commitEdits [empty] d >>= right
  check "empty edit retains revision and creates no Undo"
    (null none && revision (bufferAt bid unchanged)==revision b && null (undoStack (bufferAt bid unchanged)))
  let opaque=b {saved=error "edit forced saved text",undoStack=error "edit forced Undo",redoStack=error "edit forced Redo"}
      opaqueFile=file {diskBytes=error "adoption forced disk bytes"}
      hidden=addDocument (Just opaqueFile) opaque (initialDesktop (80,25))
  held<-Edits.prepareEdit (Just bid) opaqueFile opaque [(0,5,"changed")] >>= right
  (adopted,_)<-Edits.commitEdits [held] hidden >>= right
  _<-evaluate (revision (bufferAt bid adopted))
  check "worker preparation and adoption do not force saved text or histories"
    (bufferSlice (bufferAt bid adopted) 0 7=="changed")
  putStrLn "host buffer edit checks passed"
  where
    check label ok=unless ok (error ("host buffer edits: "++label))
    right=either (error . T.unpack) pure
    isLeft (Left _)=True
    isLeft _=False
    bufferAt bid d=documentBuffer (buffers d M.! bid)
