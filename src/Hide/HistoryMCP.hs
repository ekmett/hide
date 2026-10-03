{-# LANGUAGE OverloadedStrings #-}
-- | Bounded undo previews and revision-checked history application for agents.
--
-- Previews describe the edits that undo/redo would apply and do not mutate the
-- buffer. Application uses the normal edit path, keeps the user's window ordering
-- and focus, and rejects private/read-only buffers or insufficient history before
-- performing any steps. Undo changes memory; it does not save files.
module Hide.HistoryMCP (historyTools, historyTool, historyToolNames) where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import Data.List (find)
import qualified Data.Text as T
import Numeric (showHex)
import Hide.Buffer
import Hide.GuestAccess (protectedBuffer)
import Hide.Model

historyToolNames :: [T.Text]
historyToolNames=["editor_history","editor_undo"]
-- | Schemas for diff-style history previews and revision-guarded undo/redo.
historyTools :: [Value]
historyTools=[describe "editor_history" True "Inspect undo/redo entries as bounded diff-style previews, newest first. Steps describe changes that would be applied in that direction. Previews do not modify the buffer." ["bufferId"] [("offset",integer),("limit",integer)],
  describe "editor_undo" False "Apply undo or redo steps to a live buffer without saving. Requires its current revision, fails atomically if too few steps exist." ["bufferId","revision"] [("revision",integer),("steps",integer)]]
  where
    integer=object ["type" .= ("integer"::T.Text)]
    describe :: T.Text -> Bool -> T.Text -> [T.Text] -> [(T.Text,Value)] -> Value
    describe name readonly description required props=object ["name" .= name,"description" .= description,
      "inputSchema" .= object ["type" .= ("object"::T.Text),"properties" .= object (["bufferId" .= integer,"direction" .= object ["type" .= ("string"::T.Text),"enum" .= (["undo","redo"]::[T.Text])]]++[K.fromText k .= v | (k,v)<-props]),"required" .= required,"additionalProperties" .= False],
      "annotations" .= object ["readOnlyHint" .= readonly,"destructiveHint" .= not readonly,"openWorldHint" .= False]]

-- | Inspect a history page or apply a checked number of steps to a live buffer.
-- The returned continuation carries the reply; mutations occur in the initial phase.
historyTool :: Desktop -> T.Text -> Value -> IO (Desktop,IO (Either T.Text Value))
historyTool d name args=pure $ case parseEither parse args of
  Left err -> (d,pure (Left (T.pack err)))
  Right (bid,direction,offset,count,wanted) -> case M.lookup bid (buffers d) of
    Nothing -> bad "Buffer not found."
    Just _ | protectedBuffer d bid -> bad "This buffer is private to the user."
    Just doc | documentLabel doc/=Nothing -> bad "This buffer is read-only."
    Just doc ->
      let b=documentBuffer doc
          (step,available)=if direction=="undo" then (undo,length (undoStack b)) else (redo,length (redoStack b))
          info changed=object ["bufferId" .= bid,"revision" .= revision changed,"modified" .= dirty changed,"binary" .= byteMode changed,
            "undoCount" .= length (undoStack changed),"redoCount" .= length (redoStack changed)]
      in if name=="editor_history" then
        let states=take (available+1) (iterate step b)
            entries=[preview number before after | (number,(before,after))<-take count (drop offset (zip [1::Int ..] (zip states (drop 1 states))))]
        in (d,pure (Right (object ["buffer" .= info b,"direction" .= direction,"offset" .= offset,"entries" .= entries,"total" .= available])))
      else if wanted/=Just (revision b) then bad "Buffer revision changed; inspect it before applying history."
      else if count>available then bad "Not enough history entries; no changes applied."
      else case find ((==bid) . bufferId) (windows d) of
        Nothing -> bad "Buffer has no open window."
        Just w ->
          let focused=focusWindow (windowId w) d
              updated=iterate (editActive (const step) Nothing) focused !! count
              byId=M.fromList [(windowId win,win) | win<-windows updated]
              restored=updated {windows=[M.findWithDefault win (windowId win) byId | win<-windows d],sideTree=sideTree d,problemsFocused=problemsFocused d}
              changed=maybe b documentBuffer (M.lookup bid (buffers updated))
          in (restored,pure (Right (info changed)))
  where
    bad err=(d,pure (Left err))
    parse=withObject "history" $ \o -> do
      unless (name `elem` historyToolNames) (fail "Unknown history tool")
      let allowed=if name=="editor_history" then ["bufferId","direction","offset","limit"] else ["bufferId","direction","revision","steps"]
      unless (all (`elem` allowed) (KM.keys o)) (fail "Unknown argument")
      bid<-o .: "bufferId"
      direction<-o .:? "direction" .!= ("undo"::T.Text)
      unless (direction `elem` ["undo","redo"]) (fail "Expected undo or redo")
      offset<-o .:? "offset" .!= 0
      count<-if name=="editor_history" then o .:? "limit" .!= 5 else o .:? "steps" .!= 1
      wanted<-if name=="editor_undo" then Just <$> o .: "revision" else pure Nothing
      unless (offset>=0 && offset<=100 && count>=1 && count<=if name=="editor_history" then 10 else 100) (fail "History offset 0..100, preview limit 1..10, apply steps 1..100")
      pure (bid,direction,offset,count,wanted)

preview :: Int -> Buffer -> Buffer -> Value
preview number before after=object ["step" .= number,"beforeBinary" .= byteMode before,"afterBinary" .= byteMode after,
  "diff" .= T.take 8192 diff,"truncated" .= (T.length diff>8192 || omitted)]
  where
    (a,z,n)=maybe (0,bufferLength before,bufferLength after) id (lastChange after)
    binary=byteMode before || byteMode after
    (diff,omitted)
      | binary = let old=bufferBytes before; new=bufferBytes after
                     -- Representation switches compare encoded bytes, not Latin-1 code points.
                     start=if byteMode before==byteMode after then a else 0
                     oldEnd=if byteMode before==byteMode after then z else BS.length old
                     newEnd=if byteMode before==byteMode after then a+n else BS.length new
                     showBytes prefix bytes end=T.unlines [prefix<>T.pack (showHex position "")<>": "<>T.intercalate " " [let h=showHex byte "" in T.pack (replicate (2-length h) '0'++h) | byte<-BS.unpack (BS.take (min 16 (end-position)) (BS.drop position bytes))] | position<-take 128 [start,start+16..end-1]]
                 in ("@@ bytes "<>t start<>" @@\n"<>showBytes "-" old oldEnd<>showBytes "+" new newEnd,oldEnd-start>2048 || newEnd-start>2048)
      | otherwise = let (row,_)=bufferLineColumn before a
                        (oldLast,_)=bufferLineColumn before z
                        (newLast,_)=bufferLineColumn after (a+n)
                        linesFor prefix b lastRow=T.unlines [prefix<>bufferLineAt b r | r<-take 128 [row..lastRow]]
                    in ("@@ -"<>t (row+1)<>","<>t (oldLast-row+1)<>" +"<>t (row+1)<>","<>t (newLast-row+1)<>" @@\n"<>linesFor "-" before oldLast<>linesFor "+" after newLast,oldLast-row>=128 || newLast-row>=128)
    t=T.pack . show
