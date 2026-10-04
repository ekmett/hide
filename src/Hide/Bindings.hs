{-# LANGUAGE OverloadedStrings #-}
-- | Prepared shortcut lookup, independent of the desktop and command payloads.
--
-- Configuration replaces all chords for one command; an empty list removes them.
-- Compilation rejects ambiguous chords before publishing a table. Lookup touches
-- only the chord index, never a buffer or desktop. Modal ownership stays with the
-- caller. Context selection and ordinary typing remain with the input owner.
module Hide.Bindings (BindingContext(..), contextName, bindingContexts, Bindings, compileBindings, bindingAction, bindingKeys, bindingEntries, chordName, readChord) where

import Control.Monad (foldM, unless)
import Data.Char (toLower, isPrint)
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Text.Read (readMaybe)

-- | Focused input owners. Modal controls keep their separate authority boundary.
data BindingContext = SourceKeys | SidebarKeys | ConversationKeys | MessagesKeys | DebuggerKeys | TerminalKeys deriving (Eq,Ord,Show)

bindingContexts :: [BindingContext]
bindingContexts = [SourceKeys,SidebarKeys,ConversationKeys,MessagesKeys,DebuggerKeys,TerminalKeys]

contextName :: BindingContext -> Text
contextName context = case context of
  SourceKeys -> "source"; SidebarKeys -> "sidebar"; ConversationKeys -> "conversation"
  MessagesKeys -> "messages"; DebuggerKeys -> "debugger"; TerminalKeys -> "terminal"

data Bindings a = Bindings !(M.Map Text a) [(Text,a,[Text])] deriving (Eq,Show)

-- | Resolve command names and reject unknown actions, malformed chords and
-- collisions. Callers merge configuration layers per command before compiling.
compileBindings :: [(Text,a,[Text])] -> M.Map Text [Text] -> Either Text (Bindings a)
compileBindings defaults overrides = do
  unless (all (`elem` map (\(name,_,_)->name) defaults) (M.keys overrides))
    (Left ("Unknown keybinding command: "<>T.intercalate ", " [name | name<-M.keys overrides,name `notElem` map (\(entry,_,_)->entry) defaults]))
  entries <- traverse prepare defaults
  index <- foldM insert M.empty [(key,(name,action)) | (name,action,keys)<-entries,key<-keys]
  pure (Bindings (fmap snd index) entries)
  where
    prepare (name,action,keys)=do
      parsed<-traverse parseChord (M.findWithDefault keys name overrides)
      pure (name,action,nub parsed)
    insert index (key,value@(name,_))=case M.lookup key index of
      Nothing -> Right (M.insert key value index)
      Just (other,_) -> Left ("Keybinding "<>key<>" conflicts between "<>other<>" and "<>name)

bindingAction :: Bindings a -> V.Key -> [V.Modifier] -> Maybe a
bindingAction (Bindings index _) key mods=chordName key mods >>= (`M.lookup` index)

bindingKeys :: Eq a => Bindings a -> a -> [Text]
bindingKeys (Bindings _ entries) action=concat [keys | (_,candidate,keys)<-entries,candidate==action]

-- | The complete effective map in catalog order, including explicit unbinding.
-- Intended for worker-owned inspection, not a per-frame walk of command labels.
bindingEntries :: Bindings a -> [(Text,[Text])]
bindingEntries (Bindings _ entries)=[(name,keys) | (name,_,keys)<-entries]

-- | Canonical labels are also lookup keys. Uppercase character events are
-- normalized; Shift remains an explicit modifier, independent of list order.
chordName :: V.Key -> [V.Modifier] -> Maybe Text
chordName key mods
  | any (`notElem` [V.MCtrl,V.MAlt,V.MShift]) mods = Nothing
  | otherwise = (prefix<>) <$> name
  where
    prefix=T.concat [label<>"+" | (modifier,label)<-[(V.MCtrl,"Ctrl"),(V.MAlt,"Alt"),(V.MShift,"Shift")],modifier `elem` mods]
    name=case key of
      V.KChar ' '->Just "Space"
      V.KChar '\t'->Just "Tab"
      V.KChar c->Just (T.toUpper (T.singleton (toLower c)))
      V.KFun n | n>=1 && n<=24 -> Just ("F"<>T.pack (show n))
      _ -> lookup key [(V.KEnter,"Enter"),(V.KEsc,"Escape"),(V.KIns,"Insert"),(V.KDel,"Delete"),(V.KBS,"Backspace"),(V.KLeft,"Left"),(V.KRight,"Right"),(V.KUp,"Up"),(V.KDown,"Down"),(V.KHome,"Home"),(V.KEnd,"End"),(V.KPageUp,"PageUp"),(V.KPageDown,"PageDown")]

parseChord :: Text -> Either Text Text
parseChord raw=do
  (key,mods)<-readChord raw
  maybe (Left ("Invalid chord "<>raw)) Right (chordName key mods)

-- | Parse an explicit chord; platform routing decides which keys it may own.
readChord :: Text -> Either Text (V.Key,[V.Modifier])
readChord raw=do
  let parts=T.splitOn "+" raw
      keyName=T.toLower (last parts)
      names=init parts
  mods<-traverse (\name->case T.toLower name of "ctrl"->Right V.MCtrl; "alt"->Right V.MAlt; "shift"->Right V.MShift; _->Left ("Unknown modifier in "<>raw)) names
  unless (length mods==length (nub mods)) (Left ("Repeated modifier in "<>raw))
  key<-case lookup keyName [("space",V.KChar ' '),("tab",V.KChar '\t'),("enter",V.KEnter),("escape",V.KEsc),("insert",V.KIns),("delete",V.KDel),("backspace",V.KBS),("left",V.KLeft),("right",V.KRight),("up",V.KUp),("down",V.KDown),("home",V.KHome),("end",V.KEnd),("pageup",V.KPageUp),("pagedown",V.KPageDown)] of
    Just value->Right value
    Nothing | T.length keyName==1, isPrint (T.head keyName)->Right (V.KChar (T.head keyName))
            | Just digits<-T.stripPrefix "f" keyName,Just n<-readMaybe (T.unpack digits),n>=1,n<=24->Right (V.KFun n)
            | otherwise->Left ("Unknown key in "<>raw)
  pure (key,mods)
