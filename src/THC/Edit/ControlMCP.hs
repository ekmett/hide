{-# LANGUAGE OverloadedStrings #-}
module THC.Edit.ControlMCP (controlTools, controlToolNames, controlTool) where
import Control.Monad (unless,foldM)
import Data.Aeson
import Data.Aeson.Types (parseEither,Parser)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import THC.Edit.Defaults (parseDefaults)
import THC.Edit.MCPPermissions (readEditorDefaults,writeEditorDefaults)
import THC.Edit.GuestAccess
import THC.Edit.Model
import qualified THC.Edit.Protocol as P
import THC.Edit.Frontend (modeSize)

controlToolNames :: [T.Text]
controlToolNames=["editor_input","editor_settings"]
controlTools :: [Value]
controlTools=
  [object ["name" .= ("editor_input"::T.Text),"description" .= ("Operate the editor using up to 64 mouse/key/paste events through its normal input path. Coordinates are 0-based character cells. Mouse actions: down/up/move/wheel-up/wheel-down; button0 left,2 right. Keys include characters, Enter, Escape, Tab, ArrowUp/Down/Left/Right, F1..F24, Home/End/PageUp/PageDown/Backspace/Delete/Insert. mods is an array of ctrl/alt/shift. Events may edit, save or run commands; applied sequentially, not transactionally. Conversation input, agent settings, approvals and Streamer mode require human input. Copy/paste uses an isolated clipboard for this batch, never the human clipboard."::T.Text),
   "inputSchema" .= object ["type" .= ("object"::T.Text),"required" .= ["events"::T.Text],"additionalProperties" .= False,"properties" .= object ["events" .= object ["type" .= ("array"::T.Text),"minItems" .= (1::Int),"maxItems" .= (64::Int),"items" .= object ["type" .= ("object"::T.Text)]]]],"annotations" .= annotations False],
   object ["name" .= ("editor_settings"::T.Text),"description" .= ("Read session display/editing settings as JSON, or apply a validated settings object. Fields: appearance(light/dark/system), screenMode(3/259), columns(40..512), rows(12..256), wordStar, blinkCursor, crtFilter, pixelateUnicode, materialIcons. Omitted fields remain unchanged; selecting screenMode defaults to its 80x25/80x50 grid unless dimensions supplied. Backend/window pixel scale and agent configuration are not changed. Changes remain with the resumable session. The optional defaults object merges startup settings into global [editor.defaults]; it accepts these fields plus backend(terminal/auto/metal/vulkan/web/remote) and scale(1..8). Defaults affect future sessions. Responses include startupDefaults."::T.Text),
    "inputSchema" .= object ["type" .= ("object"::T.Text),"additionalProperties" .= False,"properties" .= object ["settings" .= object ["type" .= ("object"::T.Text)],"defaults" .= object ["type" .= ("object"::T.Text)]]],"annotations" .= annotations False]]
  where annotations readonly=object ["readOnlyHint" .= readonly,"destructiveHint" .= not readonly,"openWorldHint" .= not readonly]

controlTool :: (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> T.Text -> Value -> IO (Desktop,IO (Either T.Text Value))
controlTool apply d name args=case parseEither parse args of
  Left err -> pure (d,pure (Left (T.pack err)))
  Right (Left (settings,defaults)) -> case (parseEither (parseSettings d) settings,traverse (parseEither parseDefaults) defaults) of
    (Left err,_) -> pure (d,pure (Left (T.pack err)))
    (_,Left err) -> pure (d,pure (Left (T.pack err)))
    (Right updated,Right _) -> do
      saved<-maybe (pure (Right ())) writeEditorDefaults defaults
      case saved of
        Left err -> pure (d,pure (Left err))
        Right () -> do
          loaded<-readEditorDefaults
          case loaded of
            Left err -> pure (d,pure (Left err))
            Right value -> pure (updated,pure (Right (case settingsValue updated of Object o -> Object (KM.insert "startupDefaults" value o); result -> result)))
  Right (Right events) -> do
    (updated,count,stopped,err)<-foldM run (beginGuestInput d,0::Int,False,Nothing) events
    pure (if count==0 then d else endGuestInput d updated,pure (Right (object ["appliedEvents" .= count,"exitRequested" .= stopped,"error" .= err,"clipboard" .= T.take 131072 (clipboard updated),"settings" .= settingsValue updated])))
  where
    parse :: Value -> Parser (Either (Value,Maybe Value) [P.WebInput])
    parse=withObject "controls" $ \o -> case name of
      "editor_settings" -> do
        unless (all (`elem` ["settings","defaults"]) (KM.keys o)) (fail "Unknown argument")
        defaults<-o .:? "defaults"
        case defaults of Just (Object values) | KM.member "streamerMode" values -> fail "Streamer mode requires human input"; _ -> pure ()
        Left . (,defaults) <$> o .:? "settings" .!= object []
      "editor_input" -> do
        unless (all (`elem` ["events"]) (KM.keys o)) (fail "Unknown argument")
        events<-o .: "events"
        unless (not (null events) && length events<=64) (fail "Expected 1..64 events")
        Right <$> mapM event events
      _ -> fail "Unknown control tool"
    event value=do
      kind<-withObject "input" (.: "type") value :: Parser T.Text
      unless (kind `elem` ["key","paste","mouse","modifiers","blur"]) (fail "Use key, paste, mouse, modifiers or blur")
      withObject "input" (\o -> unless (all (`elem` allowed kind) (KM.keys o)) (fail "Unknown event field")) value
      input<-P.parseInput value
      case input of
        P.Key key _ -> unless (T.length key==1 || key `elem` (["Enter","Escape","Tab","ArrowUp","ArrowDown","ArrowLeft","ArrowRight","Home","End","PageUp","PageDown","Backspace","Delete","Insert"]++["F"<>T.pack (show n) | n<-[1::Int ..24]])) (fail "Unknown key")
        P.Paste text -> unless (T.length text<=65536) (fail "Typing is limited to 64 Ki characters per event")
        _ -> pure ()
      pure input
    allowed kind=case kind of
      "key" -> ["type","key","mods"]
      "paste" -> ["type","text"]
      "mouse" -> ["type","action","x","y","button","clicks","mods"]
      "modifiers" -> ["type","mods"]
      _ -> ["type"]
    run state@(_,_,True,_) _=pure state
    run state@(_,_,_,Just _) _=pure state
    run (current,count,False,Nothing) input=case P.applyGuestInput input current {browserFrontend=False} of
      Left err -> pure (current,count,False,Just err)
      Right (changed,effects) -> do
        (stopped,updated)<-apply changed {browserFrontend=browserFrontend current} effects
        pure (updated,count+1,stopped,Nothing)

parseSettings :: Desktop -> Value -> Parser Desktop
parseSettings d=withObject "settings" $ \o -> do
  unless (all (`elem` ["appearance","screenMode","columns","rows","wordStar","blinkCursor","crtFilter","pixelateUnicode","materialIcons"]) (KM.keys o)) (fail "Unknown setting")
  theme<-o .:? "appearance" .!= themeName (appearance d)
  selected<-maybe (fail "Expected light, dark or system") pure (lookup theme [("light",LightMode),("dark",DarkMode),("system",SystemMode)])
  mode<-o .:? "screenMode"
  unless (maybe True (`elem` [3,259]) mode) (fail "screenMode must be 3 or 259")
  let defaults=maybe (screenSize d) modeSize mode
  cols<-o .:? "columns" .!= fst defaults
  rows<-o .:? "rows" .!= snd defaults
  unless ((not (KM.member "columns" o) || cols>=40 && cols<=512) && (not (KM.member "rows" o) || rows>=12 && rows<=256)) (fail "Use 40..512 columns and 12..256 rows")
  wordstar<-o .:? "wordStar" .!= wordStar d
  blink<-o .:? "blinkCursor" .!= blinkCursor d
  crt<-o .:? "crtFilter" .!= crtFilter d
  pixelate<-o .:? "pixelateUnicode" .!= pixelateUnicode d
  material<-o .:? "materialIcons" .!= materialIcons d
  let resized=if (cols,rows)==screenSize d then d else resizeScreenMode (cols,rows) d
  pure resized {appearance=selected,videoMode=case mode of Nothing -> videoMode d; Just value -> Just value,
    wordStar=wordstar,blinkCursor=blink,crtFilter=crt,pixelateUnicode=pixelate,materialIcons=material}

settingsValue :: Desktop -> Value
settingsValue d=object ["appearance" .= themeName (appearance d),"screenMode" .= videoMode d,"columns" .= fst (screenSize d),"rows" .= snd (screenSize d),
  "streamerMode" .= streamerMode d,"wordStar" .= wordStar d,"blinkCursor" .= blinkCursor d,"crtFilter" .= crtFilter d,"pixelateUnicode" .= pixelateUnicode d,"materialIcons" .= materialIcons d]
themeName :: Appearance -> T.Text
themeName LightMode="light"
themeName DarkMode="dark"
themeName SystemMode="system"
