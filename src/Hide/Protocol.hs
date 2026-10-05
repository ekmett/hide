{-# LANGUAGE CPP, OverloadedStrings #-}
-- | Shared input schema, cell-frame compression and transport packet framing.
--
-- Input origin is assigned by the host. Frame compression uses the reconstructed
-- previous screen as its dictionary, regardless of the preceding packet encoding;
-- full-frame and changed-row candidates compete by encoded size. Byte-stream
-- framing is separate from frontend validation of decoded cells.
module Hide.Protocol where

import Data.Aeson (Value(..))
import qualified Data.ByteString as BS
#if defined(WITH_WEB) || defined(WITH_REMOTE)
import qualified Codec.Compression.Zlib.Raw as Z
import Control.Exception (evaluate)
import Control.Monad (unless, when)
import Data.Aeson hiding (Value)
import Data.Aeson.Types (Parser, Pair, parseEither)
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (nub)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty as V
import System.IO (Handle, hFlush)
import Hide.GuestAccess
import Hide.Model hiding (Paste)
import qualified Hide.Model as Model
import Hide.Buffer (dirty, newBuffer, newByteBuffer, selectedText)
import Hide.TextStyle
import Hide.Font
import Hide.Render (renderDesktop, renderCellRows)
import qualified Hide.Plugin.Menu as Plugin
import Hide.Commands (commandIdentifier)
import Hide.Unicode (CellSpan(..), graphemes, clusterWidth)

data WebInput = Key T.Text [V.Modifier] | Paste T.Text | Mouse T.Text Int Int Int Int [V.Modifier]
              | Wheel Int Int Int [V.Modifier] | SystemTheme Bool | FocusWindow Int | BrowserCommand Command | MenuCommand Command | ContributedMenu T.Text T.Text Integer | UploadFile T.Text BS.ByteString | Frontend (Maybe Int) Bool | OpenPath FilePath | Resize Int Int | SuspendSession | Blur | Modifiers [V.Modifier] deriving (Eq,Show)

-- | Validate an input packet and its bounds. Upload metadata starts with an empty
-- payload that transport code fills from the following binary packet.
parseInput :: Value -> Parser WebInput
parseInput = withObject "browser event" $ \o -> do
  kind <- o .: "type" :: Parser T.Text
  let mods = do
        values <- o .:? "mods" .!= [] :: Parser [T.Text]
        traverse (\v -> case v of "shift" -> pure V.MShift; "ctrl" -> pure V.MCtrl; "alt" -> pure V.MAlt; "cmd" -> pure V.MMeta; _ -> fail "Unknown modifier") values
  case kind of
    "menu" -> do
      name <- o .: "command"
      generation <- o .:? "generation" :: Parser (Maybe Int)
      case generation of
        Nothing -> maybe (fail "Unknown menu command") (pure . MenuCommand) (lookup name protocolMenuCommands)
        Just value -> do
          registry <- o .: "registry"
          unless (not (T.null name) && T.length name<=128 && T.all (>= ' ') name && value>0 && toInteger value<=9007199254740991 &&
            T.length registry==48 && T.all (`elem` ("0123456789abcdef"::String)) registry) (fail "Invalid contributed menu lifetime")
          pure (ContributedMenu name registry (toInteger value))
    "focus-window" -> do
      ident<-o .: "id"
      unless (ident>0 && ident<=2147483647) (fail "Invalid window identity")
      pure (FocusWindow ident)
    "theme" -> SystemTheme <$> o .: "dark"
    "command" -> do
      name <- o .: "command"
      maybe (fail "Unknown browser command") (pure . BrowserCommand) (lookup (name::T.Text) protocolBrowserCommands)
    "upload" -> do
      name <- o .: "name"
      unless (not (T.null name) && T.length name<=255 && T.all (\c -> c>=' ' && c/='/' && c/='\\') name && name/="." && name/="..") (fail "Invalid filename")
      pure (UploadFile name BS.empty)
    "key" -> do
      key <- o .: "key"
      unless (T.length key<=24) (fail "Invalid key")
      Key key <$> mods
    "paste" -> do
      text <- o .: "text"
      unless (T.length text<=1048576) (fail "Paste too large")
      pure (Paste text)
    "frontend" -> do
      mode <- o .:? "mode"
      unless (maybe True (`elem` [3,259]) mode) (fail "Invalid screen mode")
      Frontend mode <$> o .:? "mac" .!= False
    "open" -> do
      path <- o .: "path"
      unless (not (null path) && length path<=8192 && all (>=' ') path) (fail "Invalid file path")
      pure (OpenPath path)
    "resize" -> do
      w <- o .: "width"; h <- o .: "height"
      unless (w>=40 && w<=512 && h>=12 && h<=256) (fail "Invalid dimensions")
      pure (Resize w h)
    "mouse" -> do
      action <- o .: "action"; x <- o .: "x"; y <- o .: "y"
      button <- o .:? "button" .!= 0; clicks <- o .:? "clicks" .!= 1
      unless (action `elem` ["down","up","move","wheel-up","wheel-down"] && x>=(-1) && x<512 && y>=(-1) && y<256 && button>=0 && button<=2 && clicks>=0 && clicks<=3) (fail "Invalid mouse event")
      steps <- o .:? "steps" .!= 1
      unless (steps>=1 && steps<=256) (fail "Invalid wheel distance")
      if action `elem` ["wheel-up","wheel-down"] && steps/=1
        then Wheel x y (if action=="wheel-up" then steps else -steps) <$> mods
        else Mouse action x y button clicks <$> mods
    "suspend" -> pure SuspendSession
    "blur" -> pure Blur
    "modifiers" -> Modifiers <$> mods
    _ -> fail "Unknown event"

-- | Ordinary human input is a pure model transition.
applyInput :: WebInput -> Desktop -> (Desktop,[Effect])
applyInput = applyInputUnchecked

-- | Apply host-attributed agent input with policy refusals. The host brackets
-- the whole batch with beginGuestInput/endGuestInput under desktop serialization.
-- Identity validation is shallow IO and finishes before effects are dispatched.
-- Origin is assigned by the host, never parsed from JSON.
applyGuestInput :: WebInput -> Desktop -> IO (Either T.Text (Desktop,[Effect]))
applyGuestInput input d
  | not allowed = pure (Left "This editor control requires human input.")
  | otherwise = do
      valid<-guestTransitionAllowed d updated effects
      pure (if valid then Right (updated,effects) else Left "This editor action requires human input.")
  where
    (updated,rawEffects)=applyInputUnchecked input d
    effects=map (\effect->case effect of InvokeMenu reference _ target->InvokeMenu reference Plugin.AgentMenu target; LoadTree request _->LoadTree request Plugin.AgentMenu; InvokeTree trace reference _->InvokeTree trace reference Plugin.AgentMenu; _->effect) rawEffects
    allowed=not (guestModalBlocked d) && case input of
      Key name mods -> maybe False (\key->guestKeyAllowed d key mods) (inputKey name mods)
      Paste _ -> guestKeyboardAllowed d
      Mouse _ x y _ _ _ -> pointerAllowedAt d x y
      Wheel x y _ _ -> pointerAllowedAt d x y
      Blur -> True
      Modifiers _ -> guestKeyboardAllowed d
      BrowserCommand cmd -> guestKeyboardAllowed d && guestCommandAllowedIn d cmd
      MenuCommand cmd -> guestKeyboardAllowed d && guestCommandAllowedIn d cmd
      ContributedMenu name epoch generation -> guestKeyboardAllowed d && maybe False (guestCommandAllowedIn d) (contributedCommand name epoch generation d)
      _ -> False

applyInputUnchecked :: WebInput -> Desktop -> (Desktop,[Effect])
applyInputUnchecked input d = case input of
  SuspendSession -> (d,[]) -- The session owner checkpoints and stops, not the UI model.
  FocusWindow ident -> (activateEditorWindow ident d,[])
  SystemTheme value -> (d {systemDark=value},[])
  BrowserCommand cmd | dialogCommandAllowed cmd d ->
    let (next,effects)=runCommand cmd d {browserFrontend=True}
    in (next {browserFrontend=browserFrontend d},effects)
  BrowserCommand cmd | dialog d/=Nothing -> (d,[WriteBrowserClipboard "" | cmd `elem` [Copy,Cut]])
                     | menuCommandAvailable d cmd -> runCommand cmd d
                     | otherwise -> (d,[])
  MenuCommand cmd | menuCommandAvailable d cmd -> runCommand cmd d
  MenuCommand _ -> (d,[])
  ContributedMenu name epoch generation -> case contributedCommand name epoch generation d of
    Just cmd | menuCommandAvailable d cmd -> runCommand cmd d
    _ -> (d {status="Menu action is stale or unavailable."},[])
  UploadFile name bytes ->
    let b=case TE.decodeUtf8' bytes of
          Right text | not (BS.elem 0 bytes) -> newBuffer text
          _ -> newByteBuffer bytes
        opened=addDocument Nothing b d
    in (opened {buffers=M.adjust (\doc -> restyle doc {documentSuggestedName=Just (T.unpack name)}) (nextId d) (buffers opened),status="Dropped file opened; Download exports changes."},[])
  Key name mods -> maybe (d,[]) (\key -> handleEvent (V.EvKey key mods) d) (inputKey name mods)
  Paste text -> handleEvent (V.EvPaste (TE.encodeUtf8 text)) d
  Frontend mode mac -> (d {videoMode=mode,nativeMac=mac && mode/=Nothing},[])
  OpenPath path -> (d,[ReadPath path])
  Resize w h -> handleEvent (V.EvResize w h) d
  Blur -> hoverAt (-1) (-1) d {drag=Nothing,dragOriginal=Nothing,prefix=Nothing,buttonPressed=Nothing,heldModifiers=[]}
  Modifiers mods -> (d {heldModifiers=mods},[])
  Wheel x y steps mods -> wheelEvent x y steps mods d
  Mouse action x y button clicks mods -> case action of
    "move" | dialog d/=Nothing || drag d==Nothing -> hoverAt x y d
           | otherwise -> handleEvent (V.EvMouseDown x y V.BLeft mods) d
    "down" | button==0 && clicks>=2 -> handleDoubleClick x y d
           | otherwise -> handleEvent (V.EvMouseDown x y (if button==2 then V.BRight else V.BLeft) mods) d
    "up" -> handleEvent (V.EvMouseUp x y (Just (if button==2 then V.BRight else V.BLeft))) d
    "wheel-up" -> handleEvent (V.EvMouseDown x y V.BScrollUp mods) d
    "wheel-down" -> handleEvent (V.EvMouseDown x y V.BScrollDown mods) d
    _ -> (d,[])

inputKey :: T.Text -> [V.Modifier] -> Maybe V.Key
inputKey name keyMods = case T.unpack name of
  [ch] -> Just (V.KChar (if V.MCtrl `elem` keyMods || V.MAlt `elem` keyMods then toLower ch else ch))
  _ -> lookup name ([ ("F"<>T.pack (show n),V.KFun n) | n<-[1..24]] ++
    [("ArrowUp",V.KUp),("ArrowDown",V.KDown),("ArrowLeft",V.KLeft),("ArrowRight",V.KRight),
     ("Home",V.KHome),("End",V.KEnd),("PageUp",V.KPageUp),("PageDown",V.KPageDown),
     ("Enter",V.KEnter),("Escape",V.KEsc),("Backspace",V.KBS),("Delete",V.KDel),
     ("Insert",V.KIns),("Tab",if V.MShift `elem` keyMods then V.KBackTab else V.KChar '\t')])

-- Complete row-major snapshots feed DEFLATE in a stable order, refreshing its
-- history with unchanged cells as well as edits. No application-level move search.
frameRows :: Desktop -> [Value]
frameRows d = map (toJSON . spans 0 . toList) (toList (renderCellRows d))
  where
    spans _ []=[]
    spans x (cell:rest)=
      let (a,width,run)=case cell of
            CellText paint text -> (paint,T.length text,String text)
            CellGlyph paint text full start visible -> (paint,visible,toJSON (text,full,full/=clusterWidth text,start,visible))
          paint=textStyleFromAttr a
      in toJSON (x,textForeground paint,textBackground paint,textFlags paint,[run]):spans (x+width) rest

-- Both encodings use the reconstructed previous screen, never the previous
-- packet, as their dictionary. Screen rows contain only arrays, bounded integer
-- coordinates/colors, and Unicode strings: JSON.stringify reproduces this encoding.
frameDictionary :: [Value] -> BS.ByteString
frameDictionary rows = let bytes=BL.toStrict (encode rows)
                       in BS.drop (max 0 (BS.length bytes-32768)) bytes

frameCandidates :: Bool -> [Value] -> [Value] -> [Pair] -> [BL.ByteString]
frameCandidates reset old rows metadata =
  [packet (if reset then 0 else 1) allRows] ++ [packet 2 changed | not reset]
  where
    allRows=zip [0::Int ..] rows
    changed=[(y,r) | ((y,r),previous)<-zip allRows (map Just old++repeat Nothing), Just r/=previous]
    params=Z.defaultCompressParams {Z.compressLevel=Z.compressionLevel 8,
      Z.compressDictionary=if reset then Nothing else Just (frameDictionary old)}
    packet tag selected=BL.cons tag (Z.compressWith params (encode (object
      (["type" .= ("frame"::T.Text),"reset" .= reset,
        "rows" .= selected]++metadata))))

-- | Choose a compressed representation against the reconstructed previous frame.
framePacket :: Bool -> [Value] -> [Value] -> [Pair] -> BL.ByteString
framePacket reset old rows metadata = foldl1 smaller (frameCandidates reset old rows metadata)
  where smaller a b=if BL.length b<BL.length a then b else a

webDirty :: Desktop -> Bool
webDirty d = any (dirty . documentBuffer) (M.elems (buffers d)) || conversationHasDraft d


protocolVersion :: Int
protocolVersion = 1

protocolCommands :: [Command]
protocolCommands = [cmd | (_,_,items)<-menus, MenuItem _ _ cmd<-items]

-- Only named menu actions are remotely invocable through this route; no
-- arbitrary Read input and no positional command identities.
protocolMenuCommands :: [(T.Text,Command)]
protocolMenuCommands = [(name,cmd) | cmd<-nub protocolCommands, cmd/=Help, Just name<-[commandIdentifier cmd]]

-- Browser-owned clipboard/search shortcuts use the same identities as menus.
protocolBrowserCommands :: [(T.Text,Command)]
protocolBrowserCommands = [(name,cmd) | cmd<-[Quit,Model.Paste,Download,Copy,Cut,SelectAll,Undo,Redo,Find,Replace,Conversation,AgentNew,FindNext,FindPrevious], Just name<-[commandIdentifier cmd]]

editableDialogField :: Desktop -> Maybe Field
editableDialogField d=case dialog d of
  Just dg | field@TextArea{}:_<-drop (focus dg) (fields dg),editableArea field -> Just field
  _ -> Nothing

frameMetadata :: FilePath -> Desktop -> [Pair]
frameMetadata cwd d =
  ["title" .= applicationTitle cwd d,"size" .= screenSize d,"mode" .= videoMode d,
   "dirty" .= webDirty d,"cursor" .= cursor,"blink" .= blinkCursor d,
   "crt" .= crtFilter d,"pixelated" .= pixelateUnicode d,
   "selection" .= (case editableDialogField d of
     Just (TextArea _ True b sel _ _) -> selectedText sel b
     _ -> if dialog d/=Nothing then "" else clipboard (fst (runCommand Copy d {browserFrontend=False}))),
   "terminal" .= (activeTerminal d/=Nothing && dialog d==Nothing && menu d==Nothing),
   "wordstar" .= wordStar d,
   "bindingsActive" .= (bindingInputAvailable d && maybe False (const True) (effectiveBindings d)),
   "bindings" .= (focusedBindingChords d),
   "editorWindows" .= [object ["id" .= ident,"title" .= title,"selected" .= selected,"enabled" .= enabled] | (ident,title,selected,enabled)<-editorWindowEntries d],
   "menuState" .= [(ident,menuCommandAvailable d cmd) | (ident,cmd)<-protocolMenuCommands],
   "menuContributions" .= [object ["id" .= Plugin.menuName reference,"registry" .= Plugin.menuEpoch reference,"generation" .= Plugin.menuGeneration reference,
      "slot" .= Plugin.menuSlot item,"group" .= Plugin.menuGroup item,"order" .= Plugin.menuOrder item,
      "title" .= Plugin.menuTitle item,"key" .= menuShortcut d (MenuItem (Plugin.menuTitle item) (Plugin.menuKey item) (contributionCommand d item)),"enabled" .= menuCommandAvailable d (contributionCommand d item)]
      | item<-contributedMenus d,let reference=Plugin.menuReference item]]
  where cursor=case V.picCursor (renderDesktop d) of V.Cursor x y -> Just (x,y); _ -> Nothing

assetsPacket :: Font -> Double -> Value
assetsPacket font scale = object ["type" .= ("assets"::T.Text),"version" .= protocolVersion,"scale" .= scale,"menuCommands" .= map fst protocolMenuCommands,
  "glyphs" .= [(T.singleton c,glyphWidth g,glyphRows g) | (c,g)<-bitmapAtlas font]]

maxPacketSize :: Int
maxPacketSize = 16777217

-- | Write four-byte big-endian length, kind byte and payload.
-- The length includes the kind byte, making arbitrary binary data unambiguous.
-- Concurrent writers must provide external serialization.
writePacket :: Handle -> WirePacket -> IO ()
writePacket h packet = do
  let bytes=case packet of JsonPacket value -> BS.cons 0 (BL.toStrict (encode value)); BinaryPacket value -> BS.cons 1 value
      n=BS.length bytes
  unless (n<=maxPacketSize) (ioError (userError "Remote packet exceeds 16 MiB"))
  BS.hPut h (BS.pack [fromIntegral (n `shiftR` shift .&. 255) | shift<-[24,16,8,0]])
  BS.hPut h bytes
  hFlush h

-- | Read one framed packet. Clean boundary EOF is Nothing; truncation or an
-- invalid packet kind fails.
readPacket :: Handle -> IO (Maybe WirePacket)
readPacket h = do
  header<-exact 4
  if BS.null header then pure Nothing else do
    unless (BS.length header==4) (bad "Truncated remote packet header")
    let n=BS.foldl' (\a b->a*256+fromIntegral b) (0::Integer) header
    unless (n>=1 && n<=fromIntegral maxPacketSize) (bad "Invalid remote packet length")
    bytes<-exact (fromIntegral n)
    unless (BS.length bytes==fromIntegral n) (bad "Truncated remote packet")
    case BS.uncons bytes of
      Just (0,payload) -> Just . JsonPacket <$> either (bad . ("Invalid remote JSON: "++)) pure (eitherDecodeStrict' payload)
      Just (1,payload) -> pure (Just (BinaryPacket payload))
      _ -> bad "Unknown remote packet kind"
  where
    bad = ioError . userError
    exact count = go count []
    go 0 chunks = pure (BS.concat (reverse chunks))
    go n chunks = do
      chunk<-BS.hGet h n
      if BS.null chunk then pure (BS.concat (reverse chunks)) else go (n-BS.length chunk) (chunk:chunks)

-- | Reconstruct a bounded frame and validate row indices/completeness.
-- Native/frontend cell validation remains a separate boundary. Seed the raw
-- DEFLATE dictionary with a prepended stored block of previous-screen bytes,
-- then discard that prefix from the decompressed output.
decodeFrame :: [Value] -> BS.ByteString -> IO (Value,[Value])
decodeFrame old packet = do
  (tag,compressed)<-maybe (bad "Empty display packet") pure (BS.uncons packet)
  unless (tag<=2) (bad "Unknown display encoding")
  when (tag/=0 && null old) (bad "Display update has no previous frame")
  let dictionary=if tag==0 then BS.empty else frameDictionary old
      n=BS.length dictionary
      prefix=BL.pack (map fromIntegral [0,n .&. 255,n `shiftR` 8,(65535-n) .&. 255,(65535-n) `shiftR` 8])<>BL.fromStrict dictionary
      decoded=BL.take (67108864+1) (BL.drop (fromIntegral n) (Z.decompress (prefix<>BL.fromStrict compressed)))
  count<-evaluate (BL.length decoded)
  unless (count<=67108864) (bad "Display frame exceeds 64 MiB")
  value<-either (bad . ("Invalid display JSON: "++)) pure (eitherDecodeStrict (BL.toStrict decoded))
  rowPairs<-either bad pure (parseEither (withObject "frame" (\o -> o .: "rows")) value :: Either String [(Int,Value)])
  unless (all (\(y,_)->y>=0 && y<256) rowPairs && length rowPairs==M.size (M.fromList rowPairs)) (bad "Invalid display row indices")
  let rows=M.union (M.fromList rowPairs) (if tag==2 then M.fromList (zip [0..] old) else M.empty)
  unless (not (M.null rows) && M.keys rows==[0..M.size rows-1]) (bad "Incomplete display frame")
  pure (value,M.elems rows)
  where bad=ioError . userError

#endif

data WirePacket = JsonPacket Value | BinaryPacket BS.ByteString deriving (Eq,Show)
