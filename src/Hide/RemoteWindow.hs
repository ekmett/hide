{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables, BangPatterns #-}
-- |
-- Module      : Hide.RemoteWindow
-- Copyright   : (c) 2026 Edward Kmett
-- License     : BSD-2-Clause OR Apache-2.0
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : CPP, OverloadedStrings, ScopedTypeVariables, BangPatterns
--
-- Native SDL frontend for remotely owned editor sessions.
--
-- A receiver worker validates/decodes frames and downloads; SDL events and drawing
-- stay on the window thread. Draining pending messages allows one presentation of
-- the latest frame. Local pointer feedback and remote content updates have distinct
-- repaint rules, so drag/wheel rendering can await the resulting remote frame.
module Hide.RemoteWindow
  (runRemoteWindow, RemoteFrame(..), RemoteContribution(..), RemoteCell(..), parseRemoteFrame
  , RemoteCanvas(..), RemoteCanvasSurface(..), CanvasControl(..), CanvasReceiveState, emptyCanvasReceiveState, admitCanvasControl, validateCanvasChunk
  , nativeKeyInput, nativeEventInput, remoteBindingInput, remoteMenuInput, remoteNativeMenuInput, remoteDockWindowInput, remoteMenuLayout, sanitizeDownloadName
  , remoteInputAllowed, remoteCloseDetaches, remoteDetachShortcut, nativeRepaint) where
import Control.Monad (unless)
import Hide.Commands (commandIdentifier)
import Hide.Bindings (readChord, chordName)
import Data.Aeson hiding (withArray)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.Bits ((.&.), (.|.), shiftL)
import qualified Data.ByteString.Base64 as B64
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import qualified Graphics.Vty as V
import Hide.Frontend
import Hide.Model (Command(..), MenuItem(..), menus, menuContributionSlots)
import Data.List (elemIndex, nub)
import qualified Data.IntSet as IS
import Data.Maybe (fromMaybe)
import Hide.Window (nativeCommands, nativeMenuToken, nativeChordShortcut, nativeDockWindow)
import Hide.Remote (RemotePeer)
import Hide.Unicode (Script(..), scalarWidth, clusterWidth, graphemes)
import qualified Data.Vector as Vec
#if defined(WITH_WINDOW) && defined(WITH_REMOTE)
import Control.Concurrent.Async (withAsync, poll)
import Control.Concurrent.STM hiding (check)
import Control.Exception (bracket, bracket_, throwIO, IOException, catch, finally)
import Control.Monad (forM_, forever, when, foldM)
import Foreign (nullPtr, alloca, allocaArray, peek, peekArray, pokeArray)
import Foreign.C
import Data.IORef
import Data.Word (Word64)
import Data.List (mapAccumL)
import Hide.FrameTiming
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf (printf)
import System.Directory (getHomeDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)
import System.Info (os)
import System.Timeout (timeout)
import System.IO (withBinaryFile, IOMode(ReadMode), hFileSize, openBinaryTempFile, hClose, hPutStrLn, stderr)
import Hide.TextStyle
import Hide.Font
import Hide.Protocol (WirePacket(..), decodeFrame,parseClipboardRequest,clipboardReplyInput)
import Hide.Links (openResource)
import Hide.FileExport (FileExports,withFileExports,stageFileExport,startHelperFileExport)
import Hide.Remote (peerSendBatch, peerReceive)
import Hide.Window hiding (nativeCommands, nativeMenuToken, nativeChordShortcut, nativeDockWindow)
#endif

-- | Bounded host-issued catalogue metadata, independent of native menu slots.
data RemoteContribution = RemoteContribution
  { contributionId :: T.Text, contributionRegistry :: T.Text, contributionGeneration :: Integer
  , contributionSlot :: T.Text, contributionGroup :: T.Text, contributionOrder :: Int
  , contributionTitle :: T.Text, contributionKey :: T.Text, contributionEnabled :: Bool
  } deriving (Eq,Show)

-- | Validated visible spans. 'RemoteText' borrows a complete run of single
-- one-cell scalars, with its cell count. 'RemoteGlyph' retains one semantic
-- grapheme, full allocated width, clip start and visible width. Its origin is
-- visible x minus clip start; clipping never reshapes the glyph. 'RemoteScript'
-- retains natural atlas width and placement with an implicit one-cell advance.
data RemoteCell
  = RemoteText {-# UNPACK #-} !Int {-# UNPACK #-} !Int !TextStyle !T.Text {-# UNPACK #-} !Int
  | RemoteGlyph {-# UNPACK #-} !Int {-# UNPACK #-} !Int !TextStyle !T.Text
      {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | RemoteScript {-# UNPACK #-} !Int {-# UNPACK #-} !Int !TextStyle !T.Text {-# UNPACK #-} !Int !Script
  deriving (Eq,Show)

data RemoteFrame = RemoteFrame
  { remoteSize :: (Int,Int), remoteMode :: Maybe Int, remoteTitle :: T.Text
  , remoteCursor :: Maybe (Int,Int), remoteBlink :: Bool, remoteCRT :: Bool
  , remotePixelated :: Bool, remoteTerminal :: Bool, remoteWordStar :: Bool
  , remoteCanvas :: Maybe RemoteCanvas
  , remoteDialog :: Maybe BS.ByteString
  , remoteSidebar :: BS.ByteString, remoteExportView :: [Integer], remoteBindings :: [(T.Text,T.Text)]
  , remoteWindows :: [(Int,T.Text,Bool,Bool)]
  , remoteContributions :: [RemoteContribution], remoteMenus :: [Bool], remoteCells :: [RemoteCell]
  } deriving (Eq,Show)

-- | Validate dimensions, ordered nonoverlapping spans, colors, cursor and widths
-- before any remote cells reach native drawing.
parseRemoteFrame :: Value -> [Value] -> Either String RemoteFrame
parseRemoteFrame metadata rows = parseEither (withObject "frame metadata" $ \o -> do
  size@(cols,lines') <- o .: "size"
  unless (cols>=40 && cols<=512 && lines'>=12 && lines'<=256 && length rows==lines') (fail "Invalid frame dimensions")
  mode <- o .:? "mode"
  unless (maybe True (`elem` [3,259]) mode) (fail "Invalid screen mode")
  title <- o .:? "title" .!= "Haskell"
  unless (T.length title<=8192 && not (T.any (=='\0') title)) (fail "Invalid title")
  cursor <- o .:? "cursor"
  unless (maybe True (\(x,y) -> x>=0 && x<cols && y>=0 && y<lines') cursor) (fail "Invalid cursor")
  blink <- o .:? "blink" .!= True
  crt <- o .:? "crt" .!= False
  pixelated <- o .:? "pixelated" .!= False
  terminal <- o .:? "terminal" .!= False
  wordstar <- o .:? "wordstar" .!= False
  exportView <- o .:? "fileExportView" .!= []
  unless (length exportView<=13 && all (\n->n>=0 && n<=9007199254740991) exportView) (fail "Invalid file export view")
  sidebar <- o .:? "semanticSidebar"
  unless (maybe True (\value->case value of Object{}->True; _->False) sidebar) (fail "Invalid sidebar semantics")
  let sidebarBytes=maybe BS.empty (BL.toStrict . BL.take 2097153 . encode) (sidebar::Maybe Value)
  unless (BS.length sidebarBytes<=2097152) (fail "Oversized sidebar semantics")
  haptics <- o .:? "hapticFeedback" .!= False
  modal <- o .:? "semanticDialog" >>= maybe (pure Nothing) (parseRemoteDialog size haptics)
  bindings <- o .: "bindings"
  unless (length bindings<=8192 && all validBinding bindings) (fail "Invalid binding projection")
  supported <- o .:? "menuCommands" .!= [] :: Parser [T.Text]
  states <- o .:? "menuState" .!= [] :: Parser [(T.Text,Bool)]
  unless (length supported<=256 && length states<=256 && all ((<=256).T.length) (supported++map fst states)) (fail "Invalid menu state")
  contributions<-o .:? "menuContributions" .!= [] >>= mapM parseContribution
  unless (length contributions<=256 && length (nub (map contributionId contributions))==length contributions) (fail "Invalid contribution catalogue")
  let enabled=[maybe False (\name -> name `elem` supported && lookup name states==Just True) (commandIdentifier cmd) | cmd<-menuActions]
  windows<-o .:? "editorWindows" .!= [] >>= mapM (withObject "editor window" $ \entry->do
    ident<-entry .: "id"; windowTitle<-entry .: "title"; selected<-entry .: "selected"; windowEnabled<-entry .: "enabled"
    unless (ident>0 && ident<=2147483647 && T.length windowTitle<=8192 && not (T.any (\c->c<' ' || c=='\DEL') windowTitle)) (fail "Invalid editor window")
    pure (ident,windowTitle,selected,windowEnabled))
  unless (IS.size (IS.fromList [ident | (ident,_,_,_)<-windows])==length windows && length [() | (_,_,True,_)<-windows]<=1) (fail "Invalid editor window catalogue")
  canvas <- o .:? "canvas" >>= traverse (parseRemoteCanvas size)
  cells <- concat <$> sequence [parseRow cols y row | (y,row) <- zip [0..] rows]
  pure (RemoteFrame size mode title cursor blink crt pixelated terminal wordstar canvas modal sidebarBytes exportView bindings windows contributions (enabled++map contributionEnabled contributions) cells)) metadata
  where
    validBinding (chord,name)=T.length name<=256 && case readChord chord of Right (key,mods)->chordName key mods==Just chord; _->False
    parseContribution=withObject "menu contribution" $ \o->do
      ident<-o .: "id"; registry<-o .: "registry"; boundedGeneration<-o .: "generation" :: Parser Int
      let generation=toInteger boundedGeneration
      slot<-o .: "slot"; group<-o .: "group"; order<-o .: "order"
      title<-o .: "title"; key<-o .: "key"; enabled<-o .: "enabled"
      unless (all (\text->not (T.null text) && T.length text<=128 && T.all (>= ' ') text) [ident,slot,group,title] &&
        T.length registry==48 && T.all (`elem` ("0123456789abcdef"::String)) registry &&
        T.length key<=32 && T.all (>= ' ') key && generation>0 && generation<=9007199254740991 && slot `elem` menuContributionSlots) (fail "Invalid contributed menu entry")
      pure (RemoteContribution ident registry generation slot group order title key enabled)
    parseRow cols y value = do
      spans <- parseJSON value :: Parser [(Int,Int,Int,Int,[Value])]
      unless (length spans<=cols+1) (fail "Too many spans")
      reverse <$> foldRow cols y 0 [] spans
    foldRow _ _ _ acc [] = pure acc
    foldRow cols y previous acc ((x,fg,bg,flags,runs):rest) = do
      unless (x>=previous && x<=cols && flags>=0 && flags .&. 27==flags && all (\c -> c>=0 && c<=0xffffff) [fg,bg] && length runs<=cols+1) (fail "Invalid span")
      (next,cells)<-foldRuns cols y (TextStyle fg bg flags) x acc runs
      foldRow cols y next cells rest
    foldRuns _ _ _ at acc [] = pure (at,acc)
    foldRuns cols y paint at acc (String text:rest) = do
      let end=TU.lengthWord8 text
          count !offset !n
            | offset==end = Just n
            | n==512 = Nothing
            | otherwise = case TU.iter text offset of
                TU.Iter c bytes | c>=' ' && c/='\DEL' && scalarWidth c==1 -> count (offset+bytes) (n+1)
                _ -> Nothing
      case count 0 0 of
        Just n | n<=cols-at -> foldRuns cols y paint (at+n) (if n==0 then acc else RemoteText at y paint text n:acc) rest
        _ -> fail "Invalid character run"
    foldRuns cols y paint at acc (value@(Array fields):rest) | Vec.length fields==3 = do
      (text,natural,mode) <- parseJSON value :: Parser (T.Text,Int,T.Text)
      script <- case mode of "sup" -> pure Superscript; "sub" -> pure Subscript; _ -> fail "Invalid script placement"
      unless (not (T.null text) && T.length text<=32 && TU.lengthWord8 text<=128 && not (T.any (\c -> c<' ' || c=='\DEL') text) &&
        graphemes text==[text] && natural `elem` [1,2] && clusterWidth text==natural && at<cols) (fail "Invalid script grapheme")
      foldRuns cols y paint (at+1) (RemoteScript at y paint text natural script:acc) rest
    foldRuns cols y paint at acc (value:rest) = do
      (text,w,stretched,start,shown) <- parseJSON value :: Parser (T.Text,Int,Bool,Int,Int)
      unless (not (T.null text) && T.length text<=32 && TU.lengthWord8 text<=128 && not (T.any (\c -> c<' ' || c=='\DEL') text) && graphemes text==[text] && w>0 && w<=2 &&
        start>=0 && start<w && shown>0 && shown<=w-start && shown<=cols-at &&
        (if stretched then w==2 && clusterWidth text<2 else clusterWidth text==w)) (fail "Invalid grapheme")
      foldRuns cols y paint (at+shown) (RemoteGlyph at y paint text w start shown:acc) rest

-- | Validate a complete bounded read-only modal snapshot on the receiver worker.
-- A hidden modal remains present with no nodes; absence/dismissal restores the
-- ordinary surface projection. Structural IDs retain no input capability.
parseRemoteDialog :: (Int,Int) -> Bool -> Value -> Parser (Maybe BS.ByteString)
parseRemoteDialog size@(cols,rows) haptics value=withObject "dialog semantics" (\o->do
  present<-o .: "present"
  readOnly<-o .: "readOnly"
  (_::Bool)<-o .: "truncated"
  nodes<-o .: "nodes" :: Parser [Value]
  unless (readOnly && length nodes<=256 && (present || null nodes)) (fail "Invalid dialog snapshot")
  entries<-mapM node nodes
  let records=M.fromList [(ident,parent) | (ident,parent,_)<-entries]
      root=["dialog"]
      parentReachesRoot remaining ident
        | ident==root=True
        | remaining==0=False
        | otherwise=case M.lookup ident records of
            Just (Just parent)->parentReachesRoot (remaining-1) parent
            _->False
  unless (M.size records==length nodes && sum [count | (_,_,count)<-entries]<=32768 &&
    (null nodes || M.lookup root records==Just Nothing) &&
    all (\(ident,parent,_)->if ident==root then parent==Nothing else maybe False (parentReachesRoot (5::Int)) parent) entries)
    (fail "Invalid dialog identity, ancestry or text budget")
  let bytes=BL.toStrict (BL.take 524289 (encode (object ["dialog" .= value,"size" .= size,"hapticFeedback" .= haptics])))
  unless (BS.length bytes<=524288) (fail "Oversized dialog semantics")
  pure (if present then Just bytes else Nothing)) value
  where
    roles=["dialog","text","textbox","checkbox","radiogroup","radio","listbox","option","combobox","button"]::[T.Text]
    validIdentity ("dialog":parts)=length parts<=4 && all validPart parts
    validIdentity _=False
    validPart part=not (T.null part) && T.length part<=24 &&
      (part `elem` ["dialog","field","body","button","option"] || T.all (\c->c>='0' && c<='9') part)
    boundedText limit text=T.length text<=limit && not (T.any (=='\0') text)
    node=withObject "dialog node" $ \o->do
      ident<-o .: "id";parent<-o .: "parent";role<-o .: "role"
      name<-o .: "name";valueText<-o .: "value"
      (x,y,w,h)<-o .: "bounds" :: Parser (Int,Int,Int,Int)
      (_::Bool)<-o .: "focused";(_::Bool)<-o .: "multiline"
      (_::Maybe Bool)<-o .: "checked";(_::Maybe Bool)<-o .: "selected";(_::Maybe Bool)<-o .: "expanded"
      unless (validIdentity ident && maybe True validIdentity parent && role `elem` roles &&
        (if ident==["dialog"] then role=="dialog" && parent==Nothing else role/="dialog" && parent/=Nothing) &&
        boundedText 256 name && maybe True (boundedText 2048) valueText &&
        x>=0 && y>=0 && w>0 && h>0 && x<cols && y<rows && w<=cols-x && h<=rows-y) (fail "Invalid dialog node")
      pure (ident,parent,T.length name+maybe 0 T.length valueText)

-- | Complete, bounded image scene. Resource IDs identify retained pixels, never
-- input capabilities; the host window ID remains a separate field.
data RemoteCanvasSurface = RemoteCanvasSurface
  { canvasWindow :: !Int, canvasResource :: !T.Text, canvasSlot :: !Int
  , canvasViewport :: !(Int,Int,Int,Int), canvasDestination :: !(Double,Double,Double,Double)
  , canvasName :: !T.Text, canvasDescription :: !T.Text
  } deriving (Eq,Show)
data RemoteCanvas = RemoteCanvas
  { canvasEpoch :: !T.Text, canvasSurfaces :: [RemoteCanvasSurface], canvasMask :: !BS.ByteString
  , canvasAccessibility :: !BS.ByteString
  } deriving (Eq,Show)

validCanvasIdentity :: T.Text -> Bool
validCanvasIdentity ident=T.length ident==48 && T.all (`elem` ("0123456789abcdef"::String)) ident

parseRemoteCanvas :: (Int,Int) -> Value -> Parser RemoteCanvas
parseRemoteCanvas (cols,rows)=withObject "canvas scene" $ \o->do
  epoch<-o .: "epoch"
  unless (validCanvasIdentity epoch) (fail "Invalid canvas epoch")
  values<-o .: "surfaces"
  unless (length values<=64) (fail "Too many canvas surfaces")
  surfaces<-mapM surface values
  unless (IS.size (IS.fromList (map canvasWindow surfaces))==length surfaces && IS.size (IS.fromList (map canvasSlot surfaces))==length surfaces) (fail "Duplicate canvas identity or slot")
  encoded<-o .: "mask"
  let size=cols*rows*2
  unless (T.length encoded<=((size+2) `div` 3)*4) (fail "Oversized canvas mask")
  mask<-either fail pure (B64.decode (TE.encodeUtf8 encoded))
  unless (BS.length mask==size || (BS.null mask && null surfaces)) (fail "Invalid canvas mask size")
  let owners=IM.fromList [(canvasSlot entry,entry) | entry<-surfaces]
      scan !index !visible
        | index==cols*rows=Right visible
        | otherwise=let value=fromIntegral (BS.index mask (index*2)) .|. (fromIntegral (BS.index mask (index*2+1)) `shiftL` 8)
                        slot=value .&. 32767; x=index `mod` cols; y=index `div` cols in
            if value==0 then scan (index+1) visible else case IM.lookup slot owners of
              Just entry | let (a,b,w,h)=canvasViewport entry, x>=a && y>=b && x-a<w && y-b<h ->
                scan (index+1) (IM.insertWith merge slot (x,y,x,y) visible)
              _->Left "Canvas mask has no owning viewport"
      merge (a,b,c,d) (e,f,g,h)=(min a e,min b f,max c g,max d h)
  visible<-if BS.null mask then pure IM.empty else either fail pure (scan 0 IM.empty)
  let images=[object ["id" .= canvasWindow entry,"name" .= canvasName entry,"description" .= canvasDescription entry,
              "bounds" .= (x,y,z-x+1,w-y+1)] | entry<-surfaces, Just (x,y,z,w)<-[IM.lookup (canvasSlot entry) visible]]
      accessibility=BL.toStrict (encode (object ["images" .= images,"size" .= (cols,rows)]))
  pure (RemoteCanvas epoch surfaces mask accessibility)
  where
    surface=withObject "canvas surface" $ \o->do
      ident<-o .: "id"; resource<-o .: "resource"; slot<-o .: "slot"
      viewport@(x,y,w,h)<-o .: "rect"
      target@(a,b,c,d)<-o .: "target"
      name<-o .: "name"; description<-o .: "description"
      unless (ident>0 && ident<=2147483647 && validCanvasIdentity resource && slot>=1 && slot<=64 &&
        x>=0 && y>=0 && w>0 && h>0 && x<cols && y<rows && w<=cols-x && h<=rows-y &&
        all (\n->not (isNaN n || isInfinite n) && abs n<=1000000) [a,b,c,d] && c>0 && d>0 &&
        T.length name<=256 && T.length description<=1024 && not (T.any (=='\0') (name<>description))) (fail "Invalid canvas surface")
      pure (RemoteCanvasSurface ident resource slot viewport target name description)

-- | Fixed receive cursor over one image upload. Bytes are not retained here;
-- the existing bounded incoming queue lends each chunk to the SDL owner.
data CanvasControl = CanvasReset !T.Text | CanvasBegin !T.Text !T.Text !Int !Int !Int
  | CanvasChunk !T.Text !T.Text !Int !Int | CanvasRelease !T.Text !T.Text deriving (Eq,Show)
data CanvasReceiveState = CanvasReceiveState (Maybe T.Text) (M.Map T.Text Int) (Maybe (T.Text,Int,Int)) deriving (Eq,Show)
emptyCanvasReceiveState :: CanvasReceiveState
emptyCanvasReceiveState=CanvasReceiveState Nothing M.empty Nothing

-- | Validate owner epoch, declared residency and contiguous upload admission.
-- Issuer IDs always identify the same immutable bytes. A release cancels
-- the current cursor; late chunks cannot recreate a resource. A fresh explicit
-- begin (including the same immutable ID after release) is another admission,
-- without an unbounded retired-ID ledger.
admitCanvasControl :: CanvasReceiveState -> Value -> Either String (CanvasReceiveState,CanvasControl)
admitCanvasControl (CanvasReceiveState epoch live upload) value=do
  control<-parseEither (withObject "canvas control" $ \o->do
    kind<-o .: "type" :: Parser T.Text
    actual<-o .: "epoch"
    unless (validCanvasIdentity actual) (fail "Invalid canvas epoch")
    case kind of
      "canvas-reset"->pure (CanvasReset actual)
      _->do
        ident<-o .: "id"
        unless (validCanvasIdentity ident) (fail "Invalid canvas resource identity")
        case kind of
          "canvas-resource"->CanvasBegin actual ident <$> o .: "width" <*> o .: "height" <*> o .: "bytes"
          "canvas-chunk"->CanvasChunk actual ident <$> o .: "offset" <*> o .: "length"
          "canvas-release"->pure (CanvasRelease actual ident)
          _->fail "Unknown canvas control") value
  let same actual=unless (epoch==Just actual) (Left "Stale canvas epoch")
  state<-case control of
    CanvasReset actual->Right (CanvasReceiveState (Just actual) M.empty Nothing)
    CanvasBegin actual ident width height size->do
      same actual
      unless (width>0 && height>0 && width<=4096 && height<=4096 && width*height<=4194304 && size==width*height*4 &&
        M.notMember ident live && M.size live<64 && upload==Nothing && size<=67108864-sum (M.elems live)) (Left "Invalid canvas resource admission")
      Right (CanvasReceiveState epoch (M.insert ident size live) (Just (ident,size,0)))
    CanvasChunk actual ident offset size->do
      same actual
      case upload of
        Just (current,total,received) | ident==current && offset==received && size>0 && size<=262144 && size<=total-received ->
          Right (CanvasReceiveState epoch live (if received+size==total then Nothing else Just (ident,total,received+size)))
        _->Left "Invalid canvas upload cursor"
    CanvasRelease actual ident->do
      same actual
      Right (CanvasReceiveState epoch (M.delete ident live) (case upload of Just (current,_,_) | current==ident->Nothing; _->upload))
  Right (state,control)

-- | Header/binary pairing is exact; these bytes can never become a cell frame.
validateCanvasChunk :: CanvasControl -> BS.ByteString -> Either String ()
validateCanvasChunk (CanvasChunk _ _ _ count) bytes=unless (BS.length bytes==count) (Left "Canvas chunk binary length mismatch")
validateCanvasChunk _ _=Left "Binary canvas payload has no chunk header"

modifierNames :: Int -> [T.Text]
modifierNames mask = ["shift" | mask .&. 1/=0] ++ ["ctrl" | mask .&. 2/=0] ++ ["alt" | mask .&. 4/=0] ++ ["cmd" | mask .&. 8/=0]
nativeKeyInput :: Int -> Int -> Maybe Value
nativeKeyInput key mask = do
  V.EvKey k decodedMods <- decodeKey key mask
  name <- case k of
    V.KChar '\t' -> Just "Tab"
    V.KChar c -> Just (T.singleton c)
    V.KFun n -> Just ("F"<>T.pack (show n))
    _ -> lookup k [(V.KUp,"ArrowUp"),(V.KDown,"ArrowDown"),(V.KLeft,"ArrowLeft"),(V.KRight,"ArrowRight"),
      (V.KHome,"Home"),(V.KEnd,"End"),(V.KPageUp,"PageUp"),(V.KPageDown,"PageDown"),
      (V.KBackTab,"Tab"),(V.KEnter,"Enter"),(V.KEsc,"Escape"),(V.KBS,"Backspace"),(V.KDel,"Delete"),(V.KIns,"Insert")]
  pure (object ["type" .= ("key"::T.Text),"key" .= name,"mods" .= [label | (modifier,label)<-[(V.MShift,"shift"::T.Text),(V.MCtrl,"ctrl"),(V.MAlt,"alt"),(V.MMeta,"cmd")],modifier `elem` decodedMods]])
nativeEventInput :: [Int] -> Maybe Value
nativeEventInput event = case event of
  1:key:mods:_ | not (remoteDetachShortcut event) -> nativeKeyInput key mods
  3:x:y:clicks:mods:button:_ -> mouse (if clicks==0 then "move" else "down") x y button clicks mods
  4:x:y:_:mods:button:_ -> mouse "up" x y button 1 mods
  4:x:y:_ -> mouse "up" x y 0 1 0
  5:w:h:_ -> Just (object ["type" .= ("resize"::T.Text),"width" .= max 40 (min 512 w),"height" .= max 12 (min 256 h)])
  6:_ -> Just (object ["type" .= ("command"::T.Text),"command" .= ("hide.app.quit"::T.Text)])
  7:_ -> Just (object ["type" .= ("blur"::T.Text)])
  9:x:y:direction:mods:_ -> case mouse (if direction>0 then "wheel-up" else "wheel-down") x y 1 1 mods of
    Just (Object fields) -> Just (Object (KM.insert "steps" (toJSON (max 1 (min 256 (abs direction)))) fields))
    result -> result
  12:x:y:_:mods:_ -> mouse "move" x y 0 0 mods
  12:x:y:_ -> mouse "move" x y 0 0 0
  13:mods:_ -> Just (object ["type" .= ("modifiers"::T.Text),"mods" .= modifierNames mods])
  _ -> Nothing
  where
    mouse :: T.Text -> Int -> Int -> Int -> Int -> Int -> Maybe Value
    mouse action x y button clicks mods = Just (object ["type" .= ("mouse"::T.Text),"action" .= (action::T.Text),
      "x" .= max (-1) (min 511 x),"y" .= max (-1) (min 255 y),"button" .= (case button of 3->2; 2->1; 1->0; _ | action=="up"->(-1); _->0::Int),
      "clicks" .= max 0 (min 3 clicks),"mods" .= modifierNames mods])
menuActions :: [Command]
menuActions = nativeCommands

-- | Resolve a local menu slot against the server's named command state.
remoteMenuInput :: RemoteFrame -> Int -> Maybe Value
remoteMenuInput frame index
  | index>=0, True:_<-drop index (remoteMenus frame) =
    if index<length menuActions then case drop index (map commandIdentifier menuActions) of
      Just name:_ -> Just (object ["type" .= ("menu"::T.Text),"command" .= name])
      _ -> Nothing
    else case drop (index-length menuActions) (remoteContributions frame) of
      item:_ -> Just (contributionInput item)
      _ -> Nothing
  | otherwise = Nothing

contributionInput :: RemoteContribution -> Value
contributionInput item=object ["type" .= ("menu"::T.Text),"command" .= contributionId item,"registry" .= contributionRegistry item,"generation" .= contributionGeneration item]

-- | Freeze a contributed keyboard action to the exact registration published in
-- this frame. Effective labels distinguish bound contributions from descriptive
-- hints or a contribution whose name collides with a built-in command. The host
-- independently checks lifetime, focused target and actor authority at admission.
-- Inert targets are consumed instead of evaluating the supplied raw-key fallback.
-- A disabled row with a valid binding still retains its exact registration stamp.
remoteBindingInput :: RemoteFrame -> V.Event -> Maybe Value -> Maybe Value
remoteBindingInput frame (V.EvKey key modifiers) fallback=case chordName key modifiers >>= (`lookup` remoteBindings frame) of
  Just "" -> Nothing
  Just name | item:_<-[item | item<-remoteContributions frame,contributionId item==name],not (T.null (contributionKey item)) -> Just (contributionInput item)
  _ -> fallback
remoteBindingInput _ _ fallback=fallback

-- | Validate native incarnation before resolving the current host catalogue.
remoteNativeMenuInput :: RemoteFrame -> Int -> [Int] -> Maybe Value
remoteNativeMenuInput frame generation event=nativeMenuToken (length (remoteMenus frame)) generation event >>= remoteMenuInput frame

-- | Same published contributions appear in terminal/canvas popups and Cocoa.
-- The existing Help row is replaced by its exact registered action; other
-- contributions append in host-prepared group/order/ID order.
remoteMenuLayout :: RemoteFrame -> [(T.Text,[(T.Text,(String,Int),Int)])]
remoteMenuLayout frame=[(title,compose title items) | (title,_,items)<-menus]
  where
    shortcut ident=nativeChordShortcut [chord | (chord,name)<-remoteBindings frame,name==ident]
    indexed=zip [length menuActions..] (remoteContributions frame)
    compose title items=map builtin items++[(contributionTitle item,shortcut (contributionId item),token) | (token,item)<-indexed,contributionSlot item==T.toLower title,contributionId item/="hide.help.contents"]
    builtin (MenuItem _ _ Help) | (token,item):_<-[(token,item) | (token,item)<-indexed,contributionId item=="hide.help.contents"] = (contributionTitle item,shortcut (contributionId item),token)
    builtin (MenuItem title _ Disabled{})=(title,("",0),-1)
    builtin (MenuItem title _ command)=(title,maybe ("",0) shortcut (commandIdentifier command),maybe (-1) id (elemIndex command menuActions))

-- | Decode Dock events from the live session frame, refusing stale/missing IDs.
remoteDockWindowInput :: RemoteFrame -> Int -> [Int] -> Maybe Value
remoteDockWindowInput frame generation event=do
  ident<-nativeDockWindow (remoteWindows frame) generation event
  pure (object ["type" .= ("focus-window"::T.Text),"id" .= ident])

contributionCatalogue :: RemoteFrame -> [(T.Text,T.Text,Integer,T.Text,T.Text,Int,T.Text,T.Text)]
contributionCatalogue frame=[(contributionId item,contributionRegistry item,contributionGeneration item,contributionSlot item,contributionGroup item,contributionOrder item,contributionTitle item,contributionKey item) | item<-remoteContributions frame]

-- Drag/wheel updates are painted when their resulting frame arrives. Painting
-- the previous frame first spends an extra vblank on obsolete selection/layout.
-- Press/release still repaint locally to hide/restore the cell pointer.
nativeRepaint :: [Int] -> Bool
nativeRepaint event = case event of
  3:_:_:clicks:_ -> clicks>0
  n:_ -> n `elem` [4,5,7,8,12]
  _ -> False

remoteDetachShortcut :: [Int] -> Bool
remoteDetachShortcut event = case event of
  1:key:mods:_ -> key==fromEnum ']' && mods .&. 15==2
  _ -> False

-- | Gate disconnected input while retaining local zoom and presentation.
remoteInputAllowed :: Bool -> [Int] -> Bool
remoteInputAllowed connected event = connected || case event of
  1:key:mods:_ -> maybe False (const True) (zoomDirection key mods)
  17:_ -> True -- Retained sparks must expire even after the session disconnects.
  _ -> False
-- | Detach locally on a disconnected close, without queuing a Quit that
-- could unexpectedly execute after reconnection.
remoteCloseDetaches :: Bool -> [Int] -> Bool
remoteCloseDetaches connected event = not connected && case event of 6:_ -> True; _ -> False

-- | Strip path components and unsuitable characters, bound UTF-8 length and
-- supply a fallback name. File creation still requires exclusive temporary output.
sanitizeDownloadName :: T.Text -> T.Text
sanitizeDownloadName input = case limit (T.map clean (last (T.splitOn "/" (T.replace "\\" "/" input)))) of
  "" -> "download"; "." -> "download"; ".." -> "download"; name -> name
  where limit = TE.decodeUtf8With (\_ _ -> Nothing) . BS.take 180 . TE.encodeUtf8
        clean c | c<' ' || c=='\DEL' || c==':' = '_'
                | otherwise = c

#if defined(WITH_WINDOW) && defined(WITH_REMOTE)
data Incoming = Frame Int Word64 RemoteFrame | Assets (M.Map Char Glyph) | Control Value | Canvas CanvasControl | CanvasBytes CanvasControl BS.ByteString | ExportCopy FilePath (Int,Int,Int,Int) [Integer]
-- Compression and file transfers stay off the SDL thread.
receiveFrames :: FileExports -> RemotePeer -> TBQueue Incoming -> IO ()
receiveFrames exports peer queue = go [] (object []) Nothing 0 emptyCanvasReceiveState
  where
    emit item = atomically (writeTBQueue queue item) >> c_wake
    go rows metadata download demand canvasState = peerReceive peer >>= \packet -> case packet of
      Nothing -> emit (Control (object ["type" .= ("closed"::T.Text)]))
      Just (JsonPacket value) -> do
        kind <- parseIO (withObject "control" (.: "type")) value :: IO T.Text
        case kind of
          "assets" -> do
            atlas <- parseIO (withObject "assets" $ \o -> do
              glyphs <- o .: "glyphs" :: Parser [(T.Text,Int,[Int])]
              unless (length glyphs<=65536) (fail "Oversized glyph atlas")
              forM_ glyphs $ \(text,w,bits) -> unless (T.length text==1 && w `elem` [8,16] && length bits==16 && all (\n -> n>=0 && n<=65535) bits) (fail "Invalid glyph")
              pure (M.fromList [(T.head text,Glyph w (map fromIntegral bits)) | (text,w,bits)<-glyphs])) value
            supported <- parseIO (withObject "assets" (\o -> o .:? "menuCommands" .!= [])) value :: IO [T.Text]
            unless (length supported<=256 && all ((<=256).T.length) supported) (ioError (userError "Invalid menu commands"))
            emit (Assets atlas)
            go [] (object ["menuCommands" .= supported]) Nothing 0 emptyCanvasReceiveState
          "download" -> do
            offer <- parseIO (withObject "download" $ \o->do
              name<-o .: "name"
              purpose<-o .:? "purpose"
              receipt<-case purpose :: Maybe T.Text of
                Just "file-export"->do
                  row@(x,y,w,h)<-o .: "row"
                  view<-o .: "view"
                  unless (x>=0 && x<=511 && y>=0 && w>0 && w<=512 && h==1 && x+w<=512 && y<256 && length view==13 && all (\n->n>=0 && n<=9007199254740991) view) (fail "Invalid file export receipt")
                  pure (Just (row,view))
                Nothing->pure Nothing
                _->fail "Unknown download purpose"
              pure (name,receipt)) value
            go rows metadata (Just offer) demand canvasState
          "frame-ready" -> do
            (serial,changed)<-parseIO (withObject "frame readiness" (\o -> (,) <$> o .: "seq" <*> o .: "changed")) value
            if changed then go rows metadata download serial canvasState
              else emit (Control value) >> go rows metadata download demand canvasState
          "connection" -> emit (Control value) >> go rows (case metadata of Object fields->Object (KM.delete "semanticDialog" (KM.delete "canvas" fields)); _->metadata) Nothing 0 emptyCanvasReceiveState
          _ | kind `elem` ["canvas-reset","canvas-resource","canvas-chunk","canvas-release"]->do
            unless (case download of Nothing->True; _->False) (ioError (userError "Canvas control interrupted a download pair"))
            (next,control)<-either (ioError . userError) pure (admitCanvasControl canvasState value)
            case control of
              CanvasChunk{}->do
                binary<-peerReceive peer
                case binary of
                  Just (BinaryPacket bytes)->either (ioError . userError) pure (validateCanvasChunk control bytes) >> emit (CanvasBytes control bytes)
                  _->ioError (userError "Canvas chunk header requires its immediate binary packet")
              _->emit (Canvas control)
            go rows metadata Nothing demand next
          _ -> emit (Control value) >> go rows metadata download demand canvasState
      Just (BinaryPacket bytes) -> case download of
        Just (name,receipt) -> do
          (case receipt of
            Nothing->saveDownload name bytes
            Just (row,view) | os=="darwin"->do
              staged<-stageFileExport exports name bytes
              either (hPutStrLn stderr . T.unpack) (\path->emit (ExportCopy path row view)) staged
            Just _->do
              started<-startHelperFileExport exports name bytes (either (hPutStrLn stderr . T.unpack) (const (pure ())))
              either (hPutStrLn stderr . T.unpack) (const (pure ())) started) `catch` \(e::IOException) -> hPutStrLn stderr ("Download failed: "++show e)
          go rows metadata Nothing demand canvasState
        Nothing -> do
          received<-getMonotonicTimeNSec
          (delta,newRows) <- decodeFrame rows bytes
          let merged = case (delta,metadata) of (Object new,Object old) -> Object (KM.union new old); _ -> delta
          frame <- either (ioError . userError) pure (parseRemoteFrame merged newRows)
          emit (Frame demand received frame)
          go newRows merged Nothing demand canvasState
    saveDownload name bytes = do
      directory <- (</> "Downloads") <$> getHomeDirectory
      createDirectoryIfMissing True directory
      path <- bracket (openBinaryTempFile directory (T.unpack (sanitizeDownloadName name))) (hClose . snd) $ \(path,handle) -> BS.hPut handle bytes >> pure path
      hPutStrLn stderr ("Downloaded "++path)
parseIO :: (Value -> Parser a) -> Value -> IO a
parseIO parser = either (ioError . userError) pure . parseEither parser

drawRemote :: Font -> M.Map Char Glyph -> Maybe T.Text -> RemoteFrame -> IO ()
drawRemote font atlas epoch frame = allocaArray 16 $ \scratch -> do
  c_cursor_blink (flag (remoteBlink frame))
  c_crt_filter (flag (remoteCRT frame))
  c_power_mode (flag (not (remoteTerminal frame) && remoteDialog frame==Nothing))
  c_pixelate_unicode (flag (remotePixelated frame))
  check "Allocate remote frame" c_begin
  forM_ (remoteCells frame) $ \cell -> case cell of
    RemoteText x y paint text _ -> do
      let !fg=fromIntegral (textForeground paint)
          !bg=fromIntegral (textBackground paint)
          !flags=fromIntegral (textFlags paint)
          end=TU.lengthWord8 text
          chars !offset !at
            | offset==end = pure ()
            | otherwise = case TU.iter text offset of
                TU.Iter ch bytes -> do
                  c_clip (fromIntegral at) 1
                  let bitmap=case M.lookup ch atlas of
                        Just tile->Just tile
                        Nothing | bitmapGlyph font ch->Just (glyph font ch)
                        _->Nothing
                  case bitmap of
                    Just (Glyph width bits)->do
                      pokeArray scratch bits
                      c_glyph (fromIntegral at) (fromIntegral y) 1 (fromIntegral width) scratch fg bg flags 0 1
                    Nothing->utf8 (TU.takeWord8 bytes (TU.dropWord8 offset text)) $ \p ->
                      check "Draw remote Unicode" (c_unicode (fromIntegral at) (fromIntegral y) 1 p fg bg flags 0 1)
                  chars (offset+bytes) (at+1)
      chars 0 x
    RemoteGlyph visible y paint text full start shown -> drawGlyph scratch visible y paint text full start shown 0 full
    RemoteScript x y paint text natural script ->
      drawGlyph scratch x y paint text 1 0 (1::Int) (case script of Superscript -> 1; Subscript -> 2) natural
  forM_ (remoteCursor frame) $ \(x,y) -> c_cursor (fromIntegral x) (fromIntegral y)
  case remoteCanvas frame of
    Just scene | Just (canvasEpoch scene)==epoch->installNativeCanvas (canvasEpoch scene) (remoteSize frame) (canvasMask scene)
      [(canvasResource entry,canvasSlot entry,canvasViewport entry,canvasDestination entry) | entry<-canvasSurfaces scene]
    _->c_canvas_clear
  check "Present remote frame" c_present
  where
    flag value = if value then 1 else 0
    drawGlyph scratch visible y paint text full start shown script natural = do
      let x=visible-start
          bitmap = case T.uncons text of
            Just (c,after) | T.null after -> case M.lookup c atlas of
              Just tile->Just tile
              Nothing | bitmapGlyph font c->Just (glyph font c)
              _->Nothing
            _->Nothing
          fg=fromIntegral (textForeground paint); bg=fromIntegral (textBackground paint)
          flags=fromIntegral (textFlags paint+if script==0 && full/=clusterWidth text then 4 else 0)
      c_clip (fromIntegral visible) (fromIntegral shown)
      case bitmap of
        Just (Glyph width bits) -> do
          pokeArray scratch bits
          c_glyph (fromIntegral x) (fromIntegral y) (fromIntegral full) (fromIntegral width) scratch fg bg flags script (fromIntegral natural)
        Nothing -> utf8 text $ \p -> check "Draw remote Unicode" (c_unicode (fromIntegral x) (fromIntegral y) (fromIntegral full) p fg bg flags script (fromIntegral natural))

runRemoteWindow :: Backend -> Double -> (Int,Int) -> Int -> String -> RemotePeer -> IO ()
runRemoteWindow backend scale (cols,rows) mode host peer = withFileExports $ \exports -> do
  font <- loadFont
  drawTimes <- newIORef ([]::[Double])
  demands <- newIORef emptyFrameTiming
  inputDemand <- newIORef Nothing
  presentationDemand <- newIORef Nothing
  titleTiming <- newIORef (0::Double,""::T.Text)
  exportGesture <- newIORef Nothing
  installedDialog <- newIORef Nothing
  installedSidebar <- newIORef Nothing
  installedCanvas <- newIORef Nothing
  canvasEpochRef <- newIORef Nothing
  let clearSidebar=do
        check "Clear sidebar accessibility" (c_accessibility nullPtr 0)
        writeIORef installedDialog Nothing
        writeIORef installedSidebar Nothing
        writeIORef installedCanvas Nothing
      installDialog value=do
        previous<-readIORef installedDialog
        let current=remoteDialog value
        when (previous/=Just current) $ do
          let bytes=fromMaybe emptyDialogAccessibility current
          BS.useAsCStringLen bytes $ \(ptr,len)->check "Update dialog accessibility" (c_accessibility ptr (fromIntegral len))
          writeIORef installedDialog (Just current)
          writeIORef installedSidebar Nothing
          writeIORef installedCanvas Nothing
      installSidebar value=do
        previous<-readIORef installedSidebar
        let bytes=remoteSidebar value
        when (previous/=Just bytes) $ do
          BS.useAsCStringLen bytes $ \(ptr,len)->check "Update sidebar accessibility" (c_accessibility ptr (fromIntegral len))
          writeIORef installedSidebar (Just bytes)
          when (BS.null bytes) (writeIORef installedCanvas Nothing)
      installCanvas value=do
        epoch<-readIORef canvasEpochRef
        let bytes=case remoteCanvas value of
              Just scene | Just (canvasEpoch scene)==epoch->canvasAccessibility scene
              _->"{\"images\":[],\"size\":[40,12]}"
        previous<-readIORef installedCanvas
        when (previous/=Just bytes) $ do
          BS.useAsCStringLen bytes $ \(ptr,len)->check "Update image accessibility" (c_accessibility ptr (fromIntegral len))
          writeIORef installedCanvas (Just bytes)
  incoming <- newTBQueueIO 8
  outgoing <- newTBQueueIO 256
  queuedBytes <- newTVarIO (0::Int)
  let driver = case backend of Metal -> "metal"; Vulkan -> "vulkan"; _ -> if os=="darwin" then "metal" else "vulkan"
      send packets = do
        now<-getMonotonicTimeNSec
        started<-fromMaybe now <$> readIORef inputDemand
        before<-readIORef demands
        let tag state (JsonPacket (Object fields))=
              let (serial,next)=requestFrame started state
              in (next,JsonPacket (Object (KM.insert "seq" (toJSON serial) fields)))
            tag state packet=(state,packet)
            (after,tagged)=mapAccumL tag before packets
            size = sum [case packet of JsonPacket value -> fromIntegral (BL.length (encode value)); BinaryPacket bytes -> BS.length bytes | packet<-tagged]
        accepted <- atomically $ do
          full <- isFullTBQueue outgoing
          used <- readTVar queuedBytes
          if full || used+size>33554432 then pure False
          else writeTBQueue outgoing (size,tagged) >> writeTVar queuedBytes (used+size) >> pure True
        if accepted then writeIORef demands after
          else hPutStrLn stderr "Remote input queue full; input was not sent."
      sendJSON value = send [JsonPacket value]
      sendEvent = maybe (pure ()) sendJSON . nativeEventInput
      pasteReply request = do
        bytes <- c_clipboard >>= BS.packCString
        case TE.decodeUtf8' bytes of
          Right text | Just packet<-clipboardReplyInput request text -> sendJSON packet
          _ -> hPutStrLn stderr "Clipboard text is invalid or exceeds 1 MiB."
      paste = do
        bytes <- c_clipboard >>= BS.packCString
        case TE.decodeUtf8' bytes of
          Right text | T.length text<=1048576 -> sendJSON (object ["type" .= ("paste"::T.Text),"text" .= text])
          _ -> hPutStrLn stderr "Clipboard text is invalid or exceeds 1 MiB."
      resize = alloca $ \wp -> alloca $ \hp -> do
        c_size wp hp
        w <- fromIntegral <$> peek wp; h <- fromIntegral <$> peek hp
        sendEvent [5,w,h]
      dispatch connected frame event = case event of
        1:key:mods:_ | not (maybe False remoteTerminal frame && mods .&. 15==2), Just direction <- zoomDirection key mods -> check "Change window scale" (c_scale (fromIntegral direction)) >> when connected resize
        2:_ -> do
          bytes <- c_text >>= BS.packCString
          case TE.decodeUtf8' bytes of
            Right text -> do
              forM_ (T.unpack text) $ \c -> sendEvent [1,fromEnum c,0]
            Left _ -> pure ()
        17:_ -> check "Present typing animation" c_present
        11:i:_ -> do
#ifdef darwin_HOST_OS
          generation<-fromIntegral <$> c_menu_generation
          case frame >>= \value->remoteNativeMenuInput value generation event of
            Just packet -> if i==fromMaybe (-1) (elemIndex Paste nativeCommands) then paste else sendJSON packet
            _ -> pure ()
#else
          pure ()
#endif
        16:_ -> do
#ifdef darwin_HOST_OS
          generation<-fromIntegral <$> c_dock_generation
          forM_ (frame >>= \value->remoteDockWindowInput value generation event) $ \packet->sendJSON packet >> c_raise
#else
          pure ()
#endif
        14:_ | null host -> do
          bytes <- c_text >>= BS.packCString
          case TE.decodeUtf8' bytes of
            Right path -> sendJSON (object ["type" .= ("open"::T.Text),"path" .= path])
            Left _ -> pure ()
        14:_ -> do
          bytes <- c_text >>= BS.packCString
          case TE.decodeUtf8' bytes of
            Left _ -> pure ()
            Right path -> do
              let name = sanitizeDownloadName (T.pack (takeFileName (T.unpack path)))
              (withBinaryFile (T.unpack path) ReadMode $ \handle -> do
                size <- hFileSize handle
                if size>16777216 then hPutStrLn stderr "Dropped file exceeds 16 MiB."
                else do
                  payload <- BS.hGet handle 16777217
                  when (BS.length payload<=16777216) (send [JsonPacket (object ["type" .= ("upload"::T.Text),"name" .= name]),BinaryPacket payload]))
                `catch` \(e::IOException) -> hPutStrLn stderr ("Cannot upload dropped file: "++show e)
        _ -> when connected $ forM_ (case (frame,event) of
          (Just value,1:key:mods:_) | Just input<-decodeKey key mods -> remoteBindingInput value input (nativeEventInput event)
          _ -> nativeEventInput event) sendJSON
      title connection frame = do
        (_,timing)<-readIORef titleTiming
        utf8 ((maybe "Haskell" remoteTitle frame)<>(if null host then "" else " — "<>T.pack host)<>connection<>timing) c_title
      updateTiming connection frame = do
        now<-((/1000000000).fromIntegral) <$> getMonotonicTimeNSec
        (previous,_)<-readIORef titleTiming
        when (now-previous>=1) $ do
          samples<-readIORef drawTimes
          unless (null samples) $ do
            let timing=T.pack (printf " | %.1f ms/frame" (sum samples/fromIntegral (length samples)))
            writeIORef titleTiming (now,timing)
            title connection frame
      resetCanvas=do
        check "Reset image resources" (c_canvas_reset nullPtr)
        writeIORef canvasEpochRef Nothing
        let empty="{\"images\":[],\"size\":[40,12]}"
        BS.useAsCStringLen empty $ \(ptr,len)->check "Clear image accessibility" (c_accessibility ptr (fromIntegral len))
        writeIORef installedCanvas Nothing
      controls (frame,atlas,connection,changed,closed) item = case item of
        Canvas control->do
          case control of
            CanvasReset epoch->resetCanvas >> utf8 epoch (\value->check "Reset image epoch" (c_canvas_reset value)) >> writeIORef canvasEpochRef (Just epoch)
            CanvasBegin epoch ident width height size->utf8 epoch $ \owner->utf8 ident $ \resource->
              check "Allocate image resource" (c_canvas_begin owner resource (fromIntegral width) (fromIntegral height) (fromIntegral size))
            CanvasRelease epoch ident->utf8 epoch $ \owner->utf8 ident $ \resource->check "Release image resource" (c_canvas_release owner resource)
            CanvasChunk{}->ioError (userError "Image chunk has no binary payload")
          pure (frame,atlas,connection,changed || case control of CanvasBegin{}->False; _->True,closed)
        CanvasBytes (CanvasChunk epoch ident offset _) bytes->do
          ok<-utf8 epoch $ \owner->utf8 ident $ \resource->BS.useAsCStringLen bytes $ \(chunk,len)->
            c_canvas_chunk owner resource (fromIntegral offset) chunk (fromIntegral len)
          check "Upload image chunk" (pure ok)
          pure (frame,atlas,connection,changed || ok==2,closed)
        CanvasBytes _ _->ioError (userError "Image binary has no chunk header")
        ExportCopy path row view -> do
          c_cancel_file_drag
          writeIORef exportGesture (Just (path,row,view,False))
          pure (frame,atlas,connection,changed,closed)
        Assets glyphs -> clearSidebar >> resetCanvas >> pure (frame,glyphs,connection,True,closed)
        Frame serial received value -> do
          started<-atomicModifyIORef' demands (\pending -> let (time,next)=settleFrame serial pending in (next,time))
          modifyIORef' presentationDemand (Just . maybe (fromMaybe received started) (min (fromMaybe received started)))
          when (maybe (Just mode) remoteMode frame /= remoteMode value) $ do
            let (w,h) = remoteSize value
            check "Change remote screen mode" (c_mode (fromIntegral (modeHeight (maybe mode id (remoteMode value)))) (fromIntegral w) (fromIntegral h))
            resize
#ifdef darwin_HOST_OS
          when (fmap contributionCatalogue frame/=Just (contributionCatalogue value)) (installNativeMenus (remoteMenuLayout value))
          forM_ (concatMap snd (remoteMenuLayout value)) $ \(_,(shortcut,modifiers),token)->do
            when (token>=0) $ withCString shortcut $ \keyPtr->c_menu_shortcut (fromIntegral token) keyPtr (fromIntegral modifiers)
          updateDockWindows (remoteWindows value)
          forM_ (zip [0..] (remoteMenus value)) $ \(i,enabled) ->
            c_menu_enabled (fromIntegral (i::Int)) (if enabled then 1 else 0)
#endif
          installDialog value
          case remoteDialog value of
            Just _->pure ()
            Nothing->installSidebar value >> installCanvas value
          pure (Just value,atlas,connection,True,closed)
        Control value -> do
          kind <- parseIO (withObject "control" (.: "type")) value :: IO T.Text
          case kind of
            "frame-ready" -> do
              serial<-parseIO (withObject "frame readiness" (.: "seq")) value
              modifyIORef' demands (snd . settleFrame serial)
              pure (frame,atlas,connection,changed,closed)
            "closed" -> pure (frame,atlas,connection,changed,True)
            "copy" -> do
              text <- parseIO (withObject "copy" (.: "text")) value
              utf8 text c_set_clipboard
              pure (frame,atlas,connection,changed,closed)
            "open-resource" -> do
              result<-openResource value
              pure (frame,atlas,either id (const connection) result,changed,closed)
            "paste-request" -> do
              request<-parseIO parseClipboardRequest value
              pasteReply request
              pure (frame,atlas,connection,changed,closed)
            "connection" -> do
              connected <- parseIO (withObject "connection" (.: "connected")) value
              clearSidebar
              resetCanvas
              when connected resize
              pure (frame,atlas,if connected then "" else " (reconnecting)",True,closed)
            _ -> pure (frame,atlas,connection,changed,closed)
      loop receiver sender frame atlas connection previousTheme repaint = do
        forM_ [receiver,sender] $ \worker -> do
          result <- poll worker
          case result of Just (Left e) -> throwIO e; _ -> pure ()
        messages <- atomically (drain incoming)
        pending <- atomically (not <$> isEmptyTBQueue incoming)
        when pending c_wake
        (current,glyphs,status,changed,closed) <- foldM controls (frame,atlas,connection,repaint,False) messages
        unless closed $ do
          offer<-readIORef exportGesture
          forM_ offer $ \(path,(x,y,w,h),view,armed)->case current of
            Just value | remoteExportView value==view && T.null status -> unless armed $ do
              ok<-utf8 (T.pack path) (\p->c_arm_file_drag p (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))
              writeIORef exportGesture (if ok==0 then Nothing else Just (path,(x,y,w,h),view,True))
            Just value | take 1 (remoteExportView value)<take 1 view && T.null status -> pure ()
            _->c_cancel_file_drag >> writeIORef exportGesture Nothing
          let connected = T.null status
          when changed $ do
            title status current
            forM_ current $ \value -> do
              now<-getMonotonicTimeNSec
              start<-fromMaybe now <$> atomicModifyIORef' presentationDemand (\time -> (Nothing,time))
              epoch<-readIORef canvasEpochRef
              drawRemote font glyphs epoch value
              end<-getMonotonicTimeNSec
              modifyIORef' drawTimes (take 60 . (fromIntegral (end-start)/1000000:))
#ifdef darwin_HOST_OS
            unless connected $ do
              updateDockWindows []
              forM_ (zip [0::Int ..] (maybe (replicate (length nativeCommands) False) remoteMenus frame)) $ \(i,_) -> c_menu_enabled (fromIntegral i) 0
#endif
          updateTiming status current
          dark <- (/=0) <$> c_system_dark
          when (previousTheme/=Just dark && (connected || previousTheme==Nothing)) (sendJSON (object ["type" .= ("theme"::T.Text),"dark" .= dark]))
          event <- allocaArray 6 $ \p -> check "Read remote window event" (c_wait p) >> map fromIntegral <$> peekArray 6 p
          case event of
            kind:_ | kind `elem` [1,2,5,7,9,10,11,14] -> c_cancel_file_drag >> writeIORef exportGesture Nothing
            _->pure ()
          observed<-getMonotonicTimeNSec
          queuedAge<-c_event_age_ns
          let requested=observed-min observed queuedAge
          when (nativeRepaint event) (modifyIORef' presentationDemand (Just . maybe requested (min requested)))
          unless (remoteDetachShortcut event || remoteCloseDetaches connected event) $ do
            writeIORef inputDemand (Just requested)
            when (remoteInputAllowed connected event) (dispatch connected current event) `finally` writeIORef inputDemand Nothing
            loop receiver sender current glyphs status (if connected || previousTheme==Nothing then Just dark else previousTheme) (nativeRepaint event)
  bracket_ (pure ()) c_close $ do
#ifdef darwin_HOST_OS
    c_menu_prepare
#endif
    withCString driver $ \name -> check "Open remote window" (c_open name (realToFrac scale) (fromIntegral cols) (fromIntegral rows) (fromIntegral (modeHeight mode)))
    nativeMenus
    sendJSON (object ["type" .= ("frontend"::T.Text),"mode" .= mode,"mac" .= (os=="darwin")])
#ifdef darwin_HOST_OS
    forM_ (zip [0::Int ..] nativeCommands) $ \(i,_) -> c_menu_enabled (fromIntegral i) 0
#endif
    title " (connecting)" Nothing
    resize
    withAsync (receiveFrames exports peer incoming) $ \receiver ->
      withAsync (forever $ do
        (size,packets) <- atomically (readTBQueue outgoing)
        peerSendBatch peer packets
        atomically (modifyTVar' queuedBytes (subtract size))) $ \sender ->
        loop receiver sender Nothing M.empty " (connecting)" Nothing True `finally` do
          sent <- timeout 2000000 (atomically (readTVar queuedBytes >>= \bytes -> when (bytes/=0) retry))
          when (sent==Nothing) (hPutStrLn stderr "Some window input could not be handed to the session before detaching.")
  where
    drain queue = do
      item <- tryReadTBQueue queue
      case item of Nothing -> pure []; Just value@CanvasBytes{} -> pure [value]; Just value -> (value:) <$> drain queue
#else
runRemoteWindow :: Backend -> Double -> (Int,Int) -> Int -> String -> RemotePeer -> IO ()
runRemoteWindow _ _ _ _ _ _ = ioError (userError "Graphical support is not built; rebuild with -fwindow -fremote.")
#endif
