{-# LANGUAGE OverloadedStrings #-}
-- Shared policy for host-chosen guest input and guest-readable screen cells.
module Hide.GuestAccess
  ( InputOrigin(..), CellAccess(..), cellAccess, readableAt, pointerAllowedAt
  , streamerReadableAt, sensitiveLabel, sanitizedStatus, protectedPath, protectedPathParent, protectedBuffer, privateDocument, sanitizedBuffer
  , guestCommandAllowed, guestEffectsAllowed, guestKeyboardAllowed, guestKeyAllowed, guestKeyCombinations
  , guestModalBlocked, guestTransitionAllowed, beginGuestInput, endGuestInput
  ) where
import Data.Char (toLower)
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.FilePath ((</>), isAbsolute, makeRelative, splitDirectories, normalise, takeFileName)
import Hide.Browser (Entry(..))
import Hide.Files (filePath)
import Hide.Buffer
import Hide.Model

data InputOrigin = HumanInput | GuestInput deriving (Eq,Show)
data CellAccess = CellAccess { cellReadable :: Bool, cellClickable :: Bool } deriving (Eq,Show)

protectedBuffer :: Desktop -> Int -> Bool
protectedBuffer d bid=maybe False (\doc -> privateDocument d doc || maybe False (`elem` ["Conversation","Autocomplete","Agent request","Proposed agent edit"]) (documentLabel doc)) (M.lookup bid (buffers d))

-- Callers resolve filesystem paths before applying this pure policy. FileState
-- paths and the host-provided private list are already canonical.
protectedPath :: Desktop -> FilePath -> Bool
protectedPath d path=map toLower (takeFileName path)=="thc.toml" || any (`pathContains` path) (guestPrivatePaths d)

-- Renaming/deleting an ancestor, or creating a file in its place, can disable
-- the authority store even without touching its filename directly.
protectedPathParent :: Desktop -> FilePath -> Bool
protectedPathParent d path=protectedPath d path || any (pathContains path) (guestPrivatePaths d)

pathContains :: FilePath -> FilePath -> Bool
pathContains root path=let relative=makeRelative root path in not (isAbsolute relative) && ".." `notElem` splitDirectories relative

privateDocument :: Desktop -> Document -> Bool
-- Human Git review can contain authority files; guests use workspace_git's
-- filtered result instead of inheriting the unrestricted review buffer.
privateDocument d doc=maybe False privateLabel (documentLabel doc) || maybe False (protectedPath d . filePath) (documentFile doc)
  where
    privateLabel "Git diff"=True
    privateLabel label=maybe False (protectedPath d . T.unpack) (T.stripPrefix "Disk changes: " label)

-- Keep offsets/newlines stable for paginated buffer reads.
sanitizedBuffer :: Desktop -> Int -> Maybe Text
sanitizedBuffer d bid=do
  doc<-M.lookup bid (buffers d)
  let text=contents (documentBuffer doc)
  if privateDocument d doc then Nothing else case documentLabel doc of
    Just label | label `elem` ["Agent request","Proposed agent edit"] -> Nothing
    Just "Conversation" | byteMode (documentBuffer doc) -> Nothing
    Just "Conversation" -> Just (T.pack [if privateOffset d text n && c/='\n' && c/='\r' then ' ' else c | (n,c)<-zip [0..] (T.unpack text)])
    _ -> Just text
privateOffset :: Desktop -> Text -> Int -> Bool
privateOffset d text n=sessionOffset text n || any private (chatActions d)
  where
    private (a,z,"question-input",_)=n>=a+7 && n<z
    -- The selected choice changes the color of its entire label, not just (*).
    private (a,z,"question-choice",_)=n>=a && n<z
    private _=False
sessionOffset :: Text -> Int -> Bool
sessionOffset text n="Session: " `T.isPrefixOf` text && n<T.length (T.takeWhile (/='\n') text)

guestCommandAllowed :: Command -> Bool
guestCommandAllowed cmd=case cmd of
  DebugCommand action | privateDownloadAction action -> False
  GitDiff -> False
  GitCommit -> False
  AgentChoose{} -> False
  AgentSet{} -> False
  AgentDirectory -> False
  AgentOptions -> False
  ChatInputOptions -> False
  AutocompleteCommand{} -> False
  SubmitChat{} -> False
  AgentPermissions -> False
  AgentGuidance -> False
  OpenLink{} -> False
  EnvironmentOptions -> False
  Conversation -> False
  AgentCancel -> False
  AgentResume -> False
  AgentCopyRaw -> False
  AgentNew -> False
  _ -> True
agentActionAllowed :: Text -> Bool
agentActionAllowed action=action `elem` ["compile","make","build-stop","run","run-options","run-config","toolchain","terminal","terminal-input","terminal-stop"]
guestEffectsAllowed :: [Effect] -> Bool
guestEffectsAllowed=all allowed
  where
    allowed ReadGitDiff=False
    allowed AskGitCommit=False
    allowed WriteGitCommit{}=False
    allowed FollowLink{}=False
    allowed EnvironmentAction{}=False
    allowed PermissionAction{}=False
    allowed SaveChatSubmit{}=False
    allowed AutocompleteAction{}=False
    allowed (DebugAction action _)=not (privateDownloadAction action)
    allowed (AgentAction action _)=agentActionAllowed action
    allowed (SaveDocument _ _ follow)=maybe True guestCommandAllowed follow
    allowed ReadBrowserClipboard=False
    allowed WriteBrowserClipboard{}=False
    allowed _=True
protectedPurpose :: Purpose -> Bool
protectedPurpose p=case p of
  EnvironmentDialog{} -> True
  PermissionDialog{} -> True
  ChatInputSettings -> True
  AutocompleteDialog{} -> True
  DebugDialog action -> privateDownloadAction action
  AgentDialog action -> not (agentActionAllowed action)
  DiscardDraft -> True
  Confirm command -> not (guestCommandAllowed command)
  _ -> False
privateDownloadAction :: Text -> Bool
privateDownloadAction action=action=="downloads" || "hdb-" `T.isPrefixOf` action

guestModalBlocked :: Desktop -> Bool
guestModalBlocked d=maybe False (protectedPurpose . purpose) (dialog d) || case (contextMenu d,contextKind d) of
  (Just _,AgentContext{}) -> True
  _ -> False
guestKeyboardAllowed :: Desktop -> Bool
guestKeyboardAllowed d=not (guestModalBlocked d) && not (focusedPrivateField d) && (isJust (dialog d) || problemsFocused d || maybe False treeFocused (sideTree d) || maybe True (not . protectedBuffer d . bufferId) (activeWindow d))
guestKeyAllowed :: Desktop -> V.Key -> [V.Modifier] -> Bool
guestKeyAllowed d key mods=not (guestModalBlocked d) && (guestKeyboardAllowed d || navigation || fieldNavigation)
  where
    fieldNavigation=isJust (dialog d) && key `elem` [V.KChar '\t',V.KBackTab,V.KEsc]
    navigation=dialog d==Nothing && (key==V.KFun 6 || key `elem` [V.KChar '\t',V.KBackTab] && any (`elem` mods) [V.MCtrl,V.MAlt] || V.MAlt `elem` mods && case key of V.KChar c -> c>='1' && c<='9'; _ -> False)

-- Named navigation/editing shortcuts, plus Ctrl/Alt character shortcuts.
-- Ordinary text is represented by the separate keyboardAllowed metadata.
guestKeyCombinations :: [(Text,[V.Modifier])]
guestKeyCombinations=[(key,mods) | key<-["Enter","Escape","Tab","ArrowUp","ArrowDown","ArrowLeft","ArrowRight","Home","End","PageUp","PageDown","Backspace","Delete","Insert"]++["F"<>T.pack (show n) | n<-[1::Int ..24]],mods<-[[],[V.MShift],[V.MCtrl],[V.MAlt]]]++[(T.singleton c,mods) | c<-['a'..'z']++['1'..'9']++[' '],mods<-[[V.MCtrl],[V.MAlt]]]

-- Effects alone miss widgets which mutate directly, including draft edits,
-- question choices, settings checkboxes and discard confirmation.
guestTransitionAllowed :: Desktop -> Desktop -> [Effect] -> Bool
guestTransitionAllowed before after effects=guestEffectsAllowed effects && not (guestModalBlocked after) &&
  streamerMode before==streamerMode after && chatSubmit before==chatSubmit after && privateFieldsUnchanged before after && composerBuffer before==composerBuffer after &&
  composerSelection before==composerSelection after && chatQuestion before==chatQuestion after &&
  autocompleteACPEnabled before==autocompleteACPEnabled after &&
  revision (autocompleteDraft before)==revision (autocompleteDraft after) &&
  autocompleteSelection before==autocompleteSelection after && autocompleteFocused before==autocompleteFocused after &&
  agentSettings before==agentSettings after && childAgentSettings before==childAgentSettings after &&
  childAgentSteering before==childAgentSteering after && childAgentContextUsage before==childAgentContextUsage after &&
  all (\(bid,doc)->not (protectedBuffer before bid) || M.lookup bid (buffers after)==Just doc) (M.toList (buffers before))

privateField :: Field -> Bool
privateField (Input label _ _)=sensitiveLabel label
privateField (CheckBox "Streamer mode" _)=True
privateField _=False
privateDialogField :: Desktop -> Dialog -> Field -> Bool
privateDialogField d dg field=privateField field || case field of
  Input _ value _ -> case purpose dg of
    Opening base _ _ -> privateName base value
    ChangingDirectory base _ -> privateName base value
    Saving bid _ -> protectedBuffer d bid || privateName (startingDirectory d) value
    _ -> False
  _ -> False
  where
    privateName base value=let name=T.unpack value in protectedPath d (normalise (if isAbsolute name then name else base </> name))
focusedPrivateField :: Desktop -> Bool
focusedPrivateField d=case dialog d of Just dg -> maybe False (privateDialogField d dg) (at (fields dg) (focus dg)); _ -> False
privateFieldsUnchanged :: Desktop -> Desktop -> Bool
privateFieldsUnchanged before after=case (dialog before,dialog after) of
  (Just a,Just b) | purpose a==purpose b -> values before a==values after b
  _ -> True
  where
    values d dg=[value field | field<-fields dg,privateDialogField d dg field]
    value (Input label text _)=Left (label,text)
    value (CheckBox label checked)=Right (label,checked)
    value _=Left ("","")

sanitizedStatus :: Desktop -> Text
sanitizedStatus d | "Session " `T.isPrefixOf` status d="Session [redacted]"
                  | otherwise=status d

-- The host brackets a whole batch under the desktop lock. Guest gestures and
-- clipboard may flow between its events, but never across the human boundary.
beginGuestInput :: Desktop -> Desktop
beginGuestInput d=(clearGestures d) {clipboard="",clipboardCode=Nothing}
endGuestInput :: Desktop -> Desktop -> Desktop
endGuestInput original updated=(clearGestures updated) {clipboard=clipboard original,clipboardCode=clipboardCode original}
clearGestures :: Desktop -> Desktop
clearGestures d=d {drag=Nothing,dragOriginal=Nothing,prefix=Nothing,heldModifiers=[],buttonPressed=Nothing,buttonHover=Nothing,blockStart=Nothing}

cellAccess :: Desktop -> Int -> Int -> CellAccess
cellAccess d x y=CellAccess (readableAt d x y) (pointerAllowedAt d x y)
readableAt :: Desktop -> Int -> Int -> Bool
readableAt d x y
  | Just dg<-dialog d, DebugDialog action<-purpose dg, privateDownloadAction action,
    inside (dialogRect d dg) x y || y==snd (screenSize d)-1=False
  | Just dg<-dialog d, PermissionDialog{}<-purpose dg, inside (dialogRect d dg) x y || y==snd (screenSize d)-1=False
  | otherwise=onScreen d x y && streamerReadableAt d x y && case dialog d of
  Just dg | inside (dialogRect d dg) x y -> True
  _ | overlayAt d x y -> True
    | otherwise -> case topWindow d x y of
        Just w | protectedBuffer d (bufferId w) -> case M.lookup (bufferId w) (buffers d) of
          Just doc | documentLabel doc==Just "Autocomplete" -> not (autocompletePane d w && inside (autocompleteComposerRect d w) x y)
          Just doc | documentLabel doc==Just "Conversation" -> not (byteMode (documentBuffer doc)) && not (inside (composerRect d w) x y) && not (contentPrivate privateOffset d doc w x y)
          _ -> False
        _ -> True

-- Independent of the human Streamer-mode toggle. Guests ALWAYS use this mask.
-- Labels remain visible; only sensitive value rows are blanked.
streamerReadableAt :: Desktop -> Int -> Int -> Bool
streamerReadableAt d x y
  | y==snd (screenSize d)-1, "Session " `T.isPrefixOf` status d=False
  | otherwise=case dialog d of
  Just dg | inside (dialogRect d dg) x y -> not (any (sensitiveValue dg) (zip (fieldRects d dg) (fields dg))) && not (privateBrowserCell d dg x y)
  _ | privateAgentChoice d x y -> False
    | privateTreeCell d x y -> False
    | overlayAt d x y -> True
    | otherwise -> case topWindow d x y of
        Just w | Just doc<-M.lookup (bufferId w) (buffers d),privateDocument d doc -> False
        Just w | Just doc<-M.lookup (bufferId w) (buffers d),documentLabel doc==Just "Conversation" -> not (contentPrivate (\_ -> sessionOffset) d doc w x y)
        _ -> True
  where
    sensitiveValue dg (r,Input label _ _)=sensitiveLabel label && y>top r && inside r x y && y>=top (dialogRect d dg)+2 && y<top (dialogRect d dg)+height (dialogRect d dg)-3
    sensitiveValue _ _=False
-- Only recognized browser/tree paths are checked. Ordinary source text and
-- unrelated filenames are never scanned for strings which resemble secrets.
privateTreeCell :: Desktop -> Int -> Int -> Bool
privateTreeCell d x y
  | maybe False (\(r,_)->inside r x y) (contextMenu d) || maybe False (\(i,_)->inside (menuRect d i) x y) (menu d)=False
  | Just tree<-sideTree d,x>=1,x<treeWidth tree-2,y>=2,y<2+treeContentRows d =
      maybe False (protectedPath d . nodePath) (at (treeRows tree) (treeScroll tree+y-2))
  | otherwise=False

privateBrowserCell :: Desktop -> Dialog -> Int -> Int -> Bool
privateBrowserCell d dg x y=case purpose dg of
  Opening base _ _ -> any (privateFieldCell base) rows
  ChangingDirectory base _ -> any (privateFieldCell base) rows
  Saving bid _ -> any (\(r,field)->case field of
    Input _ value _ -> inside r x y && y==top r+1 && (protectedBuffer d bid || privateName (startingDirectory d) (T.unpack value))
    _ -> False) rows
  _ -> False
  where
    rows=zip (fieldRects d dg) (fields dg)
    privateName base name=protectedPath d (normalise (if isAbsolute name then name else base </> name))
    privateFieldCell base (r,Input _ value _)=inside r x y && y==top r+1 && privateName base (T.unpack value)
    privateFieldCell base (r,FileList entries chosen)
      | not (inside r x y)=False
      | y==top r+12=privateEntry chosen
      | y==top r+11=case purpose dg of Opening _ pattern _ -> privateName base (T.unpack pattern); _ -> privateName base base
      | y>=top r+2 && y<top r+10,x>left r,x<left r+cw+1=privateEntry (page+y-top r-2)
      | y>=top r+2 && y<top r+10,x>left r+cw+1,x<left r+2*cw+2=privateEntry (page+8+y-top r-2)
      | otherwise=False
      where
        cw=max 1 ((width r-3) `div` 2)
        page=(max 0 chosen `div` 16)*16
        privateEntry index=maybe False (privateName base . T.unpack . entryName) (at entries index)
    privateFieldCell _ _=False

sensitiveLabel :: Text -> Bool
sensitiveLabel label=any (`T.isInfixOf` lower) ["password","token","secret","credential","auth","api key","api_key","apikey","accesskey","privatekey","passphrase","session","environment"] || "key" `elem` T.words (T.map (\c->if c `elem` ['_','-'] then ' ' else c) lower) || lower=="arguments (json array)"
  where lower=T.map toLower label
privateAgentChoice :: Desktop -> Int -> Int -> Bool
privateAgentChoice d x y=case (contextMenu d,contextKind d) of
  (Just (r,chosen),AgentContext items) | inside r x y -> case at items (contextOffset r chosen+y-top r-1) of
    Just (_,AgentChoose ident) -> case find ((==ident).settingId) (conversationSettings d) of
      Just option | secret option -> x>=left r+2+displayColumn (settingName option) (T.length (settingName option))+2
      _ -> False
    Just (_,AgentSet ident _) -> maybe False secret (find ((==ident).settingId) (conversationSettings d)) && x>=left r+2
    _ -> False
  _ -> False
  where secret option=any sensitiveLabel [settingId option,settingName option,settingCategory option]

pointerAllowedAt :: Desktop -> Int -> Int -> Bool
pointerAllowedAt d x y
  | not (onScreen d x y) || guestModalBlocked d=False
  | Just dg<-dialog d=inside (dialogRect d dg) x y && not (any (\(r,f)->privateDialogField d dg f && inside r x y) (zip (fieldRects d dg) (fields dg)))
  | Just (r,chosen)<-contextMenu d,inside r x y=maybe False (guestCommandAllowed . snd) (at (contextItems (contextKind d)) (contextOffset r chosen+y-top r-1))
  | Just (index,_)<-menu d,inside (menuRect d index) x y=maybe False (\(MenuItem _ _ command)->guestCommandAllowed command) (at (menuItemsFor d index) (y-top (menuRect d index)-1))
  | overlayAt d x y=True
  | Just w<-topWindow d x y=not (protectedBuffer d (bufferId w))
  | otherwise=True
onScreen :: Desktop -> Int -> Int -> Bool
onScreen d=inside (uncurry (Rect 0 0) (screenSize d))
overlayAt :: Desktop -> Int -> Int -> Bool
overlayAt d x y=y==0 || y==snd (screenSize d)-1 || (messagesDisplayed d && inside (problemsRect d) x y) || maybe False (\tree->x<treeWidth tree) (sideTree d) || maybe False (\(r,_)->inside r x y) (contextMenu d) || maybe False (\(index,_)->inside (menuRect d index) x y) (menu d)
topWindow :: Desktop -> Int -> Int -> Maybe Window
topWindow d x y=find (\w->windowVisible d w && inside (bounds w) x y) (windows d)
contentPrivate :: (Desktop -> Text -> Int -> Bool) -> Desktop -> Document -> Window -> Int -> Int -> Bool
contentPrivate predicate d doc w x y
  | x<=left r || x>=left r+width r-1 || y<=top r || y>=top r+1+windowContentRows d doc w=False
  | otherwise=predicate d (contents b) offset
  where
    r=bounds w; b=documentBuffer doc
    row=y-top r-1+scrollRow w
    offset=bufferLineOffset b row+columnOffset (bufferLineAt b row) (x-left r-1+scrollColumn w)
at :: [a] -> Int -> Maybe a
at values index | index<0=Nothing | otherwise=case drop index values of value:_->Just value; _->Nothing
