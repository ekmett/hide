{-# LANGUAGE OverloadedStrings #-}
-- | Policy for host-attributed agent input, readable cells and authority files.
--
-- Readability, clickability, command/effect validation and post-transition checks
-- protect different paths into the editor. The host selects input origin and
-- isolates the clipboard for an entire agent batch. Filesystem callers must
-- canonicalize paths before using pure path policy. Agent read masks apply even
-- when the human has disabled streamer mode.
module Hide.GuestAccess
  ( InputOrigin(..), CellAccess(..), cellAccess, readableAt, pointerAllowedAt
  , streamerReadableAt, sensitiveLabel, sanitizedStatus, protectedPath, protectedFilePath, protectedPathParent, protectedBuffer, protectedWindow, privateDocument, sanitizedBuffer, sanitizedBufferContent
  , validateGuestEffects, guestCommandAllowed, guestCommandAllowedIn, guestEffectsAllowed, guestKeyboardAllowed, guestKeyAllowed, guestKeyCombinations
  , guestModalBlocked, guestTransitionAllowed, beginGuestInput, endGuestInput
  ) where
import Control.Exception (evaluate)
import Control.Monad (foldM,when)
import System.Mem.StableName (makeStableName)
import qualified Hide.Plugin.BufferHost as BufferHost
import Hide.Sidebar
import qualified Hide.Plugin.Tree as Tree
import System.Directory (canonicalizePath)
import Data.Char (toLower)
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.FilePath ((</>), isAbsolute, normalise)
import Hide.Browser (Entry(..))
import Hide.Buffer
import qualified Hide.Plugin.Menu as Plugin
import Hide.Model
import Hide.Privacy (protectedFilePath,protectedFilePathParent)

-- | Trusted host attribution; never accept an origin claimed by input JSON.
data InputOrigin = HumanInput | GuestInput deriving (Eq,Show)
data CellAccess = CellAccess { cellReadable :: Bool, cellClickable :: Bool } deriving (Eq,Show)

protectedBuffer :: Desktop -> Int -> Bool
protectedBuffer d bid=maybe False (\doc -> privateDocument d doc || maybe False (`elem` ["Conversation","Autocomplete","Agent request","Proposed agent edit"]) (documentLabel doc)) (M.lookup bid (buffers d))

-- | Plugin content is private until the host accepts explicit semantic grants.
-- Labels and painted cells cannot grant agent interaction or clipboard access.
protectedWindow :: Desktop -> Window -> Bool
protectedWindow d w=case bufferId w of Just bid->protectedBuffer d bid; Nothing->True

-- | Check authority/privacy policy on an already canonicalized filesystem path.
protectedPath :: Desktop -> FilePath -> Bool
protectedPath d=protectedFilePath (guestPrivatePaths d)

-- | Also protect ancestors whose removal could destroy authority stores.
protectedPathParent :: Desktop -> FilePath -> Bool
protectedPathParent d=protectedFilePathParent (guestPrivatePaths d)

-- | Omit private documents and blank private conversation spans, preserving offsets.
sanitizedBuffer :: Desktop -> Int -> Maybe Text
sanitizedBuffer d bid=do
  doc<-M.lookup bid (buffers d)
  let text=contents (documentBuffer doc)
  if privateDocument d doc then Nothing else case documentLabel doc of
    Just label | label `elem` ["Agent request","Proposed agent edit"] -> Nothing
    Just "Conversation" | byteMode (documentBuffer doc) -> Nothing
    Just "Conversation" -> Just (T.pack [if privateOffset d text n && c/='\n' && c/='\r' then ' ' else c | (n,c)<-zip [0..] (T.unpack text)])
    _ -> Just text
-- | Policy-checked immutable content and whether privacy masking changed text.
-- Ordinary buffers retain their measured tree without projecting whole text.
-- Conversation masking remains an explicit full-text worker operation.
sanitizedBufferContent :: Desktop -> Int -> Maybe (Bool,BufferContent)
sanitizedBufferContent d bid=do
  doc<-M.lookup bid (buffers d)
  safe<-sanitizedBuffer d bid
  let original=documentBuffer doc
  pure $ if documentLabel doc==Just "Conversation"
    then (safe/=contents original,bufferContent (newBuffer safe))
    else (False,bufferContent original)

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
  RegisteredMenu _ allowed -> allowed
  DebugCommand action | privateDebuggerAction action -> False
  GitDiff -> False
  GitCommit -> False
  AgentChoose{} -> False
  AgentSet{} -> False
  AgentDirectory -> False
  AgentOptions -> False
  ChatInputOptions -> False
  AutocompleteCommand{} -> False
  SubmitChat{} -> False
  ReloadBindings -> False
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
-- | Context-sensitive read checks also cover clipboard commands, which produce
-- no filesystem effect. Screen masks alone cannot protect Messages copy actions.
guestCommandAllowedIn :: Desktop -> Command -> Bool
guestCommandAllowedIn d cmd=guestCommandAllowed cmd && case cmd of
  Copy | messagesDisplayed d && problemsFocused d->all public (take 1 (drop (problemsSelected d) (diagnostics d)))
  CopyAllMessages->all public (diagnostics d)
  _->True
  where public=not . protectedPath d . diagnosticPath

-- | Validate resolved filesystem effects before agent-facing dispatch. Delayed
-- contributions retain their host-captured location; workers/adoption recheck
-- prepared canonical targets against their owning current policy as well.
validateGuestEffects :: Desktop -> [Effect] -> IO ()
validateGuestEffects d=mapM_ check
  where
    denied=ioError (userError "Agent tools cannot access editor authority or session-key files.")
    path name=canonicalizePath name >>= \resolved->when (protectedPath d resolved) denied
    buffer ident=when (protectedBuffer d ident) denied
    treePath trace=case (trace,sideTree d) of
      (hit:_,Just tree)->maybe (pure ()) path (Tree.infoResource . stateInfo =<< nodeAt hit tree)
      _->denied
    check effect=case effect of
      ReadPath name->path name
      ReadTree name->path name
      RefreshRenamedPath old new->path old >> path new
      LoadTree request _->treePath (requestAncestors request)
      InvokeTree trace _ _->treePath trace
      JumpTo name _ _->path name
      InvokeMenu _ _ (Just (MessagesTarget _ _ (Just (name,_,_))))->path name
      OpenChoice base input pattern->let chosen=T.unpack (if T.null input then pattern else input) in path (if isAbsolute chosen then chosen else base </> chosen)
      SaveDocument ident target _->buffer ident >> maybe (pure ()) path target
      DownloadDocument ident->buffer ident
      ResolveConflict conflict _->buffer (conflictBuffer conflict)
      _->pure ()

agentActionAllowed :: Text -> Bool
agentActionAllowed action=action `elem` ["compile","make","build-stop","run","run-options","run-config","toolchain","terminal","terminal-input","terminal-stop"]
guestEffectsAllowed :: [Effect] -> Bool
guestEffectsAllowed=all allowed
  where
    allowed AdoptPreparedBuild=False
    allowed SessionSidebarAction{}=False
    allowed SubmitInputForm{}=False
    allowed RetireInputForm{}=False
    allowed AgentSidebarAction{}=False
    allowed ReloadKeyBindings{}=False
    allowed (InvokeMenu _ origin _)=origin==Plugin.AgentMenu
    allowed (InvokeTree _ _ origin)=origin==Plugin.AgentMenu
    allowed (LoadTree _ origin)=origin==Plugin.AgentMenu
    allowed ReadGitDiff=False
    allowed AskGitCommit=False
    allowed WriteGitCommit{}=False
    allowed FollowLink{}=False
    allowed FollowTreeLink{}=False
    allowed EnvironmentAction{}=False
    allowed PermissionAction{}=False
    allowed SaveWideSectionTitles{}=False
    allowed SaveChatSubmit{}=False
    allowed AutocompleteAction{}=False
    allowed DownloadCancelAction{}=False
    allowed (DebugAction action _)=not (privateDebuggerAction action)
    allowed (AgentAction action _)=agentActionAllowed action
    allowed (SaveDocument _ _ follow)=maybe True guestCommandAllowed follow
    allowed ReadBrowserClipboard=False
    allowed WriteBrowserClipboard{}=False
    allowed _=True
protectedPurpose :: Purpose -> Bool
protectedPurpose p=case p of
  AgentChoiceDialog{} -> True
  CompletionChoiceDialog{} -> True
  PluginInputForm{} -> True
  AgentNewDialog -> True
  EnvironmentDialog{} -> True
  PermissionDialog{} -> True
  ChatInputSettings -> True
  AutocompleteDialog{} -> True
  DebugSourceWatchDialog{} -> True
  DebuggerWatchDialog{} -> True
  DebugDialog action -> privateDebuggerAction action
  AgentDialog action -> not (agentActionAllowed action)
  DiscardDraft -> True
  Confirm command -> not (guestCommandAllowed command)
  _ -> False
privateDebuggerAction :: Text -> Bool
-- Frame chooser labels are adapter metadata, not prepared public semantic rows.
-- Keep the host-owned chooser private until it carries canonical row provenance.
privateDebuggerAction action=action=="downloads" || "downloads:" `T.isPrefixOf` action || "hdb-" `T.isPrefixOf` action || privateFrameChooserAction action
privateFrameChooserAction :: Text -> Bool
privateFrameChooserAction action=case T.splitOn ":" action of
  ["select",_,_,"frame"]->True
  _->False

guestModalBlocked :: Desktop -> Bool
guestModalBlocked d=maybe False (protectedPurpose . purpose) (dialog d) || case (contextMenu d,contextKind d) of
  (Just _,AgentContext{}) -> True
  (Just _,WindowRowsContext) -> True
  _ -> False
guestKeyboardAllowed :: Desktop -> Bool
guestKeyboardAllowed d=not (guestModalBlocked d) && not (focusedPrivateField d) && (isJust (dialog d) || problemsFocused d || maybe False treeFocused (sideTree d) || maybe True (not . protectedWindow d) (activeWindow d))
guestKeyAllowed :: Desktop -> V.Key -> [V.Modifier] -> Bool
guestKeyAllowed d key mods=maybe True (guestCommandAllowedIn d) (boundKeyCommand key mods d) && not (guestModalBlocked d) && (guestKeyboardAllowed d || navigation || fieldNavigation)
  where
    fieldNavigation=isJust (dialog d) && case boundKeyCommand key mods d of
      Just cmd->cmd `elem` [DialogFocusNext,DialogFocusPrevious,DialogCancel]
      Nothing->key `elem` [V.KChar '\t',V.KBackTab,V.KEsc]
    navigation=dialog d==Nothing && case boundKeyCommand key mods d of
      Just cmd -> cmd `elem` [NextWindow,PreviousWindow,FocusSource]
      Nothing -> key==V.KFun 6 || key `elem` [V.KChar '\t',V.KBackTab] && any (`elem` mods) [V.MCtrl,V.MAlt] || V.MAlt `elem` mods && case key of V.KChar c -> c>='1' && c<='9'; _ -> False

-- Named navigation/editing shortcuts, plus Ctrl/Alt character shortcuts.
-- Ordinary text is represented by the separate keyboardAllowed metadata.
guestKeyCombinations :: [(Text,[V.Modifier])]
guestKeyCombinations=[(key,mods) | key<-["Enter","Escape","Tab","ArrowUp","ArrowDown","ArrowLeft","ArrowRight","Home","End","PageUp","PageDown","Backspace","Delete","Insert"]++["F"<>T.pack (show n) | n<-[1::Int ..24]],mods<-[[],[V.MShift],[V.MCtrl],[V.MAlt]]]++[(T.singleton c,mods) | c<-['a'..'z']++['1'..'9']++[' '],mods<-[[V.MCtrl],[V.MAlt]]]

-- | Validate a proposed guest transition while the owning session is serialized.
-- Only small UI metadata is compared structurally. Immutable protected documents,
-- drafts and questions use constructor identities; equal revisions alone do not
-- establish unchanged content. Capture evaluates WHNF, never text or Undo.
-- Conservatively reject replacement of a protected document, including metadata.
guestTransitionAllowed :: Desktop -> Desktop -> [Effect] -> IO Bool
guestTransitionAllowed before after effects
  | not metadataUnchanged = pure False
  | otherwise = do
      composer<-sameContent (composerBuffer before) (composerBuffer after)
      autocomplete<-sameContent (autocompleteDraft before) (autocompleteDraft after)
      question<-case (chatQuestion before,chatQuestion after) of
        (Nothing,Nothing)->pure True
        (Just a,Just b)->sameConstructor a b
        _->pure False
      if not (composer && autocomplete && question) then pure False else
        foldM unchanged True (M.toList (buffers before))
  where
    metadataUnchanged=guestEffectsAllowed effects && not (guestModalBlocked after) &&
      streamerMode before==streamerMode after && chatSubmit before==chatSubmit after && privateFieldsUnchanged before after &&
      composerSelection before==composerSelection after &&
      autocompleteACPEnabled before==autocompleteACPEnabled after &&
      autocompleteSelection before==autocompleteSelection after && autocompleteFocused before==autocompleteFocused after &&
      agentSettings before==agentSettings after && childAgentSettings before==childAgentSettings after &&
      childAgentSteering before==childAgentSteering after && childAgentContextUsage before==childAgentContextUsage after
    sameContent a b=BufferHost.captureVersion a >>= \version->BufferHost.versionCurrent version b
    sameConstructor a b=(==) <$> (evaluate a >>= makeStableName) <*> (evaluate b >>= makeStableName)
    unchanged False _=pure False
    unchanged True (bid,doc)
      | not (protectedBuffer before bid)=pure True
      | Just next<-M.lookup bid (buffers after)=sameConstructor doc next
      | otherwise=pure False

-- Both text-entry widgets carry the same privacy semantics; selection is UI state.
inputValue :: Field -> Maybe (Text,Text)
inputValue (Input label value _)=Just (label,value)
inputValue (SelectedInput label value _)=Just (label,value)
inputValue _=Nothing

privateField :: Field -> Bool
privateField field | Just (label,_)<-inputValue field=sensitiveLabel label
privateField (CheckBox "Streamer mode" _)=True
privateField _=False
pluginForm :: Dialog -> Bool
pluginForm dg=case purpose dg of PluginInputForm{}->True; _->False
privateDialogField :: Desktop -> Dialog -> Field -> Bool
privateDialogField d dg field=pluginForm dg || privateSourceWatch d dg || privateField field || case inputValue field of
  Just (_,value) -> case purpose dg of
    Opening base _ _ -> privateName base value
    ChangingDirectory base _ -> privateName base value
    Saving bid _ -> protectedBuffer d bid || privateName (startingDirectory d) value
    _ -> False
  _ -> False
  where
    privateName base value=let name=T.unpack value in protectedPath d (normalise (if isAbsolute name then name else base </> name))
-- Privacy is captured from the source owner when the watch prompt opens, never
-- inferred from an ordinary field label. Current policy can add protection.
privateSourceWatch :: Desktop -> Dialog -> Bool
privateSourceWatch d dg=case purpose dg of
  DebugSourceWatchDialog _ bid origin captured->captured || protectedBuffer d bid || maybe False (protectedPath d) origin
  DebuggerWatchDialog _ origin captured->captured || maybe False (protectedPath d) origin
  _->False

focusedPrivateField :: Desktop -> Bool
focusedPrivateField d=case dialog d of Just dg -> maybe False (privateDialogField d dg) (at (fields dg) (focus dg)); _ -> False
-- Compare private field metadata across dialog replacement as well. Purpose may
-- carry a disk baseline or conflict payload, so its derived equality is unsuitable.
privateFieldsUnchanged :: Desktop -> Desktop -> Bool
privateFieldsUnchanged before after=case (dialog before,dialog after) of
  (Just a,Just b) -> values before a==values after b
  _ -> True
  where
    values d dg=[value field | field<-fields dg,privateDialogField d dg field]
    value field | Just pair<-inputValue field=Left pair
    value (CheckBox label checked)=Right (label,checked)
    value _=Left ("","")

sanitizedStatus :: Desktop -> Text
sanitizedStatus d | "Session " `T.isPrefixOf` status d="Session [redacted]"
                  | otherwise=status d

-- | Begin a serialized agent batch with isolated clipboard and cleared gesture state.
beginGuestInput :: Desktop -> Desktop
beginGuestInput d=(clearGestures d) {clipboard="",clipboardCode=Nothing}
-- | Restore the human clipboard after the entire agent input batch.
endGuestInput :: Desktop -> Desktop -> Desktop
endGuestInput original updated=(clearGestures updated) {clipboard=clipboard original,clipboardCode=clipboardCode original}
clearGestures :: Desktop -> Desktop
clearGestures d=d {drag=Nothing,dragOriginal=Nothing,prefix=Nothing,heldModifiers=[],buttonPressed=Nothing,buttonHover=Nothing,blockStart=Nothing}

cellAccess :: Desktop -> Int -> Int -> CellAccess
cellAccess d x y=CellAccess (readableAt d x y) (pointerAllowedAt d x y)
readableAt :: Desktop -> Int -> Int -> Bool
readableAt d x y
  | Just dg<-dialog d,pluginForm dg,inside (dialogRect d dg) x y || y==snd (screenSize d)-1=False
  | Just dg<-dialog d, DebugDialog action<-purpose dg, privateDebuggerAction action,
    inside (dialogRect d dg) x y || y==snd (screenSize d)-1=False
  | Just dg<-dialog d, PermissionDialog{}<-purpose dg, inside (dialogRect d dg) x y || y==snd (screenSize d)-1=False
  | otherwise=onScreen d x y && streamerReadableAt d x y && case dialog d of
  Just dg | inside (dialogRect d dg) x y -> True
  _ | overlayAt d x y -> True
    | otherwise -> case topWindow d x y of
        Just w | protectedWindow d w -> case windowDocument (buffers d) w of
          Just doc | documentLabel doc==Just "Autocomplete" -> not (autocompletePane d w && inside (autocompleteComposerRect d w) x y)
          Just doc | documentLabel doc==Just "Conversation" -> not (byteMode (documentBuffer doc)) && not (inside (composerRect d w) x y) && not (contentPrivate privateOffset d doc w x y)
          _ -> False
        _ -> True

-- Independent of the human Streamer-mode toggle. Guests ALWAYS use this mask.
-- Labels remain visible; only sensitive value rows are blanked.
streamerReadableAt :: Desktop -> Int -> Int -> Bool
streamerReadableAt d x y
  | contextKind d==WindowRowsContext,Just (rect,_)<-contextMenu d,inside rect x y=False
  | Just dg<-dialog d,pluginForm dg,inside (dialogRect d dg) x y || y==snd (screenSize d)-1=False
  | Just dg<-dialog d, DebugDialog action<-purpose dg, (action=="downloads" || "downloads:" `T.isPrefixOf` action || privateFrameChooserAction action),
    inside (dialogRect d dg) x y || y==snd (screenSize d)-1=False
  | y==snd (screenSize d)-1, "Session " `T.isPrefixOf` status d=False
  | otherwise=case dialog d of
  Just dg | inside (dialogRect d dg) x y -> not (any (sensitiveValue dg) (zip (fieldRects d dg) (fields dg))) && not (privateBrowserCell d dg x y)
  _ | privateAgentChoice d x y -> False
    | privateTreeCell d x y -> False
    | privateMessageCell d x y -> False
    | overlayAt d x y -> True
    | otherwise -> case topWindow d x y of
        Just w | PluginContent _<-windowContent w,streamerMode d -> False
        Just w | Just doc<-windowDocument (buffers d) w,privateDocument d doc -> False
        Just w | Just doc<-windowDocument (buffers d) w,documentLabel doc==Just "Conversation" -> not (contentPrivate (\_ -> sessionOffset) d doc w x y)
        _ -> True
  where
    sensitiveValue dg (r,f)=privateDialogField d dg f && y>top r && inside r x y && y>=top (dialogRect d dg)+2 && y<top (dialogRect d dg)+height (dialogRect d dg)-3
-- Only recognized browser/tree paths are checked. Ordinary source text and
-- unrelated filenames are never scanned for strings which resemble secrets.
privateTreeCell :: Desktop -> Int -> Int -> Bool
privateTreeCell d x y
  | maybe False (\(r,_)->inside r x y) (contextMenu d) || maybe False (\(i,_)->inside (menuRect d i) x y) (menu d)=False
  | Just tree<-sideTree d,x>=1,x<treeWidth tree-2,y>=2,y<2+treeContentRows d =
      maybe False (maybe False (protectedPath d) . Tree.infoResource . rowInfo) (rowAt (treeScroll tree+y-2) tree)
  | otherwise=False

-- Keep diagnostic indices intact for the human; mask the whole protected row,
-- including its source name and message, in both agent and Streamer projections.
privateMessageCell :: Desktop -> Int -> Int -> Bool
privateMessageCell d x y
  | maybe False (\(r,_)->inside r x y) (contextMenu d) || maybe False (\(i,_)->inside (menuRect d i) x y) (menu d)=False
  | messagesDisplayed d, inside r x y,y>top r,y<top r+height r-1 =
      maybe False (protectedPath d . diagnosticPath) (at (diagnostics d) (problemsScroll d+y-top r-1))
  | otherwise=False
  where r=problemsRect d

privateBrowserCell :: Desktop -> Dialog -> Int -> Int -> Bool
privateBrowserCell d dg x y=case purpose dg of
  Opening base _ _ -> any (privateFieldCell base) rows
  ChangingDirectory base _ -> any (privateFieldCell base) rows
  Saving bid _ -> any (\(r,field)->case inputValue field of
    Just (_,value) -> inside r x y && y==top r+1 && (protectedBuffer d bid || privateName (startingDirectory d) (T.unpack value))
    _ -> False) rows
  _ -> False
  where
    rows=zip (fieldRects d dg) (fields dg)
    privateName base name=protectedPath d (normalise (if isAbsolute name then name else base </> name))
    privateFieldCell base (r,field) | Just (_,value)<-inputValue field=inside r x y && y==top r+1 && privateName base (T.unpack value)
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
  | Just (r,chosen)<-contextMenu d,inside r x y=maybe False (guestCommandAllowedIn d . snd) (at (contextItemsFor d) (contextOffset r chosen+y-top r-1))
  | Just (index,_)<-menu d,inside (menuRect d index) x y=maybe False (\(MenuItem _ _ command)->guestCommandAllowedIn d command) (at (menuItemsFor d index) (y-top (menuRect d index)-1))
  | Just (_,_,action)<-find (\(r,_,_)->inside r x y) (statusItemRects d)=case action of
      Left command->guestCommandAllowedIn d command
      Right (V.EvKey key mods)->guestKeyAllowed d key mods
      Right _->False
  | overlayAt d x y=True
  | Just w<-topWindow d x y=not (protectedWindow d w)
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
    offset=windowTextOffset d w (bufferContent b) row (x-left r-1+scrollColumn w)
at :: [a] -> Int -> Maybe a
at values index | index<0=Nothing | otherwise=case drop index values of value:_->Just value; _->Nothing
