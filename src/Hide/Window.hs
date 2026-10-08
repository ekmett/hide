{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface, OverloadedStrings #-}
-- | SDL/Metal/Vulkan adapter for the common Vty picture and input model.
--
-- Grapheme-aware spans select exact bitmap tiles for interface geometry and native
-- shaping for other text. The window thread owns SDL calls, event handling and
-- presentation; render keys decide when a frame is needed. Exported FFI helpers
-- also serve the remote native frontend rather than a second drawing ABI.
module Hide.Window (runWindow, nativeMenuShortcut, nativeChordShortcut, nativeMenuEvent, nativeMenuEventFor, nativeMenuToken, nativeCommands, nativeCommandsFor, nativeDockWindow
#ifdef WITH_WINDOW
  , check, utf8, nativeMenus, nativeMenusFor, installNativeMenus, updateDockWindows
  , c_accessibility, c_cancel_file_drag, c_arm_file_drag, c_system_dark, c_open, c_mode, c_scale, c_title, c_raise, c_close, c_size
  , c_begin, c_clip, c_glyph, c_unicode, c_pixelate_unicode, c_cursor, c_cursor_blink
  , c_crt_filter, c_present, c_wait, c_event_age_ns, c_wake, c_text, c_clipboard, c_set_clipboard
#ifdef darwin_HOST_OS
  , c_dock_generation, c_menu_enabled, c_menu_prepare, c_menu_generation, c_menu_shortcut
#endif
#endif
  ) where
import Hide.Frontend
import Hide.Model
import Hide.Commands (builtinCommands, builtinAction)
import Hide.Bindings (readChord)
import qualified Data.Text as Text
import qualified Graphics.Vty as Keys
import Data.Char (chr, toLower)
import Data.Maybe (mapMaybe)
#ifdef WITH_WINDOW
import Data.List (elemIndex)
import Control.Exception (bracket_)
import Control.Monad (forM_, when, unless, foldM)
import Data.Foldable (toList)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Unsafe as TU
import Foreign
import Foreign.C
import qualified Graphics.Vty as V
import System.Environment (lookupEnv)
import System.Directory (getCurrentDirectory)
import System.Info (os)
import System.IO (hPutStrLn, stderr)
import Hide.Unicode (Script(..), CellSpan(..), clusterWidth)
import Hide.TextStyle
import Hide.Font
import Hide.Render

#endif

-- | Native tokens index the command catalogue, never a menu occurrence. The
-- platform stamps each event with the menu incarnation that produced it.
nativeCommands :: [Command]
nativeCommands = map builtinAction builtinCommands

-- | Reject queued events from a retired native menu before resolving a token.
-- Availability and caller policy must still be checked against current host state.
nativeMenuEvent :: Int -> [Int] -> Maybe Command
nativeMenuEvent=nativeMenuEventFor nativeCommands

-- | Bounded catalogue assembled from the published session snapshot. Dynamic
-- entries retain exact refs; labels and frontend positions are never identities.
nativeCommandsFor :: Desktop -> [Command]
nativeCommandsFor d=nativeCommands++map (contributionCommand d) (contributedMenus d)

nativeMenuEventFor :: [Command] -> Int -> [Int] -> Maybe Command
nativeMenuEventFor commands generation event=do
  token<-nativeMenuToken (length commands) generation event
  case drop token commands of command:_ -> Just command; _ -> Nothing

nativeMenuToken :: Int -> Int -> [Int] -> Maybe Int
nativeMenuToken count generation (11:token:incarnation:_)
  | generation>0, incarnation==generation, token>=0,token<count=Just token
nativeMenuToken _ _ _=Nothing

-- | Dock incarnation and stable view IDs never share main-menu token positions.
nativeDockWindow :: [(Int,Text.Text,Bool,Bool)] -> Int -> [Int] -> Maybe Int
nativeDockWindow entries generation (16:ident:incarnation:_)
  | generation>0,incarnation==generation,any (\(wid,_,_,enabled)->wid==ident && enabled) entries = Just ident
nativeDockWindow _ _ _ = Nothing

#ifdef WITH_WINDOW
foreign import ccall unsafe "thc_system_dark" c_system_dark :: IO CInt
foreign import ccall unsafe "thc_open" c_open :: CString -> CDouble -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "thc_mode" c_mode :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "thc_scale" c_scale :: CInt -> IO CInt
foreign import ccall unsafe "thc_raise" c_raise :: IO ()
foreign import ccall unsafe "thc_accessibility" c_accessibility :: CString -> CSize -> IO CInt
foreign import ccall unsafe "thc_cancel_file_drag" c_cancel_file_drag :: IO ()
foreign import ccall unsafe "thc_arm_file_drag" c_arm_file_drag :: CString -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "thc_title" c_title :: CString -> IO ()
foreign import ccall unsafe "thc_close" c_close :: IO ()
foreign import ccall unsafe "thc_error" c_error :: IO CString
foreign import ccall unsafe "thc_backend" c_backend :: IO CString
foreign import ccall unsafe "thc_size" c_size :: Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall unsafe "thc_begin" c_begin :: IO CInt
foreign import ccall unsafe "thc_clip" c_clip :: CInt -> CInt -> IO ()
foreign import ccall unsafe "thc_glyph" c_glyph :: CInt -> CInt -> CInt -> CInt -> Ptr Word16 -> Word32 -> Word32 -> Word32 -> CInt -> CInt -> IO ()
foreign import ccall unsafe "thc_unicode" c_unicode :: CInt -> CInt -> CInt -> CString -> Word32 -> Word32 -> Word32 -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "thc_pixelate_unicode" c_pixelate_unicode :: CInt -> IO ()
foreign import ccall unsafe "thc_cursor" c_cursor :: CInt -> CInt -> IO ()
foreign import ccall unsafe "thc_cursor_blink" c_cursor_blink :: CInt -> IO ()
foreign import ccall unsafe "thc_crt_filter" c_crt_filter :: CInt -> IO ()
-- Presentation can wait for vblank. Let receiver/sender threads run meanwhile.
foreign import ccall safe "thc_present" c_present :: IO CInt
foreign import ccall unsafe "thc_wake" c_wake :: IO ()
foreign import ccall safe "thc_wait" c_wait :: Ptr Int32 -> IO CInt
-- Relative SDL event age avoids comparing SDL and Haskell clock epochs.
foreign import ccall unsafe "thc_event_age_ns" c_event_age_ns :: IO Word64
foreign import ccall unsafe "thc_text" c_text :: IO CString
foreign import ccall unsafe "thc_clipboard" c_clipboard :: IO CString
foreign import ccall unsafe "thc_set_clipboard" c_set_clipboard :: CString -> IO ()
#ifdef darwin_HOST_OS
foreign import ccall unsafe "thc_dock_generation" c_dock_generation :: IO CInt
foreign import ccall unsafe "thc_dock_begin" c_dock_begin :: IO ()
foreign import ccall unsafe "thc_dock_item" c_dock_item :: CInt -> CString -> CInt -> CInt -> IO ()
foreign import ccall unsafe "thc_dock_end" c_dock_end :: IO ()
foreign import ccall unsafe "thc_menu_prepare" c_menu_prepare :: IO ()
foreign import ccall unsafe "thc_menu_generation" c_menu_generation :: IO CInt
foreign import ccall unsafe "thc_menu_clear" c_menu_clear :: CInt -> CInt -> CInt -> IO ()
foreign import ccall unsafe "thc_menu_add" c_menu_add :: CString -> IO ()
foreign import ccall unsafe "thc_menu_separator" c_menu_separator :: IO ()
foreign import ccall unsafe "thc_menu_item" c_menu_item :: CString -> CString -> CInt -> CInt -> CInt -> IO ()
foreign import ccall unsafe "thc_menu_shortcut" c_menu_shortcut :: CInt -> CString -> CInt -> IO ()
foreign import ccall unsafe "thc_menu_enabled" c_menu_enabled :: CInt -> CInt -> IO ()
#endif

-- | Turn a zero native status into an IO error with native diagnostic text.
check :: String -> IO CInt -> IO ()
check context action = do
  ok <- action
  when (ok == 0) $ do
    err <- c_error >>= peekCString
    ioError (userError (context ++ ": " ++ err))

-- | Lend a temporary NUL-terminated UTF-8 C string for the callback only.
utf8 :: T.Text -> (CString -> IO a) -> IO a
utf8 text = BS.useAsCString (TE.encodeUtf8 text)

nativeMenus :: IO ()
nativeMenus=nativeMenusFor (initialDesktop (80,25))

-- | Install exactly the menu rows painted by the host, assigning tokens from
-- the retained command catalogue. Rebuilding stamps a fresh native incarnation.
nativeMenusFor :: Desktop -> IO ()
nativeMenusFor d=installNativeMenus [(title,[(name,nativeMenuShortcut d cmd,number cmd) | MenuItem name _ cmd<-menuItemsFor d i]) | (i,(title,_,_))<-zip [0..] menus]
  where
    number Disabled{} = -1
    number cmd=maybe (error "Missing native command registration") id (elemIndex cmd (nativeCommandsFor d))

-- | The remote frontend uses the same Cocoa installer with host-issued tokens.
installNativeMenus :: [(T.Text,[(T.Text,(String,Int),Int)])] -> IO ()
#ifdef darwin_HOST_OS
installNativeMenus layout=do
  c_menu_clear (number About) (number EditorOptions) (number Quit)
  forM_ layout $ \(title,items)->do
    utf8 title c_menu_add
    forM_ items $ \(name,(key,mods),token)->
      if token<0 then c_menu_separator else unless (token `elem` map (fromIntegral . number) [About,EditorOptions,Quit]) $
        utf8 name $ \namePtr->withCString key $ \keyPtr->c_menu_item namePtr keyPtr (fromIntegral mods) (fromIntegral token) 1
  where number cmd=maybe (error "Missing native application command") fromIntegral (elemIndex cmd nativeCommands)
#else
installNativeMenus _ = pure ()
#endif

-- | Publish only prepared window metadata, on the native window thread.
updateDockWindows :: [(Int,T.Text,Bool,Bool)] -> IO ()
#ifdef darwin_HOST_OS
updateDockWindows entries=do
  c_dock_begin
  forM_ entries $ \(ident,title,selected,enabled)->utf8 title $ \text->
    c_dock_item (fromIntegral ident) text (if selected then 1 else 0) (if enabled then 1 else 0)
  c_dock_end
#else
updateDockWindows _=pure ()
#endif

updateMenus :: Desktop -> IO ()
#ifdef darwin_HOST_OS
updateMenus d = do
  updateDockWindows (editorWindowEntries d)
  forM_ (zip [0..] (nativeCommandsFor d)) $ \(i,cmd) -> do
    let (shortcut,modifiers)=nativeMenuShortcut d cmd
    withCString shortcut $ \keyPtr -> c_menu_shortcut (fromIntegral i) keyPtr (fromIntegral modifiers)
    c_menu_enabled i (if menuCommandAvailable d cmd then 1 else 0)
#else
updateMenus _ = pure ()
#endif

-- Consume the composed grid directly; partial glyphs retain full origin/width.
draw :: Font -> Desktop -> IO ()
-- C copies all sixteen scanlines before returning. One frame-owned array can
-- stage every bitmap glyph without retaining a pointer into temporary storage.
draw font d = allocaArray 16 $ \scratch -> do
  c_cursor_blink (if blinkCursor d then 1 else 0)
  c_crt_filter (if crtFilter d then 1 else 0)
  c_pixelate_unicode (if pixelateUnicode d then 1 else 0)
  check "Allocate window frame" c_begin
  forM_ (zip [0::Int ..] (toList (renderCellRows d))) $ \(y,spans) -> go scratch y 0 (toList spans)
  case renderCursor d of
    V.Cursor x y -> c_cursor (fromIntegral x) (fromIntegral y)
    _ -> pure ()
  check "Present window frame" c_present
  where
    go :: Ptr Word16 -> Int -> Int -> [CellSpan] -> IO ()
    go _ _ _ [] = pure ()
    go scratch y x (CellText a text:rest) = do
      -- CellText contains complete single-codepoint, one-cell glyphs. Advancing
      -- by iter's byte delta keeps fallback slices on original UTF-8 boundaries.
      let paint=textStyleFromAttr a
          !fg=fromIntegral (textForeground paint)
          !bg=fromIntegral (textBackground paint)
          !flags=fromIntegral (textFlags paint)
          end=TU.lengthWord8 text
          chars !offset !at
            | offset==end = pure at
            | otherwise = case TU.iter text offset of
                TU.Iter ch bytes -> do
                  c_clip (fromIntegral at) 1
                  if bitmapGlyph font ch then do
                    let Glyph gw bitmap=glyph font ch
                    pokeArray scratch bitmap
                    c_glyph (fromIntegral at) (fromIntegral y) 1
                      (fromIntegral gw) scratch fg bg flags 0 1
                  else utf8 (TU.takeWord8 bytes (TU.dropWord8 offset text)) $ \encoded ->
                    check "Draw Unicode"
                      (c_unicode (fromIntegral at) (fromIntegral y) 1 encoded fg bg flags 0 1)
                  chars (offset+bytes) (at+1)
      next<-chars 0 x
      go scratch y next rest
    go scratch y x (CellGlyph a text full start shown:rest) = do
      drawGlyph scratch y x a text full start shown 0 full
      go scratch y (x+shown) rest
    go scratch y x (CellScript a text natural script:rest) = do
      drawGlyph scratch y x a text 1 0 1 (case script of Superscript -> 1; Subscript -> 2) natural
      go scratch y (x+1) rest
    drawGlyph :: Ptr Word16 -> Int -> Int -> V.Attr -> T.Text -> Int -> Int -> Int -> CInt -> Int -> IO ()
    drawGlyph scratch y visible a text full start shown script natural = do
      let paint=textStyleFromAttr a
          fg=fromIntegral (textForeground paint); bg=fromIntegral (textBackground paint)
          flags=fromIntegral (textFlags paint+if script==0 && full/=clusterWidth text then 4 else 0)
          x=visible-start
      c_clip (fromIntegral visible) (fromIntegral shown)
      case T.unpack text of
        [ch] | bitmapGlyph font ch -> do
          let Glyph gw bitmap=glyph font ch
          pokeArray scratch bitmap
          c_glyph (fromIntegral x) (fromIntegral y) (fromIntegral full) (fromIntegral gw) scratch fg bg flags script (fromIntegral natural)
        _ -> utf8 text $ \encoded -> check "Draw Unicode" (c_unicode (fromIntegral x) (fromIntegral y) (fromIntegral full) encoded fg bg flags script (fromIntegral natural))


-- | Run on the main bound OS thread and scope native-window cleanup.
-- Translate native events/effects while using explicit metadata/identity redraw keys.
runWindow :: Backend -> Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> Desktop -> IO ()
runWindow backend scale effects tick initial = do
  font <- loadFont
#ifdef darwin_HOST_OS
  c_menu_prepare
#endif
  let driver = case backend of Metal -> "metal"; Vulkan -> "vulkan"; _ -> if os == "darwin" then "metal" else "vulkan"
  -- SDL must stay on the main OS thread; GHC's main action is a bound thread.
  bracket_ (pure ()) c_close $ do
    withCString driver $ \name -> check ("Cannot start " ++ driver ++ " window") (c_open name (realToFrac scale) (fromIntegral (fst (screenSize initial))) (fromIntegral (snd (screenSize initial))) (fromIntegral (modeHeight (maybe 3 id (videoMode initial)))))
    c_backend >>= peekCString >>= hPutStrLn stderr . ("Haskell renderer: " ++)
    nativeMenusFor initial
    sized <- alloca $ \wp -> alloca $ \hp -> do
      c_size wp hp
      w <- fromIntegral <$> peek wp
      h <- fromIntegral <$> peek hp
      pure ((fst (handleEvent (V.EvResize w h) initial {nativeMac=os == "darwin"})) {menu=menu initial,contextMenu=if (w,h)==screenSize initial then contextMenu initial else Nothing})
    captureOnly <- (== Just "1") <$> lookupEnv "THC_EDIT_CAPTURE_EXIT"
    if captureOnly then systemTheme sized >>= draw font else systemTheme sized >>= tick >>= loop font Nothing
  where
    systemTheme d = do value<-c_system_dark; pure d {systemDark=value/=0}
    loop font previous pending = do
      d<-case clipboardExport pending of
        (serial,Just text)->do
          utf8 text c_set_clipboard
          pure pending {clipboardExport=(serial,Nothing)}
        _->pure pending
      let ready=d
      key<-renderKey ready
      when (fmap (\(_,old,_)->old) previous /= Just key) $ do
        when (fmap (\(_,_,catalogue)->catalogue) previous/=Just (contributedMenus d)) (nativeMenusFor ready)
        when (fmap (\(title,_,_)->title) previous /= Just (applicationTitle "" d)) $ do
          cwd <- getCurrentDirectory
          utf8 (applicationTitle cwd ready) c_title
        updateMenus ready
        draw font ready
      event <- allocaArray 6 $ \p -> check "Read window event" (c_wait p) >> map fromIntegral <$> peekArray 6 p
      (next,requests) <- dispatch event ready
      (exit,updated) <- foldM windowEffect (False,next) requests
      when (clipboard updated /= clipboard d) (utf8 (clipboard updated) c_set_clipboard)
      let displayed = case event of kind:_ | kind `elem` [3,4,5,7,8,9,12] -> Nothing; _ -> Just (applicationTitle "" d,key,contributedMenus d)
      unless exit (systemTheme updated >>= tick >>= loop font displayed)
    windowEffect state@(True,_) _ = pure state
    windowEffect (_,d) request = applyWindowEffect d request
    applyWindowEffect d (SetScreenMode mode) = do
      let (cols,rows) = modeSize mode
      ok <- c_mode (fromIntegral (modeHeight mode)) (fromIntegral cols) (fromIntegral rows)
      err <- if ok == 0 then c_error >>= peekCString else pure ""
      alloca $ \wp -> alloca $ \hp -> do
        c_size wp hp
        w <- fromIntegral <$> peek wp
        h <- fromIntegral <$> peek hp
        pure (False, if ok == 0
          then message "Cannot change screen mode" [T.pack err] (fst (handleEvent (V.EvResize w h) d))
          else (resizeScreenMode (w,h) d) {videoMode=Just mode})
    applyWindowEffect d request = effects d [request]
    dispatch (1:key:mods:_) d
      | not (activeTerminal d/=Nothing && mods .&. 15==2), Just direction <- zoomDirection key mods = changeScale (fromIntegral direction) d
      | otherwise = case decodeKey key mods of
          Nothing -> pure (d,[])
          Just ev -> dispatchKey ev d
    dispatch (14:_) d = do
      path <- c_text >>= BS.packCString
      pure (d,[ReadPath (T.unpack (TE.decodeUtf8 path))])
    dispatch (2:_) d = do
      bytes <- c_text >>= BS.packCString
      case TE.decodeUtf8' bytes of
        Left _ -> pure (d,[])
        Right text -> foldText text d
    dispatch (3:x:y:clicks:mods:button:_) d
      | button == 3 = pure (handleEvent (V.EvMouseDown x y V.BRight (keyMods mods)) d)
      | clicks == 0, dialog d /= Nothing || drag d == Nothing = pure (hoverAt x y d)
      | clicks >= 2 = pure (handleDoubleClick x y d)
      | otherwise = clipboardResult (copyClick x y d) d (handleEvent (V.EvMouseDown x y V.BLeft (keyMods mods)) d)
    dispatch (4:x:y:_) d = pure (handleEvent (V.EvMouseUp x y (Just V.BLeft)) d)
    dispatch (5:w:h:_) d = pure (handleEvent (V.EvResize w h) d)
    dispatch (6:_) d | dialog d /= Nothing = pure (d,[])
                    | otherwise = pure (runCommand Quit d)
    dispatch (7:_) d = pure (hoverAt (-1) (-1) d {drag=Nothing,dragOriginal=Nothing,prefix=Nothing,buttonPressed=Nothing,heldModifiers=[]})
    dispatch (9:x:y:direction:mods:_) d = pure (wheelEvent x y direction (keyMods mods) d)
    dispatch event@(11:_) d = do
#ifdef darwin_HOST_OS
      generation<-fromIntegral <$> c_menu_generation
      case nativeMenuEventFor (nativeCommandsFor d) generation event of
        Just cmd | menuCommandAvailable d cmd ->
          if cmd==Paste then paste d else clipboardResult (cmd `elem` [Copy,Cut]) d (runCommand cmd d)
        _ -> pure (d,[])
#else
      pure (d,[])
#endif
    dispatch event@(16:_) d = do
#ifdef darwin_HOST_OS
      generation<-fromIntegral <$> c_dock_generation
      case nativeDockWindow (editorWindowEntries d) generation event of
        Just ident -> c_raise >> pure (activateEditorWindow ident d,[])
        _ -> pure (d,[])
#else
      pure (d,[])
#endif
    dispatch (12:x:y:_) d = pure (hoverAt x y d)
    dispatch (13:mods:_) d = pure (d {heldModifiers=keyMods mods},[])
    dispatch _ d = pure (d,[])
    changeScale direction d = do
      ok <- c_scale direction
      if ok /= 0 then pure (d,[]) else do
        err <- c_error >>= peekCString
        pure (message "Cannot resize character tiles" [T.pack err] d,[])
    keyMods m = case decodeKey 0 m of Just (V.EvKey _ ms) -> ms; _ -> []
    dispatchKey ev@(V.EvKey k ms) d
      | boundKeyCommand k ms d==Just Paste && commandEnabled d Paste = paste d {prefix=Nothing}
      | k==V.KChar 'v', dialog d/=Nothing, effectiveBindings d==Nothing,
        any (`elem` ms) [V.MCtrl,V.MMeta], dialogCommandAllowed Paste d = paste d
      | otherwise = clipboardResult (copies ev d) d (handleEvent ev d)
    dispatchKey ev d = pure (handleEvent ev d)
    paste d = do bytes <- c_clipboard >>= BS.packCString; pure (handleEvent (V.EvPaste bytes) d)
    clipboardResult force before result@(after,requests) = do
      let deferred CopyConversation{}=True
          deferred _=False
      when (not (any deferred requests) && (force || clipboard before /= clipboard after))
        (utf8 (clipboard after) c_set_clipboard)
      pure result
    copyClick x y d = case contextMenu d of
      Just (r,chosen) | inside r x y, y>top r, y<top r+height r-1 ->
        case drop (contextOffset r chosen+y-top r-1) (contextItemsFor d) of (_,cmd):_ -> cmd `elem` [Copy,CopyAllMessages,CopyLocation]; _ -> False
      _ -> any (\(r,_,action) -> inside r x y && action `elem` [Left Copy,Left CopyAllMessages,Left CopyLocation]) (statusItemRects d)
    copies (V.EvKey key ms) d | Just cmd<-boundKeyCommand key ms d = cmd `elem` [Copy,Cut,CopyAllMessages,CopyLocation]
    copies (V.EvKey _ _) d | bindingInputAvailable d, Just _<-effectiveBindings d = False
    copies (V.EvKey key ms) d | dialog d/=Nothing && (dialogCommandAllowed Copy d || dialogCommandAllowed Cut d) && any (`elem` ms) [V.MCtrl,V.MMeta] && key `elem` map V.KChar "cx" = True
    copies (V.EvKey key ms) d =
      (V.MCtrl `elem` ms && (not (wordStar d) || problemsFocused d) && key `elem` [V.KChar 'c',V.KChar 'x']) ||
      (key == V.KIns && V.MCtrl `elem` ms) || (key == V.KDel && V.MShift `elem` ms) ||
      (prefix d == Just 'k' && key `elem` [V.KChar 'c',V.KChar 'v'])
    copies _ _ = False
    -- Text events still pass through key handling for menu mnemonics and WordStar prefixes.
    foldText text d = case T.uncons text of
      Nothing -> pure (d,[])
      Just (c,rest) -> do
        result@(d',fx)<-dispatchKey (V.EvKey (V.KChar c) []) d
        if null fx then foldText rest d' else pure result
#else
runWindow :: Backend -> Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> Desktop -> IO ()
runWindow _ _ _ _ _ = ioError (userError "Graphical support is not built. Install SDL3 and rebuild with: cabal build -fwindow")
#endif

-- | The native menu and raw input resolve the same focused prepared table.
-- Empty bindings remove the accelerator; every additional chord remains raw input.
nativeMenuShortcut :: Desktop -> Command -> (String,Int)
nativeMenuShortcut d cmd
  | Just _<-wordStarPrefixContext d = ("",0)
  | bindingInputAvailable d = nativeChordShortcut (commandBindingKeys d cmd)
  | otherwise = ("",0)

-- | Cocoa key equivalents carry explicit SDL modifier bits, including Command.
-- Function and navigation keys use Cocoa's documented Unicode key equivalents.
nativeChordShortcut :: [Text.Text] -> (String,Int)
nativeChordShortcut chords=case mapMaybe encode chords of value:_ -> value; [] -> ("",0)
  where
    encode chord=case readChord chord of
      Left _->Nothing
      Right (key,mods)->do
        name<-case key of
          Keys.KChar c->Just [toLower c]
          Keys.KFun n | n>=1 && n<=24->Just [chr (0xf704+n-1)]
          _->lookup key [(Keys.KEnter,"\r"),(Keys.KEsc,"\ESC"),(Keys.KBS,"\DEL"),(Keys.KDel,[chr 0xf728]),(Keys.KIns,[chr 0xf727]),
            (Keys.KUp,[chr 0xf700]),(Keys.KDown,[chr 0xf701]),(Keys.KLeft,[chr 0xf702]),(Keys.KRight,[chr 0xf703]),
            (Keys.KHome,[chr 0xf729]),(Keys.KEnd,[chr 0xf72b]),(Keys.KPageUp,[chr 0xf72c]),(Keys.KPageDown,[chr 0xf72d])]
        pure (name,foldr (.|.) 0 [mask | (modifier,mask)<-[(Keys.MShift,1),(Keys.MCtrl,2),(Keys.MAlt,4),(Keys.MMeta,8)],modifier `elem` mods])
