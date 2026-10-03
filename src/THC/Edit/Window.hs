{-# LANGUAGE CPP, ForeignFunctionInterface, OverloadedStrings #-}
module THC.Edit.Window (runWindow, nativeMenuShortcut
#ifdef WITH_WINDOW
  , check, utf8, nativeMenus, nativeCommands
  , c_system_dark, c_open, c_mode, c_scale, c_title, c_close, c_size
  , c_begin, c_glyph, c_unicode, c_pixelate_unicode, c_cursor, c_cursor_blink
  , c_crt_filter, c_present, c_wait, c_wake, c_text, c_clipboard, c_set_clipboard
#ifdef darwin_HOST_OS
  , c_menu_enabled, c_menu_prepare
#endif
#endif
  ) where
import THC.Edit.Frontend
import THC.Edit.Model
#ifdef WITH_WINDOW
import Control.Exception (bracket_)
import Control.Monad (forM_, when, unless, foldM)
import Data.Foldable (toList)
import Data.List (elemIndex)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Foreign
import Foreign.C
import qualified Graphics.Vty as V
import THC.Edit.Unicode (displayOpsForPic)
import Graphics.Vty.Span (SpanOp(..))
import System.Environment (lookupEnv)
import System.Directory (getCurrentDirectory)
import System.Info (os)
import System.IO (hPutStrLn, stderr)
import THC.Edit.Unicode (graphemes, clusterWidth)
import THC.Edit.Font
import THC.Edit.Render

foreign import ccall unsafe "thc_system_dark" c_system_dark :: IO CInt
foreign import ccall unsafe "thc_open" c_open :: CString -> CDouble -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "thc_mode" c_mode :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "thc_scale" c_scale :: CInt -> IO CInt
foreign import ccall unsafe "thc_title" c_title :: CString -> IO ()
foreign import ccall unsafe "thc_close" c_close :: IO ()
foreign import ccall unsafe "thc_error" c_error :: IO CString
foreign import ccall unsafe "thc_backend" c_backend :: IO CString
foreign import ccall unsafe "thc_size" c_size :: Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall unsafe "thc_begin" c_begin :: IO CInt
foreign import ccall unsafe "thc_glyph" c_glyph :: CInt -> CInt -> CInt -> CInt -> Ptr Word16 -> Word32 -> Word32 -> IO ()
foreign import ccall unsafe "thc_unicode" c_unicode :: CInt -> CInt -> CInt -> CString -> Word32 -> Word32 -> IO CInt
foreign import ccall unsafe "thc_pixelate_unicode" c_pixelate_unicode :: CInt -> IO ()
foreign import ccall unsafe "thc_cursor" c_cursor :: CInt -> CInt -> IO ()
foreign import ccall unsafe "thc_cursor_blink" c_cursor_blink :: CInt -> IO ()
foreign import ccall unsafe "thc_crt_filter" c_crt_filter :: CInt -> IO ()
-- Presentation can wait for vblank. Let receiver/sender threads run meanwhile.
foreign import ccall safe "thc_present" c_present :: IO CInt
foreign import ccall unsafe "thc_wake" c_wake :: IO ()
foreign import ccall safe "thc_wait" c_wait :: Ptr Int32 -> IO CInt
foreign import ccall unsafe "thc_text" c_text :: IO CString
foreign import ccall unsafe "thc_clipboard" c_clipboard :: IO CString
foreign import ccall unsafe "thc_set_clipboard" c_set_clipboard :: CString -> IO ()
#ifdef darwin_HOST_OS
foreign import ccall unsafe "thc_menu_prepare" c_menu_prepare :: IO ()
foreign import ccall unsafe "thc_menu_clear" c_menu_clear :: CInt -> CInt -> CInt -> IO ()
foreign import ccall unsafe "thc_menu_add" c_menu_add :: CString -> IO ()
foreign import ccall unsafe "thc_menu_item" c_menu_item :: CString -> CString -> CInt -> CInt -> IO ()
foreign import ccall unsafe "thc_menu_enabled" c_menu_enabled :: CInt -> CInt -> IO ()
#endif

check :: String -> IO CInt -> IO ()
check context action = do
  ok <- action
  when (ok == 0) $ do
    err <- c_error >>= peekCString
    ioError (userError (context ++ ": " ++ err))

utf8 :: T.Text -> (CString -> IO a) -> IO a
utf8 text = BS.useAsCString (TE.encodeUtf8 text)

nativeCommands :: [Command]
nativeCommands = [cmd | (_,_,items) <- menus, MenuItem _ _ cmd <- items]

nativeMenus :: IO ()
#ifdef darwin_HOST_OS
nativeMenus = do
  c_menu_clear (number About) (number EditorOptions) (number Quit)
  let numbered = zip menus (scanl (+) 0 [length items | (_,_,items) <- menus])
  forM_ numbered $ \((title,_,items),start) -> do
    utf8 title c_menu_add
    forM_ (zip [start..] items) $ \(i,MenuItem name _ cmd) ->
      unless (cmd `elem` [About,EditorOptions,Quit]) $ utf8 name $ \namePtr -> withCString (nativeMenuShortcut cmd) $ \keyPtr ->
        c_menu_item namePtr keyPtr (fromIntegral i) (if enabled cmd then 1 else 0)
  where
    number cmd=maybe (error "Missing native application command") fromIntegral (elemIndex cmd nativeCommands)
    enabled Disabled{} = False
    enabled _ = True
#else
nativeMenus = pure ()
#endif

updateMenus :: Desktop -> IO ()
#ifdef darwin_HOST_OS
updateMenus d = forM_ (zip [0..] nativeCommands) $ \(i,cmd) ->
  c_menu_enabled i (if commandEnabled d cmd && canInvoke cmd then 1 else 0)
  where
    canInvoke cmd | dialogCommandAllowed cmd d = True
    canInvoke Paste = not (maybe False treeFocused (sideTree d)) || dialog d /= Nothing
    canInvoke cmd = dialog d == Nothing && (activeWindow d /= Nothing || (problemsVisible d && problemsFocused d && cmd==Copy) || cmd `elem` [New,Open,ChangeDir,Quit,Help,About,Gallery,EditorOptions,RunTarget,RunOptions,OpenTerminal,StopTerminal,AgentOptions,Conversation,AgentCancel,AgentResume,AgentNew,AgentCopyRaw,ToggleTree,GitDiff,GitCommit,Problems,NextMessage,PreviousMessage,DebugCommand "downloads"])
#else
updateMenus _ = pure ()
#endif

-- The same Vty picture used by the terminal is flattened into bitmap cells.
draw :: Font -> Desktop -> IO ()
draw font d = do
  c_cursor_blink (if blinkCursor d then 1 else 0)
  c_crt_filter (if crtFilter d then 1 else 0)
  c_pixelate_unicode (if pixelateUnicode d then 1 else 0)
  check "Allocate window frame" c_begin
  let picture = renderDesktop d
  forM_ (zip [0::Int ..] (toList (displayOpsForPic picture (screenSize d)))) $ \(y,spans) -> go y 0 (toList spans)
  case V.picCursor picture of
    V.Cursor x y -> c_cursor (fromIntegral x) (fromIntegral y)
    _ -> pure ()
  check "Present window frame" c_present
  where
    go _ _ [] = pure ()
    go y x (op:ops) = case op of
      TextSpan {textSpanAttr=a,textSpanText=t} -> do
        end <- chars y x a (graphemes (TL.toStrict t))
        go y end ops
      Skip n -> go y (x+n) ops
      RowEnd _ -> pure ()
    chars _ x _ [] = pure x
    chars y x a (cluster:rest) = do
      let width=clusterWidth cluster
          fg=rgb (V.attrForeColor a); bg=rgb (V.attrBackColor a)
      case T.unpack cluster of
        [ch] | bitmapGlyph font ch -> do
          let Glyph gw bitmap=glyph font ch
          withArray bitmap $ \bits -> c_glyph (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral gw) bits fg bg
        _ -> utf8 cluster $ \text -> check "Draw Unicode" (c_unicode (fromIntegral x) (fromIntegral y) (fromIntegral width) text fg bg)
      chars y (x+width) a rest
    rgb (V.SetTo (V.RGBColor r g b)) = fromIntegral r `shiftL` 16 .|. fromIntegral g `shiftL` 8 .|. fromIntegral b
    rgb (V.SetTo (V.ISOColor n)) = [0,0xaa0000,0x00aa00,0xaa5500,0x0000aa,0xaa00aa,0x00aaaa,0xaaaaaa,0x555555,0xff5555,0x55ff55,0xffff55,0x5555ff,0xff55ff,0x55ffff,0xffffff] !! (fromIntegral n `mod` 16)
    rgb _ = 0

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
    c_backend >>= peekCString >>= hPutStrLn stderr . ("Turbo Haskell renderer: " ++)
    nativeMenus
    sized <- alloca $ \wp -> alloca $ \hp -> do
      c_size wp hp
      w <- fromIntegral <$> peek wp
      h <- fromIntegral <$> peek hp
      pure ((fst (handleEvent (V.EvResize w h) initial {nativeMac=os == "darwin"})) {menu=menu initial,contextMenu=if (w,h)==screenSize initial then contextMenu initial else Nothing})
    captureOnly <- (== Just "1") <$> lookupEnv "THC_EDIT_CAPTURE_EXIT"
    if captureOnly then systemTheme sized >>= draw font else systemTheme sized >>= tick >>= loop font Nothing
  where
    systemTheme d = do value<-c_system_dark; pure d {systemDark=value/=0}
    loop font previous d = do
      key<-renderKey d
      when (fmap snd previous /= Just key) $ do
        when (fmap fst previous /= Just (applicationTitle "" d)) $ do
          cwd <- getCurrentDirectory
          utf8 (applicationTitle cwd d) c_title
        updateMenus d
        draw font d
      event <- allocaArray 6 $ \p -> check "Read window event" (c_wait p) >> map fromIntegral <$> peekArray 6 p
      (next,requests) <- dispatch event d
      (exit,updated) <- foldM windowEffect (False,next) requests
      when (clipboard updated /= clipboard d) (utf8 (clipboard updated) c_set_clipboard)
      let displayed = case event of kind:_ | kind `elem` [3,4,5,7,8,9,12] -> Nothing; _ -> Just (applicationTitle "" d,key)
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
      | Just direction <- zoomDirection key mods = changeScale (fromIntegral direction) d
      | key == fromEnum 'v' && mods .&. 10 /= 0 && (not (wordStar d) || mods .&. 8 /= 0) = paste d
      | otherwise = case decodeKey key mods of
          Nothing -> pure (d,[])
          Just ev -> clipboardResult (copies ev d) d (handleEvent ev d)
    dispatch (14:_) d = do
      path <- c_text >>= BS.packCString
      pure (d,[ReadPath (T.unpack (TE.decodeUtf8 path))])
    dispatch (2:_) d = do
      bytes <- c_text >>= BS.packCString
      case TE.decodeUtf8' bytes of
        Left _ -> pure (d,[])
        Right text -> clipboardResult (any (\c -> copies (V.EvKey (V.KChar c) []) d) (T.unpack text)) d (foldText (T.unpack text) d)
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
    dispatch (11:i:_) d | i >= 0, cmd:_ <- drop i nativeCommands =
      if cmd == Paste then paste d
      else if dialog d == Nothing || dialogCommandAllowed cmd d then clipboardResult (cmd `elem` [Copy,Cut]) d (runCommand cmd d)
      else pure (d,[])
    dispatch (12:x:y:_) d = pure (hoverAt x y d)
    dispatch (13:mods:_) d = pure (d {heldModifiers=keyMods mods},[])
    dispatch _ d = pure (d,[])
    changeScale direction d = do
      ok <- c_scale direction
      if ok /= 0 then pure (d,[]) else do
        err <- c_error >>= peekCString
        pure (message "Cannot resize character tiles" [T.pack err] d,[])
    keyMods m = case decodeKey 0 m of Just (V.EvKey _ ms) -> ms; _ -> []
    paste d = do bytes <- c_clipboard >>= BS.packCString; pure (handleEvent (V.EvPaste bytes) d)
    clipboardResult force before result@(after,_) = do
      when (force || clipboard before /= clipboard after) (utf8 (clipboard after) c_set_clipboard)
      pure result
    copyClick x y d = case contextMenu d of
      Just (r,_) | inside r x y, y>top r, y<top r+height r-1 ->
        case drop (y-top r-1) (contextItems (contextKind d)) of (_,cmd):_ -> cmd `elem` [Copy,CopyAllMessages,CopyLocation]; _ -> False
      _ -> any (\(r,_,action) -> inside r x y && action `elem` [Left Copy,Left CopyAllMessages,Left CopyLocation]) (statusItemRects d)
    copies (V.EvKey key ms) d =
      (V.MCtrl `elem` ms && (not (wordStar d) || problemsFocused d) && key `elem` [V.KChar 'c',V.KChar 'x']) ||
      (key == V.KIns && V.MCtrl `elem` ms) || (key == V.KDel && V.MShift `elem` ms) ||
      (prefix d == Just 'k' && key `elem` [V.KChar 'c',V.KChar 'v'])
    copies _ _ = False
    -- Text events still pass through key handling for menu mnemonics and WordStar prefixes.
    foldText [] d = (d,[])
    foldText (c:cs) d = let (d',fx)=handleEvent (V.EvKey (V.KChar c) []) d
                       in if null fx then foldText cs d' else (d',fx)
#else
runWindow :: Backend -> Double -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> (Desktop -> IO Desktop) -> Desktop -> IO ()
runWindow _ _ _ _ _ = ioError (userError "Graphical support is not built. Install SDL3 and rebuild with: cabal build -fwindow")
#endif

-- Uppercase requests Shift; ~ requests Option in the Cocoa menu bridge.
nativeMenuShortcut :: Command -> String
nativeMenuShortcut cmd = case cmd of
  New -> "n"; Open -> "o"; Save -> "s"; SaveAs -> "S"; Close -> "w"; Quit -> "q"
  Undo -> "z"; Redo -> "Z"; Copy -> "c"; Cut -> "x"; Paste -> "v"; SelectAll -> "a"
  Find -> "f"; Replace -> "~f"; FindNext -> "g"; FindPrevious -> "G"
  EditorOptions -> ","
  Conversation -> "C"; AgentNew -> "N"; _ -> ""
