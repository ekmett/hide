{-# LANGUAGE CPP, ForeignFunctionInterface, OverloadedStrings #-}
module THC.Edit.Window (runWindow) where
import THC.Edit.Frontend
import THC.Edit.Model
#ifdef WITH_WINDOW
import Control.Exception (bracket_)
import Control.Monad (forM_, when, unless, foldM)
import Data.Foldable (toList)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Foreign
import Foreign.C
import qualified Graphics.Vty as V
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp(..))
import System.Environment (lookupEnv)
import System.Info (os)
import System.IO (hPutStrLn, stderr)
import THC.Edit.Font
import THC.Edit.Render

foreign import ccall unsafe "thc_open" c_open :: CString -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "thc_mode" c_mode :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "thc_close" c_close :: IO ()
foreign import ccall unsafe "thc_error" c_error :: IO CString
foreign import ccall unsafe "thc_backend" c_backend :: IO CString
foreign import ccall unsafe "thc_size" c_size :: Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall unsafe "thc_begin" c_begin :: IO CInt
foreign import ccall unsafe "thc_glyph" c_glyph :: CInt -> CInt -> CInt -> CInt -> Ptr Word16 -> Word32 -> Word32 -> IO ()
foreign import ccall unsafe "thc_cursor" c_cursor :: CInt -> CInt -> IO ()
foreign import ccall unsafe "thc_present" c_present :: IO CInt
foreign import ccall safe "thc_wait" c_wait :: Ptr Int32 -> IO CInt
foreign import ccall unsafe "thc_text" c_text :: IO CString
foreign import ccall unsafe "thc_clipboard" c_clipboard :: IO CString
foreign import ccall unsafe "thc_set_clipboard" c_set_clipboard :: CString -> IO ()
#ifdef darwin_HOST_OS
foreign import ccall unsafe "thc_menu_clear" c_menu_clear :: IO ()
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
  c_menu_clear
  let numbered = zip menus (scanl (+) 0 [length items | (_,_,items) <- menus])
  forM_ numbered $ \((title,_,items),start) -> do
    utf8 title c_menu_add
    forM_ (zip [start..] items) $ \(i,MenuItem name _ cmd) ->
      utf8 name $ \namePtr -> withCString (shortcut cmd) $ \keyPtr ->
        c_menu_item namePtr keyPtr (fromIntegral i) (if enabled cmd then 1 else 0)
  where
    shortcut cmd = case cmd of
      New -> "n"; Open -> "o"; Save -> "s"; SaveAs -> "S"; Close -> "w"; Quit -> "q"
      Undo -> "z"; Redo -> "Z"; Copy -> "c"; Cut -> "x"; Paste -> "v"; SelectAll -> "a"
      Find -> "f"; FindNext -> "g"; _ -> ""
    enabled Disabled{} = False
    enabled _ = True
#else
nativeMenus = pure ()
#endif

updateMenus :: Desktop -> IO ()
#ifdef darwin_HOST_OS
updateMenus d = forM_ (zip [0..] nativeCommands) $ \(i,cmd) ->
  c_menu_enabled i (if canInvoke cmd then 1 else 0)
  where
    canInvoke Disabled{} = False
    canInvoke Paste = not (maybe False treeFocused (sideTree d)) || dialog d /= Nothing
    canInvoke cmd = dialog d == Nothing && (activeWindow d /= Nothing || cmd `elem` [New,Open,Quit,Help,About,Gallery,EditorOptions,ToggleTree,GitDiff,GitCommit])
#else
updateMenus _ = pure ()
#endif

-- The same Vty picture used by the terminal is flattened into bitmap cells.
draw :: Font -> Desktop -> IO ()
draw font d = do
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
        end <- chars y x a (TL.unpack t)
        go y end ops
      Skip n -> go y (x+n) ops
      RowEnd _ -> pure ()
    chars _ x _ [] = pure x
    chars y x a (ch:rest) = do
      let width = max 0 (V.safeWcwidth ch)
          gx = if width == 0 then max 0 (x-1) else x
          Glyph gw bitmap = glyph font ch
      withArray bitmap $ \bits -> c_glyph (fromIntegral gx) (fromIntegral y) (fromIntegral width) (fromIntegral gw) bits (rgb (V.attrForeColor a)) (rgb (V.attrBackColor a))
      chars y (x+width) a rest
    rgb (V.SetTo (V.RGBColor r g b)) = fromIntegral r `shiftL` 16 .|. fromIntegral g `shiftL` 8 .|. fromIntegral b
    rgb (V.SetTo (V.ISOColor n)) = [0,0xaa0000,0x00aa00,0xaa5500,0x0000aa,0xaa00aa,0x00aaaa,0xaaaaaa,0x555555,0xff5555,0x55ff55,0xffff55,0x5555ff,0xff55ff,0x55ffff,0xffffff] !! (fromIntegral n `mod` 16)
    rgb _ = 0

runWindow :: Backend -> Int -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO ()
runWindow backend scale effects initial = do
  font <- loadFont
  let driver = case backend of Metal -> "metal"; Vulkan -> "vulkan"; _ -> if os == "darwin" then "metal" else "vulkan"
  -- SDL must stay on the main OS thread; GHC's main action is a bound thread.
  bracket_ (pure ()) c_close $ do
    withCString driver $ \name -> check ("Cannot start " ++ driver ++ " window") (c_open name (fromIntegral scale) (fromIntegral (fst (screenSize initial))) (fromIntegral (snd (screenSize initial))) (fromIntegral (modeHeight (maybe 3 id (videoMode initial)))))
    c_backend >>= peekCString >>= hPutStrLn stderr . ("Turbo Haskell renderer: " ++)
    nativeMenus
    sized <- alloca $ \wp -> alloca $ \hp -> do
      c_size wp hp
      w <- fromIntegral <$> peek wp
      h <- fromIntegral <$> peek hp
      pure ((fst (handleEvent (V.EvResize w h) initial {nativeMac=os == "darwin"})) {menu=menu initial})
    captureOnly <- (== Just "1") <$> lookupEnv "THC_EDIT_CAPTURE_EXIT"
    if captureOnly then draw font sized else loop font sized
  where
    loop font d = do
      updateMenus d
      draw font d
      event <- allocaArray 6 $ \p -> check "Read window event" (c_wait p) >> map fromIntegral <$> peekArray 6 p
      (next,requests) <- dispatch event d
      (exit,updated) <- foldM windowEffect (False,next) requests
      unless exit (loop font updated)
    windowEffect state@(True,_) _ = pure state
    windowEffect (_,d) (SetScreenMode mode) = do
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
    windowEffect (_,d) request = effects d [request]
    dispatch (1:key:mods:_) d
      | key == fromEnum 'v' && mods .&. 10 /= 0 && (not (wordStar d) || mods .&. 8 /= 0) = paste d
      | otherwise = case decodeKey key mods of
          Nothing -> pure (d,[])
          Just ev -> clipboardResult (copies ev d) d (handleEvent ev d)
    dispatch (2:_) d = do
      bytes <- c_text >>= BS.packCString
      case TE.decodeUtf8' bytes of
        Left _ -> pure (d,[])
        Right text -> clipboardResult (any (\c -> copies (V.EvKey (V.KChar c) []) d) (T.unpack text)) d (foldText (T.unpack text) d)
    dispatch (3:x:y:_:mods:_) d = pure (handleEvent (V.EvMouseDown x y V.BLeft (keyMods mods)) d)
    dispatch (4:x:y:_) d = pure (handleEvent (V.EvMouseUp x y (Just V.BLeft)) d)
    dispatch (5:w:h:_) d = pure (handleEvent (V.EvResize w h) d)
    dispatch (6:_) d | dialog d /= Nothing = pure (d,[])
                    | otherwise = pure (runCommand Quit d)
    dispatch (7:_) d = pure (d {drag=Nothing,prefix=Nothing},[])
    dispatch (9:x:y:direction:mods:_) d = pure (handleEvent (V.EvMouseDown x y (if direction>0 then V.BScrollUp else V.BScrollDown) (keyMods mods)) d)
    dispatch (11:i:_) d | i >= 0, cmd:_ <- drop i nativeCommands =
      if cmd == Paste then paste d
      else if dialog d == Nothing then clipboardResult (cmd `elem` [Copy,Cut]) d (runCommand cmd d)
      else pure (d,[])
    dispatch _ d = pure (d,[])
    keyMods m = case decodeKey 0 m of Just (V.EvKey _ ms) -> ms; _ -> []
    paste d = do bytes <- c_clipboard >>= BS.packCString; pure (handleEvent (V.EvPaste bytes) d)
    clipboardResult force before result@(after,_) = do
      when (force || clipboard before /= clipboard after) (utf8 (clipboard after) c_set_clipboard)
      pure result
    copies (V.EvKey key ms) d =
      (V.MCtrl `elem` ms && not (wordStar d) && key `elem` [V.KChar 'c',V.KChar 'x']) ||
      (key == V.KIns && V.MCtrl `elem` ms) || (key == V.KDel && V.MShift `elem` ms) ||
      (prefix d == Just 'k' && key `elem` [V.KChar 'c',V.KChar 'v'])
    copies _ _ = False
    -- Text events still pass through key handling for menu mnemonics and WordStar prefixes.
    foldText [] d = (d,[])
    foldText (c:cs) d = let (d',fx)=handleEvent (V.EvKey (V.KChar c) []) d
                       in if null fx then foldText cs d' else (d',fx)
#else
runWindow :: Backend -> Int -> (Desktop -> [Effect] -> IO (Bool,Desktop)) -> Desktop -> IO ()
runWindow _ _ _ _ = ioError (userError "Graphical support is not built. Install SDL3 and rebuild with: cabal build -fwindow")
#endif
