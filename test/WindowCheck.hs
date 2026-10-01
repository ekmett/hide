{-# LANGUAGE OverloadedStrings #-}
module WindowCheck (checks) where
import Control.Monad (unless)
import THC.Edit.Frontend
import THC.Edit.Model
import THC.Edit.Buffer (newBuffer)
import THC.Edit.Files (FileState(..))
import qualified Graphics.Vty as V
checks :: IO ()
checks = do
  let check name ok = unless ok (error name)
  check "tile scale defaults, environment, overrides and validation"
    (chooseScale Nothing []==Right 0 && chooseScale (Just "3") []==Right 3 &&
     chooseScale (Just "bad") ["1"]==Right 1 && chooseScale (Just "") []==Right 0 &&
     all (either (const True) (const False)) [chooseScale (Just "0") [],chooseScale Nothing ["9"],chooseScale Nothing ["2","3"]])
  check "fractional scales work through flags and environment"
    (chooseScale (Just "1.125") []==Right 1.125 && chooseScale Nothing ["2.5"]==Right 2.5 &&
     chooseScale Nothing ["1.3"]==Right 1.25 &&
     all (either (const True) (const False)) [chooseScale Nothing ["NaN"],chooseScale Nothing ["Infinity"],chooseScale Nothing ["0.9"]])
  check "Control and Alt zoom reset use the same modifiers as zoom in and out"
    (all (\mods -> map (\key -> zoomDirection (fromEnum key) mods) "0+=-"==map Just [0,1,1,-1]) [2,4,3,5] &&
     zoomDirection (fromEnum '0') 0==Nothing && zoomDirection (fromEnum 'a') 2==Nothing)
  check "terminal remains default" (chooseBackend Nothing [] == Right Terminal)
  check "environment selects metal" (chooseBackend (Just "metal") [] == Right Metal)
  check "explicit terminal beats environment" (chooseBackend (Just "vulkan") [Terminal] == Right Terminal)
  check "explicit backend overrides invalid environment" (chooseBackend (Just "typo") [Metal] == Right Metal)
  check "invalid default rejected" (case chooseBackend (Just "typo") [] of Left _ -> True; _ -> False)
  check "conflicting backend flags rejected" (case chooseBackend Nothing [Metal,Vulkan] of Left _ -> True; _ -> False)
  check "shift-tab" (decodeKey (-9) 1 == Just (V.EvKey V.KBackTab [V.MShift]))
  check "command maps to control" (decodeKey 115 8 == Just (V.EvKey (V.KChar 's') [V.MCtrl]))
  check "unknown key ignored" (decodeKey (-999) 0 == Nothing)
  check "character dimensions" (parseWindowSize "100x32" == Right (100,32))
  check "reject tiny dimensions" (case parseWindowSize "1x2" of Left _ -> True; _ -> False)
  check "reject malformed dimensions" (case parseWindowSize "80.5x25" of Left _ -> True; _ -> False)
  check "numbered screen modes accept decimal and hexadecimal"
    (map parseScreenMode ["3","259","0x03","0x103","$103"] == map Right [3,259,3,259,259])
  check "unsupported screen modes rejected" (case parseScreenMode "257" of Left _ -> True; _ -> False)
  check "50 lines fit the same physical height"
    (modeSize 3 == (80,25) && modeSize 259 == (80,50) && modeHeight 3 == 16 && modeHeight 259 == 8)
  let desktop = addDocument Nothing (newBuffer "unsaved buffer") (initialDesktop (80,25)) {videoMode=Just 3}
      preferences = fst (runCommand EditorOptions desktop)
      choose50 = preferences {dialog=fmap (\dg -> dg {fields=[Radio "Key bindings" ["Modern","WordStar"] 0,Radio "Screen size" ["Mode 3 (80x25)","Mode 259 (80x50)"] 1]}) (dialog preferences)}
      (updated,requests) = handleEvent (V.EvKey V.KEnter []) choose50
  check "preferences expose classic screen modes"
    (maybe False (any (\f -> case f of Radio "Screen size" _ 0 -> True; _ -> False) . fields) (dialog preferences))
  check "mode changes preserve buffers and independent key bindings"
    (requests == [SetScreenMode 259] && not (wordStar updated) && buffers updated == buffers desktop)
  check "cancelled preferences do not change modes"
    (snd (handleEvent (V.EvKey V.KEsc []) choose50) == [])
  let taller = resizeScreenMode (80,50) desktop
      split = fst (runCommand SplitHorizontal desktop)
      tallerSplit = resizeScreenMode (80,50) split
  check "mode change fills the taller desktop without changing buffers"
    (map bounds (windows taller) == [Rect 0 1 80 48] && buffers taller == buffers desktop)
  check "mode change preserves tiled split layout"
    (sum (map (height . bounds) (windows tallerSplit)) == 48 &&
     all (\w -> width (bounds w) == 80) (windows tallerSplit) &&
     buffers tallerSplit == buffers split)
  let terminalPreferences = fst (runCommand EditorOptions (initialDesktop (80,25)))
  check "terminal preferences omit window modes"
    (maybe False (not . any (\field -> case field of Radio "Screen size" _ _ -> True; _ -> False) . fields) (dialog terminalPreferences))
  check "CRT defaults off and appears only in window preferences"
    (not (crtFilter desktop) && maybe False (elem (CheckBox "CRT filter" False) . fields) (dialog preferences) &&
     maybe False (not . any (\f -> case f of CheckBox "CRT filter" _ -> True; _ -> False) . fields) (dialog terminalPreferences))
  let toggle dg=dg {fields=map (\f -> case f of CheckBox "CRT filter" _ -> CheckBox "CRT filter" True; _ -> f) (fields dg)}
      changed=preferences {dialog=fmap toggle (dialog preferences)}
  check "CRT applies on OK and cancels without changing buffers"
    (crtFilter (fst (handleEvent (V.EvKey V.KEnter []) changed)) &&
     not (crtFilter (fst (handleEvent (V.EvKey V.KEsc []) changed))) && buffers changed==buffers desktop)
  check "File menu contains Terminal and Change dir"
    (all (`elem` [cmd | (name,_,items)<-menus,name=="File",MenuItem _ _ cmd<-items]) [OpenTerminal,ChangeDir] &&
     OpenTerminal `notElem` [cmd | (name,_,items)<-menus,name=="Run",MenuItem _ _ cmd<-items])
  let two=fst (runCommand New desktop)
      controlTab=fst (handleEvent (V.EvKey (V.KChar '\t') [V.MCtrl]) two)
      controlBack=fst (handleEvent (V.EvKey V.KBackTab [V.MCtrl,V.MShift]) controlTab)
      tree=installTree "/tmp" [] two
      altTab=fst . handleEvent (V.EvKey (V.KChar '\t') [V.MAlt])
      menuFocused=tree {menu=Just (0,0)}
      treeFocusedAgain=altTab menuFocused
      fileFocused=altTab treeFocusedAgain
  check "Ctrl Tab cycles windows and Shift reverses without editing"
    (fmap windowId (activeWindow controlTab)/=fmap windowId (activeWindow two) &&
     fmap windowId (activeWindow controlBack)==fmap windowId (activeWindow two) && buffers controlTab==buffers two)
  check "Alt Tab moves menu to Files to editor and arrows stay in the selected region"
    (maybe False treeFocused (sideTree treeFocusedAgain) && menu treeFocusedAgain==Nothing &&
     maybe False (windowFocused fileFocused) (activeWindow fileFocused) && buffers fileFocused==buffers tree)
  check "Alt Tab cycles dialog fields"
    (fmap focus (dialog (altTab preferences))==fmap ((+1).focus) (dialog preferences))
  let docked=installTree "/tmp" [] desktop
      edge=maybe (error "no Files") ((subtract 1).treeWidth) (sideTree docked)
      floating=modifyActive (\w -> w {bounds=Rect edge 3 30 10}) docked
      rect=fmap bounds . activeWindow
      leftward=resizeTree 19 floating
      rightward=resizeTree 35 floating
      touching=resizeTree 55 rightward
      stuck=resizeTree 35 touching
      movedByMouse=fst (handleEvent (V.EvMouseDown 35 23 V.BLeft [])
        (fst (handleEvent (V.EvMouseDown edge 23 V.BLeft []) floating)))
  check "Files and the adjacent editor share one border column"
    (edge==23 && fmap (left.bounds) (activeWindow docked)==Just edge)
  check "dock resizing carries a floating window without scaling it"
    (rect leftward==Just (Rect 19 3 30 10) && rect rightward==Just (Rect 35 3 30 10) && rect movedByMouse==rect rightward)
  check "editor right edge sticks to the screen once reached"
    (rect touching==Just (Rect 55 3 25 10) && rect stuck==Just (Rect 35 3 45 10) &&
     rect (resizeTree 19 docked)==Just (Rect 19 1 61 23) && buffers stuck==buffers floating)
  let resize size=fst . handleEvent (uncurry V.EvResize size)
      grow=resize (100,40)
      withRect r=modifyActive (\w -> w {bounds=r}) desktop
      bottomOnly=withRect (Rect 5 14 30 10)
      rightOnly=withRect (Rect 40 3 40 10)
      unattached=withRect (Rect 5 3 30 10)
      dockedMessages=setProblemsVisible True docked
      grownMessages=grow dockedMessages
      zoomed=fst (runCommand Zoom rightOnly)
      restored=fst (runCommand Zoom (grow zoomed))
  check "screen growth stretches windows touching both outer edges"
    (rect (grow desktop)==Just (Rect 0 1 100 38) && buffers (grow desktop)==buffers desktop)
  check "screen growth stretches only the attached axis"
    (rect (grow bottomOnly)==Just (Rect 5 14 30 25) && rect (grow rightOnly)==Just (Rect 40 3 60 10))
  check "screen growth leaves floating windows in place" (rect (grow unattached)==rect unattached)
  check "edge attachment survives shrinking and regrowing"
    (rect (grow (resize (80,25) (grow desktop)))==rect (grow desktop))
  check "screen growth respects Files and the bottom Messages dock"
    (rect grownMessages==Just (Rect 23 1 77 30) && problemsRect grownMessages==Rect 0 31 100 8 &&
     fmap treeWidth (sideTree grownMessages)==Just 24)
  check "zoom restores bounds with their original edge attachments"
    (rect (grow zoomed)==Just (Rect 0 1 100 38) && rect restored==rect (grow rightOnly))
  let mouse x y=fst . handleEvent (V.EvMouseDown x y V.BLeft [])
      grabbed=mouse 10 1 desktop
      dragged=mouse 20 6 grabbed
      minimumWindow=mouse 150 100 dragged
      released=fst (handleEvent (V.EvMouseUp 150 100 (Just V.BLeft)) minimumWindow)
      cancelled=fst (handleEvent (V.EvKey V.KEsc []) minimumWindow)
      floatingMove=mouse 20 6 (mouse 15 3 unattached)
      dockGrab=mouse 33 1 (dockedMessages {sideTree=fmap (\t->t {treeFocused=False}) (sideTree dockedMessages)})
      dockMove=mouse 43 6 dockGrab
      keyboardMove=fst (handleEvent (V.EvKey V.KRight []) grabbed)
  check "title dragging shrinks a full-size window against both edges"
    (rect dragged==Just (Rect 10 6 70 18) && buffers dragged==buffers desktop)
  check "title dragging stops at the window minimum size"
    (rect minimumWindow==Just (Rect 64 19 16 5) && rect released==rect minimumWindow && drag released==Nothing)
  check "Escape restores the window before movement and shrinkage" (rect cancelled==rect desktop)
  check "title dragging preserves size when there is room" (rect floatingMove==Just (Rect 10 6 30 10))
  check "title dragging shrinks within dock boundaries" (rect dockMove==Just (Rect 33 6 47 10))
  check "keyboard movement uses the same edge shrink behavior" (rect keyboardMove==Just (Rect 1 1 79 23))
  putStrLn "window input checks passed"

  let named=addDocument (Just (FileState "/project/src/Main.hs" Nothing)) (newBuffer "") desktop
      other=addDocument (Just (FileState "/project/test/Spec.hs" Nothing)) (newBuffer "") named
  check "outer title includes the relative file path"
    (applicationTitle "/project" named=="th src/Main.hs")
  check "outer title follows the active file and project directory"
    (applicationTitle "/project" other=="th test/Spec.hs" &&
     applicationTitle "/elsewhere" (installTree "/project" [] named)=="th src/Main.hs" &&
     applicationTitle "/project" named {defaultDirectory=Just "/project/test"}=="th ../src/Main.hs")
  check "outer title handles empty desktops and unnamed files"
    (applicationTitle "/project" (initialDesktop (80,25))=="th" &&
     applicationTitle "/project" desktop=="th NONAME1.HS")
