{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Hide.Accessibility
-- Copyright   : (c) Edward Kmett 2026
-- License     : BSD-3-Clause
-- Maintainer  : Edward Kmett
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Read-only sidebar and modal semantics from current host presentation. A complete
-- bounded projection accompanies a frame; it adds no action authority and never
-- reads unrelated document payloads. Resource annotations enter central privacy policy but
-- are never serialized.
module Hide.Accessibility
  ( SemanticAudience(..)
  , sidebarSemantics
  , dialogSemantics
  ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.List as List (foldl',sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Hide.GuestAccess (protectedPath,privateDialogField,readableAt,streamerReadableAt)
import Hide.Model (Desktop(..), Dialog(..), Field(..), Purpose(..), Rect(..), problemsHeight, treeContentRows,
  dialogRect,fieldRects,buttonRects,textAreaRect,openComboBox,comboBoxRect,inside)
import Hide.Buffer (caret,displayColumn,bufferContent,contentSourceLinesFrom,sourceLineWindow)
import Hide.Browser (Entry(..))
import Hide.Unicode (displayItems,itemDisplayText,itemSourceText,itemWidth,sourceItemAdvance)
import Data.Char (isControl)
import Hide.Plugin.Tree
import Hide.Sidebar

-- | Owner display metadata follows streamer mode. Agent screen metadata always
-- applies the same centralized protected-path policy, independently of that mode.
data SemanticAudience = OwnerSemantics | GuestSemantics deriving (Eq,Show)

-- | /O(v * h * log n)/ with at most 256 viewport rows, 65 ancestors per row and
-- 512 emitted nodes, including the host root. IDs survive viewport/selection
-- changes and expire with their actual provider registration/pane lifetime. Private ancestry
-- is omitted as a whole; covered sidebars publish an empty projection.
--
-- Revision tracks provider metadata, while layout also tracks selection, focus
-- and geometry. Consumers replace the complete field whenever it arrives: equal
-- revisions do not imply equal layout or privacy. Offscreen ancestors carry no
-- bounds; there is no offscreen-read API or callable semantic action.
sidebarSemantics :: SemanticAudience -> Desktop -> Value
sidebarSemantics audience d=case sideTree d of
  Nothing->snapshot 0 [cols,rows,0,0,0,0,bottom] 0 0 0 []
  Just tree
    | covered || listWidth<=0 || visible<=0->snapshot (revision tree) (layout tree) 0 0 0 []
    | otherwise->snapshot (revision tree) (layout tree) start visible (M.size (treeRows tree))
        (host tree:map (node tree) (List.sortOn (stateAddress . snd) (M.toList accepted)))
    where
      start=max 0 (treeScroll tree)
      visible=min 256 (min (max 0 (rows-2-bottom)) (treeContentRows d))
      listWidth=max 0 (min (cols-1) (treeWidth tree-if treeFocused tree then 3 else 2))
      listing=visibleRows start visible tree
      positions=M.fromList [(key,(index,2+index-start)) | (index,row)<-listing,NodeRow key<-[rowKey row]]
      -- Check only prepared ancestry for viewport rows. Whole-tree equality or
      -- scans would read unrelated payloads and lose the viewport bound.
      accepted=List.foldl' (include tree) M.empty (map (keyOf . rowHit . snd) listing)
      include current retained key=case reverse (hitTrace key current) of
        []->retained
        trace@(top:_)
          | not (maybe False ((==Nothing) . stateParent) (nodeAt top current))->retained
          | otherwise->case traverse (\hit->(,) (keyOf hit) <$> nodeAt hit current) trace of
              Just chain | all (\(_,state)->allowed (stateInfo state) && maybe True (allowed . rowInfo) (M.lookup (stateAddress state) (treeRows current))) chain,
                let combined=M.union retained (M.fromList chain),M.size combined<=511->combined
              _->retained
      allowed info=not maskPrivate || maybe True (not . protectedPath d) (infoResource info)
      maskPrivate=audience==GuestSemantics || streamerMode d
      host current=object
        ["id" .= (["sidebar"]::[Text]),"parent" .= (Nothing::Maybe [Text]),"role" .= ("tree"::Text),
         "name" .= ("Sidebar"::Text),"bounds" .= ([1,2,listWidth,visible]::[Int]),
         "selected" .= False,"focused" .= treeFocused current,"expanded" .= (Nothing::Maybe Bool),
         "loading" .= False,"childrenKnown" .= S.length (treeRoots current),"moreChildren" .= False,
         "index" .= (Nothing::Maybe Int),"generation" .= (0::Int),
         "level" .= (0::Int),"posInSet" .= (0::Int),"setSize" .= S.length (treeRoots current)]
      node current (key,state)=object
        ["id" .= identity current key state,"parent" .= Just (maybe ["sidebar"] parentIdentity (stateParent state)),
         "role" .= ("treeitem"::Text),"name" .= infoLabel info,
         "bounds" .= fmap (\(_,y)->[1,y,listWidth,1]) position,
         "selected" .= selected,"focused" .= (selected && treeFocused current),
         "expanded" .= (if infoBranch info then Just (maybe (stateExpanded state) rowExpanded cached) else Nothing),
         "loading" .= (case stateLoad state of Loading{}->True; _->False),
         "childrenKnown" .= S.length (stateChildren state),"moreChildren" .= incomplete state,
         "index" .= M.lookupIndex (stateAddress state) (treeRows current),
         "generation" .= safeInteger (stateGeneration state),
         "level" .= length address,"posInSet" .= (case reverse address of i:_->i+1; _->1),
         "setSize" .= siblingCount current state]
        where
          parentIdentity parent=case M.lookup parent (treeNodes current) of
            Just value->identity current parent value
            Nothing->["sidebar"]
          position=M.lookup key positions
          selected=maybe False ((==treeSelected current) . fst) position
          cached=M.lookup (stateAddress state) (treeRows current)
          info=maybe (stateInfo state) rowInfo cached
          address=take 65 (stateAddress state)
  where
    (cols,rows)=screenSize d
    bottom=problemsHeight d
    covered=isJust (dialog d) || isJust (menu d) || isJust (contextMenu d)
    revision=safeInteger . treeRevision
    layout tree=[cols,rows,treeWidth tree,treeScroll tree,treeSelected tree,if treeFocused tree then 1 else 0,bottom]
    snapshot :: Integer -> [Int] -> Int -> Int -> Int -> [Value] -> Value
    snapshot rev geometry start count total nodes=object
      ["revision" .= rev,"layout" .= geometry,"visibleStart" .= start,"visibleCount" .= count,
       "logicalRows" .= total,"readOnly" .= True,"nodes" .= nodes]
    identity tree (NodeKey ref _) state=["tree",treeIdentity ref,T.pack (show (treeEpoch tree))<>"."<>T.pack (show (stateWireId state))]
    incomplete state=case stateLoad state of Loaded Nothing->False; _->infoBranch (stateInfo state)
    siblingCount tree _ | treeProjectionRevision tree/=treeRevision tree= -1
    siblingCount tree state=case stateParent state of
      Nothing->S.length (treeRoots tree)
      Just parent->case M.lookup parent (treeNodes tree) of
        Just node | Loaded Nothing<-stateLoad node->S.length (stateChildren node)
        _-> -1
    safeInteger :: Integer -> Integer
    safeInteger value=max 0 (min 9007199254740991 value)

-- | A complete, read-only projection of the current modal. Structural IDs use
-- field/option indices, never names or values, and remain stable while typing or
-- resizing. They are snapshot coordinates, not callable lifetime capabilities.
-- Dismissal emits @present = False@; a privacy-hidden modal remains present with
-- no nodes, so clients still suppress the covered desktop's semantics.
--
-- Bounds and text follow visible host geometry. Values are filtered before
-- serialization; guest permissions/private forms can hide the whole modal.
-- Text areas borrow measured visible rows without reading the rest of a buffer,
-- its saved text or Undo. At most 256 nodes, 256 name/2048 value scalars per node
-- and 32768 combined text scalars are published (at most 128 KiB of UTF-8 text).
-- Consumers replace the complete snapshot and must not infer editing authority.
dialogSemantics :: SemanticAudience -> Desktop -> Value
dialogSemantics audience desktop=case dialog desktop of
  Nothing->snapshot False False []
  Just dg
    | not rootReadable->snapshot True False []
    | otherwise->let (nodes,cut)=bounded 256 32768 candidates in snapshot True cut nodes
    where
      rootBounds=dialogRect desktop dg
      Rect rx ry rw rh=rootBounds
      contentBounds=Rect rx (ry+2) rw (max 0 (rh-5))
      geometry=zip3 [0::Int ..] (fieldRects desktop dg) (fields dg)
      popup=case openComboBox dg of
        Just (i,_,choices,_,_)->Just (i,comboBoxRect desktop dg i choices)
        Nothing->Nothing
      obscured x y=maybe False (\(_,rect)->inside rect x y) popup
      visible popupChild x y=onGrid x y && allowed x y && (popupChild || not (obscured x y))
      allowed x y=case audience of
        GuestSemantics->readableAt desktop x y
        OwnerSemantics->not (streamerMode desktop) || streamerReadableAt desktop x y
      onGrid x y=x>=0 && y>=0 && x<columns && y<rows
      -- The frame corner is outside all field/value rectangles. Central policy
      -- can hide this cell only by hiding the whole modal (approval/private form).
      -- Check that policy before evaluating any title, label, body or value.
      rootReadable=case clip screen rootBounds of
        Nothing->False
        Just (Rect x y _ _)->allowed x y
      rootName=shown False screen (Rect (rx+max 1 ((rw-nameWidth) `div` 2)) ry nameWidth 1) 0 (" "<>dialogTitle dg<>" ")
        where nameWidth=T.length (T.take 4096 (dialogTitle dg))+2
      candidates=case clip screen rootBounds of
        Nothing->[]
        Just rootRect->node ["dialog"] Nothing "dialog" rootName Nothing rootRect False Nothing Nothing Nothing False:
          concatMap fieldNodes (take 128 geometry) ++
          concat [emit False ["dialog","body",number i] (Just ["dialog"]) "text"
            (shown False contentBounds rect 0 text) Nothing rect False Nothing Nothing Nothing False
            | (i,text)<-take 64 (zip [0::Int ..] (body dg)),let rect=Rect (rx+3) (ry+2+i) (max 0 (rw-6)) 1,
              not (any (\(_,fieldRect,_)->overlaps rect fieldRect) geometry)] ++
          concat [emit False ["dialog","button",number i] (Just ["dialog"]) "button"
            (shown False screen rect 0 ("  "<>text<>"  ")) Nothing rect (focus dg==length (fields dg)+i) Nothing Nothing Nothing False
            | (i,(rect,text))<-take 32 (zip [0::Int ..] (zip (buttonRects desktop dg) (buttons dg)))]
      fieldNodes (i,rect@(Rect x y w _),field)=case clip screen =<< clip contentBounds rect of
        Nothing->[]
        Just bounds | not (anyVisible False contentBounds bounds)->[]
        Just bounds->
          let ident=["dialog","field",number i]
              focused=focus dg==i
              private=(audience==GuestSemantics || streamerMode desktop) && privateDialogField desktop dg field
              caption at labelText=shown False contentBounds at 0 labelText
              name=case field of
                Input label _ _->caption (Rect x y w 1) label
                SelectedInput label _ _->caption (Rect x y w 1) label
                ComboBox label _ _ _->caption (Rect x y w 1) label
                CheckBox label _->caption (Rect (x+4) y (max 0 (w-4)) 1) label
                Radio label _ _->caption (Rect x y w 1) label
                ListBox label _ _->caption (Rect x y w 1) label
                FileList{}->caption (Rect x y w 1) (case purpose dg of ChangingDirectory{}->"Directories"; _->"Files")
                ReadOnly label _->caption (Rect x y (min 18 (w `div` 3)) 1) (label<>":")
                TextArea label editable _ _ _ _->let area=textAreaRect rect field in
                  caption (Rect x y (if editable then w else left area-x) 1) (label<>if editable then "  (editable)" else ":")
              stateWith popupChild at value=if private || not (fullyReadable popupChild at) then Nothing else Just value
              state=stateWith False
              textValue at offset text=if private || not (anyVisible False contentBounds at) then Nothing else Just (shown False contentBounds at offset text)
              inputValue text position=if focused && position>16384 then Nothing else
                textValue (Rect x (y+1) w 1) (if focused then max 0 (displayColumn text position-w+1) else 0) text
              container role value checked selected expanded multiline children=
                node ident (Just ["dialog"]) role name value bounds focused checked selected expanded multiline:children
              option popupChild role index selected checked at text=
                if private then [] else emit popupChild (ident++["option",number index]) (Just ident) role
                  (shown popupChild (if popupChild then screen else contentBounds) at 0 text) Nothing at
                  (focused && selected) (if checked then stateWith popupChild at selected else Nothing) (stateWith popupChild at selected) Nothing False
          in case field of
            Input _ text position->container "textbox" (inputValue text position) Nothing Nothing Nothing False []
            SelectedInput _ text selection->container "textbox" (inputValue text (caret selection)) Nothing Nothing Nothing False []
            ReadOnly _ text->let labelWidth=min 18 (w `div` 3) in
              container "text" (textValue (Rect (x+labelWidth) y (w-labelWidth) 1) 0 text) Nothing Nothing Nothing False []
            TextArea _ _ buffer _ firstRow firstColumn->
              let area=textAreaRect rect field
                  visibleArea=clip screen =<< clip contentBounds area
                  value=if private then Nothing else case visibleArea of
                    Nothing->Nothing
                    Just (Rect ax ay aw ah)->Just (T.intercalate "\n"
                      [sourceText (Rect ax (ay+index) aw 1) (firstColumn+ax-left area) line
                      | (index,line)<-zip [0::Int ..] (take (min 256 ah) (contentSourceLinesFrom (bufferContent buffer) (max 0 firstRow+ay-top area)))])
              in container "textbox" value Nothing Nothing Nothing True []
            CheckBox _ checked->container "checkbox" Nothing (state (Rect x y w 1) checked) Nothing Nothing False []
            Radio _ choices selected->container "radiogroup" Nothing Nothing Nothing Nothing False
              (concat [option False "radio" index (index==selected) True (Rect (x+4) (y+1+index) (max 0 (w-4)) 1) text
                | (index,text)<-take 256 (zip [0::Int ..] choices)])
            ListBox _ choices selected->let start=max 0 (selected-3) in
              container "listbox" Nothing Nothing Nothing Nothing False
                (concat [option False "option" index (index==selected) False (Rect (x+1) (y+1+row) (max 0 (w-1)) 1) text
                  | (row,(index,text))<-zip [0::Int ..] (take 4 (drop start (zip [0::Int ..] choices)))])
            ComboBox _ choices selected preview->
              let chosen=case drop (max 0 selected) choices of text:_ | selected>=0->text; _->""
                  options=case popup of
                    Just (owner,Rect px py pw _) | owner==i->concat
                      [option True "option" index (index==maybe selected id preview) False (Rect (px+2) (py+1+index) (max 0 (pw-3)) 1) text
                        | (index,text)<-take 256 (zip [0::Int ..] choices)]
                    _->[]
              in container "combobox" (textValue (Rect x (y+1) (max 0 (w-2)) 1) 0 chosen) Nothing Nothing
                (state (Rect x y w 1) (maybe False (const True) preview)) False options
            FileList entries selected->let cw=max 1 ((w-3) `div` 2); start=max 0 selected `div` 16*16 in
              container "listbox" Nothing Nothing Nothing Nothing False
                (concat [option False "option" index (index==selected) False
                  (Rect (x+2+(offset `div` 8)*(cw+1)) (y+2+offset `mod` 8) (max 0 (cw-1)) 1)
                  (entryName entry<>if entryDirectory entry then "/" else "")
                  | (offset,(index,entry))<-zip [0::Int ..] (take 16 (drop start (zip [0::Int ..] entries)))])
      fullyReadable popupChild rect=case clip screen rect of
        Nothing->False
        Just (Rect x y w h)->and [visible popupChild cx cy | cy<-[y..y+h-1],cx<-[x..x+w-1]]
      anyVisible popupChild limit rect=case clip screen =<< clip limit rect of
        Nothing->False
        Just (Rect x y w h)->or [visible popupChild cx cy | cy<-[y..y+h-1],cx<-[x..x+w-1]]
      emit popupChild ident parent role name value rect focused checked selected expanded multiline=case clip screen =<< clip (if popupChild || role=="button" then screen else contentBounds) rect of
        Just bounds | anyVisible popupChild screen bounds,not (T.null name) || value/=Nothing->
          [node ident parent role name value bounds focused checked selected expanded multiline]
        _->[]
      shown popupChild limit rect@(Rect x y w _) offset text=case clip screen =<< clip limit rect of
        Nothing->""
        Just clipped->T.stripEnd (renderItems (\cx->inside clipped cx y && visible popupChild cx y) x w offset False 0 (displayItems text))
      sourceText rect@(Rect x y w _) column line=
        let (_,start,groups)=sourceLineWindow line column
        in T.stripEnd (renderItems (\cx->inside rect cx y && visible False cx y) x w column True start (concatMap snd groups))
      renderItems permit x width offset source start=go start 0
        where
          go column count _ | column>=offset+width || count>=2048=""
          go _ _ []=""
          go column count (item:rest)=paint<>go (column+advance) (count+T.length paint) rest
            where
              advance=if source then sourceItemAdvance column item else itemWidth item
              at=x+column-offset
              unmasked=column>=offset && column+advance<=offset+width && all permit [at..at+advance-1]
              text=itemDisplayText item
              paint | advance<=0=""
                    | not unmasked=T.replicate (max 0 (min (offset+width) (column+advance)-max offset column)) " "
                    | source && itemSourceText item=="\t"=T.replicate advance " "
                    | T.any isControl text="�"
                    | otherwise=text
  where
    (columns,rows)=screenSize desktop
    screen=Rect 0 0 columns rows
    snapshot :: Bool -> Bool -> [Value] -> Value
    snapshot present cut nodes=object ["present" .= present,"readOnly" .= True,"truncated" .= cut,"nodes" .= nodes]
    number=T.pack . show
    clip (Rect ax ay aw ah) (Rect bx by bw bh)
      | w<=0 || h<=0=Nothing
      | otherwise=Just (Rect x y w h)
      where x=max ax bx; y=max ay by; w=min (ax+aw) (bx+bw)-x; h=min (ay+ah) (by+bh)-y
    overlaps a b=case clip a b of Just _->True; _->False
    node ident parent role name value (Rect x y w h) focused checked selected expanded multiline=
      let title=T.take 256 (T.strip name); text=T.take 2048 <$> value
          size=T.length title+maybe 0 T.length text
          cut=T.length (T.take 257 (T.strip name))>256 || maybe False ((>2048) . T.length . T.take 2049) value
      in (size,cut,object ["id" .= (ident::[Text]),"parent" .= (parent::Maybe [Text]),"role" .= (role::Text),
        "name" .= title,"value" .= text,"bounds" .= [x,y,w,h],"focused" .= focused,
        "checked" .= (checked::Maybe Bool),"selected" .= (selected::Maybe Bool),"expanded" .= (expanded::Maybe Bool),"multiline" .= multiline])
    bounded :: Int -> Int -> [(Int,Bool,Value)] -> ([Value],Bool)
    bounded _ _ []=([],False)
    bounded left budget ((size,shortened,value):rest)
      | left<=0 || size>budget=([],True)
      | otherwise=let (values,cut)=bounded (left-1) (budget-size) rest in (value:values,shortened || cut)
