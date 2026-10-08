{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, BangPatterns, UnboxedTuples, MagicHash #-}
-- | Shared grapheme segmentation, cell widths and picture composition.
--
-- utf8proc supplies stateful, lazy segmentation; source offsets remain
-- Unicode characters while display widths follow graphemes and editor overrides.
-- Clipped GPU cells retain their full semantic glyph. Text-mode partial clusters
-- become blanks. Terminal output advances
-- explicitly past two-cell clusters even when the user's font draws them narrowly.
module Hide.Unicode (SourceCursor, initialSourceCursor, sourceItemStep, sourceSpanStep, sourceItemsFromCursor, sourceLeafFrom, sourceScalarColumn, DisplayItem, displayItems, itemSourceText, itemScalarCount, itemDisplayText, itemOverflow, itemWidth, sourceItemAdvance, graphemes, sourceGraphemesFrom, sourceTextWidth, sourceGlyphAdvance, scalarWidth, clusterWidth, textImage, wideTextImage, displayClusters, terminalProjection, scriptTerminalText, terminalSpan, Script(..), CellSpan(..), CellLayer(..), cellRowsForLayers, cellRowsAndOwnership, cellRowsForPic, cellDisplayOps, flattenPicture, displayOpsForPic, updateDisplayOps, terminalText, textInputChar) where

import Control.Monad (forM_, when)
import Data.Char (isPrint)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Word (Word64,Word16)
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.IORef (readIORef, writeIORef)
import Blaze.ByteString.Builder (Write, writeToByteString)
import Blaze.ByteString.Builder.ByteString (writeByteString)
import Graphics.Vty.Output
import Graphics.Vty.Attributes (FixedAttr(..), defaultStyleMask)
import Graphics.Vty.DisplayAttributes (fixDisplayAttr, displayAttrDiffs)
import Graphics.Vty.Span (SpanOp(..), DisplayOps)
import Control.Monad.ST (runST)
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as TI
import qualified Data.Text.Internal.Unsafe.Char as TC
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MV
import Foreign.C (CInt(..))
import GHC.Exts (Int(I#),Char(C#),Int#,Char#)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Image.Internal as I

foreign import ccall unsafe "utf8proc_charwidth" c_width :: CInt -> CInt
foreign import ccall unsafe "thc_grapheme_step" c_graphemeStep :: CInt -> CInt -> CInt -> Word64

-- | A borrowed source fragment of at most 32 Unicode scalars / 128 UTF8 bytes.
-- Overflow fragments retain every original byte but display as one visible cell.
-- The flag remains set until the natural extended-grapheme boundary.
data DisplayItem = DisplayItem {-# UNPACK #-} !T.Text {-# UNPACK #-} !Int deriving (Eq,Show)

-- | Exact borrowed original bytes, for source offsets and copying.
itemSourceText :: DisplayItem -> T.Text
itemSourceText (DisplayItem text _)=text
-- Low two bits cache natural width; the following flags cache overflow,
-- single tab, single CR and controls. Scalar count begins at bit six.
-- | Whether this fragment belongs to a capped natural grapheme.
itemOverflow :: DisplayItem -> Bool
itemOverflow (DisplayItem _ meta)=meta .&. 4/=0
-- | Bounded font/terminal input; overflow is one replacement character.
itemDisplayText :: DisplayItem -> T.Text
itemDisplayText item=if itemOverflow item then "�" else itemSourceText item
-- | Shared natural display width; overflow fragments always occupy one cell.
itemWidth :: DisplayItem -> Int
itemWidth (DisplayItem _ meta)=if meta .&. 4/=0 then 1 else meta .&. 3
-- | Resolve source tabs/controls or the one-cell overflow presentation.
sourceItemAdvance :: Int -> DisplayItem -> Int
sourceItemAdvance col (DisplayItem _ meta)
  | meta .&. 4/=0=1
  | meta .&. 8/=0=8-col `mod` 8
  | meta .&. 16/=0=0
  | meta .&. 32/=0=1
  | otherwise=meta .&. 3

-- | Original scalar extent, cached by the shared numeric cursor.
itemScalarCount :: DisplayItem -> Int
itemScalarCount (DisplayItem _ meta)=meta `shiftR` 6

-- | Numeric segmentation checkpoint, with no source payload. Equality observes
-- only the previous codepoint, utf8proc state and continuation/lookahead flags.
-- A cut before real EOF retains the next scalar's already-processed transition;
-- a real EOF checkpoint has no pending scalar. Appending source still requires
-- repairing its former last item before reusing that item's boundary.
data SourceCursor = SourceCursor {-# UNPACK #-} !CInt {-# UNPACK #-} !CInt {-# UNPACK #-} !Int deriving Eq

-- | Start a physical source row with no preceding Unicode context.
initialSourceCursor :: SourceCursor
initialSourceCursor=SourceCursor (-1) 0 0

-- | Advance one bounded item in the complete original source. The byte offset
-- must be zero or a previous returned endpoint in this same source. The unboxed
-- result is @(endByte, scalarCount, advanceAtColumnZero, singleTab, overflow,
-- nextCursor)@. Tabs are resolved against the caller's absolute column. At real
-- EOF count is zero and the cursor is unchanged. This INLINE boundary lets a
-- numeric consumer retain checkpoints only at actual storage cuts.
sourceItemStep :: T.Text -> Int -> SourceCursor -> (# Int,Int,Int,Bool,Bool,SourceCursor #)
sourceItemStep text byte cursor@(SourceCursor previous state flags)
  | byte>=TU.lengthWord8 text=(# byte,0,0,False,False,cursor #)
  | otherwise=case itemEnd text byte previous state (flags .&. 1/=0) (flags .&. 2/=0) of
      (# end#,count#,first#,natural#,controls,overflow,continued,prev#,nextState# #)->
        let end=I# end#; count=I# count#; first=C# first#
            advance=if overflow then 1 else sourceAdvance 0 count first (I# natural#) controls
            next=SourceCursor (fromIntegral (I# prev#)) (fromIntegral (I# nextState#))
              ((if continued then 1 else 0) .|. (if end<TU.lengthWord8 text then 2 else 0))
        in (# end,count,advance,count==1 && first=='\t',overflow,next #)
{-# INLINE sourceItemStep #-}

-- | Consume complete bounded items until a byte target, scalar limit or real
-- EOF, returning @(endByte, scalarCount, itemCount, tabPrefix, tabSuffix,
-- lastOverflow, nextCursor)@. A negative tab prefix denotes an additive advance;
-- otherwise the transform is @nextTab (column + tabPrefix) + tabSuffix@.
-- The incoming cursor and byte offset follow 'sourceItemStep' provenance rules.
-- Limits are tested at item edges, so the last item may cross either target.
-- With following source, fewer than 132 remaining bytes stops before the next
-- item: the storage owner must then supply a real lookahead bridge. An initially
-- stopped call returns zero counts and the unchanged cursor. Only a returned
-- span receipt constructs a cursor; the inner item loop carries numeric state.
sourceSpanStep :: T.Text -> Int -> SourceCursor -> Int -> Int -> Bool -> (# Int,Int,Int,Int,Int,Bool,SourceCursor #)
sourceSpanStep text start incoming@(SourceCursor previous state flags) target limit following=
  case go start 0 0 (-1) 0 previous state flags False of
    (# end#,chars#,items#,prefix#,suffix#,receipt,prev#,state#,bits# #)->
      (# I# end#,I# chars#,I# items#,I# prefix#,I# suffix#,receipt,
         if I# items#==0 then incoming else SourceCursor
           (fromIntegral (I# prev#)) (fromIntegral (I# state#)) (I# bits#) #)
  where
    size=TU.lengthWord8 text
    -- Primitive inner results prevent boxed API counters becoming loop state.
    finish (I# byte) (I# chars) (I# items) (I# prefix) (I# suffix) receipt prev st (I# bits)=
      case fromIntegral prev of
        I# previous#->case fromIntegral st of
          I# state#->(# byte,chars,items,prefix,suffix,receipt,previous#,state#,bits #)
    go !byte !chars !items !prefix !suffix !prev !st !bits !receipt
      | byte>=target || chars>=limit || byte>=size || following && size-byte<132=
          finish byte chars items prefix suffix receipt prev st bits
      | otherwise=case itemEnd text byte prev st (bits .&. 1/=0) (bits .&. 2/=0) of
          (# end#,count#,first#,natural#,controls,overflow,continued,prev#,nextState# #)->
            let end=I# end#; count=I# count#; first=C# first#
                tab=count==1 && first=='\t'
                advance=if overflow then 1 else sourceAdvance 0 count first (I# natural#) controls
                prefix'=if tab && prefix<0 then suffix else prefix
                suffix'=if tab then if prefix<0 then 0 else 8*(suffix `div` 8+1) else suffix+advance
                bits'=(if continued then 1 else 0) .|. (if end<size then 2 else 0)
            in go end (chars+count) (items+1) prefix' suffix'
              (fromIntegral (I# prev#)) (fromIntegral (I# nextState#)) bits' overflow
{-# NOINLINE sourceSpanStep #-}

-- | Emit a borrowed storage leaf using its captured incoming checkpoint. The
-- Boolean is the proven overflow flag of its last item in the complete source;
-- storage EOF alone cannot determine that flag. It is used only at leaf EOF.
-- Concatenating original item bytes returns the exact leaf, including zero-width
-- leading items. Successive leaves use their own stored incoming checkpoints.
sourceItemsFromCursor :: SourceCursor -> Bool -> T.Text -> [DisplayItem]
sourceItemsFromCursor (SourceCursor previous state flags) lastOverflow text=
  scanItems text lastOverflow 0 previous state (flags .&. 1/=0) (flags .&. 2/=0)
{-# INLINE sourceItemsFromCursor #-}

-- | Lazy bounded display segmentation. Concatenating original fragments returns
-- the exact input. Only one scalar beyond a fragment is examined; state survives
-- cap cuts, RI pairs and ZWJ sequences. Normal complete graphemes remain intact.
displayItems :: T.Text -> [DisplayItem]
displayItems=sourceItemsFromCursor initialSourceCursor False
{-# NOINLINE displayItems #-}

-- | Original-byte projection of the shared bounded display segmentation.
-- This accessor is for source editing/copying, never font shaping.
graphemes :: T.Text -> [T.Text]
graphemes=map itemSourceText . displayItems
{-# NOINLINE graphemes #-}

scanItems :: T.Text -> Bool -> Int -> CInt -> CInt -> Bool -> Bool -> [DisplayItem]
scanItems text lastOverflow=scan
  where
    size=TU.lengthWord8 text
    scan !start !previous !state !continued !processed
      | start>=size=[]
      | otherwise=case itemEnd text start previous state continued processed of
          (# end#,count#,first#,natural#,controls,overflow,nextContinued,prev#,nextState# #)->
            let end=I# end#; prev=fromIntegral (I# prev#); nextState=fromIntegral (I# nextState#)
                shownOverflow=overflow || end==size && lastOverflow in
            DisplayItem (TU.takeWord8 (end-start) (TU.dropWord8 start text))
              ((I# count# `shiftL` 6) .|. I# natural# .|. (if shownOverflow then 4 else 0) .|.
               (if I# count#==1 && C# first#=='\t' then 8 else 0) .|.
               (if I# count#==1 && C# first#=='\r' then 16 else 0) .|. (if controls then 32 else 0)):
              scan end prev nextState nextContinued True

-- One numeric cursor owns both emission and seek. The unboxed receipt does not
-- allocate discarded source fragments. A cap stop leaves the lookahead scalar
-- unconsumed in the source, but retains its processed codepoint/state. The next
-- fragment therefore skips that transition and applies it exactly once.
{-# INLINE itemEnd #-}
itemEnd :: T.Text -> Int -> CInt -> CInt -> Bool -> Bool -> (# Int#,Int#,Char#,Int#,Bool,Bool,Bool,Int#,Int# #)
itemEnd text start previous state continued processed=go start 0 '\0' 0 False previous state
  where
    size=TU.lengthWord8 text
    finish (I# byte) (I# count) (C# first) (I# natural) controls overflow nextContinued prev st=
      case fromIntegral prev of
        I# previous#->case fromIntegral st of
          I# state#->(# byte,count,first,natural,controls,overflow,nextContinued,previous#,state# #)
    go !byte !count !first !natural !controls !prev !st
      | byte>=size=finish byte count first natural controls continued False prev st
      | otherwise=case TU.iter text byte of
          TU.Iter char bytes->
            let point=fromIntegral (fromEnum char)
                -- The incoming state already includes the first scalar transition.
                -- Force later FFI results once, avoiding a shared lazy thunk.
                !step=if count==0 && (processed || prev<0) then 0 else c_graphemeStep prev point st
                nextState=if count==0 && processed then st else fromIntegral (step `shiftR` 1)
                boundary=prev>=0 && step .&. 1/=0
            in if count>0 && boundary then finish byte count first natural controls continued False point nextState
               else if count>=32 || byte+bytes-start>128 then finish byte count first natural controls True True point nextState
               else go (byte+bytes) (count+1) (if count==0 then char else first)
                 (max natural (scalarWidth char)) (controls || sourceControl char) point nextState

-- | Seek the bounded source display item overlapping a display column.
-- Returns scalar offset, byte offset, column and borrowed item suffix. Skipped
-- items allocate no Text/list fragments. Overflow fragments have one-cell advance
-- and retain exact source ranges; state and overflow survive prefix seeking.
sourceGraphemesFrom :: Int -> T.Text -> (Int,Int,Int,[DisplayItem])
sourceGraphemesFrom requested=sourceLeafFrom requested 0 initialSourceCursor False
{-# NOINLINE sourceGraphemesFrom #-}

-- | Seek a borrowed storage leaf with absolute source/tab columns. Returns its
-- local scalar offset, local UTF8 byte offset, absolute column and item suffix.
-- The incoming cursor and last-item overflow receipt were captured against the
-- complete source. Skipped items allocate no Text/list fragments. The law is
-- @sourceLeafFrom goal 0 initialSourceCursor False text ≡ sourceGraphemesFrom goal text@.
sourceLeafFrom :: Int -> Int -> SourceCursor -> Bool -> T.Text -> (Int,Int,Int,[DisplayItem])
sourceLeafFrom requested initialColumn (SourceCursor previous state flags) lastOverflow text=
  case seek 0 0 initialColumn previous state (flags .&. 1/=0) (flags .&. 2/=0) of
    (# char,byte,col,suffix #)->(I# char,I# byte,I# col,suffix)
  where
    finish (I# char) (I# byte) (I# col) suffix=(# char,byte,col,suffix #)
    goal=max 0 requested
    size=TU.lengthWord8 text
    seek !byte !char !col !prev !st !continued !processed
      | byte>=size=finish char byte col []
      | otherwise=case itemEnd text byte prev st continued processed of
          (# end#,count#,first#,natural#,controls,overflow,nextContinued,nextPrev#,nextState# #)->
            let end=I# end#; count=I# count#; first=C# first#; natural=I# natural#
                nextPrev=fromIntegral (I# nextPrev#); nextState=fromIntegral (I# nextState#)
                advance=if overflow || end==size && lastOverflow then 1 else sourceAdvance col count first natural controls
            in if col+advance>goal then finish char byte col (scanItems text lastOverflow byte prev st continued processed)
               else seek end (char+count) (col+advance) nextPrev nextState nextContinued True
{-# NOINLINE sourceLeafFrom #-}

-- | Exact absolute column for a local source-scalar position in a captured leaf.
-- An interior position snaps to its complete item's starting column. Incoming
-- cursor and artificial-EOF overflow facts follow 'sourceLeafFrom'. The numeric
-- loop retains no per-item cursor or source fragments; only the final column is
-- boxed. Tabs resolve against the supplied absolute column.
sourceScalarColumn :: Int -> Int -> SourceCursor -> Bool -> T.Text -> Int
sourceScalarColumn requested initialColumn (SourceCursor previous state flags) lastOverflow text=
  I# (seek 0 0 initialColumn previous state (flags .&. 1/=0) (flags .&. 2/=0))
  where
    goal=max 0 requested
    size=TU.lengthWord8 text
    finish (I# col)=col
    seek !byte !char !col !prev !st !continued !processed
      | char>=goal || byte>=size=finish col
      | otherwise=case itemEnd text byte prev st continued processed of
          (# end#,count#,first#,natural#,controls,overflow,nextContinued,nextPrev#,nextState# #)->
            let end=I# end#; count=I# count#
                advance=if overflow || end==size && lastOverflow then 1 else sourceAdvance col count (C# first#) (I# natural#) controls
            in if char+count>goal then finish col else seek end (char+count) (col+advance)
              (fromIntegral (I# nextPrev#)) (fromIntegral (I# nextState#)) nextContinued True
{-# NOINLINE sourceScalarColumn #-}

-- | Natural source cell extent, including tab stops and control placeholders.
-- Reuses numeric UTF8/stateful grapheme seeking; counting width does not allocate
-- a Text fragment/list node per glyph. Plain viewport emission uses the same law:
-- @sourceTextWidth text ≡ sum of sourceItemAdvance at consecutive columns@.
sourceTextWidth :: T.Text -> Int
sourceTextWidth text=let (_,_,width,_)=sourceGraphemesFrom maxBound text in width

-- | Source display advance, independent of UTF8 bytes and scalar count.
-- CR alone has zero advance, tabs reach the next eight-cell stop, and control
-- graphemes use the existing one-cell placeholder. Other glyphs use the shared
-- maximum scalar width. Numeric prefix seeking and visible emission share this
-- policy; CRLF therefore remains a single one-cell control grapheme.
sourceGlyphAdvance :: Int -> T.Text -> Int
sourceGlyphAdvance col text=sourceAdvance col (T.length text)
  (maybe '\0' fst (T.uncons text)) (clusterWidth text) (T.any sourceControl text)

sourceControl :: Char -> Bool
sourceControl c=c<' ' || c=='\DEL'

sourceAdvance :: Int -> Int -> Char -> Int -> Bool -> Int
sourceAdvance col count first natural controls
  | count==1 && first=='\r'=0
  | count==1 && first=='\t'=8-col `mod` 8
  | controls=1
  | otherwise=natural

-- | Width of one Unicode scalar under the shared display overrides. Printable
-- ASCII occupies one cell; combining/control scalars remain zero-width.
-- @scalarWidth c ≡ clusterWidth (T.singleton c)@.
scalarWidth :: Char -> Int
scalarWidth c
  | c>=' ' && c<'\127' = 1
  | c `elem` ['⌥','⌘','\xf024b','\xf0770','\xfe0f','\x20e3'] = 2
  | c>='\x1f1e6' && c<='\x1f1ff' = 2
  | otherwise = max 0 (fromIntegral (c_width (fromIntegral (fromEnum c))))

clusterWidth :: T.Text -> Int
clusterWidth = T.foldl' (\width c->max width (scalarWidth c)) 0

textImage :: V.Attr -> T.Text -> V.Image
textImage _ t | T.null t = V.emptyImage
textImage a t = I.HorizText a (TL.fromStrict t) (if T.all simpleChar t then T.length t else sum (map itemWidth (displayItems t))) (T.length t)

-- | Render each semantic grapheme in exactly two cells. Naturally wide text
-- remains two cells. The original text remains in HorizText, whose explicit
-- advance survives the compositor; no Unicode substitution or attribute marker.
wideTextImage :: V.Attr -> T.Text -> V.Image
wideTextImage a=unmergedImages . map (\item->let g=itemDisplayText item; n=if itemOverflow item then 1 else 2 in I.HorizText a (TL.fromStrict g) n (T.length g)) . displayItems

-- Vty merges adjacent HorizText values by attribute. That optimization assumes
-- natural advances, so an explicit-width primitive retains a join boundary.
unmergedImages :: [V.Image] -> V.Image
unmergedImages=foldr (\image rest->I.HorizJoin image rest
  (V.imageWidth image+V.imageWidth rest) (max (V.imageHeight image) (V.imageHeight rest))) V.emptyImage

-- | Read a text primitive's explicit advance. A deliberately widened primitive
-- contains one grapheme; ordinary runs retain natural widths.
displayClusters :: Int -> T.Text -> [(T.Text,Int)]
displayClusters width text=case displayItems text of
  [item] | width==2 && not (itemOverflow item) -> [(itemDisplayText item,2)]
  items -> [(itemDisplayText item,itemWidth item) | item<-items]

-- | The final visible row representation shared by terminal and GPU frontends.
-- CellText contains complete one-codepoint, one-cell glyphs. CellGlyph retains
-- one semantic grapheme, its full allocated width, visible start and visible
-- width. Its glyph origin is the current row position minus the clip start.
-- Backend projection happens after composition; privacy masks replace the
-- semantic glyph before these rows can leave the capture owner.
data Script = Superscript | Subscript deriving (Eq,Show)

-- | Script glyphs retain one complete source grapheme and validated natural
-- width one or two. Their allocated width is always one cell. Ordinary spans
-- keep their existing representation.
data CellSpan = CellText !V.Attr !T.Text | CellGlyph !V.Attr !T.Text !Int !Int !Int
  | CellScript !V.Attr !T.Text !Int !Script
  deriving (Eq,Show)

-- Every occupied cell retains the glyph and its position within that glyph.
-- Overwriting one cell preserves the visible portion of an underlying glyph.
data Cell = Cell !V.Attr !T.Text !Int !Int | ScriptCell !V.Attr !T.Text !Int !Script | CharCell !V.Attr !Char | Unfilled !(Maybe V.Attr)

-- | Ordered opaque images and small style-only halo regions, front to back.
-- Halo processing preserves glyph origin, identity and allocated width.
data CellLayer = CellCanvas !Int ![CellLayer] | CellImage !V.Image | CellHalo !V.Attr ![(Int,Int,Int,Int)]
               | CellMask !V.Attr ![(Int,Int,Int)]
               -- Positioned row with absolute left/right clip coordinates.
               | CellRow !Int !Int !Int !Int !(Vec.Vector CellSpan)

-- | Compose layers once into bounded mutable storage, retaining partial glyphs.
-- Cost depends on the visible scene, never on Document or Buffer equality.
cellRowsForPic :: V.Picture -> (Int,Int) -> Vec.Vector (Vec.Vector CellSpan)
cellRowsForPic picture=cellRowsForLayers (map CellImage (V.picLayers picture))

-- | Compose image and halo layers in one visible grid. Halo work touches only
-- its bounded exposed bands after occlusion. Front-to-back traversal skips
-- writes to already occupied cells; only unfilled cells accept a halo style.
cellRowsForLayers :: [CellLayer] -> (Int,Int) -> Vec.Vector (Vec.Vector CellSpan)
cellRowsForLayers layers size=fst (cellRowsAndOwnership layers size)

-- | Compose fallback cells and canvas ownership in one front-to-back pass.
-- Ordinary cells have slot zero; only accepted writes within CellCanvas acquire
-- its slot. Halos set bit 15 and post-composition privacy masks clear ownership.
-- With no canvas layer, ownership is empty rather than a grid of zero slots.
cellRowsAndOwnership :: [CellLayer] -> (Int,Int) -> (Vec.Vector (Vec.Vector CellSpan),BS.ByteString)
cellRowsAndOwnership layers size=let (cells,ownership)=composeCellGrid layers size
  in (rowsFromCells cells size,ownership)

-- Printable ASCII and box/block drawing codepoints each occupy one cell and
-- have no internal grapheme boundary interaction; combining/variation text goes
-- through the full segmenter instead.
simpleChar :: Char -> Bool
simpleChar c=(c>=' ' && c<='~') || (c>='\x2500' && c<='\x259f')

forCells :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
forCells lo hi f=go lo
  where go !i | i>=hi=pure ()
              | otherwise=f i >> go (i+1)
{-# INLINE forCells #-}

composeCellGrid :: [CellLayer] -> (Int,Int) -> (Vec.Vector Cell,BS.ByteString)
composeCellGrid layers (w,h)=runST $ do
  grid<-MV.replicate (w*h) (Unfilled Nothing)
  let canvasPresent=any (\layer->case layer of CellCanvas{}->True; _->False) layers
  owners<-UM.replicate (if canvasPresent then w*h else 0) (0::Word16)
  let put slot at cell=do
        original<-MV.unsafeRead grid at
        case original of
          Unfilled paint->do
            MV.unsafeWrite grid at (maybe cell (dim cell) paint)
            when (slot/=0) (UM.unsafeWrite owners at (fromIntegral slot .|. (if maybe False (const True) paint then 32768 else 0)))
          _->pure ()
      dim (CharCell old c) paint=CharCell paint {V.attrStyle=V.attrStyle old} c
      dim (Cell old text width offset) paint=Cell paint {V.attrStyle=V.attrStyle old} text width offset
      dim (ScriptCell old text natural script) paint=ScriptCell paint {V.attrStyle=V.attrStyle old} text natural script
      dim cell _=cell
  let draw slot (l,top,r,b) x y img=case img of
        I.HorizText a text advance _ | y>=top && y<b -> do
          let strict=TL.toStrict text
          if advance==T.length strict && T.all simpleChar strict
            then let lo=max l x; hi=min r (x+advance)
                     visible=T.drop (lo-x) strict
                     -- visible begins at a codepoint boundary. The character
                     -- count bounds the cursor; each iteration advances UTF8 bytes.
                     next !i !_ | i>=hi=pure ()
                     next !i !byte=case TU.iter visible byte of
                       TU.Iter c n->put slot (y*w+i) (CharCell a c) >> next (i+1) (byte+n)
                 in next lo 0
            else if advance==T.length strict && not (T.null strict) && T.all (==T.head strict) strict && clusterWidth (T.take 1 strict)==1
              then let glyph=T.take 1 strict in forCells (max l x) (min r (x+advance)) $ \i->put slot (y*w+i) (Cell a glyph 1 0)
            else do
              let chunks=displayClusters advance strict
              forM_ (zip (scanl (+) x (map snd chunks)) chunks) $ \(cx,(t,n))->do
                let lo=max l cx; hi=min r (cx+n)
                forCells lo hi $ \i->put slot (y*w+i)
                  (if n==1 && T.length t==1 && T.head t<'\128' then CharCell a (T.head t) else Cell a t n (i-cx))
        I.HorizJoin left right _ _->draw slot (l,top,r,b) x y left >> draw slot (l,top,r,b) (x+V.imageWidth left) y right
        I.VertJoin above below _ _->draw slot (l,top,r,b) x y above >> draw slot (l,top,r,b) x (y+V.imageHeight above) below
        I.Crop inside dx dy cw ch->
          let clip=(max l x,max top y,min r (x+cw),min b (y+ch))
          in when (max l x<min r (x+cw) && max top y<min b (y+ch)) (draw slot clip (x-dx) (y-dy) inside)
        _->pure ()
  let layer slot (CellImage image)=draw slot (0,0,w,h) 0 0 image
      layer _ (CellCanvas slot children)=mapM_ (layer slot) children
      layer slot (CellRow origin y clipLeft clipRight spans)
        | y<0 || y>=h || max 0 clipLeft>=min w clipRight=pure ()
        | otherwise=drawRow origin 0
        where
          lo=max 0 clipLeft; hi=min w clipRight
          drawRow !x !index
            | x>=hi || index>=Vec.length spans=pure ()
            | otherwise=case Vec.unsafeIndex spans index of
                CellText paint text->do
                  let count=T.length text
                      left=max lo x; right=min hi (x+count)
                      visible=T.drop (max 0 (left-x)) text
                      run !at !byte
                        | at>=right=pure ()
                        | otherwise=case TU.iter visible byte of
                            TU.Iter c bytes->put slot (y*w+at) (CharCell paint c) >> run (at+1) (byte+bytes)
                  run left 0
                  drawRow (x+count) (index+1)
                CellGlyph paint text full start shown->do
                  forCells (max lo x) (min hi (x+shown)) $ \at->put slot (y*w+at) (Cell paint text full (start+at-x))
                  drawRow (x+shown) (index+1)
                CellScript paint text natural script->do
                  when (x>=lo && x<hi) (put slot (y*w+x) (ScriptCell paint text natural script))
                  drawRow (x+1) (index+1)
      layer _ (CellHalo paint regions)=forM_ regions $ \(x,y,columns,rows)->
        forCells (max 0 y) (min h (y+rows)) $ \cy->
          forCells (max 0 x) (min w (x+columns)) $ \cx->do
            original<-MV.unsafeRead grid (cy*w+cx)
            case original of
              Unfilled Nothing->MV.unsafeWrite grid (cy*w+cx) (Unfilled (Just paint))
              _->pure ()
      layer _ CellMask{}=pure ()
      mask paint regions=forM_ regions $ \(x,y,columns)->when (y>=0 && y<h) $
        forCells (max 0 x) (min w (x+columns)) $ \cx->do
          original<-MV.unsafeRead grid (y*w+cx)
          case original of
            Cell _ text width offset->forCells (max 0 (cx-offset)) (min w (cx-offset+width)) $ \i->do
              visible<-MV.unsafeRead grid (y*w+i)
              case visible of
                Cell _ glyph full part | glyph==text && full==width && i-part==cx-offset->
                  MV.unsafeWrite grid (y*w+i) (CharCell paint '*') >> when canvasPresent (UM.unsafeWrite owners (y*w+i) 0)
                _->pure ()
            _->MV.unsafeWrite grid (y*w+cx) (CharCell paint '*') >> when canvasPresent (UM.unsafeWrite owners (y*w+cx) 0)
  mapM_ (layer 0) layers
  mapM_ (uncurry mask) [(paint,regions) | CellMask paint regions<-layers]
  cells<-Vec.unsafeFreeze grid
  ownership<-UV.unsafeFreeze owners
  let bytes | not canvasPresent=BS.empty
            | otherwise=fst (BS.unfoldrN (2*w*h) (\i->let value=ownership UV.! (i `div` 2)
             in Just (fromIntegral (if even i then value .&. 255 else value `shiftR` 8),i+1)) 0)
  pure (cells,bytes)

rowsFromCells :: Vec.Vector Cell -> (Int,Int) -> Vec.Vector (Vec.Vector CellSpan)
rowsFromCells cells (w,h)=Vec.generate h row
  where
    row y=Vec.unfoldr spanAt 0
      where
        at x=Vec.unsafeIndex cells (y*w+x)
        spanAt x | x>=w=Nothing
        spanAt x=case at x of
          CharCell a _->simpleSpan a x
          Unfilled paint->simpleSpan (maybe V.defAttr id paint) x
          Cell a t 1 0 | T.length t==1->simpleSpan a x
          ScriptCell a text natural script->Just (CellScript a text natural script,x+1)
          Cell a t n offset->let end=follow a t n (offset+1) (x+1)
                             in Just (CellGlyph a t n offset (end-x),end)
        simpleSpan a x=let end=textEnd a (x+1)
                       in Just(CellText a (packText x end),end)
        -- Four bytes per admitted codepoint is sufficient even outside the
        -- BMP. Only this exact run is written, then its array is shrunk/frozen.
        packText x end=runST $ do
          bytes<-TA.new ((end-x)*4)
          let fill !i !offset | i>=end=pure offset
              fill !i !offset=do
                written<-TC.unsafeWrite bytes offset (charAt i)
                fill (i+1) (offset+written)
          used<-fill x 0
          TA.shrinkM bytes used
          array<-TA.unsafeFreeze bytes
          pure (TI.Text array 0 used)
        charAt x=case at x of
          CharCell _ c->c
          Unfilled _->' '
          Cell _ t _ _->T.head t
          ScriptCell{}->error "Script cell entered an ordinary text run."
        textEnd _ !x | x>=w=x
        textEnd a !x=case at x of
          CharCell b _ | a==b->textEnd a (x+1)
          Unfilled paint | a==maybe V.defAttr id paint->textEnd a (x+1)
          Cell b t 1 0 | a==b && T.length t==1->textEnd a (x+1)
          _->x
        follow a t n !expected !x
          | x<w,expected<n,Cell b g width part<-at x,
            a==b && t==g && n==width && part==expected=follow a t n (expected+1) (x+1)
          | otherwise=x
-- | Text-mode projection suppresses partial graphemes with occupied-cell blanks.
-- A complete explicit-width glyph retains its advance for terminal correction.
displayOpsForPic :: V.Picture -> (Int,Int) -> DisplayOps
displayOpsForPic picture size=cellDisplayOps (cellRowsForPic picture size)

-- | Project an already composed common grid for text-mode output. Partially
-- visible graphemes occupy blanks; complete glyphs retain their explicit width.
cellDisplayOps :: Vec.Vector (Vec.Vector CellSpan) -> DisplayOps
cellDisplayOps=Vec.map (Vec.map terminal)
  where
    terminal (CellText a text)=TextSpan a (T.length text) (T.length text) (TL.fromStrict text)
    terminal (CellScript a text natural _)=let shown=scriptTerminalText natural text
                                           in TextSpan a 1 (T.length shown) (TL.fromStrict shown)
    terminal (CellGlyph a text full start width)
      | start==0 && width==full=TextSpan a full (T.length text) (TL.fromStrict text)
      | otherwise=TextSpan a width width (TL.fromStrict (T.replicate width " "))

-- | Explicit text-mode picture projection. Display frontends consume cell rows
-- directly; this helper is for callers needing an ordinary clipped Vty image.
flattenPicture :: (Int,Int) -> V.Picture -> V.Picture
flattenPicture size picture=picture {V.picLayers=[V.vertCat (map row (Vec.toList (displayOpsForPic picture size)))]}
  where row=unmergedImages . map image . Vec.toList
        image (TextSpan a tWidth chars text)=I.HorizText a text tWidth chars
        image _=V.emptyImage

-- | Emit already prepared spans at captured terminal bounds through Vty
-- capabilities and its row-diff cache. Rows must fill those bounds. Explicit
-- advances survive terminal font correction; no picture composition occurs here.
updateDisplayOps :: Output -> (Int,Int) -> V.Cursor -> DisplayOps -> IO ()
updateDisplayOps output size@(w,h) position ops = do
  dc <- displayContext output size
  previous <- readIORef (assumedStateRef output)
  urls <- getModeStatus output Hyperlink
  let initial=FixedAttr defaultStyleMask Nothing Nothing Nothing
      changed y row=case prevOutputOps previous of
        Just old | Vec.length old==Vec.length ops -> old Vec.! y/=row
        _ -> True
      emit y (prefix,old,x) (TextSpan a advance _ t) =
        let limited=limitAttrForDisplay output a
            fixed=fixDisplayAttr old limited
            (text,end)=terminalSpan (\col -> writeMoveCursor dc (min (w-1) col) y) x advance (TL.toStrict t)
        in (prefix <> writeSetAttr dc urls old limited (displayAttrDiffs old fixed) <> text,fixed,end)
      emit _ state _=state
      rowBytes y row=let (text,_,_)=foldl' (emit y) (mempty,initial,0) (Vec.toList row)
                     in writeMoveCursor dc 0 y <> writeDefaultAttr dc urls <> text
      cursor=case position of
        V.Cursor x y -> at x y
        V.AbsoluteCursor x y -> at x y
        _ -> mempty
      at x y=writeShowCursor dc <> writeMoveCursor dc (max 0 (min (w-1) x)) (max 0 (min (h-1) y))
      bytes=writeHideCursor dc <> mconcat [rowBytes y row | (y,row)<-zip [0..] (Vec.toList ops),changed y row] <> cursor
  outputByteBuffer output (writeToByteString bytes)
  writeIORef (assumedStateRef output) previous {prevOutputOps=Just ops}

-- | Accept printable input and the joiner/tag characters needed by complex graphemes.
textInputChar :: Char -> Bool
textInputChar c=isPrint c || c `elem` ['\x200c','\x200d'] || c>='\xe0020' && c<='\xe007f'

-- | Scripts retain one cell in text mode. Natural one-cell glyphs keep their
-- original text; natural two-cell glyphs use the existing one-cell placeholder.
-- This is a display projection only; semantic spans and copied source stay whole.
scriptTerminalText :: Int -> T.Text -> T.Text
scriptTerminalText natural text | natural==2="\xfffd"
                                | otherwise=text

-- | Encode positioned terminal text with explicit advancement across two-cell clusters.
terminalText :: (Int -> Write) -> Int -> T.Text -> (Write,Int)
terminalText move start text = foldl' emit (mempty,start) (map itemDisplayText (displayItems text))
  where
    emit (bytes,x) g =
      let n=clusterWidth g
          raw=writeByteString (TE.encodeUtf8 g)
          drawn=if n==2 then writeByteString "  " <> move x <> raw <> move (x+2) else raw
      in (bytes<>drawn,x+n)

-- | Project explicit advances only at terminal output. Fullwidth ASCII and
-- ideographic spaces preserve two-cell geometry; other narrow graphemes use
-- their original text plus a padding cell. Semantic text remains unchanged.
terminalSpan :: (Int -> Write) -> Int -> Int -> T.Text -> (Write,Int)
terminalSpan move start advance text=terminalText move start (terminalProjection advance text)

-- | Display-only fullwidth/padding projection, also used by plain grid snapshots.
terminalProjection :: Int -> T.Text -> T.Text
terminalProjection advance text=case displayClusters advance text of
  [(g,2)] | clusterWidth g<2 -> fullwidth g
  _->text
  where
    fullwidth g | clusterWidth g==0 = " "<>g<>" "
                | Just (base,rest)<-T.uncons g,base==' ' = "\x3000"<>rest
                | Just (base,rest)<-T.uncons g,base>='!' && base<='~' = T.cons (toEnum (fromEnum base+0xfee0)) rest
                | otherwise=g<>" "
