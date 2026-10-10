{-# LANGUAGE OverloadedStrings, UnboxedTuples #-}
module UnicodeCheck (checks) where
import AllocationProfile (AllocationProfile, withinBudget)
import Control.Monad (unless, forM_)
import Control.Exception (evaluate)
import GHC.Conc (getAllocationCounter)
import qualified Data.Vector as Vec
import Blaze.ByteString.Builder (writeToByteString)
import Blaze.ByteString.Builder.ByteString (writeByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import Data.Foldable (toList)
import qualified Graphics.Vty as V
import Graphics.Vty.Span (SpanOp(..))
import Hide.Buffer
import Hide.Model
import Hide.Unicode

checks :: AllocationProfile -> IO ()
checks profile = do
  let check name ok=unless ok (error name)
      clusters=["👩🏽\x200d\&💻","👨\x200d\&👩\x200d\&👧\x200d\&👦","🏳️\x200d\&🌈","🇯🇵","❤️","1️⃣","e\x301","क्\x200d\&ष"]
      wide=take 6 clusters
      segments=["λ","e\x301","👩🏽\x200d\&💻","🇨🇦","終","\r\n","x"]
      source="prefix"<>T.concat segments<>"suffix"
      fragment=T.dropEnd 6 (T.drop 6 source)
  let (_,emptyMask)=cellRowsAndOwnership [CellImage (V.string V.defAttr "ordinary")] (40,18)
  check "ordinary composition carries no canvas mask bytes" (BS.null emptyMask)
  check "scalar widths retain controls, combining, wide and display overrides"
    (map scalarWidth ['a','é','│','\0','\x301','\x200d','界','⌥','⌘','\xf024b','\xf0770','\xfe0f','\x20e3','\x1f1e6']==[1,1,1,0,0,0,2,2,2,2,2,2,2,2])
  check "cluster width remains the maximum scalar display width"
    (map clusterWidth ["","abc","e\x301","a⌘","a\xfe0f","a\x20e3","🇨🇦"]==[0,1,1,2,2,2,2])
  check "mixed UTF8 graphemes preserve source slices and CRLF"
    (graphemes fragment==segments && T.concat (graphemes fragment)==fragment)
  check "stateful iterator retains parity across yielded flag pairs"
    (graphemes "🇦🇧🇨🇩🇪"==["🇦🇧","🇨🇩","🇪"])
  let oversized="a"<>T.replicate 70 "\x301"<>"Z"
      fragments=graphemes oversized
  check "display fragments are bounded and retain every original byte"
    (all (\g->T.length g<=32 && TU.lengthWord8 g<=128) fragments && T.concat fragments==oversized)
  let exact="a"<>T.replicate 31 "\x301"
      excess=exact<>"\x301"
      tagged="🏴"<>T.replicate 65 "\xe0061"<>"\xe007f"
      joined=T.intercalate "\x200d" (replicate 40 "👩")
  check "a complete cap-sized cluster retains normal presentation"
    (map itemDisplayText (displayItems exact)==[exact] && not (any itemOverflow (displayItems exact)))
  forM_ [excess,oversized,tagged<>"🇦🇧🇨",joined<>"Z"] $ \text->do
    let items=displayItems text
        widths=sum (map itemWidth items)
    check "overflow fragments retain original bytes with bounded visible fallback"
      (T.concat (map itemSourceText items)==text && all (\i->T.length (itemSourceText i)<=32 && TU.lengthWord8 (itemSourceText i)<=128) items &&
       all (\i->not (itemOverflow i) || itemDisplayText i=="�" && itemWidth i==1) items)
    check "source columns and numeric seek agree across overflow fragments"
      (sourceTextWidth text==displayColumn text (T.length text) && sourceTextWidth text==widths &&
       all (\column->let (char,_,col,pending)=sourceGraphemesFrom column text in columnOffset text column==char &&
                       col==displayColumn text char && T.concat (map itemSourceText pending)==T.drop char text) [0..widths])
  let cursorCases=["", "a\tb\r\n界e\x301\tZ", "🇦🇧🇨🇩🇪", "👩🏽\x200d\&💻x", oversized,
        T.replicate 511 "a"<>"界"<>T.replicate 70 "\x301"<>"🇦🇧🇨Z"]
      capture text=go 0 0 initialSourceCursor
        where
          go byte _ _ | byte>=TU.lengthWord8 text=[]
          go byte col cursor=case sourceItemStep text byte cursor of
            (# end,count,advance,tab,overflow,next #)->
              let width=if tab then 8-col `mod` 8 else advance
              in (byte,end,col,count,width,overflow,cursor):go end (col+width) next
      spanReference text start cursor target limit following=go start 0 0 [] False cursor
        where
          size=TU.lengthWord8 text
          go byte chars count advances receipt current
            | byte>=target || chars>=limit || byte>=size || following && size-byte<132=
                (byte,chars,count,reverse advances,receipt,current)
            | otherwise=case sourceItemStep text byte current of
                (# end,n,width,tab,overflow,next #)->go end (chars+n) (count+1) ((width,tab):advances) overflow next
      leafReference goal col=go 0 0 col
        where
          go char byte column []=(char,byte,column,[])
          go char byte column pending@(item:rest)
            | column+sourceItemAdvance column item>max 0 goal=(char,byte,column,pending)
            | otherwise=go (char+itemScalarCount item) (byte+TU.lengthWord8 (itemSourceText item))
                (column+sourceItemAdvance column item) rest
      scalarReference goal col=go 0 col
        where
          go _ column []=column
          go char column (item:rest)
            | char+itemScalarCount item>max 0 goal=column
            | otherwise=go (char+itemScalarCount item) (column+sourceItemAdvance column item) rest
  forM_ cursorCases $ \text->do
    let captured=capture text
        flat=displayItems text
    check "complete-source cursor steps retain byte/count/width/overflow facts"
      (length captured==length flat && and [end-byte==TU.lengthWord8 (itemSourceText item) && count==itemScalarCount item &&
        advance==sourceItemAdvance col item && overflow==itemOverflow item | ((byte,end,col,count,advance,overflow,_),item)<-zip captured flat])
    let starts=(0,initialSourceCursor):[(byte,cursor) | index<-[1,31,511,length captured-1],
          (byte,_,_,_,_,_,cursor):_<-[drop index captured],index>=0]
    forM_ starts $ \(start,cursor)->forM_ [start,start+1,start+128] $ \target->
      forM_ [0,1,32,maxBound] $ \limit->forM_ [False,True] $ \following->do
        let (end,chars,count,advances,overflow,next)=spanReference text start cursor target limit following
            apply col=foldl (\c (width,tab)->if tab then 8*(c `div` 8+1) else c+width) col advances
        check "numeric span receipt matches single-item stepping and absolute tab transforms"
          (case sourceSpanStep text start cursor target limit following of
            (# actualEnd,actualChars,actualCount,prefix,suffix,actualOverflow,actualNext #)->
              (actualEnd,actualChars,actualCount,actualOverflow,actualNext)==(end,chars,count,overflow,next) &&
              all (\col->(if prefix<0 then col+suffix else 8*((col+prefix) `div` 8+1)+suffix)==apply col) [0..15])
    -- Storage groups are cut only at captured item boundaries. Each resumes its
    -- Unicode context and restores the known last overflow flag at artificial EOF.
    forM_ [1,2] $ \groupSize->forM_ [0,groupSize..length flat-1] $ \start->do
      let group=take groupSize (drop start captured)
          expected=take groupSize (drop start flat)
          (byte,_,col,_,_,_,incoming)=case group of item:_->item; []->error "resumed storage group missing"
          (_,end,_,_,_,lastOverflow,_)=last group
          leaf=TU.takeWord8 (end-byte) (TU.dropWord8 byte text)
      check "resumed storage groups preserve exact original items at artificial EOF"
        (sourceItemsFromCursor incoming lastOverflow leaf==expected)
      forM_ [col-1,col,col+1,col+sum [sourceItemAdvance 0 i | i<-expected]+8] $ \goal->
        check "resumed leaf numeric seek shares absolute tab/overflow geometry"
          (sourceLeafFrom goal col incoming lastOverflow leaf==leafReference goal col expected)
      forM_ [0,1,31,32,sum (map itemScalarCount expected)-1,sum (map itemScalarCount expected),sum (map itemScalarCount expected)+1] $ \scalar->
        check "scalar-target numeric seek preserves interior snapping and captured leaf context"
          (sourceScalarColumn scalar col incoming lastOverflow leaf==scalarReference scalar col expected)
  let short="🇦"
      terminal=case sourceItemStep short 0 initialSourceCursor of (# _,_,_,_,_,next #)->next
      appended=short<>"🇧🇨"
  check "real EOF receipt applies the next transition rather than treating it as processed lookahead"
    (case sourceItemStep appended (TU.lengthWord8 short) terminal of (# _,count,_,_,_,_ #)->count==1)
  check "empty source step retains the exact numeric cursor"
    (case sourceItemStep short (TU.lengthWord8 short) terminal of
      (# end,count,advance,tab,overflow,next #)->
        end==TU.lengthWord8 short && count==0 && advance==0 && not tab && not overflow && next==terminal)
  check "overflow ends at natural boundary without poisoning the next cluster"
    (map itemDisplayText (displayItems oversized)==["�","�","�","Z"] &&
     map itemDisplayText (displayItems (tagged<>"🇦🇧🇨"))==["�","�","�","🇦🇧","🇨"])
  let overflowBase=addDocument Nothing (newBuffer oversized) (initialDesktop (80,25))
      copied=fst (runCommand Copy (modifyActive (\w->w {selection=Selection 0 (T.length oversized)}) overflowBase))
      deleted=fst (handleEvent (V.EvKey V.KDel []) overflowBase)
  check "overflow presentation preserves source copy and undo bytes"
    (clipboard copied==oversized && activeText deleted==T.drop 32 oversized && activeText (fst (runCommand Undo deleted))==oversized)
  let emitted=cellRowsForPic (V.picForImage (textImage V.defAttr oversized)) (4,1)
      shown=T.concat [case cell of CellText _ t->t; CellGlyph _ t _ _ _->t; CellScript _ t _ _->t | cell<-toList (emitted Vec.! 0)]
  check "common visible grid sends bounded fallback glyphs instead of original overflow"
    (shown=="���Z")
  let longOverflow="a"<>T.replicate 100000 "\x301"
  _<-evaluate (T.length longOverflow)
  firstBefore<-getAllocationCounter
  firstLength<-evaluate (sum (map (T.length . itemSourceText) (take 1 (displayItems longOverflow))))
  firstAfter<-getAllocationCounter
  check "the first overflow fragment does not prepare its unconsumed tail"
    (firstLength==32 && withinBudget profile (firstBefore-firstAfter) (262144))
  let longPrefix=T.replicate 100000 "é"
  _<-evaluate (T.length longPrefix)
  prefixBefore<-getAllocationCounter
  prefixCount<-evaluate (sum (map T.length (take 3 (graphemes longPrefix))))
  prefixAfter<-getAllocationCounter
  check "a grapheme prefix does not prepare the unconsumed UTF8 tail"
    (prefixCount==3 && withinBudget profile (prefixBefore-prefixAfter) (262144))
  let sourceReference goal text=go 0 0 0 (displayItems text)
        where
          go char byte col []=(char,byte,col,[])
          go char byte col pending@(glyph:rest)=
            let advance=sourceItemAdvance col glyph
            in if col+advance>max 0 goal then (char,byte,col,pending)
               else go (char+T.length (itemSourceText glyph)) (byte+TU.lengthWord8 (itemSourceText glyph)) (col+advance) rest
      palette=["a","é","界","\t","\r","\0","e\x301","─\x301","👩🏽\x200d\&💻","🇦","🇧","\x301"]
      generated seed=T.concat [palette!!(n `mod` length palette) | n<-take 24 (iterate (\n->(n*73+19) `mod` 65521) seed)]
      sourceCases=["", "a\tb", "\rabc", "ab\r\ncd", "a\x301界x", "🇦🇧🇨🇩🇪"]++map generated [1..80]
  forM_ sourceCases $ \text->forM_ [-1..90] $ \column->do
    let actual@(char,byte,_,suffix)=sourceGraphemesFrom column text
    check "numeric source seek preserves the original stateful grapheme suffix"
      (actual==sourceReference column text && T.concat (map itemSourceText suffix)==TU.dropWord8 byte text &&
        T.length (TU.takeWord8 byte text)==char)
  forM_ ["", "a\tb", "\rabc", "a\x301界x", "👩🏽\x200d\&💻界", "🇦🇧", "a\r\n"] $ \text->
    check "cached source extent agrees with ordinary display columns"
      (sourceTextWidth text==displayColumn text (T.length text))
  beforeSeek<-getAllocationCounter
  seekCount<-evaluate (let (char,byte,col,suffix)=sourceGraphemesFrom 50000 longPrefix
                       in char+byte+col+sum (map (T.length . itemSourceText) (take 3 suffix)))
  afterSeek<-getAllocationCounter
  check "numeric source seek does not allocate discarded prefix fragments"
    (seekCount==200003 && withinBudget profile (beforeSeek-afterSeek) (262144))
  forM_ clusters $ \g -> do
    check "platform segments a complete grapheme" (graphemes g==[g])
    check "cursor crosses a complete grapheme" (nextCharacter (g<>"x") 0==T.length g && previousCharacter ("x"<>g) (1+T.length g)==1)
    let base=addDocument Nothing (newBuffer (g<>"x")) (initialDesktop (80,25))
        deletedCluster=fst (handleEvent (V.EvKey V.KDel []) base)
    check "paste preserves complete emoji sequences" (activeText (fst (handleEvent (V.EvPaste (TE.encodeUtf8 g)) (addDocument Nothing (newBuffer "") (initialDesktop (80,25)))))==g)
    check "Delete removes one cluster, undo restores bytes" (activeText deletedCluster=="x" && activeText (fst (runCommand Undo deletedCluster))==g<>"x")
  forM_ wide $ \g -> check "emoji occupies two cells with no internal mouse offsets"
    (displayColumn (g<>"x") (T.length g+1)==3 && columnOffset (g<>"x") 1==0 && columnOffset (g<>"x") 2==T.length g)
  check "combining and tabs use grapheme columns" (displayColumn "e\x301\t🇯🇵" 6==10)
  let plain pic size=T.concat [TL.toStrict t | row<-toList (displayOpsForPic (flattenPicture size pic) size),TextSpan{textSpanText=t}<-toList row]
      image=textImage V.defAttr "A👩🏽\x200d\&💻B"
      crop n=plain (V.picForImage (V.cropRight n image)) (n,1)
  check "wide clusters are blanked at the right edge" (crop 2=="A " && crop 3=="A👩🏽\x200d\&💻")
  check "wide clusters are blanked at the left edge" (plain (V.picForImage (V.translateX (-2) image)) (2,1)==" B")
  check "a window covering half a glyph blanks the exposed half"
    (plain (V.picForLayers [V.translateX 2 (textImage V.defAttr "│"),image]) (4,1)=="A │B")
  let semantic pic size=[(text,full,start,visible) | row<-toList (cellRowsForPic pic size),CellGlyph _ text full start visible<-toList row]
  check "shared GPU rows keep right-clipped semantic glyph and width"
    (semantic (V.picForImage (V.cropRight 2 image)) (2,1)==[("👩🏽\x200d\&💻",2,0,1)])
  check "shared GPU rows keep left-clipped original glyph origin"
    (semantic (V.picForImage (V.translateX (-2) image)) (2,1)==[("👩🏽\x200d\&💻",2,1,1)])
  check "opaque occlusion preserves only the visible semantic half"
    (semantic (V.picForLayers [V.translateX 2 (textImage V.defAttr "│"),image]) (4,1)==[("👩🏽\x200d\&💻",2,0,1)])
  let halo=cellRowsForLayers [CellHalo (V.defAttr `V.withBackColor` V.black) [(2,0,1,1)],CellImage image] (4,1)
      masked=cellRowsForLayers [CellMask V.defAttr [(2,0,1)],CellImage image] (4,1)
  check "style-only halo preserves semantic glyph identity and clipping"
    ([(text,full,start,visible) | row<-toList halo,CellGlyph _ text full start visible<-toList row]==[("👩🏽\x200d\&💻",2,0,1),("👩🏽\x200d\&💻",2,1,1)])
  check "privacy masks both semantic glyph halves before export"
    (null [text | row<-toList masked,CellGlyph _ text _ _ _<-toList row] &&
     [text | row<-toList masked,CellText _ text<-toList row]==["A**B"])
  let hiddenHalf=cellRowsForLayers [CellMask V.defAttr [(1,0,1)],CellImage (V.translateX 2 (textImage V.defAttr "X")),CellImage image] (4,1)
  check "whole-glyph privacy preserves an unrelated opaque foreground cell"
    ([text | row<-toList hiddenHalf,CellText _ text<-toList row]==["A*XB"])
  let scriptRow=CellRow 1 0 0 4 (Vec.fromList [CellScript V.defAttr "界" 2 Superscript,CellText V.defAttr "X"])
      scriptedRows=cellRowsForLayers [scriptRow] (4,1)
  check "script cell retains whole semantic text and natural width while advancing one"
    (Vec.toList (scriptedRows Vec.! 0)==[CellText V.defAttr " ",CellScript V.defAttr "界" 2 Superscript,CellText V.defAttr "X "])
  check "script terminal projection uses one-cell wide placeholder without moving the sentinel"
    ([TL.toStrict text | TextSpan _ _ _ text<-Vec.toList (cellDisplayOps scriptedRows Vec.! 0)]==[" ","\xfffd","X "])
  let clippedScript=cellRowsForLayers [CellRow 0 0 1 3 (Vec.fromList [CellScript V.defAttr "界" 2 Subscript,CellText V.defAttr "X"])] (3,1)
  check "whole script cell is suppressed outside the positioned row clip"
    (Vec.toList (clippedScript Vec.! 0)==[CellText V.defAttr " X "])
  check "an empty prepared row clip never forces hidden glyph payloads"
    (cellRowsForLayers [CellRow 0 0 3 3 (Vec.singleton (error "empty row clip forced glyph"))] (4,1)==cellRowsForLayers [] (4,1))
  let scriptMask=cellRowsForLayers [CellMask V.defAttr [(1,0,1)],scriptRow] (4,1)
  check "privacy masks the whole scripted source grapheme before export"
    (Vec.toList (scriptMask Vec.! 0)==[CellText V.defAttr " *X "])
  let paint=V.defAttr `V.withBackColor` V.black
      scriptHalo=cellRowsForLayers [CellHalo paint [(1,0,1,1)],scriptRow] (4,1)
  check "halo preserves script and natural width metadata"
    ([ (text,natural,script) | CellScript _ text natural script<-Vec.toList (scriptHalo Vec.! 0)]==[("界",2,Superscript)])
  let border=textImage V.defAttr ("║"<>T.replicate 178 " "<>"║")
      prepared=V.vertCat (replicate 55 border)
  _<-evaluate (V.imageWidth prepared+V.imageHeight prepared)
  before<-getAllocationCounter
  let packed=cellRowsForPic (V.picForImage prepared) (180,55)
  occupied<-evaluate (Vec.foldl' (\n row->Vec.foldl' (\m cell->m+case cell of
    CellText _ text->T.length text
    CellGlyph _ _ _ _ visible->visible
    CellScript{}->error "unexpected script cell in border fixture") n row) 0 packed)
  after<-getAllocationCounter
  check "prepared border and padding grid stays within 1.5 MB allocation"
    (occupied==9900 && withinBudget profile (before-after) (1500000))
  let mixed=textImage V.defAttr "Aéδ░│𝄞Z"
  check "compact single-cell Unicode runs preserve UTF8 bytes"
    ([text | row<-toList (cellRowsForPic (V.picForImage mixed) (7,1)),CellText _ text<-toList row]==["Aéδ░│𝄞Z"])
  check "combining box drawing stays a complete semantic grapheme"
    (semantic (V.picForImage (textImage V.defAttr "─\x301\&x")) (2,1)==[("─\x301",1,0,1)])
  let settings=fst (runCommand EditorOptions (initialDesktop (80,25)) {videoMode=Just 3})
      checked=settings {dialog=fmap (\d -> d {fields=[CheckBox "Pixelate Unicode" True]}) (dialog settings)}
  check "preferences apply Unicode pixelation" (pixelateUnicode (fst (handleEvent (V.EvKey V.KEnter []) checked)))
  let position x=writeByteString (BS.pack ("<"++show x++">"))
      (output,end)=terminalText position 3 "A🇯🇵B"
  check "terminal reserves two cells and corrects its cursor after wide glyphs"
    (writeToByteString output==TE.encodeUtf8 "A  <4>🇯🇵<6>B" && end==7)
  forM_ ["\xf024b","\xf0770"] $ \icon -> do
    let (drawn,next)=terminalText position 3 (icon<>" x")
    check "Material icons reserve two columns with terminal cursor correction"
      (clusterWidth icon==2 && next==7 && writeToByteString drawn==TE.encodeUtf8 ("  <3>"<>icon<>"<5> x"))
  forM_ ["⌘","⌥"] $ \symbol -> do
    let (drawn,next)=terminalText position 3 (symbol<>"X")
    check "Mac modifiers reserve two cells and reposition terminal text"
      (V.imageWidth (textImage V.defAttr (symbol<>"X"))==3 && next==6 &&
       writeToByteString drawn==TE.encodeUtf8 ("  <3>"<>symbol<>"<5>X"))
  let macTerminal=(initialDesktop (100,25)) {macKeySymbols=True}
      mac=macTerminal {nativeMac=True,videoMode=Just 3}
      save=MenuItem "Save" "Ctrl+S" Save
      (enabled,effects)=handleEvent (V.EvKey V.KEnter [])
        ((fst (runCommand EditorOptions (initialDesktop (80,25))))
          {dialog=fmap (\dg -> dg {fields=[CheckBox "Mac key symbols" True]}) (dialog (fst (runCommand EditorOptions (initialDesktop (80,25)))))})
  check "symbol labels distinguish native Command from terminal Control"
    (menuShortcut macTerminal save=="⌃S" && menuShortcut mac save=="⌘S")
  check "text preferences change labels and request persistence"
    (macKeySymbols enabled && effects==[SaveMacKeySymbols True])
  let hits=statusItemRects macTerminal
      expected=scanl (+) 0 (map (keyLabelWidth . fst) (statusHints macTerminal))
  check "status hit targets follow two-cell modifier labels"
    (and [left rect==expected!!index | (rect,index,_)<-hits,index<length (statusHints macTerminal)])
  putStrLn "Unicode grapheme/layout checks passed"
