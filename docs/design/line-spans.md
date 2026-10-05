# Regular spans for long lines

Status: in progress, 5 October 2026. Tracked in [issue #116](https://github.com/ekmett/hide/issues/116).
Loaded long rows retain their original text and share a lazy stream of regular
span receipts. Exact queries prepare only the prefix they visit. The first edit
promotes that row to a measured tree for persistent local repair. Exact width
remains a separate memoized full-row calculation. Files sidebar, diagnostic-menu
opening, debugger source following and recovery prepare those numeric receipts
before UI adoption; other opening routes can still demand them on first paint.

We want cheap horizontal seeks and small edits in long lines. Dice the text into
borrowed spans at roughly **128-byte intervals**. Keep old spans after an edit;
merge short neighbors and split ones that grow too long. The cuts do not need to
be canonical. There is no reason to compute a rolling hash to choose them.

The outer finger tree still owns lines, provenance and Undo. An inner span tree
must help the operations that actually need it: drawing a horizontal viewport,
hit testing, selection and navigation. Flattening it before those operations
would undo the work.

## Cuts

A nominal mark is a byte position. Advance past UTF8 continuation bytes to a
scalar start, at most three bytes for valid UTF8. If that position is inside a
display item, advance to its end. Keep a mark already at an item boundary where
it is; discard duplicate marks. Byte positions, scalar offsets and display
columns are different coordinates.

Use the shared bounded Unicode cursor. Its display items contain at most 32
Unicode scalars / 128 UTF8 bytes. An oversized natural grapheme is displayed as
one-cell replacement fragments with exact source ranges; copying and saving
preserve every original byte. This bounds boundary adjustment and shaping input.
It does not make every Unicode boundary independently recognizable: regional
indicators and ZWJ sequences need preceding segmentation state. Candidate byte
marks are independent; their final Unicode boundaries must agree with the
stateful source iterator. A storage boundary must not pretend to be a new line.

Choose small byte budgets around the 128-byte target for local splitting and
merging, with room for one bounded item at a cut. Do not retain the old rolling
window, scalar hash table, cut predicate or canonical-cut repair machinery.
Existing content fingerprints used by buffer provenance are separate; they do
not choose span boundaries or prove that source bytes are equal.

## Borrowed storage and edits

Keep ordinary short lines as compact `Text`. A loaded long line keeps its original
`Text`, cached scalar/encoding/provenance metadata and a shared lazy stream of
borrowed spans. Loading and whole-text export do not force that stream. Saving
encodes raw stored pieces directly: the original loaded text or an edited row's
borrowed leaves, without scalar slicing, display indexing or an intermediate
whole-line `Text`. The saved disk baseline remains a strict byte string, prepared
before atomic replacement. A span
stores no absolute source position: prefix measures supply it, so an insertion
does not renumber the suffix. New typed text supplies
new backing storage. Adjacent slices of the same array can be joined without a
copy; crossing arrays requires copying only the small repaired region.

The first edit of a loaded long row forces its remaining span receipts and
promotes the row to a measured finger tree. This preparation is linear in that
row; its span payloads remain shared with the previous immutable line in Undo.
Later local edits locate the affected spans, splice the new text, and repair the
join and adjacent short or oversized spans. Reuse the untouched suffix. Editing
one character must not make us recut an otherwise unchanged periodic line.
Chunk boundaries may depend on edit history; bytes, coordinates and rendering
must not.

Unicode context still matters at a join. Reuse a suffix only when its captured
segmentation state is valid there, and use the immutable edit splice to establish
that the bytes are unchanged. A run of regional indicators can require a longer
scan after an edit changes pairing. Keep that rare cost explicit; a small span
bound is not a proof of constant-time Unicode repair. The current pure edit
owner performs that fallback, so pathological pairing changes can still delay
an edit.

Provenance stays line-owned. Original, inserted and deleted lines keep their
existing ordering and saved baseline. Splitting an internal span does not create
another changed line. Newline/CRLF, the final empty editor row, byte mode and
Undo/Redo retain their current contracts. Same-row edits repair local spans;
multiline splices retain the existing full-physical-line fallback. Explicit
whole-text exports remain
available to file output, HLS and highlighting on their owning workers.

## Prepare only what a seek needs

Horizontal scrolling is uncommon. Do not eagerly dice every loaded long line
into a complete display index. Its shared lazy span stream extends through the
requested source position or viewport, with bounded item lookahead, then stops.
A first distant seek inspects the uncached prefix synchronously. Later seeks walk
small cached receipts in O(reached spans), decoding only the selected bounded
span and visible successors. There is no second complete tree or presentation
cache, and no user input is discarded while preparation runs.

The stream belongs to the immutable source line. Forcing its thunks changes no
content identity, revision, dirty state or Undo. Only a real edit promotes it to
the existing persistent measured tree. Scalar metadata and explicit whole-text
reads remain independent of display preparation.

LSP UTF-16 position lookup borrows only the requested scalar prefix of its
measured source row, preserving interior CR and clamping at its actual
terminator. Small columns do not demand a whole-row projection. Ordinary short
rows keep their
compact representation and existing query path.

Exact total width is an exception: loaded rows memoize a numeric full-row scan
independently of span receipts. The Files sidebar, diagnostic-menu opening,
debugger source following and recovery force cached long-row widths on their
preparation owner before adopting
source windows. They leave short-row widths, source bytes and lazy span receipts
alone. Raw buffer construction stays lazy; other opening routes, including the
synchronous ReadPath route, can still demand total width on first paint. Repeated
width requests reuse the numeric result. The scrollbar retains its existing
visible-row maximum and exact proportional extent. Bounded draft sizing stops
at its requested cap instead of demanding total width.

Hover rejects EOF using the reached source scalar offset and cached row length.
Viewport queries normalize consumed line terminators to the editor's EOF without
asking for total width. Tabs, partial wide glyphs, scalar positions inside an
item and artificial versus real EOF retain the shared Unicode policy. Prepared
Markdown and other transformed layouts keep their own geometry.

## Display advance

Tabs prevent widths from being simple sums. For source text, store a column
transform:

```
next8(x) = 8 * (floor(x / 8) + 1)
Add w:   c -> c + w
Tab p s: c -> next8(c + p) + s
```

Here `p` is the advance before the first tab and `s` is the remaining advance
starting at that tab stop. Concatenation applies the left transform, then the
right:

```
Add a   <> Add b   = Add (a + b)
Add a   <> Tab p s = Tab (a + p) s
Tab p s <> Add b   = Tab p (s + b)
Tab p s <> Tab q t = Tab p (next8(s + q) + t)
identity           = Add 0
```

The operation is associative because it composes these transforms. A measured
prefix gives the absolute display column; only the selected span and visible
successors need decoding. Source styles remain scalar ranges. Fused text runs
stop at their backing span and style boundary, rather than demanding a flattened
line to borrow from.

## Checks

- Concatenating span bytes reproduces the original input exactly. Scalar and
  display queries agree with the flat source model, including positions inside
  a glyph, tabs, zero-width items, CRLF and overflow fragments.
- Local edits preserve untouched spans and Undo sharing. Check middle rows with
  following rows as well as a single long row; the existing splice contract
  includes the final empty row when appropriate.
- Splitting or rebalancing preserves captured overflow at artificial leaf EOF.
  Test caps, combining marks, regional indicators and ZWJ sequences at cuts.
- A left-edge viewport does not prepare a distant suffix. Extending the viewport
  reuses its prefix; stale source revisions cannot adopt a result. Cursor-only
  work does not rebuild the index.
- Verify the actual paint, cursor, hit-test, hover and scrollbar routes. Keep
  remaining review/draft/dialog conversions explicit rather than claiming every
  view is indexed.

Keep performance evidence private: allocations and retained Undo memory, bytes
inspected on first and repeated seeks, local repair, and demand-to-draw time.
Compare against the existing flat-line path. Use the existing checks and build
machinery; no new benchmark framework is needed.
