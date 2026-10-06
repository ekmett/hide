# Regular spans for long lines

Status: in progress, 5 October 2026. Tracked in [issue #116](https://github.com/ekmett/hide/issues/116).
Loaded long rows retain their original text and share lazy measured blocks of
regular span receipts. Queries extend the cached prefix as needed. Edits retain
unchanged ranges of those immutable owners and repair the joins. Exact width is
an explicit query; ordinary Current source scrollbars estimate unvisited suffixes
on both loaded and edited rows.

We want cheap horizontal seeks and small edits in long lines. Dice the text into
borrowed spans at roughly **128-byte intervals**. Keep old spans after an edit;
merge short neighbors and split ones that grow too long. The cuts do not need to
be canonical. There is no reason to compute a rolling hash to choose them.

The outer finger tree owns lines, provenance and Undo. An edited long row has
an inner tree of source ranges. Its raw measures locate bytes and scalars without
preparing display receipts. Drawing, hit testing and navigation demand only the
necessary display prefix and visible spans.

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
before atomic replacement.

An edited piece stores a range in an immutable source owner. Those coordinates
belong to the original backing text; an insertion does not renumber the suffix.
The piece tree measures bytes, scalars and content fingerprints strictly, while
its complete display advance stays lazy. Each piece references an owner directly,
never a previous edited row or a pending recipe for repairing it.

The first edit retains the original owner, locates the affected receipts and
repairs the join. It does not convert the untouched suffix into another tree.
New typed text supplies new backing storage. Repaired spans retain the actual
incoming Unicode cursor and outgoing overflow receipt. Adjacent slices of the
same array can be joined without copying; crossing arrays copies only the small
repaired region. Cuts may depend on edit history; bytes, coordinates and
rendering must not.

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
into a complete display index. Group its receipts into disjoint vector
checkpoints containing 1, 2, 4, 8, ... spans. The next block stays lazy. A first distant
seek prepares fewer than twice the number of receipts needed to reach it, plus
bounded item lookahead. A left-edge viewport starts with one span.

Later seeks skip whole blocks by their cached scalar counts or column transforms,
then binary-search cached prefix coordinates within one block. This takes logarithmic index work in the reached
span count; only the selected bounded span and visible successors need decoding.
Each receipt belongs to one block, with no duplicate retained receipt list or
second complete presentation tree. The first uncached seek still runs
synchronously; grouping does not move that work onto a background worker.

The receipt index belongs to its immutable source owner. Forcing its thunks
changes no content identity, revision, dirty state or Undo. Editing preserves
that owner for unchanged ranges. Scalar metadata and explicit whole-text reads
remain independent of display preparation.

For an edited row, a scalar seek splits the raw piece tree and measures only the
selected owner's prefix. Complete preceding pieces contribute their cached
column transforms. A cell seek brackets the requested column with scalar probes,
then searches for its complete display-item boundary. This composes piece-tree
and owner-index lookups; it is not the single Loaded checkpoint search described
above. A first distant query can still prepare the intervening source prefix.

LSP UTF-16 position lookup borrows only the requested scalar prefix of its
measured source row, preserving interior CR and clamping at its actual
terminator. Small columns do not demand a whole-row projection. Ordinary short
rows keep their
compact representation and existing query path.

Ordinary Current source scrollbars use the indexed prefix plus a conservative
byte-derived estimate for the unvisited suffix. Arrow/page scrolling and drag
seeks query the real prospective viewport; reaching EOF immediately clamps to
its actual extent. An end drag can demand the whole row. Edited rows use the
same estimate-until-needed rule. Tabs, Unicode and CR/LF use the same source
coordinate rules as rendering and hit testing.

A small per-window thumb hint retains a discovered extent when moving left. It
is not source identity or proof of EOF: each actual scroll queries current source
independently, so even an equal-revision replacement cannot clamp navigation to
an old width. The thumb can briefly show an old estimate until the next seek
refines it. Review and prepared Markdown geometry remain unchanged. File opening,
recovery, debugger source preparation and interactive highlighting no longer
prewarm total widths just for source scrollbars. Explicit exact width queries
still demand the whole row: loaded rows use their independent numeric scan,
while edited rows compose their owners' range advances. No hidden background
scan is scheduled to stabilize the thumb.

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

The operation is associative because it composes these transforms. Owner ranges
use cached endpoint columns and the first tab-containing receipt to derive their
transform; cumulative tab transforms cannot simply be subtracted. Raw range
fingerprints use the corresponding scalar prefix hashes. Neither query changes
the owner or requires a second edited display index.

A measured prefix gives the absolute display column; only the selected span and
visible successors need decoding. Source styles remain scalar ranges. Fused text runs
stop at their backing span and style boundary, rather than demanding a flattened
line to borrow from.

## Checks

- Concatenating span bytes reproduces the original input exactly. Scalar and
  display queries agree with the flat source model, including positions inside
  a glyph, tabs, zero-width items, CRLF and overflow fragments.
- Local edits preserve untouched ranges and Undo sharing. Check the first edit
  through actual paint, hit testing and scrolling, then separated edits and
  batches made without painting. Check middle rows with following rows as well
  as a single long row; the splice contract includes the final empty row when
  appropriate.
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
