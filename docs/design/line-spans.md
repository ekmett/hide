# Regular spans for long lines

Status: in progress, 5 October 2026. Tracked in [issue #116](https://github.com/ekmett/hide/issues/116).
The shared bounded Unicode cursor is implemented; regular span storage and demand-driven indexing are the remaining work.

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

Keep ordinary short lines as compact `Text`. Long lines can share spans of
immutable UTF8 arrays. A span stores no absolute source position: prefix measures
supply it, so an insertion does not renumber the suffix. New typed text supplies
new backing storage. Adjacent slices of the same array can be joined without a
copy; crossing arrays requires copying only the small repaired region.

A local edit locates the affected spans, splices the new text, and repairs the
join and adjacent short or oversized spans. Reuse the untouched suffix. Editing
one character must not make us recut an otherwise unchanged periodic line.
Chunk boundaries may depend on edit history; bytes, coordinates and rendering
must not.

Unicode context still matters at a join. Reuse a suffix only when its captured
segmentation state is valid there, and use the immutable edit splice to establish
that the bytes are unchanged. A run of regional indicators can require a longer
scan after an edit changes pairing. Keep that rare cost explicit; a small span
bound is not a proof of constant-time Unicode repair.

Provenance stays line-owned. Original, inserted and deleted lines keep their
existing ordering and saved baseline. Splitting an internal span does not create
another changed line. Newline/CRLF, the final empty editor row, byte mode and
Undo/Redo retain their current contracts. Explicit whole-text exports remain
available to file output, HLS and highlighting on their owning workers.

## Prepare only what a seek needs

Horizontal scrolling is uncommon. Do not eagerly build a complete display index
for every line just because it was loaded. Keep an indexed prefix and a borrowed
unindexed tail with its Unicode checkpoint. Extend through the requested source
position or the right edge of the viewport, then stop. A first distant seek has
to inspect the uncached prefix; later seeks should reuse that work.

A strict width-measured finger tree built in a thunk is still eager when its root
measure is demanded. Only the prepared prefix belongs in that measured index.
Byte/scalar storage and its editing metadata must not force display preparation
as a side effect.

Use the existing presentation worker and small source/version keys to prepare
and adopt derived indexes. Retain useful prefix work when a viewport moves;
changing the request does not invalidate the unchanged source. Retain the latest
prefix, not a history of all intermediate prefixes. Derived index adoption must
not dirty the file, add Undo entries or compare a whole desktop.

The exact first uncached hit or navigation query may use the existing numeric
source cursor while preparation catches up. Do not guess coordinates or claim
that a pure query cached its progress. Its first-use cost must be measured.
Avoid an extra full-prefix pass when the reached state can be retained through
the existing owner.

Audit callers as well as the index. Hover can use the reached source offset and
cached row length to reject EOF; it does not need the full display width. A
scrollbar uses a known worker-computed extent or an explicit pending extent.
Neither should force every unfinished line index through EOF. Prepared Markdown
and other transformed layouts keep their own geometry.

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
