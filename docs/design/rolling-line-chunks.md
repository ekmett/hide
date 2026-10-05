# Rolling-hash chunks for horizontal scrolling

Status: proposed, 5 October 2026. Tracked in [issue #116](https://github.com/ekmett/hide/issues/116). This describes the next long-line representation;
it is not a claim that the chunk tree or the benchmarks are implemented.

We should be able to scroll to the middle of a megabyte-long line without walking
its entire prefix. Editing one character should likewise avoid copying and
remeasuring that entire line. Keep the outer finger tree of lines, but represent
long lines with persistent, measured chunks whose boundaries are chosen from
nearby contents. Start with **32–512 display items per chunk, targeting roughly
128**, and force a cut at the maximum when the rolling predicate never matches.

## What this replaces

`Hide.Buffer` currently stores each newline-inclusive line as one `Text`, with
cached character count, provenance and fingerprint. `editTree` joins the edited
boundary lines and reconstructs their measures. `Hide.Unicode.sourceGraphemesFrom`
seeks a display column by scanning UTF8 from the start of the line. This avoids
allocating discarded glyphs, but a distant horizontal position still costs work
proportional to the prefix. `Hide.Syntax.plainSourceRow` now avoids an unnecessary
whole-line character count; it does not solve this prefix scan or long-line edits.

The new tree must serve rendering, hit testing, selection and cursor navigation.
Adding an index only to a renderer leaves the same scan in the other paths.
Flattening chunks back into a line before each operation would lose the benefit.
Explicit whole-line reads remain available to highlighting, HLS, copying and
file output on their owning workers.

## Boundary rule

A display item here is one source grapheme under the editor's Unicode policy,
with the 32-scalar rendering bound described below. A tab is one item even though it occupies several cells; UTF8 bytes,
Unicode scalar offsets, grapheme counts and display columns remain distinct.
Chunk boundaries must not divide a UTF8 codepoint or a bounded display item.
Only the documented overflow policy may fragment an oversized grapheme.

Maintain a rolling hash over the preceding 32 Unicode scalars. Advance it while
scanning the original UTF8, retaining a small ring of outgoing scalars rather
than unpacking the line into a list. Test candidates only at complete grapheme
boundaries. Do not restart the hash at every chunk: the window describes nearby
content, independently of the previous cut.

At each eligible boundary, in this order:

1. At 512 items since the last cut, cut unconditionally.
2. Before 32 items, keep scanning.
3. Otherwise cut if the rolling hash matches the chosen predicate.
4. At the line end, emit the remaining tail even when it has fewer than 32 items.

Thus ordinary chunks have 32–512 items; the final chunk can be shorter. A short
line needs no internal tree. Neither a missing hash match nor repetitive source
can produce a chunk with thousands of independent glyphs.

Begin with a 7-bit predicate as the cheap nominal one-in-128 candidate. It does
**not** imply a measured average of 128 after imposing a minimum. Even independent
uniform candidates shift that average upward; grapheme eligibility and source
patterns shift it further. Measure the distribution. Compare an 8-bit predicate
and, if useful, a mixed-hash threshold calibrated nearer the target. The target is
an engineering choice, not an invariant or a user-facing knob.

Adler32 is a candidate, not a requirement. Masking only its low bits largely tests
the window sum, which can correlate badly with source and repeated characters.
Compare a cheaply mixed rolling checksum with a table-based rolling hash such as
buzhash. Check the actual window/removal formula against recomputation; a fast
incorrect rolling update defeats local repair. This hash chooses boundaries only.
It is neither a cryptographic check nor evidence that two texts are equal.

[Data.Hash.Rolling in ekmett/hash](https://github.com/ekmett/hash/blob/master/src/Data/Hash/Rolling.hs)
is the reference for content-dependent cuts with minimum/maximum sizes. Reuse the
idea, not its lazy ByteString/list traversal, concatenations or larger chunk
parameters. Our chunks borrow UTF8 text and need editor-coordinate measures.

## Persistent line representation

Keep provenance at the existing outer line: an original, inserted or tombstoned
line contains a line body. Chunking a line must not turn one changed line into
several changes or disturb the ordering of deleted and inserted lines.

Use a compact `Text` leaf for short lines and a measured finger tree for long
ones. Each chunk borrows a UTF8 slice from an existing immutable backing array;
newly typed content supplies new backing storage. The tree caches:

- UTF8 byte length and Unicode scalar count for transport and buffer offsets;
- grapheme/item count for chunk bounds;
- source display advance, including tabs;
- composable content fingerprint and factor, and flags needed by the outer line.

The outer line takes its existing measures from the inner root. Newline/CRLF and
the final empty editor row retain their existing contracts. Byte/hex buffers keep
their byte-oriented coordinate policy; do not run binary data through grapheme
segmentation as an incidental consequence of this change.

Do not store absolute byte or display positions on every chunk. An insertion
would invalidate every suffix position. Prefix measures provide those positions
when a tree search needs them. Undo and baseline trees share untouched chunks.
Text projections stay lazy and explicit; do not retain a freshly flattened line
alongside each edited chunk tree. Borrowed slices can retain large old arrays,
so measure retained memory with Undo as well as allocation during edits.

## Display advance is a monoid, not a width integer

Plain source glyphs have the existing width policy, but a tab advances to the
next eight-cell stop. A width measured at column zero cannot simply be added to
a preceding chunk's width.

Store a compact column-transform summary. For a chunk without tabs, `Add w`
means `f(c) = c + w`. For a chunk with tabs, `Tab p s` means:

```
next8(x) = 8 * (floor(x / 8) + 1)
Tab p s: f(c) = next8(c + p) + s
```

`p` is the width before the first tab. `s` is the advance after that tab starting
at an aligned column, including any later tabs. The first tab erases the incoming
column residue, so two integers suffice; we need not put eight widths in every
finger-tree measure.

Composition follows concatenation, with `a <> b` applying `a` then `b`:

```
Add a   <> Add b   = Add (a + b)
Add a   <> Tab p s = Tab (a + p) s
Tab p s <> Add b   = Tab p (s + b)
Tab p s <> Tab q t = Tab p (next8(s + q) + t)
identity           = Add 0
```

This is associative because it composes the same column transforms. A prefix's
advance applied to zero gives its absolute column. Search for the first chunk
whose cumulative ending column exceeds the requested column; then inspect only
that chunk and visible successors. Preserve the existing zero-width and clipped
wide-glyph behavior, including which source offsets a click selects.

These measures describe **source** display, under a specified width/tab policy.
Markdown headings, superscript/subscript and other styled layouts have their own
prepared presentation measures. Do not apply source widths to transformed text.
A width-policy change invalidates the appropriate prepared measures through a
small explicit policy/version key, never a whole-desktop comparison.

## Local edit and suffix reuse

1. Locate the edited scalar range through the outer and inner measured trees.
2. Preserve the untouched prefix. Restart from a preceding reusable cut with
   enough preceding scalar context to reconstruct the rolling window and with a
   valid Unicode segmentation checkpoint.
3. Splice borrowed prefix/suffix fragments around the inserted text. Recompute
   chunks only through the affected region and forward until a safe old suffix
   can be reused.
4. Rejoin the rebuilt middle with the old suffix using finger-tree operations.
   Update the outer line's measures and existing provenance as one normal edit.

An unchanged rolling window has the same candidate hash after a local insertion
or deletion. That is why content-dependent cuts usually resynchronize nearby.
However, the distance since the last accepted cut affects minimum/maximum
eligibility. A minimum-suppressed candidate or a forced maximum cut can move
several following boundaries. Repeated text can keep forced cuts out of phase
far beyond the edited chunk. Do not promise that every edit touches one or two
chunks, or confuse the 512-item chunk bound with a bound on repair work.

Reuse is safe at an unchanged suffix boundary once the rolling-window context,
segmentation context and accepted-cut state agree. Establish unchanged suffixes
from the immutable edit splice and shared source identity; a hash collision must
never substitute for this proof.

There are two possible repair contracts to compare before choosing:

- **Canonical chunking:** incremental repair produces exactly the same boundaries
  as rebuilding the whole line. This is simple to specify, but forced cuts in
  repetitive text can require a long suffix repair.
- **Bounded local repair:** retain old suffix cuts after repairing the join and
  enough neighbors to satisfy size and segmentation invariants. The representation
  may depend on edit history, while text, coordinates and rendering do not. This
  avoids mandatory recutting of an enormous unchanged periodic suffix.

Prefer bounded local repair if canonical rebuilding proves costly: content-defined
cuts are a storage optimization, not part of the file format. Its exact join rule
must be stated and checked before implementation. It may not retain an invalid
grapheme boundary merely because the underlying suffix bytes are unchanged.

## Unicode and worst cases

A grapheme boundary is contextual. Regional-indicator pairing, combining marks
and ZWJ sequences can change segmentation after an edit. Resume the existing
utf8proc iterator with its state; do not segment each chunk independently as if
it began a new line. Reuse requires compatible segmentation state, not just an
unchanged 32-scalar hash window.

### A 32-scalar display-item limit

Set the maximum to **32 Unicode scalars / 128 UTF8 bytes per display item**.
This is our bounded display policy, not a claim that Unicode graphemes have a
universal maximum. [UAX #29](https://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundary_Rules)
allows arbitrarily long extending-character runs. In contrast,
[UTS #51 Annex C](https://www.unicode.org/reports/tr51/#valid-emoji-tag-sequences)
limits an entire valid emoji tag sequence, including its base and terminator,
to 32 code points.

Counting the published Unicode 17
[emoji sequences](https://www.unicode.org/Public/17.0.0/emoji/emoji-sequences.txt)
and [ZWJ sequences](https://www.unicode.org/Public/17.0.0/emoji/emoji-zwj-sequences.txt)
gives maxima of 7 scalars / 28 bytes and 10 scalars / 35 bytes respectively.
The longest ZWJ cases include two skin tones, joiners, a heart variation selector
and the kiss symbol. The 32-scalar bound preserves those complete sequences and
the tag-sequence format limit with room for future standard emoji. Recheck the
published sequence set when upgrading Unicode; do not silently raise the cap.

When an extended grapheme would exceed the cap, partition it into source-preserving
fragments of at most 32 scalars. Mark the fragments as overflow presentation and
render each as a one-cell visible placeholder, consistently on TUI, native and
browser frontends. Never feed an unbounded cluster to a font shaper or atlas key,
insert replacement bytes into the buffer, or discard the remaining marks. Each
placeholder maps to its exact original scalar/byte range for selection and copy.
Caret navigation can stop at these explicit fallback fragment boundaries.
Copy, save, HLS and external buffer reads retain the original text.

The iterator may inspect one additional scalar to decide whether the 32-scalar
item ends naturally. Keep Unicode boundary state across overflow fragments, plus
an overflow flag until the next natural boundary; do not falsely declare that
Unicode restarted at a storage cut. This prevents the cap from changing subsequent
regional-indicator/ZWJ behavior. Seeking and visible iteration must share this
policy and its coordinates, including privacy/capture masks and clipping.

This bounds a 512-item chunk at 16,384 scalars / 65,536 UTF8 payload bytes
(excluding a separately owned line terminator and constant lookahead). Ordinary
ASCII chunks remain much smaller. Also test a cluster ending at exactly 32
scalars, overflow at 33, long combining and ZWJ runs, tags and complete longest
standard emoji. No character is lost, and no renderer scans an entire oversized
cluster merely to decide what its first cell should show.

## Benchmark and choice

Keep performance results private. Use the existing optimized Haskell checks and
benchmark infrastructure, with exact revisions, parameters and commands. Do not
build another benchmark framework or infer interactive wins from hash throughput.

Compare current whole-line storage, fixed-size chunks, and rolling chunks with:

| Parameter | Starting point | Comparison |
| --- | --- | --- |
| Rolling window | 32 scalars | Hold fixed initially |
| Minimum | 32 items | 64 |
| Candidate probability | Nominal 1/128 | 1/256; tuned threshold if needed |
| Maximum | 512 items | 1,024 only to quantify the tradeoff |
| Repair | Reuse valid suffix | Canonical rebuild as a control |

Use real source lines, minified JSON, multi-megabyte single lines, tabs, mixed
scripts/emoji, repeated characters and periodic strings. Include deliberately
oversized graphemes. Exercise insertion, deletion and replacement near the start,
middle, cut boundaries and EOF, plus newline splits/joins and repeated Undo.

Record:

- chunk distribution, maximum, metadata per source byte and forced-cut frequency;
- initial construction, retained memory and heap allocation, including Undo roots;
- edited bytes/items rescanned, chunks replaced and suffix resynchronization;
- horizontal seeks near both ends, selection/hit testing, and visible glyph output;
- actual input-to-frame latency and allocations while scrolling or editing, with
  matching contents, viewport and frontend. Keep these separate from microbenchmarks.

Choose the smallest representation that gives cheap distant seeks and local edits
without penalizing ordinary short lines. The initial preference is 32–512 with a
roughly 128-item target; this is not yet a measured winner. Apply the project's
per-frame allocation budget; do not reset its baseline to hide new metadata costs.

## Laws and integration

Before routing production editing/rendering through this representation, check:

- concatenated chunk bytes equal the original text exactly;
- byte/scalar/item measures agree with an independent flat reference;
- measure composition is associative and `Add 0` is the display identity;
- display seeks and visible graphemes equal flat rendering for every tested incoming
  tab residue, Unicode boundary, control placeholder and partial-wide clip;
- edits produce the same text as an independent splice, and Undo/Redo restore it;
- normal chunks obey minimum/maximum rules, with the stated tail exception, and items obey the
  32-scalar bound without losing source text;
- local repair preserves segmentation and reuses only proven unchanged suffixes;
- source/render selection coordinates and line-level change counts stay unchanged.

First introduce the internal line-body operations and their laws. Then migrate
line edits and coordinate queries, followed by source rendering and hit testing.
Prepared styles must align by scalar ranges while borrowing the underlying chunks;
a flat `SourceRow Text` adapter on every frame is not the final path. File/HLS
exports can flatten explicitly on workers. Keep recovery content-oriented rather
than serializing an accidental in-memory tree shape. No new network protocol is
needed: frontends still receive the same cells and glyphs.
