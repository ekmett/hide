# Architecture

The desktop model is shared by the terminal, native and browser frontends.
Input produces desktop transitions and effects; rendering turns the resulting
state into a character grid. File access, language tools, Git, conversations and
debugging feed results back into that desktop.

## Reading the source

`Hide.App` composes the session services. `Hide.Model` owns pure desktop
transitions and returns effects; service handlers consume their effects and
delegate the rest. Their ticks adopt completed work into the serialized desktop.
`Hide.Render` paints that state, with an explicit metadata/identity key deciding
when another frame is needed.

The main implementation boundaries are:

| Area | Modules to start with | Design boundary |
| --- | --- | --- |
| Editing and change views | `Buffer`, `BufferView`, `Model` | Persistent text versus per-window coordinates and presentation |
| Rendering | `Render`, `Unicode`, `Font`, `Markdown` | Prepared styled text versus grapheme/cell composition |
| Sessions and displays | `Remote`, `Protocol`, `Recovery`, `Session` | Persistent owner versus replaceable frontend attachment |
| Language/build/debug | `Tooling`, `LSP`, `BuildJobs`, `Debugger`, `DAP` | Worker preparation versus checked adoption and reply waits |
| Conversations | `Conversation`, `AgentHub`, `AgentRuntime`, `Autocomplete` | Provider lifecycle, authenticated actors and immutable request context |
| Agent tools and policy | `EditorMCP`, `MCPPermissions`, `GuestAccess`, the `*MCP` modules | Serialized initiation versus outside-lock continuation; independent privacy checks |
| Disk and Git | `Files`, `External`, `Reconcile`, `GitOperations` | Observed baselines versus current unsaved edits |

Module overviews and initial API Haddocks expand these contracts. They distinguish
character offsets, display cells, byte offsets and protocol positions rather than
using “position” interchangeably. The native C/Objective-C interfaces remain in
`cbits`; Haskell resource owners describe when those handles may be used.

## Buffers and views

A text file uses a persistent finger tree of newline-inclusive lines, measured
by visible characters and lines, NUL/CRLF presence, and added/deleted lines.
Deleted baseline lines remain as tombstones with zero visible width; inserted
lines contribute to the addition count. Edits rebuild boundary lines and share
unchanged subtrees. Window titles and Files read the change totals from the
root measure. Saving normalizes changed leaves and establishes a new baseline. Up to 100 undo states retain trees rather than complete text
copies. Split windows refer to the same buffer.

Indexed line lookup serves navigation and cursor placement. A lazy text
projection is cached per revision for highlighting, file output and HLS.
Highlighting and LSP full-document synchronization consume the whole document
after an edit. The bounded highlighting worker retains original source-row Text
and compact style ranges with character and UTF8 byte boundaries. Split views
share these prepared rows. A numeric UTF8 seek scans the horizontal prefix
without creating discarded fragments, preserving the original utf8proc state.
A lazy iterator then stops after complete graphemes overlapping the viewport;
only those glyphs form fused display runs.
The returned source-character and display starts preserve absolute tab columns
and selection coordinates when a window clips part of a glyph. Ordinary
one-cell characters borrow a same-style source slice, while exceptional
graphemes retain a complete source slice and an explicit display advance.
Segmentation precedes styling so a combining suffix cannot split at a style
boundary. Tabs and control placeholders synthesize display text; source offsets
and copied text remain unchanged. Cursor-only redraws reuse prepared styling. UTF-16 position conversion uses measured line lookup
and scans only the prefix of the target line. Disjoint edit batches share
untouched subtrees and create one undo step.

[Regular line spans](design/line-spans.md) use roughly 128-byte borrowed slices
and local join repair. Immutable source owners share lazy display checkpoints;
edited rows retain ranges of those owners in a tree measured by raw bytes and
scalars. Viewport queries demand display metadata as needed, preserving Unicode
context and tab-aware advances. Raw save and Undo do not require that index.

Recovery writes a checkpoint-local string table for physical lines and long-line
pieces. Current, saved, undo and redo roots refer to those strings; serialization
does not flatten each retained state into another copy of the file. Cached
fingerprints select table buckets, and exact text equality resolves collisions.
Restore checks references, byte representation, edit coordinates and saved-line
provenance, then rebuilds raw measured roots with shared strings. Display indexes
remain lazy. The checkpoint stores text and line provenance, not the internal
finger-tree graph or a cache of its geometry.

Skylighting supplies the language definitions. The filename chooses the grammar
and the editor maps token categories into its palette. Hex buffers preserve raw
bytes and use the same file and undo machinery.

To measure highlighting on the included fixture:

```sh
cabal exec -- ghc -O2 -package hide test/HighlightBench.hs -o /tmp/thc-highlight-bench
/tmp/thc-highlight-bench
```

## Files and saves

Saving compares the original bytes, writes a sibling temporary file, preserves
permissions, checks the disk version again and renames the temporary file.
Loaded symlinks resolve to their targets. Save as refuses an existing destination.

The reconciliation worker observes open files and expanded directories. Clean
buffers reload with Undo retained; conflicts retain the editor, last-saved and
disk versions. The save-time check remains authoritative after an external
change or a Git or conversation operation.

There is a race between the final comparison and rename if another process
writes at that instant. Atomic replacement is not a guarantee of power-loss
durability. [Editing](editing.md#save-close-and-external-changes) describes the
user-facing conflict choices.

## Rendering and text

A composed cell grid is the shared rendering boundary. Ordinary Vty panels and
prepared styled rows feed that grid once. Native windows and WebGL draw glyphs
from retained atlases; text terminals provide their own glyph rendering.
Grapheme segmentation and cell widths use utf8proc. Bold and italic affect the
atlas tile; underline and strikethrough are cell paint. These traits do not
change grapheme advances.

Styled text borrows same-style `Text` runs. Finalized rows use the same
`ConsChars`/`ConsSigil` representation as source rendering: ordinary text stays
in runs, while exceptional graphemes retain their complete source fragment and
display width. A style boundary cannot split a grapheme. Layout keeps ordinary
runs intact and computes positions within them; only visible wide or scripted
glyphs need individual paint operations. Copy and privacy coordinates remain
source coordinates, independent of wrapping and decorative cells.

Prepared styled rows can use `ScriptStyle Superscript` or `ScriptStyle Subscript`.
A scripted grapheme occupies one cell and keeps its complete source text for
copying and hit testing. Graphical backends scale the normal atlas tile to half
size, in the upper or lower half of that cell. A narrow glyph occupies the left
quarter; a naturally wide glyph fills the half. Text terminals print narrow
script glyphs normally and use a one-cell replacement for wide ones. Explicit
script geometry survives transport and capture; it does not borrow terminal
attribute bits or change the privacy mask's cell boundaries.

The native Pixelate Unicode path shapes at four-times resolution, filters in
linear light and applies Floyd–Steinberg error diffusion to four coverage
levels before enlargement. The browser's pixelated fallback does not use that
native filter. Bundled bitmap glyphs retain nearest-neighbor scaling.

A Markdown buffer view keeps its original source document and undo history.
`TextPresentation` captures immutable source content, revision, width and title
preferences, then parses and lays it out on its existing background worker.
Adoption checks those small keys; rendering and input consume the prepared rows.
Each window holds separate rendered selection and scrolling, while Current keeps
its source interaction. A pending or failed preview stays read-only.

Markdown links retain their targets through wrapping, tables and chat-bubble
layout. That layout records character spans once; hit testing uses the buffer's
line index. A bounded session worker loads and lays out linked documents outside
the input lock, then installs the prepared buffer. A click follows its target on release, while a drag selects text.
`FollowLink` opens relative Markdown on the session host. For external targets,
`open-resource` carries either an HTTP(S) `url` or bounded `mime`/base64 `data`
to the client. These live control messages are excluded from reply replay.
Native clients invoke the OS opener with an argument vector; the browser opens
a tab with an explicit click fallback when popup activation has expired.

## Background work

Native and browser redraw gates compare explicit window and control metadata
with identities for immutable payloads. Whole-desktop equality is forbidden in
runtime change detection; render keys cannot contain Desktop or Buffer values.
They do not walk source text, undo history, transcript cells or diagnostic bodies
just to decide whether another frame is needed.
Changed hidden buffers still invalidate native menu state.

Build output is decoded, accumulated and parsed by an owned worker. It publishes
one coalesced, evaluated buffer/diagnostic snapshot; a desktop tick takes the
latest snapshot instead of rebuilding the output history. Stop records a request,
then the supervisor releases and joins the child process outside the UI tick.

Agent file capture and prompt-context preparation run in bounded owned workers.
File-read workers also slice the requested lines and encode the complete bounded
ACP response. Completion checks current buffer/file identities and privacy before
enqueuing prepared bytes, without comparing whole snapshots; cancel
and disconnect retire unfinished work. Idle compiler-setting reads also run in
a worker. HLS synchronization queues immutable buffer references, with text
comparison and encoding on the protocol writer; incoming events are consumed
in bounded batches. Cursor-only ticks reuse buffer/client identity keys. The
Messages projection reuses its parsed, sorted diagnostics until an HLS batch,
source identity/version, or build diagnostic list changes.

Project-root discovery and HLS process startup also run in bounded workers.
MCP requests made during startup keep their original deadlines and wait for document
synchronization; edits, reloads, closing a file or making it private invalidate
captured requests. Restart retires old workers asynchronously and reserves each
root until its previous client has stopped. Rename and code-action snapshots
classify project boundaries and read closed files in their preparation worker.
Workspace edits also validate ranges and build replacement buffers in a worker.
Adoption checks every target against its current identity and privacy before
applying any patch, preserves intervening navigation and unrelated edits, and
adds one undo step per affected buffer. Later server events wait behind each
edit batch so a command result cannot overtake its edits.

These boundaries keep routine tool output and held file reads out of navigation
and rendering. Explicit save/configuration operations and some tool launch
preparation still perform synchronous work; the desktop is not yet free of all
blocking effects.

## Browser display transport

Frames contain UTF-8 color runs and explicit cell widths for complex graphemes.
For a changed display, the server compresses a full frame and changed rows,
then sends the smaller representation. A prefix byte identifies the encoding:

| Byte | Encoding |
| --- | --- |
| 0 | Reset/full frame |
| 1 | Full frame using the previous screen |
| 2 | Changed rows |

The final 32 KiB of the reconstructed previous screen supplies the dictionary.
Unchanged metadata is omitted. Both formats retain the same screen history;
the browser's bundled raw DEFLATE decoder seeds its dictionary locally. No
additional WebSocket compression layer or downloaded decoder is needed.

## Sessions and remote editing

Every interactive frontend attaches to an editor session in an independent
process. The session owns buffers, desktop state and tools. `Session.hs` records
the identity, host and startup arguments so `--resume` can reconnect without
reconstructing the launch command. Local and SSH sessions use the same display
protocol, with terminal, native and browser frontends.

SSH starts the fixed `hide --remote` command without a PTY. Startup options
and paths travel in bounded, length-prefixed JSON/binary packets over stdin and
stdout; diagnostics use stderr. The relay owns the connection rather than the
editor state. Clients share the adaptive display compression above.

POSIX hosts use a private Unix socket; Windows uses an authenticated loopback
endpoint with an owner-only descriptor. One frontend controls a session at a
time. MCP requests use separate connections without taking
over the display. Reattachment resets the display, and sequenced input avoids
repeating acknowledged edits.

Detaching stops the frontend attachment; Exit finishes the editor session.
Sessions survive a lost frontend or SSH connection, not a session-process crash
or host reboot. [Sessions](sessions.md) explains resumption and frontend switching;
[remote editing](remote.md) gives the SSH setup.

## Integration boundaries

HLS, ACP providers and DAP servers use their existing protocols. ACP sessions
receive the built-in editor MCP server automatically. Its [session tools](session-tools.md)
read and edit live buffers, arrange windows, inspect language/debug state, and
share build and terminal services. Per-tool policies apply before dispatch.
Initiation runs under the desktop lock; protocol replies and user questions wait
outside it. Rename and
editor-mediated conversation writes check the buffer and disk versions before
applying changes. Git review fingerprints the saved repository and index before
staging and committing. Embedded shells and ACP terminal requests share the
libghostty-vt parser, grid and PTY backend.

The design and implementation records remain under [design](design/) and
[plans](plans/). They record decisions and earlier scope; the [user guide](README.md)
is the entry point for current behavior.
