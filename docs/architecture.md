# Architecture

The desktop model is shared by the terminal, native and browser frontends.
Input produces desktop transitions and effects; rendering turns the resulting
state into a character grid. File access, language tools, Git, conversations and
debugging feed results back into that desktop.

## Buffers and views

A text file uses a persistent finger tree of newline-inclusive lines, measured
by character count and line count. Edits rebuild boundary lines and share
unchanged subtrees. Up to 100 undo states retain trees rather than complete text
copies. Split windows refer to the same buffer.

Indexed line lookup serves navigation and cursor placement. A lazy text
projection is cached per revision for highlighting, file output and HLS.
Highlighting and LSP full-document synchronization consume the whole document
after an edit. Cached syntax tokens are shared between split views and reused
for cursor-only redraws.

Skylighting supplies the language definitions. The filename chooses the grammar
and the editor maps token categories into its palette. Hex buffers preserve raw
bytes and use the same file and undo machinery.

To measure highlighting on the included fixture:

```sh
cabal exec -- ghc -O2 -package thc-edit test/HighlightBench.hs -o /tmp/thc-highlight-bench
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

Vty is the common rendering representation. Native windows draw bitmap tiles
and system-shaped fallback text; the browser draws the same grid with WebGL
and canvas-shaped fallback. Text terminals provide their own glyph rendering.
Grapheme segmentation and cell widths use utf8proc.

The native Pixelate Unicode path shapes at four-times resolution, filters in
linear light and applies Floyd–Steinberg error diffusion to four coverage
levels before enlargement. The browser's pixelated fallback does not use that
native filter. Bundled bitmap glyphs retain nearest-neighbor scaling.

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

SSH starts the fixed `thc-edit --remote` command without a PTY. Startup options
and paths travel in bounded, length-prefixed JSON/binary packets over stdin and
stdout; diagnostics use stderr. The relay owns the connection rather than the
editor state. Clients share the adaptive display compression above.

POSIX hosts use a private Unix socket; Windows uses an authenticated loopback
endpoint with an owner-only descriptor. One frontend controls a session at a
time. Read-only editor introspection uses separate connections without taking
over the display. Reattachment resets the display, and sequenced input avoids
repeating acknowledged edits.

Detaching stops the frontend attachment; Exit finishes the editor session.
Sessions survive a lost frontend or SSH connection, not a session-process crash
or host reboot. [Sessions](sessions.md) explains resumption and frontend switching;
[remote editing](remote.md) gives the SSH setup.

## Integration boundaries

HLS, ACP providers and DAP servers use their existing protocols. ACP sessions
receive the built-in editor MCP server automatically. Its read-only tools inspect
windows, buffers and selections directly from the live desktop, including
unsaved text and hex buffers. Rename and
editor-mediated conversation writes check the buffer and disk versions before
applying changes. Git review fingerprints the saved repository and index before
staging and committing. Embedded shells and ACP terminal requests share the
libghostty-vt parser, grid and PTY backend.

The design and implementation records remain under [design](design/) and
[plans](plans/). They record decisions and earlier scope; the [user guide](README.md)
is the entry point for current behavior.
