# thc-edit

Turbo Haskell's source editor.

A standalone Haskell/Vty editor with menus, overlapping windows and split views.
Editor dependencies stay out of THC's compiler and runtime build. The proposed
THC external-command dispatch will expose this executable as `thc edit` when
`thc-edit` is on `PATH`; direct invocation already works.

## Run

Requires GHC 9.6 or newer and Cabal; tested with GHC 9.14.1 on macOS arm64.
Use a UTF-8 terminal, preferably at least 80 columns by 25 rows. Mouse support,
modified keys and exact colors depend on the terminal's capabilities.

```sh
cabal build all
cabal run thc-edit -- --demo
cabal run thc-edit -- Main.hs Other.hs
cabal run thc-edit -- --wordstar Main.hs
cabal test
```

To install the executable on your path:

```sh
cabal install exe:thc-edit --installdir="$HOME/.local/bin"
```

## Window mode

Build with SDL3 installed (`brew install sdl3` on macOS), then run:

```sh
cabal run -fwindow thc-edit -- --metal --size 100x32 Main.hs
cabal run -fwindow thc-edit -- --vulkan --size 100x32 Main.hs
```

`--window` selects Metal on macOS and Vulkan elsewhere. The default is the
terminal; set `THC_EDIT_BACKEND=metal`, `vulkan`, `auto` or `terminal` to change
it. An explicit backend flag takes precedence. The window starts in **Mode 3 (80x25)**; `--size COLSxROWS` accepts 40..512 columns and 12..256 rows.
`--mode 259` (or `--mode 0x103`) selects **80x50** with half-height rendering;
`--mode 3` returns to **80x25**. Options > Preferences > Screen size switches
between them while running. The numbers follow Turbo Pascal's `C80 = 3` and
`C80 + Font8x8 = 259`; 259 is a Borland text-mode constant, not VESA mode 0x103.
Both modes use the same 8x16 glyphs, with a different vertical aspect ratio.
`--size` overrides the character dimensions of either mode. These options do
not change your terminal's font or dimensions.

```sh
cabal run -fwindow thc-edit -- --metal --mode 259 Main.hs
```

`--scale 1` through `--scale 8` sets the pixel scale independently. In Mode 259,
scale 2 or higher retains every bitmap row; scale 1 downsamples vertically.
Resizing the window changes the character grid. SDL3 is an optional build
dependency and is not needed for the terminal frontend or THC itself.

Both frontends draw the same UI. The window uses a bundled IBM VGA bitmap font,
with GNU Unifont for additional Unicode characters; see the licenses in
`assets/fonts`. The window has a system clipboard and, on macOS, native menus
with Command shortcuts. Option remains available for accented characters.
Use F10 and letter mnemonics to operate the in-window menus.

On Mac keyboards, hold Fn/Globe to send a function key, or enable “Use F1, F2,
etc. keys as standard function keys” in System Settings > Keyboard > Keyboard
Shortcuts > Function Keys. Command+O/S/W/Q also opens, saves, closes and exits.

## Working today

- Borland palette, menu bar and dropdowns, active/inactive window borders,
  glyph-preserving gray-on-black shadows, title dragging, resize handles, zoom, cascade, tile and shared splits.
- File Open with directory navigation, wildcard filtering and keyboard/mouse
  selection. Directory arguments open a left file tree; toggle it with Ctrl+B
  or Tools > File tree, and drag its divider to resize it.
- F1 opens this Markdown documentation as a laid-out, read-only text window.
- Git branch and saved-change indicator on the status bar, colored diff review
  and commits from Tools > Approve changes.
- Modal text inputs, checkboxes, radio buttons, lists and buttons. Try
  **Tools > Widget gallery** to exercise the dialog controls.
- UTF-8 files, filename-selected syntax highlighting from Skylighting,
  selections, undo/redo, internal clipboard, bracketed terminal paste,
  find/replace-one and go-to-line.
- Checked saves, dirty-close prompts, CRLF preservation, and optional WordStar
  movement and block-selection keys. Split views share text and undo history.

| Action | Keys |
| --- | --- |
| Help / save / open | F1 / F2 / F3 |
| Zoom / next window / menus | F5 / F6 / F10 |
| Close / exit | Alt+F3 / Alt+X |
| Select / move by word | Shift+arrows / Ctrl+arrows |
| Undo / redo | Ctrl+Z / Ctrl+Y |
| Copy / cut / paste | Ctrl+C / Ctrl+X / Ctrl+V |
| Find / replace / next match / go to line | Ctrl+F / Ctrl+R / Ctrl+L / Ctrl+G |
| Dialog focus / accept / cancel | Tab or Shift+Tab / Enter / Escape |

Select WordStar under **Options > Preferences**, or use `--wordstar`:
Ctrl+E/S/D/X moves up/left/right/down; Ctrl+A/F moves by word; Ctrl+Y deletes
one line. Ctrl+K then B/K marks block start/end, C/V copies/cuts, Y deletes
the block, S saves and D closes. Ctrl+Q then S/D moves to line start/end,
R/C to file start/end, F finds and A replaces. Escape cancels a prefix.
This is a useful subset, not a complete WordStar emulation.

## Syntax highlighting

[Skylighting](https://github.com/jgm/skylighting) supplies the maintained
KDE/Kate language definitions, including Haskell and over 100 other languages.
The filename selects the grammar; unnamed buffers default to Haskell, and
unknown file types stay plain. The editor only maps token categories to its
palette. No editor-specific lexer or keyword lists are maintained.

Tokens are cached per document and shared by split views. Edits and filename
changes refresh the cache; cursor movement reuses it. A local GHC 9.14.1 benchmark
on 1,001 Haskell lines measured about 36 ms per full retokenization and 2 ms per
cached split-view redraw. Large files may need an incremental engine later.
To rerun: `cabal exec -- ghc -O2 -package thc-edit test/HighlightBench.hs -o /tmp/thc-highlight-bench`,
then `/tmp/thc-highlight-bench`.

The `skylighting` package and its bundled grammar set are GPL-2 licensed;
`skylighting-core` is BSD-3-Clause. This dependency belongs only to thc-edit.

## Git review and approval

Tools > Git diff shows staged, unstaged and untracked saved changes for the
current repository. The status bar shows the branch (`*` means saved repository
changes; buffer title stars indicate unsaved edits). Open or save refreshes it.

Read the diff, then choose Tools > Approve changes and enter a commit message.
This stages and commits **all reviewed saved changes in that repository**,
including untracked files and deletions. Save dirty editor buffers first.
Changed files or index state invalidate the review; refresh the diff before
retrying. Git hooks run normally and may transform the committed contents; the editor
reports when the resulting tree differs from the staged review. Failed commits leave changes on disk and may
leave them staged. Nothing is pushed. Submodule review is not supported.

## Scope and limitations

HLS tooling, Cabal-plan project browsing, compile/run/debug integration and
persistent preferences are subsequent milestones.

Haskell Language Server will supply diagnostics, completion, hover/type
information and definition navigation. Runtime debugging will use THC's Truffle
debugger when the program runs through THC: the Debug menu should expose its
breakpoints, stepping, call stack and variable inspection. These are separate
backends; HLS is not the runtime debugger.

Unimplemented menu actions explain that they are unavailable. Directory arguments do not pretend to be projects.
The terminal clipboard is editor-local; use bracketed paste for external text.
Search is literal and case-sensitive. Undo uses up to 100 complete text
snapshots; this first implementation targets ordinary source files, not huge
logs. Combining marks and wide characters are handled, but complex emoji
clusters still depend on terminal rendering.

Files must be valid UTF-8 without NUL bytes. Saves compare the original bytes,
write a sibling temporary file, preserve permissions, check for conflicts again
and rename. Loaded symlinks resolve to their targets. Detected external changes
leave the buffer dirty and the disk file untouched; an unavoidable race remains
between the final comparison and rename if another process writes concurrently.
Save As refuses an existing destination. Atomic replacement is not a promise
of power-loss durability.

## Rendering harness

These deterministic previews use the actual Vty rendering output at 80x25,
without starting an interactive terminal. The HTML is a static preview.

```sh
cabal run -v0 thc-edit -- --demo --snapshot
cabal run -v0 thc-edit -- --demo --scene menu --snapshot-html > menu.html
cabal run -v0 thc-edit -- --demo --scene gallery --snapshot-html > gallery.html
```

Scenes: `desktop`, `menu`, `about`, `gallery`, `split`, `open`, `tree`, `help`, `diff`, `preferences`. Event-replay tests exercise
the same pure desktop transitions used by the terminal application; file tests
use real temporary files for conflict, permission, symlink and encoding cases.

See the [design](docs/superpowers/specs/2026-09-30-thc-edit-design.md) and
[implementation record](docs/superpowers/plans/2026-09-30-desktop-editor.md).
Visual reference: [Ilya Birman's Turbo Pascal UI museum](https://ilyabirman.net/meanwhile/all/ui-museum-turbo-pascal-7-1/).
