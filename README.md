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

## Working today

- Borland palette, menu bar and dropdowns, active/inactive window borders,
  shadows, title dragging, resize handles, zoom, cascade, tile and shared splits.
- Modal text inputs, checkboxes, radio buttons, lists and buttons. Try
  **Tools > Widget gallery** to exercise the dialog controls.
- UTF-8 files, Haskell lexical highlighting including nested comments,
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

Select WordStar under **Options > Editor**, or use `--wordstar`:
Ctrl+E/S/D/X moves up/left/right/down; Ctrl+A/F moves by word; Ctrl+Y deletes
one line. Ctrl+K then B/K marks block start/end, C/V copies/cuts, Y deletes
the block, S saves and D closes. Ctrl+Q then S/D moves to line start/end,
R/C to file start/end, F finds and A replaces. Escape cancels a prefix.
This is a useful subset, not a complete WordStar emulation.

## Scope and limitations

HLS tooling, Cabal-plan project browsing, compile/run/debug integration and
persistent preferences are subsequent milestones. Their menu actions explain
that they are unavailable. Directory arguments do not pretend to be projects.
The clipboard is editor-local; use terminal bracketed paste for external text.
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

Scenes: `desktop`, `menu`, `about`, `gallery`, `split`. Event-replay tests exercise
the same pure desktop transitions used by the terminal application; file tests
use real temporary files for conflict, permission, symlink and encoding cases.

See the [design](docs/superpowers/specs/2026-09-30-thc-edit-design.md) and
[implementation record](docs/superpowers/plans/2026-09-30-desktop-editor.md).
Visual reference: [Ilya Birman's Turbo Pascal UI museum](https://ilyabirman.net/meanwhile/all/ui-museum-turbo-pascal-7-1/).
