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

`--scale 1` through `--scale 8` sets the pixel scale independently.
`THC_EDIT_SCALE` supplies the default; an explicit `--scale` takes precedence. In Mode 259,
scale 2 or higher retains every bitmap row; scale 1 downsamples vertically.
Ctrl or Alt plus `+`/`-` changes tile scale while preserving the character grid;
`=` also increases it. Alt+0 restores the display-aware default (2 on standard displays, 4 on
Retina displays at 2× density). Tiles use physical drawable pixels with nearest
neighbor scaling; the graphical window requests high-density rendering.
Resizing the window changes the character grid. SDL3 is an optional build
dependency and is not needed for the terminal frontend or THC itself.

Both frontends draw the same UI. The window uses a bundled IBM VGA bitmap font,
with GNU Unifont for additional Unicode characters; see the licenses in
`assets/fonts`. The window has a system clipboard and, on macOS, native menus
with Command shortcuts. Option remains available for accented characters except the window-number
and tile-scale shortcuts above.
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
- UTF-8 files, filename-selected syntax highlighting from Skylighting (including C, C++,
  Cabal package files and `cabal.project`),
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
changes; buffer title stars indicate unsaved edits), followed by green additions
and red deletions. These counts cover saved changes, including untracked text.
Open, save and Git operations refresh the badge.

Right-click the branch badge for Fetch, Pull, or Merge. Pull accepts only a
fast-forward; Merge asks which branch to merge. Operations run in the background
and open a result window. Save changed buffers before Pull or Merge. Clean
buffers are refreshed after files change; text edited while an operation runs
is retained. Conflicts remain available to resolve in the editor and Git.
The editor waits for a running Git operation before allowing Exit.

Read the diff, then choose Tools > Approve changes and enter a commit message.
This stages and commits **all reviewed saved changes in that repository**,
including untracked files and deletions. Save dirty editor buffers first.
Changed files or index state invalidate the review; refresh the diff before
retrying. Git hooks run normally and may transform the committed contents; the editor
reports when the resulting tree differs from the staged review. Failed commits leave changes on disk and may
leave them staged. Nothing is pushed. Submodule review is not supported.

## Haskell language tools

Install Haskell Language Server for the GHC used by your package. The editor starts
`haskell-language-server-wrapper --lsp` in the detected project root and keeps
open buffers synchronized, including unsaved edits. Set `THC_EDIT_HLS` to an
alternate server executable if needed. HLS runs independently of the UI;
Tools > Restart language server reconnects after a configuration change.

Hover over source in a graphical window to show its type in the status bar.
Pausing the text cursor does the same in either frontend. Shift+F1 requests a
type explicitly, F12 goes to a definition, and Ctrl+Space opens completions.
Right-click source for these actions and Rename. HLS rename edits are applied
to buffers as one undo step per file; review and save those buffers to write
them to disk. Changed buffer versions or intervening disk edits reject a rename.
File creation/deletion operations and executable completion commands are not
accepted. Completion snippets are disabled in the protocol capabilities.

Errors appear as chevrons beside source lines and in a bottom Messages window.
Click an entry to jump to it; the panel also supports arrow keys and Enter.
Tools > Messages toggles it. Alt+F8 and Alt+F7 jump to the next or previous
message, opening its source file when necessary. Editing clears diagnostics from older buffer
versions while HLS checks the new text. Closing the panel leaves the source
markers available.

The side explorer opens the nearest enclosing Cabal package at startup, falling
back to the current directory. An explicit directory argument takes precedence.
When launched without a filename, or with a package directory, the editor opens
its `.cabal` file too. With multiple package files, it prefers the directory
namesake, then the first alphabetically. Explicit source-file arguments keep
those files selected.
Double-click an entry in the Open dialog to enter its directory or open its file.
The file list uses green selection on cyan and displays the path/filter, byte
count and local modification date/time.
Dialog buttons highlight on hover and depress while held; releasing outside
cancels the click. The focused button has white text; other buttons use black
text with a white mnemonic. Ctrl or Alt plus that letter activates the button.
Each editor and Messages window has a stable number in its title bar.
Alt+1 through Alt+9 activate that window (Option+1 through Option+9 on macOS).
Preferences includes a Blink cursor appearance option, enabled by default.
The graphical insertion cursor blinks every half second; terminal cursor
style follows this preference where DECSCUSR is supported.
While moving or resizing a window, arrows move it, Shift+arrows resize it,
Enter finishes, and Escape restores its original position and size. Menu
navigation shows the highlighted command's description in the status bar. The graphical window uses a DOS text cursor that complements the colors of the cell under the mouse.

## Buffer representation

Each file uses a persistent finger tree of newline-inclusive lines, measured by
character count and line count. Edits rebuild boundary lines and share unchanged
subtrees. Up to 100 undo states retain these trees, rather than full text copies.
Split windows share the buffer. Indexed line lookup drives cursor placement and
navigation; a lazy text projection is cached per revision for syntax highlighting,
file output, and HLS. Highlighting and LSP full-document synchronization still
consume the whole document after an edit.

## Agents

**Options > Agents** configures an ACP stdio executable, a JSON array of arguments,
and a JSON object of environment overrides. Commands are launched directly,
without shell interpolation. Settings live in the user configuration directory
(`$XDG_CONFIG_HOME/thc-edit`, normally `~/.config/thc-edit`).

For local Codex, install the published
[codex-acp adapter](https://github.com/agentclientprotocol/codex-acp), then use
`codex-acp` as the executable. The tested adapter is
`@agentclientprotocol/codex-acp@2.0.1`; its `CODEX_PATH` environment override can
select an existing Codex executable. Authenticate with the provider's own CLI
before starting it here. The adapter and model credentials are separate from
this repository.

The **Conversation** window has a three-line query pane below the transcript.
**Enter** or **OK** submits the draft; while a reply is active it queues the query
for the next turn. **Shift+Enter** inserts a newline. **Ctrl+Enter** steers the
active turn when the provider advertises that extension. **Cancel** (or Esc in
the query pane) stops the active reply; queued queries then proceed in order.
OK is enabled for a nonempty draft, and Cancel only while a request is active.
Click the transcript or press Tab to browse/copy replies; typing returns to the
draft. Drafts retain undo/redo and are preserved while replies stream.

**Tools > Prompt** sends a message with optional selection, current-file and
Messages context, including unsaved text. **Conversation** opens its numbered
window; **Cancel reply** cancels the current turn. Markdown uses CommonMark,
with Skylighting for fenced code, and raw text remains available through
**Copy raw conversation**. Tool activity is displayed separately from replies.

Permission dialogs show the provider's actual choices. Review opens the request
in a read-only window; **Tools > Conversation** returns to the pending choice.
Escape denies the request. Editor-mediated file writes require approval, use the
normal checked save path, and preserve the old buffer in Undo. Intervening editor
or disk changes reject a stale write. File access is bounded by the session's
project directory. This boundary does not sandbox the provider subprocess or its
own tools; configure those permissions in the provider.

**New session** starts a fresh conversation. **Resume session** accepts a saved
session ID, and uses only capabilities advertised by the provider. The latest ID
is saved across editor restarts. Loading requires provider support; transcript
replay depends on whether it supports load or only resume.

## Embedded terminals and Run

Build with **`-fterminal`** to enable the optional
[libghostty-vt](https://github.com/ghostty-org/ghostty) backend. Both **Run > Terminal**
and ACP terminal requests use the same parser, character grid and PTY implementation.
ACP requests ask before executing commands and support output, wait, kill and release.
Closing a terminal window leaves its command running; **Run > Stop terminal** stops
the selected process. Leaving the editor cleans up its terminal processes.

Ordinary keys and paste go to the focused terminal. F5/F6/F10, Alt window-number
keys and menu shortcuts remain editor controls. Terminal windows resize their PTYs
and render truecolor, attributes and Unicode through the existing grid.

**Run > Run** (Ctrl+F9) invokes `thc run --project-dir DIR`. THC selects the current
package's runnable component. **Run > Target** optionally selects a Cabal target
such as `package:exe:program`, a THC executable, source/build root and runtime.
Modified source buffers must be saved first. Target settings are saved in the user
configuration directory. This uses the current THC CLI; older builds with the
former `--exe` interface need updating.

The Ghostty C API is currently unstable. The verified source revision is
`76895d97b74ff6b24c2b1543bcd69ccc18048a4d`, built with Zig 0.16.0:

```sh
git clone https://github.com/ghostty-org/ghostty /tmp/thc-ghostty
cd /tmp/thc-ghostty
git checkout 76895d97b74ff6b24c2b1543bcd69ccc18048a4d
zig build -Demit-lib-vt=true -Demit-xcframework=false -Doptimize=ReleaseFast --prefix /tmp/thc-ghostty-install
cd /path/to/thc-edit
export PKG_CONFIG_PATH=/tmp/thc-ghostty-install/share/pkgconfig:$PKG_CONFIG_PATH
cabal run -fwindow -fterminal --ghc-options=-optl-Wl,-rpath,/tmp/thc-ghostty-install/lib thc-edit -- --metal
```

Use `--vulkan` on Linux. The terminal backend is optional independently of SDL;
without it, the editor reports embedded terminals unavailable and does not advertise
terminal support to ACP providers. No alternative escape-sequence parser is used.

## External changes

The editor observes open files and expanded tree directories in a background worker.
Clean files reload with Undo preserved. Dirty or deleted files retain the editor,
last-saved and disk versions and offer Compare, Reload, Keep and Save as.
**File > Disk changes** reopens an acknowledged conflict. Keep retains the old disk
baseline, so a later Save cannot silently overwrite the external version.
Repeated observations of the same conflict do not repeatedly interrupt editing.
Save-time checks remain authoritative, including after Git and ACP operations.

## Scope and limitations

Cabal-plan source filtering, compilation controls and persistent appearance
preferences remain subsequent milestones. The editor can attach to a loopback DAP
server through **Debug > Attach**. THC launcher forwarding and runtime debugger
hooks are still separate integration work; **Debug > Launch** remains unavailable.
The stock Graal server uses Content-Length DAP over TCP, separate from program I/O.
Breakpoints, continue/pause, trace into/step over/out, threads, call stack, scopes,
explicit variable expansion and advertised exception filters are available.
Source references open read-only source windows when no disk file is available.
Variables show the runtime's supplied values; the editor never automatically
requests evaluation or changes a variable. Frame and variable handles expire when
execution resumes. Disconnect detaches without requesting program termination.
The default endpoint is 127.0.0.1:4711; only loopback endpoints are accepted.
See [Graal DAP](https://www.graalvm.org/latest/tools/dap/) for stock-server options;
those options are not yet claimed to work through thc run.

Unimplemented menu actions explain that they are unavailable. Directory arguments do not pretend to be projects.
The terminal clipboard is editor-local; use bracketed paste for external text.
Search is literal and case-sensitive. Combining marks and wide characters are handled, but complex emoji
clusters still depend on terminal rendering.

Files containing NUL bytes or invalid UTF-8 open in hex mode. **Edit > Text / hex
mode** toggles valid text files without changing their bytes. The display has
16 bytes per row, hexadecimal offsets and an ASCII column. Type hex pairs to
replace bytes; Tab switches to ASCII entry. Insert adds a zero byte, Delete and
Backspace remove bytes, and Undo/Redo retain exact bytes across mode changes.
Copy/Paste use hexadecimal byte pairs. Files with NUL or invalid UTF-8 cannot
switch to text mode until their contents are valid text. HLS and ACP text file
operations exclude hex buffers. Numeric reinterpretation and endian previews
are not part of this first mode.

Saves compare the original bytes,
write a sibling temporary file, preserve permissions, check for conflicts again
and rename. Loaded symlinks resolve to their targets. Unresolved external conflicts preserve the editor buffer and disk file; an unavoidable race remains
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

See the [design](docs/design/2026-09-30-thc-edit-design.md) and
[implementation record](docs/plans/2026-09-30-desktop-editor.md).
Visual reference: [Ilya Birman's Turbo Pascal UI museum](https://ilyabirman.net/meanwhile/all/ui-museum-turbo-pascal-7-1/).

Real-server checks (requires a compatible HLS and GHC in PATH):

```sh
cabal exec -- ghc -threaded -package thc-edit test/HLSLive.hs -o /tmp/thc-hls-live
/tmp/thc-hls-live
cabal exec -- ghc -threaded -package thc-edit test/EditorLive.hs -o /tmp/thc-editor-live
/tmp/thc-editor-live
```
