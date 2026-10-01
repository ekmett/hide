# thc-edit

Turbo Haskell's source editor.

A standalone Haskell/Vty editor with menus, overlapping windows and split views.
Editor dependencies stay out of THC's compiler and runtime build. The proposed
THC external-command dispatch will expose this executable as `thc edit` when
`thc-edit` is on `PATH`; direct invocation already works.

## Run

Requires GHC 9.6 or newer and Cabal; tested with GHC 9.14.1 on macOS arm64.
Grapheme segmentation and cell widths require utf8proc 2.10 or newer
(`brew install utf8proc` on macOS). Linux window builds also require Pango/Cairo
development headers (`libpango1.0-dev` on Debian/Ubuntu) and benefit from
`fonts-noto-color-emoji`. macOS uses the system CoreText framework for fonts.
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

## Browser mode

```sh
cabal run -fweb thc-edit -- --web --crt --scale 1.5 .
```

The optional browser frontend requires no SDL installation. `THC_EDIT_BACKEND=web`
selects it by default. It opens a local WebGL page on an ephemeral loopback port;
`THC_EDIT_WEB_OPEN=0` prints the URL without opening it. Keep the editor process
running: reloading/reconnecting preserves buffers in that process.

The browser uses the bundled bitmap font, Material folder tiles, DOS cell cursor,
Retina canvas and CRT shader. Mode 3/259 and scale options also work here. Native
Unicode fallback uses browser canvas shaping; its pixelated mode does not yet use
the native frontend's error-diffusion filter.

Frames use UTF-8 color runs, with explicit cell widths for complex graphemes.
For each changed display the server compresses both a full screen and changed
rows, sends the smaller result, and prefixes one byte: 0 for a reset/full frame,
1 for a full frame using the previous screen, or 2 for changed rows. Both
candidates use the last reconstructed screen's final 32 KiB as their dictionary;
unchanged metadata is omitted. Switching formats does not lose screen history.
The browser seeds its built-in raw DEFLATE decoder locally with that dictionary;
no second WebSocket compression layer or downloaded decoder is needed.

File > Download exports the current buffer, including unsaved edits and binary
bytes. It does not mark a local file saved. Dropping browser files opens separate
editable buffers (up to 16 MiB each); their names are suggestions, never server
paths. In Metal/Vulkan, dropping a file opens its actual filesystem path.

Copy, Cut and Paste use the browser clipboard. If clipboard permission requires
a fresh gesture, a toolbar button lets you retry; ordinary browser clipboard
shortcuts also work. Ctrl/Cmd+A selects all; Ctrl/Cmd+Z undoes; Ctrl/Cmd+Shift+Z or
Ctrl/Cmd+Y redoes. Browser `beforeinput` history commands are handled where emitted,
but some browsers disable native Undo/Redo menus for a canvas-backed editor.
Ctrl/Cmd+F opens editor Find; Ctrl/Cmd+G and Shift+Ctrl/Cmd+G find next/previous.
Browser-native Find menu items cannot be redirected through a standard web API.

Click **Fullscreen / capture keys** to request fullscreen and Keyboard Lock where
available. OS/browser-reserved shortcuts may still be intercepted. Leaving a page
with modified buffers or an unsent query uses the browser's native confirmation;
choose Stay to save in the editor. File > Exit uses the editor's save dialogs and
attempts to close the tab; browsers may refuse to close tabs opened manually.

## Remote editing over SSH

Install `thc-edit` on both machines. The remote host needs the `remote` build
flag; it does not need the THC compiler, SDL, a browser, or a graphical session:

```sh
cabal install exe:thc-edit -fremote -f-web -f-window -f-terminal
```

Build the local client with `-fremote -fwindow` for Metal/Vulkan, or
`-fremote -fweb` for the browser. Connect using your normal SSH host or alias:

```sh
thc-edit --metal username@eak-pc.local:some-path
thc-edit --web username@eak-quartus.local:some-path
```

When THC's external-command dispatcher is installed, `thc edit` accepts the same
arguments. The client runs `ssh -T HOST "thc-edit --remote"`; `thc-edit` must be
on the remote SSH command's `PATH`. File paths and startup options travel inside
the protocol, so spaces and shell punctuation in a path are not shell commands.
`--ssh HOST PATH` is an alternative. Prefix a local filename containing a colon
with `./` to distinguish it from a remote target.

Files, Git, HLS, builds, debugger and conversation processes run on the remote
host. Drawing, clipboard access and downloaded files stay on the local client.
Dropped local files are uploaded into new, unsaved remote buffers. Uploads are
limited to 16 MiB each. Remote terminal support additionally requires that host's
optional `terminal` build; the embedded terminal currently requires POSIX.

A dropped connection leaves the remote session running and the client attempts
to reconnect. The session identifier is printed on stderr; use
`--remote-session ID` with the same host to attach from a new client. Only one
client controls a session at a time. File > Exit closes the session through the
normal save prompts. Closing a disconnected client detaches it. Sessions survive
SSH disconnects, but not a daemon crash or host reboot; save important changes.

The server uses bounded, length-prefixed JSON/binary packets on stdin/stdout,
with diagnostics on stderr. Both clients share the browser's adaptive display
compression. Reattachment sends a full display reset, and sequenced input avoids
repeating acknowledged edits. POSIX hosts use a private Unix socket; Windows
hosts use an authenticated loopback endpoint with an owner-only descriptor.

## Markdown

Markdown source headings have distinct colors while retaining their `#` markers.
Help uses the CommonMark renderer shared with conversations: colored headings,
bulleted/numbered/nested lists, hanging indents, padded code panels with language
highlighting, and GitHub-style pipe tables. Tables wrap to the available width
and use stacked labeled cells when the window is too narrow for columns.

Options > Preferences > Appearance selects **Light**, **Dark**, or **System**.
Dark Help stays blue with bright text and black code panels. Light Help uses
cyan, blue code panels, and black-on-gray shell blocks. Embedded terminals use
white-on-black in Dark mode and black-on-gray in Light mode; explicitly colored
terminal output and application color overrides are preserved.

Use `--appearance light|dark|system` or `THC_EDIT_APPEARANCE`; System is the default.
Metal/Vulkan follow SDL's OS appearance and the browser follows
`prefers-color-scheme`, including changes while running. Text terminals use
`COLORFGBG` at startup, falling back to Dark when unavailable. Classic menu,
window-frame, and editor colors stay unchanged.

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

`--scale 1` through `--scale 8` sets the pixel scale independently. Fractions
such as `--scale 2.5` work; values round to the nearest 1/8 step.
`THC_EDIT_SCALE` supplies the default; an explicit `--scale` takes precedence. In Mode 259,
scale 2 or higher retains every bitmap row; scale 1 downsamples vertically.
Ctrl or Alt plus `+`/`-` changes tile width by one physical pixel (1/8 scale)
while preserving the character grid;
`=` also increases it. Ctrl+0 or Alt+0 restores the display-aware default (2 on standard displays, 4 on
Retina displays at 2× density). Tiles use physical drawable pixels with nearest
neighbor scaling; the graphical window requests high-density rendering.
Resizing the window changes the character grid. SDL3 is an optional build
dependency and is not needed for the terminal frontend or THC itself.

Both frontends draw the same UI. The window uses a bundled IBM VGA bitmap font,
for its original repertoire and interface geometry; see the licenses in
`assets/fonts`. Other text uses system font fallback, including color emoji.
Joined emoji, skin tones, flags and combining sequences are edited and clipped
as complete graphemes in documents, conversations and filenames. Wide glyphs
partially covered by a border or another window become blank cells.
**Options > Preferences > Pixelate Unicode** reduces each shaped cluster to an
8×16 or 16×16 tile before enlargement. It uses four-times oversampling, linear-light
area filtering and Floyd–Steinberg error diffusion with four coverage levels.
Leave it off for full-resolution rendering.
The terminal frontend uses the same layout, with glyph appearance supplied by
your terminal emulator. The window has a system clipboard and, on macOS, native menus
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
Click to select a message; double-click or Enter jumps to its source. Copy
(Cmd+C on Mac, Ctrl+C or Ctrl+Insert) copies its full text and source location.
Right-click offers Go to source, Copy message, Copy all messages, and Hide Messages.
Drag the Messages title bar vertically to resize it. Adjacent editors follow its
edge, resizing once they reach the menu bar; Files ends above Messages too.
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
Start with `--metal --crt` (or `--vulkan --crt`) to enable the effect immediately.
In SDL windows, **CRT filter** adds subtle scanlines and a vignette; it is off by
default. When glyph rows are less than two physical pixels tall, only the vignette is
applied to keep text legible (below scale 2 in Mode 3 or scale 4 in Mode 259).
**File > Change dir...** selects a new working directory and refreshes the Files
window without closing your buffers. Enter on a directory browses into it;
OK accepts the displayed directory, and Browse opens a typed path.
The Files pane has a floating title and collapse arrow, with an internal scrollbar.
Its right edge belongs to adjacent windows; uncovered portions use a single white
line on blue. Unsaved filenames are red and return to normal after save or undo.
Drag the shared edge to resize the dock; editors
move with it until their right edge reaches the screen, then stay attached there
and resize as the divider moves. Status-bar key labels are clickable shortcuts and highlight
green under the pointer. Ctrl+Tab cycles windows; Alt+Tab cycles the menu,
Files, windows, and Messages. Shift reverses either cycle. Dialogs cycle their
controls with Alt+Tab. OS-reserved Alt+Tab may not reach the editor.
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

The **Conversation** window uses compact speech bubbles: yours align right in
cyan, replies align left in gray. Rounded corners share the text rows, with a
straight top edge into the speaker's tail. SDL uses single-cell bitmap tiles;
terminal fonts use block-character approximations. Code keeps syntax colors,
and **Copy raw conversation** preserves the original Markdown.

The draft is a cyan thought bubble anchored on the right with an `•.` trail.
Its width follows the longest line, with room for the caret and a twelve-column
minimum, capped by the window width. It starts
at one line, grows with newlines up to twelve visible rows, and scrolls beyond that,
without a divider or buttons. Click the status-bar actions or use their shortcuts: **Enter** submits
(or queues while a reply is active), **Shift+Enter** inserts a newline, and
**Ctrl+Enter** steers when the provider supports it. **Esc Cancel** appears while
replying and stops the active response; queued queries then proceed in order.
Clicks in the conversation keep the draft caret in place. Drag across reply
text to select it: copying within one bubble gives plain text; copying across
bubbles adds `User:` / `Bot:` labels. Bubble shapes and timestamp separators
are excluded. Pauses of five minutes or more get a centered local timestamp.
Press Tab to browse replies with the keyboard; typing returns to the draft. Drafts retain undo/redo and are preserved while replies stream.
Click the conversation title to choose its model and reasoning effort from the
provider-advertised options. The title updates after confirmation; choices are
disabled during a reply. Providers without these options keep a plain title.
The lower-left frame shows the provider-reported context usage, for example
`37% · 148k/400k`; it shows `--` until usage and capacity are available.

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
[libghostty-vt](https://github.com/ghostty-org/ghostty) backend. Both **File > Terminal**
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
16 bytes per row when the complete row fits (76 window columns including the
frame), or 8 bytes per row in narrower windows, with hexadecimal offsets and
an ASCII column. Each split chooses its layout independently; resizing preserves
the selected byte. Type hex pairs to
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

The windowed Files pane uses bundled Material Design folder icons.
For the same symbols in a terminal, use `thc-edit --terminal --material-icons`
with a Nerd Font supporting Material Design Icons (U+F024B and U+F0770).
Without the flag, terminals retain their standard Unicode folder symbols.
Icons reserve two columns, with explicit cursor correction after each glyph.
