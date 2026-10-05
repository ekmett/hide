# hide — Haskell IDE

hide is a Haskell IDE written in Haskell, and the Turbo Haskell editor.
It does not require Turbo Haskell: use it with GHC, HLS, Git and your choice of agent. [Read the documentation](https://ekmett.github.io/hide/).

I'm building a Haskell environment where source, types, diagnostics, conversations
and running programs live in the same place. Open a package, follow a definition,
change the code and review the result without having to reconstruct what you were
doing in another application.

The editor has menus, overlapping windows and shared split views. It runs in a
terminal, a native Metal/Vulkan window, or a browser. The same desktop can work
on a remote machine over SSH, with the files and tools beside the project and
the display in front of you.

![Source editing, compiler output and an agent conversation sharing the Turbo Haskell desktop.](docs/site/screenshots/editor-conversation.png)

## Open a project

```sh
hide .
```

`make install` installs `hide`, the short command `th`, and the `thc-edit` launcher
next to one another. With THC installed, `thc edit .` invokes the same editor.
THC is optional.

This opens the current directory in the Files pane and its Cabal package file,
when there is one. Pass source files to start with those instead:

```sh
hide Main.hs Other.hs
```

Use `--window` for a native window or `--web` for the browser. The default is the
terminal. To get the executable, see [Build and run](#build-and-run) below.

Each desktop runs in its own session. Detach and return with `hide --resume`,
or choose a different display with `hide --web --resume`. In a terminal,
**Ctrl+]** detaches; **File > Exit** finishes the session. [Sessions](docs/sessions.md)
covers choosing an unfinished desktop and continuing on another frontend.

## Working in the editor

Use **F3** to open a file, **F2** to save and **F10** to enter the menus. **F1**
opens this guide. Menu descriptions and clickable shortcuts appear in the status
bar. Mac function keys may need Fn/Globe.

The Files pane follows the selected directory. Drag a window by its title,
resize it from the corner, or use **Window > Tile** and **Window > Cascade**.
**Window > Split vertically** and **Split horizontally** open another view of
the same buffer, including its undo history.

These are the default source-editor bindings. The Mac column describes the
native window and browser: ⌃ Control, ⌥ Option, ⇧ Shift and ⌘ Command, in that order
when combined. Mac keyboards label Enter as Return.

| Action | Windows / Linux | Mac |
| --- | --- | --- |
| New / open | Ctrl+N / Ctrl+O (F3) | ⌘N / ⌘O (F3) |
| Save / save as | Ctrl+S (F2) / File > Save as | ⌘S (F2) / ⇧⌘S |
| Preferences | Options > Preferences | ⌘, (Command-Comma) |
| Close / exit | Alt+F3 / Ctrl+Q (Alt+X) | ⌘W / ⌘Q |
| Help / zoom / menus | F1 / F5 / F10 | F1 / F5 / F10 |
| Next / previous window | Ctrl+Tab (F6) / Ctrl+Shift+Tab | ⌃Tab (F6) / ⌃⇧Tab |
| Numbered window / Files pane | Alt+1…9 / Ctrl+B | ⌥1…9 / ⌃B |
| Select text | Shift+arrow key | ⇧ with an arrow key |
| Move by word | Ctrl+← / Ctrl+→ | ⌃← / ⌃→ |
| Undo / redo | Ctrl+Z / Ctrl+Shift+Z (Ctrl+Y) | ⌘Z / ⇧⌘Z |
| Copy / cut / paste / select all | Ctrl+C / Ctrl+X / Ctrl+V / Ctrl+A | ⌘C / ⌘X / ⌘V / ⌘A |
| Find / replace | Ctrl+F / Ctrl+H | ⌘F / ⌥⌘F |
| Next / previous match | Ctrl+L / Ctrl+Shift+L | ⌘G / ⇧⌘G |
| Go to line | Ctrl+G | ⌃G |
| Complete identifier (HLS) | Ctrl+Space | ⌃Space |
| Request inline suggestion | Alt+\ | ⌘\ |
| Previous / next suggestion | Alt+[ / Alt+] | ⌘[ / ⌘] |
| Accept suggestion / next word / dismiss | Tab / Alt+→ / Escape | Tab / ⌥→ / Esc |
| Conversation / new conversation | Ctrl+Shift+C / Ctrl+Shift+N | ⇧⌘C / ⇧⌘N |
| Dialog next / previous / accept / cancel | Tab / Shift+Tab / Enter / Escape | Tab / ⇧Tab / Return / Esc |

Choose an inline provider in **Options > Autocomplete**. Holding Alt (⌥ on Mac)
alone for a second also requests a suggestion in native windows and browsers.
Text terminals do not report modifier-only holds; use the explicit shortcut.

Browser and native editors use the same graphical or macOS profile. Ctrl+L /
Ctrl+Shift+L (⌘G / ⇧⌘G on Mac) find the next and previous matches; Ctrl+G goes
to a line. Ctrl+R also opens Replace in native non-Mac windows and text terminals;
browsers retain Reload. Native Mac menus expose configured accelerators, and
⌘H retains Hide. Other Control bindings stay
available; do not substitute ⌘ for every Ctrl shortcut. In a text terminal on
any OS, use the Windows/Linux column when the terminal forwards those keys:
⌘ shortcuts belong to the terminal app. OS-reserved shortcuts may be intercepted.
**Options > Preferences** can show Mac modifier symbols in text-mode key labels;
this changes their appearance, not the Control/Alt bindings. See
[configuration](docs/configuration.md) for the saved setting. Commands can be
replaced or unbound per terminal, graphical or macOS profile and source, WordStar, dialog editing/search, sidebar,
conversation, debugger, Messages or PTY context in TOML. The focused context supplies
menu and status hints; **Options > Reload keybindings** adopts validated edits,
and **Inspect keybindings** shows the effective map. See
[keybindings](docs/configuration.md#keybindings) for examples.

Native and browser frontends use the system clipboard. Browser remaps may need
the visible Copy/Paste button when clipboard permission or user activation has
expired; browser Edit-menu actions also remain available. In a text terminal,
Copy and Cut also request the system clipboard through OSC 52 where supported;
paste external text through your terminal. **Options > Preferences** also offers
WordStar keys. WordStar movement/prefix/block grammar and dialog navigation
retain their fixed input ownership.

A star in a file's title means unsaved edits. Saving checks whether the disk
file changed underneath you. Clean buffers reload external changes; when both
versions changed, the editor offers Compare, Reload, Keep and Save as.
**File > Disk changes** returns to an unresolved conflict.

The [editing guide](docs/editing.md) covers navigation, selection, searching,
splits and external changes. Files containing binary data open in the
[hex editor](docs/hex.md); **Edit > Text / hex mode** changes views without
changing the bytes.

## Haskell

Put a Haskell Language Server compatible with your package's GHC on `PATH`.
The editor starts it in the project and sends unsaved edits as you work.

* **Shift+F1** inspects the type at the cursor; pausing over source also shows
  type information in the status bar.
* **F12** follows a definition. **Ctrl+Space** completes an identifier.
* Right-click source and choose **Rename** to update a symbol across files.
  Review and save the resulting buffers.
* **Tools > Messages** shows diagnostics. Enter or double-click takes you to
  the source; **Alt+F8** and **Alt+F7** visit the next and previous message.

**Compile > Make** (**F9**) builds the selected THC or GHC target.
**Run > Run** (**Ctrl+F9**) runs it, with an embedded terminal for interactive programs.
Use **Run > Target** to choose the toolchain and executable. **Debug > Launch**
starts a debugger; **Debug > Attach** connects to a running DAP server. Set
breakpoints, step, and inspect the stack and variables alongside the source.
The [Haskell guide](docs/haskell.md) covers language tools; [running and
debugging](docs/running.md) covers setup and the build/debug workflow.

## Review and conversation

**Tools > Git diff** shows saved staged, unstaged and untracked changes.
**Tools > Approve changes** commits all reviewed saved changes in the repository
after asking for a message. Save buffers before reviewing. The branch badge
shows repository status; right-click it for Fetch, Pull and Merge. The
[Git guide](docs/git.md) explains what is reviewed and committed.

Configure an ACP provider under **Options > Agents**, then open
**Tools > Conversation**. The provider can inspect live window titles, buffer
contents and selections through the editor’s MCP tools, including unsaved text.
It can also use [session tools](docs/session-tools.md) for HLS, window layout,
checked edits, build/run/test, shared terminals and source debugging.
**Enter** (Return on Mac) sends a query by default; **Ctrl+Enter** (⌃Return or
⌘Return in the Mac window) steers when supported by the provider. **Options > Chat input** can
swap those actions. **Shift+Enter** (⇧Return) adds a line and **Escape** cancels an
active reply. Queries sent while a reply is running are queued. Click the
conversation title to choose its model and reasoning effort.

Replies, code and tool activity remain beside your files. Permission requests
let you inspect proposed work before allowing it; editor-mediated file changes
preserve Undo. The [conversation guide](docs/conversations.md) covers provider
setup, review and resuming sessions.

## Work on another machine

With `hide` installed on both machines, open a remote project using an
SSH host or alias:

```sh
hide --window buildbox:projects/example
hide --web buildbox:projects/example
```

Files, HLS, Git and project commands run there. Drawing and clipboard access
stay local. A dropped connection leaves the session running so you can reconnect
to the same buffers. `hide --resume` also remembers the host for remote
sessions. The [remote guide](docs/remote.md) covers installation and paths.

## Build and run

The default build includes native windows, the browser and embedded terminals,
along with terminal display and SSH sessions. You need GHC 9.6 or
newer, Cabal, `pkg-config`, utf8proc 2.10+, SDL3 3.4+ and libghostty-vt.
[Installation](docs/install.md) covers these dependencies and the Linux font stack.

```sh
cabal run hide -- --window .
cabal run hide -- --web .
cabal install exe:hide --installdir="$HOME/.local/bin"
```

`--window` selects Metal on macOS and Vulkan elsewhere. Build flags are opt-out:
`-f-window`, `-f-web` and `-f-terminal` omit individual components. A minimal
terminal-display or remote-server build is:

```sh
cabal install exe:hide -f-window -f-web -f-terminal
```

It needs utf8proc but neither SDL nor Ghostty. Sessions and SSH editing remain
available in every build.

## Finding your way around

* [The user guide](docs/README.md) groups the editing, Haskell, Git, conversation
  and remote workflows.
* [Display and frontends](docs/display.md) covers screen size, fonts, browser
  uploads/downloads, appearance and keyboard handling.
* [Development](docs/contributing.md) covers checks, rendering previews and the
  source layout. [Architecture](docs/architecture.md) describes buffers and
  display transport.
* [`docs/design/`](docs/design/) and [`docs/plans/`](docs/plans/) retain the design
  and implementation records.
* [THC](https://github.com/ekmett/thc) is the compiler and runtime. The editor is
  a separate executable and can edit projects without a THC installation.

## Contact Information

Contributions and bug reports are welcome! Please feel free to contact me through
GitHub.

-Edward Kmett
