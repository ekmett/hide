# thc-edit

Turbo Haskell's source editor.

I'm building a Haskell environment where source, types, diagnostics, conversations
and running programs live in the same place. Open a package, follow a definition,
change the code and review the result without having to reconstruct what you were
doing in another application.

The editor has menus, overlapping windows and shared split views. It runs in a
terminal, a native Metal/Vulkan window, or a browser. The same desktop can work
on a remote machine over SSH, with the files and tools beside the project and
the display in front of you.

## Open a project

```sh
thc-edit .
```

This opens the current directory in the Files pane and its Cabal package file,
when there is one. Pass source files to start with those instead:

```sh
thc-edit Main.hs Other.hs
```

Use `--window` for a native window or `--web` for the browser. The default is the
terminal. To get the executable, see [Build and run](#build-and-run) below.

Each desktop runs in its own session. Detach and return with `thc-edit --resume`,
or choose a different display with `thc-edit --web --resume`. In a terminal,
**Ctrl+]** detaches; **File > Exit** finishes the session. [Sessions](docs/sessions.md)
covers choosing an unfinished desktop and continuing on another frontend.

## Working in the editor

Use **F3** to open a file, **F2** to save and **F10** to enter the menus. **F1**
opens this guide. Menu descriptions and clickable shortcuts appear in the status
bar. On a Mac, Command+O/S/W/Q also opens, saves, closes and exits; function keys
may need Fn/Globe.

The Files pane follows the selected directory. **Ctrl+B** shows or hides it.
Drag a window by its title, resize it from the corner, or use **Window > Tile**
and **Window > Cascade**. **Window > Split vertically** and **Split horizontally**
open another view of the same buffer, including its undo history. **F6** cycles
windows; **Alt+1** through **Alt+9** selects a numbered window directly.

| Action | Keys |
| --- | --- |
| Help / save / open | F1 / F2 / F3 |
| Zoom / next window / menus | F5 / F6 / F10 |
| Close / exit | Alt+F3 / Alt+X |
| Select / move by word | Shift+arrows / Ctrl+arrows |
| Undo / redo | Ctrl+Z / Ctrl+Y |
| Copy / cut / paste | Ctrl+C / Ctrl+X / Ctrl+V |
| Find / replace / search again | Ctrl+F / Ctrl+R / Ctrl+L |
| Go to line | Ctrl+G |
| Dialog next / previous / accept / cancel | Tab / Shift+Tab / Enter / Escape |

In the browser, Ctrl/Cmd+G finds the next match; use **Search > Go to line** for
a line number. Native and browser frontends use the system clipboard. In a text
terminal, Copy and Cut also request the system clipboard through OSC 52 where
supported; paste external text through your terminal. **Options > Preferences** also offers WordStar keys.

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

**Run > Run** (**Ctrl+F9**) runs the package through THC in an embedded terminal.
Use **Run > Target** to choose an executable. This needs the optional terminal
build and a current THC installation. **Debug > Attach** connects to a running
DAP server so you can set breakpoints, step and inspect the stack and variables.
The [Haskell guide](docs/haskell.md) covers language tools; [running and
debugging](docs/running.md) covers setup and the attach workflow.

## Review and conversation

**Tools > Git diff** shows saved staged, unstaged and untracked changes.
**Tools > Approve changes** commits all reviewed saved changes in the repository
after asking for a message. Save buffers before reviewing. The branch badge
shows repository status; right-click it for Fetch, Pull and Merge. The
[Git guide](docs/git.md) explains what is reviewed and committed.

Configure an ACP provider under **Options > Agents**, then open
**Tools > Conversation**. The provider can inspect live window titles, buffer
contents and selections through the editor’s MCP tools, including unsaved text.
**Enter** sends a query,
**Shift+Enter** adds a line and **Escape** cancels an active reply. Queries sent
while a reply is running are queued; **Ctrl+Enter** steers when supported by the
provider. Click the conversation title to choose its model and reasoning effort.

Replies, code and tool activity remain beside your files. Permission requests
let you inspect proposed work before allowing it; editor-mediated file changes
preserve Undo. The [conversation guide](docs/conversations.md) covers provider
setup, review and resuming sessions.

## Work on another machine

With `thc-edit` installed on both machines, open a remote project using an
SSH host or alias:

```sh
thc-edit --window buildbox:projects/example
thc-edit --web buildbox:projects/example
```

Files, HLS, Git and project commands run there. Drawing and clipboard access
stay local. A dropped connection leaves the session running so you can reconnect
to the same buffers. `thc-edit --resume` also remembers the host for remote
sessions. The [remote guide](docs/remote.md) covers installation and paths.

## Build and run

You need GHC 9.6 or newer, Cabal, `pkg-config` and utf8proc 2.10 or newer. On
macOS, `brew install utf8proc` supplies the latter. From the repository root:

```sh
cabal run thc-edit -- .
```

For a native window, install SDL3 (`brew install sdl3` on macOS) and enable the
`window` flag. The browser frontend uses the `web` flag and needs no SDL:

```sh
cabal run -fwindow thc-edit -- --window .
cabal run -fweb thc-edit -- --web .
```

`--window` selects Metal on macOS and Vulkan elsewhere. To install the executable
on your path:

```sh
cabal install exe:thc-edit --installdir="$HOME/.local/bin"
```

Add `-fwindow` or `-fweb` for those displays. Sessions and SSH editing are included
in the default build. [Installation](docs/install.md) covers Linux dependencies,
optional tools and the embedded terminal.

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
