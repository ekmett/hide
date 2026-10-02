# Editing

Open a directory to work on its files, or pass filenames to put them directly
on the desktop:

```sh
thc-edit .
thc-edit src/Main.hs src/Library.hs
```

With no argument, the editor finds the nearest enclosing Cabal package, falling
back to the current directory. A directory argument takes precedence. Opening a
package also opens its `.cabal` file; with several package files, the editor
prefers the directory namesake, then the first alphabetically. Explicit source
files stay selected.

## Files and directories

**F3** opens the file dialog. Enter a name or wildcard filter, select a file and
choose Open. Double-click a directory to browse into it, or a file to open it.
The list shows the file's size and local modification time.

**Ctrl+B**, or **Tools > File tree**, toggles the Files pane. Expand directories
to browse their contents and open files from the tree. Drag the divider to make
room for long names; adjacent windows follow it. Unsaved filenames appear in
red and return to normal after saving or undoing the change.

**File > Change dir** changes the working directory and refreshes Files without
closing open buffers. Enter browses into the selected directory, Browse opens
a typed path, and OK accepts the displayed directory.

## Text and selection

Type to insert text. Hold Shift while moving to select it, or drag with the
mouse. Ctrl+arrows moves by word. **Edit > Select all** selects the buffer.
Copy, Cut and Paste are available in the Edit menu and on Ctrl+C/X/V;
Ctrl+Insert, Shift+Delete and Shift+Insert provide the other familiar bindings.

**Ctrl+Z** undoes and **Ctrl+Shift+Z** redoes (**Ctrl+Y** also works).
On macOS, use Cmd for the Edit-menu shortcuts. Split views share text
and undo history; each view keeps its own cursor and selection. UTF-8 text retains its line endings, including CRLF.
Combining sequences and joined emoji move and delete as complete graphemes;
your terminal or selected frontend supplies their appearance.

Syntax highlighting follows the filename. Haskell, C, C++, Cabal files and over
100 other languages use Skylighting's language definitions. New unnamed text
buffers start with Haskell highlighting; unknown extensions use plain text.
Colors update in the background after edits; the current text remains visible
while coloring is pending. A slow coloring job leaves plain text until the next
edit, and results from older text are discarded.

The native window and browser use the system clipboard. Text terminals keep an
editor-local clipboard and request a system clipboard write through OSC 52 on
Copy and Cut. Whether that reaches your system clipboard depends on the terminal.
Use the terminal's paste command for external text. See
[frontend controls](display.md) for browser shortcut differences and clipboard
permissions.

## Search

**Ctrl+F** opens Find and **Ctrl+H** opens Replace. On macOS, use **Cmd+F**
and **Cmd+Option+F**; Cmd+H keeps its standard Hide action. Find and Replace
share a dialog: click either tab or press **Ctrl+Tab** to switch without losing
the search or replacement text. The corresponding Find/Replace shortcut also
switches tabs. Search is literal and case-sensitive; Replace changes one match.

[![Find and Replace tabs with both search and replacement text.](site/screenshots/find-replace.png)](site/screenshots/find-replace.png)

**Ctrl+L** finds the next match and **Ctrl+Shift+L** the previous match in native
non-Mac and terminal frontends. **Ctrl+R** remains a Replace alias there, and
**Ctrl+G**, or **Search > Go to line**, goes to a line number. F3 still opens files.
On macOS, **Cmd+G** / **Cmd+Shift+G** find the next/previous match. In the browser,
use Ctrl/Cmd+G and Shift+Ctrl/Cmd+G; use the menu for Go to line there. Embedded
terminals retain their control keys while the terminal has focus.

## Windows

Use **Window > Split vertically** or **Split horizontally** to keep two parts
of a file visible. A split is another view of the same file, so an edit or Undo
in either window appears in both.

[![Two views of Buffer.hs, split horizontally with independent scroll positions.](site/screenshots/split.png)](site/screenshots/split.png)

Drag a title to move a window, a frame edge to resize that side, or the
bottom-right corner to resize both dimensions. **Window > Tile** arranges
windows alongside one another; **Cascade** staggers them. **F5** zooms the active
window. During a move or resize, arrows move, Shift+arrows resize and Enter
finishes. Escape restores all windows affected by the drag.

Touching edges stay together when you drag a title or move a divider. Equal sides
share the divider; a smaller window whose whole side touches the edge moves with it. Once
that window reaches a desktop boundary, its outer edge stays there and further
divider movement resizes it. The same rule carries a chain of touching windows.
Partial overlaps stay independent, and a diagonal corner drag breaks the contact.

Windows touching a desktop edge grow with that edge when the desktop grows.
Floating windows keep their position and size. Dragging a title preserves size
while there is room, then shrinks the window against the desktop or dock boundary.
Files and Messages remain the boundaries for adjacent windows.

**F6** or **Ctrl+Tab** cycles windows. Shift+Ctrl+Tab reverses direction.
Each editor and Messages window has a stable number: **Alt+1** through **Alt+9**
activates it. On macOS, use Option for these numbered shortcuts.

**Alt+Tab** cycles the menu, Files, windows and Messages; Shift reverses it.
Some operating systems reserve this shortcut. Click the destination or use the
window shortcuts when the OS intercepts it.

## Menus and dialogs

Press **F10** to enter the menu bar, then use arrows or letter mnemonics.
The status bar describes the highlighted action. Its key labels are clickable.
On macOS, the native menu bar also provides Command shortcuts.

[![File popup menu with Open selected in green and native Mac shortcuts.](site/screenshots/file-menu.png)](site/screenshots/file-menu.png)

Tab and Shift+Tab cycle dialog controls. Enter accepts and Escape cancels.
Ctrl or Alt plus a button's marked letter activates it. Dialogs also accept
Alt+Tab for focus traversal. Mouse buttons activate when released over the
button; releasing outside cancels the click.

## Save, close and external changes

**F2** saves. **File > Save as** writes under a new name and refuses to replace
an existing destination. **Alt+F3** closes the current window and **Alt+X**
exits. Closing the last view of a changed file, or exiting with changed files,
asks what to save. **File > Exit** ends the editor session. To leave the desktop
running, detach and use `thc-edit --resume` later; **Ctrl+]** detaches from the
terminal frontend. See [sessions](sessions.md) for the other frontends.

A title star means the buffer has unsaved edits. The branch badge's star means
saved changes in Git; saving a buffer can clear one while setting the other.

The editor watches open files and expanded directories. A clean buffer reloads
when the file changes on disk, retaining Undo. If the buffer also has edits, or
the file was deleted, the editor keeps the versions and offers:

| Choice | Effect |
| --- | --- |
| Compare | Inspect the editor, last-saved and disk versions |
| Reload | Use the disk version |
| Keep | Keep editing your version without overwriting the disk file |
| Save as | Write your version under another name |

**File > Disk changes** reopens the conflict. Keep does not accept the new disk
contents as a save baseline: the next Save still checks the conflict. Repeated
observations of the same change do not repeatedly interrupt you. These checks
also apply after Git operations and conversation edits.

## WordStar keys

Select WordStar under **Options > Preferences**, or start with `--wordstar`.

| Keys | Action |
| --- | --- |
| Ctrl+E / S / D / X | Up / left / right / down |
| Ctrl+A / F | Previous / next word |
| Ctrl+Y | Delete line |
| Ctrl+K, then B / K | Mark block start / end |
| Ctrl+K, then C / V / Y | Copy / cut / delete block |
| Ctrl+K, then S / D | Save / close |
| Ctrl+Q, then S / D | Start / end of line |
| Ctrl+Q, then R / C | Start / end of file |
| Ctrl+Q, then F / A | Find / replace |

Escape cancels a prefix. This is a useful subset of WordStar's commands.

See also [Haskell tools](haskell.md), [Git](git.md) and [hex editing](hex.md).
