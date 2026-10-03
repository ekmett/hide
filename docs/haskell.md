# Haskell language tools

Use types and diagnostics while editing, then follow definitions or rename a
symbol across the project. Haskell Language Server works on the current buffers,
including edits you have not saved yet.

## Start HLS

Install Haskell Language Server for the GHC used by your package and put
`haskell-language-server-wrapper` on `PATH`. Open the package or one of its
`.hs` or `.lhs` files:

```sh
thc-edit .
```

The editor starts `haskell-language-server-wrapper --lsp` in the detected project
root. It looks for a Cabal package, `cabal.project`, `hie.yaml`, `stack.yaml` or
Git root while walking upward from the file. HLS reads the project's normal
configuration.

Set `THC_EDIT_HLS` to use another server executable. **Tools > Restart language
server** reconnects after a configuration change. HLS runs independently of the
interface; language requests do not stop you editing.

## Types, definitions and completions

| Action | Keys or menu |
| --- | --- |
| Inspect the type at the cursor | Shift+F1 (⇧F1 on Mac) or Tools > Inspect type |
| Follow a definition | F12 or Search > Go to definition |
| Complete an identifier | Ctrl+Space (⌃Space on Mac) or Edit > Complete identifier |
| Rename a symbol | Right-click source, then Rename |
| Quick fixes and refactorings | Tools > Code actions or right-click source, then Code actions |

Pausing the text cursor shows type information in the status bar. In graphical
frontends, hovering over source does the same. The source context menu also
offers type information, definitions and completion.

Rename applies HLS's edits to the affected buffers, with one undo step per file.
Review those buffers and save them to write the result to disk. If a buffer or
disk file changed while the request was pending, the editor rejects the stale
edit. Text edits are supported; HLS file creation/deletion operations and
executable completion commands are not applied. Completion snippets are disabled.

## Code actions

Select source or place the cursor at a diagnostic, then choose **Code actions**.
The chooser lists HLS quick fixes and refactorings for that range. Select an
action and press Enter to apply its text edits to buffers; review and save them
when ready. HLS may resolve an action lazily when you select it.

Actions can supply text edits, an advertised HLS command, or both. A supplied
edit runs before its command. For example, HLS's **Evaluate...** action evaluates
a doctest comment and inserts its result. Commands may have server-side effects;
only text edits are confined to unsaved editor buffers.

Each edit batch rejects changed source, private or unrelated files, and file
creation/deletion operations as a whole. A later command failure or cancellation
keeps earlier accepted edits; review the status and changed buffers before saving.
Only one command runs at a time per project. Every completed, failed, or canceled
command retires that HLS process, so its late edits cannot affect another action.
The next command needs a fresh action list and may incur HLS startup time;
edit-only actions keep the existing server. Actions from an older list expire
when you request another list or restart HLS.

## Diagnostics

Source-line chevrons mark diagnostics. **Tools > Messages** opens their list at
the bottom of the desktop. Click a message to select it; Enter or double-click
opens its source. **Alt+F8** visits the next message and **Alt+F7** the previous
one, opening the file when needed.

Copy a selected message with Ctrl+C, Ctrl+Insert, or ⌘C on macOS. The copy
includes the full message and source location. Right-click Messages for Go to
source, Copy message, Copy all messages and Hide Messages.

Drag the Messages title bar vertically to make more room. Adjacent editors and
the Files pane follow its edge. Hiding Messages leaves the source markers
available. Editing clears diagnostics for older buffer versions while HLS
checks the new text.

For commands that build from disk, save first. [Running and debugging](running.md)
covers the THC target, terminals and debugger attachment.

## Cabal project browser

**Tools > Project browser** lists the local components in Cabal's existing
`dist-newstyle/cache/plan.json`. Use Prev and Next to move between pages,
then Details to open a read-only window with the package, component, source
root and library/build-tool dependencies. Unresolved dependency IDs remain
visible when their units are absent from the bounded graph.

The chooser reports the plan's compiler, freshness and any omitted units.
Changed manifests or unsaved manifest buffers mark it stale; unchanged
timestamps do not prove it current. Refresh reads the plan again. If no plan
exists, build with Cabal first. Browsing never starts a build, configures a
project, or changes the run target.
