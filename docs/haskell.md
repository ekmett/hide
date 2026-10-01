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
| Inspect the type at the cursor | Shift+F1 or Tools > Inspect type |
| Follow a definition | F12 or Search > Go to definition |
| Complete an identifier | Ctrl+Space or Edit > Complete identifier |
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

Only edit-based actions are supported. Command-only actions, actions combining
an edit and command, and file creation/deletion operations are not applied;
the chooser explains why they are unavailable. No attached command is silently
skipped. Changed source or affected files reject the entire edit. Actions from
an older list expire when you request another list or restart HLS.

## Diagnostics

Source-line chevrons mark diagnostics. **Tools > Messages** opens their list at
the bottom of the desktop. Click a message to select it; Enter or double-click
opens its source. **Alt+F8** visits the next message and **Alt+F7** the previous
one, opening the file when needed.

Copy a selected message with Ctrl+C, Ctrl+Insert, or Cmd+C on macOS. The copy
includes the full message and source location. Right-click Messages for Go to
source, Copy message, Copy all messages and Hide Messages.

Drag the Messages title bar vertically to make more room. Adjacent editors and
the Files pane follow its edge. Hiding Messages leaves the source markers
available. Editing clears diagnostics for older buffer versions while HLS
checks the new text.

For commands that build from disk, save first. [Running and debugging](running.md)
covers the THC target, terminals and debugger attachment.
