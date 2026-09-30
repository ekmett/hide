# THC Edit: Turbo Haskell's editor

Status: proposed design for review; implementation has not started.

## Intent and decisions

Build a useful Haskell editor whose interface deliberately resembles Borland
Turbo Pascal circa 6.0. Fidelity is central to the joke: the windows, menus,
dialogs, palette and interactions should feel original, while editing and
language tooling work by modern expectations.

The repository and executable are both `thc-edit`. This is an independent
Haskell project. Its dependencies do not enter THC's compiler or runtime build.
THC's proposed external command dispatch will make `thc edit ...` launch
`thc-edit ...`, with built-ins retaining precedence. Direct invocation works
independently of that dispatch being implemented.

Use Haskell and Vty, with `vty-crossplatform` selecting the terminal backend.
Use the existing `text`, `containers`, `bytestring`, `directory`, `filepath`,
`process` and `stm` packages where appropriate. Add protocol packages only when
implementing HLS. Do not import Turbo Vision, Scintilla, or a C++ build system.
Brick is an alternative if a concrete widget saves substantial work, not an
initial dependency: this desktop needs precise control of overlapping windows,
frame cells, focus and hit testing. A graphical frontend is a fallback only if
terminal limitations prevent the intended experience.

## Desktop and visual contract

Use an 80-by-25 cell layout as the reference, adapting to larger terminals.
Preserve blue editor backgrounds, light gray menus and dialogs, green selected
menu rows, highlighted mnemonic letters, hard offset shadows, stippled desktop,
single inactive borders and double active borders. Match title positions,
window numbers, close/zoom handles, scrollbars and the bottom key legend.
The supplied TP 7.1 museum images are a visual reference for the requested
TP 6-era character; do not describe them as exact TP 6 screenshots.

The top menu keeps the familiar File, Edit, Search, Run, Compile, Debug, Tools,
Options, Window, Help order. Modern functionality lives inside these menus,
classic dialogs, completion lists and tool windows. Unimplemented commands are
visibly disabled and explain why; they must not simulate successful actions.

A desktop owns window order, focused window, the active modal dialog, and any
mouse capture during a drag. It routes pointer events to the topmost eligible
view using the same rectangles used for drawing. A modal dialog prevents input
from reaching windows behind it. Closing menus/dialogs restores prior focus.
Implement overlapping windows, cascade, horizontal/vertical tile, zoom, move,
resize, scrollbars, mouse selection and shared-buffer split views.

F1 opens help; F2 saves; F3 opens; F5 zooms; F6 cycles windows; F10 selects the
menu; Alt+X exits; Alt+F3 closes. Menus also support Alt mnemonics, arrow keys,
Enter and Escape. Keyboard equivalents remain available when a terminal cannot
deliver a modified key. Mouse support is optional at runtime, never required.
Terminal shrink/expand must keep dialogs and focused controls reachable.

## Editing and files

One buffer owns text, file identity, dirty state, undo/redo history and revision.
Each view owns its cursor, selection and scroll position; splitting a buffer
creates a view, not another independent copy. Begin with standard text and
sequence structures; measure before introducing a rope or piece table.

Support insertion/deletion, word and line movement, selection, cut/copy/paste,
undo/redo, indentation, find/replace, go-to-line, multiple files and Haskell
syntax highlighting. Handle nested block comments, pragmas, strings, character
literals and apostrophes in identifiers. Highlighting must work without HLS.
Bracketed paste is a single editing action, not a stream of editor commands.

Keep byte offsets, Unicode character positions, terminal display columns and
LSP positions distinct. Tabs, wide characters and combining marks must not
corrupt selections or target the wrong source location. Preserve line endings
and final-newline state. Reject unsupported encodings with an actionable error
rather than silently rewriting them.

Save through a temporary sibling and checked replacement, preserving applicable
permissions. Detect changes on disk before overwriting. Handle symbolic links
deliberately. Save failure keeps the buffer dirty and original content intact.
Closing or exiting with modified buffers offers Save, Discard and Cancel.

Options -> Editor selects modern or WordStar bindings. WordStar supports the
Ctrl+E/S/D/X movement diamond, Ctrl+A/F word movement, Ctrl+Y delete-line, and
Ctrl+K / Ctrl+Q prefixes for block and navigation commands. Show a pending prefix
in the status line; Escape cancels it. Both layouts issue the same editor
commands, so saving and undo behavior cannot diverge. Document the complete
implemented key table in F1 help.

## Haskell tooling and projects

`thc-edit FILE.hs` opens or creates a file; no argument opens the desktop.
`thc-edit DIRECTORY` opens a project browser. Explicit `--project` removes
ambiguity for other project forms. Recognize `--` before filenames.

HLS runs as a separate process in the project root, normally via
`haskell-language-server-wrapper --lsp`. Expose its executable in Options.
Missing HLS or an unsupported project GHC leaves ordinary editing available
and presents a precise status and log. HLS uses its own project cradle;
THC compilation and HLS type checking remain separate operations.

Implement initialization/capability negotiation, document synchronization,
diagnostics, hover, completion and definition navigation first. Formatting,
rename and code actions follow with version-checked edits and undo grouping.
Advertise only supported capabilities. Map UTF-8 buffers to the negotiated LSP
position encoding, discard stale responses, keep server I/O off the UI loop,
and shut down/reap the server on exit. Protocol output and server stderr never
write onto the editor screen. Use existing LSP types and JSON support rather
than inventing another protocol schema.

The project browser is package/component/module-oriented. A Cabal plan lists
units, not every source file: combine it with component build information or
Cabal-library metadata. Honor selected flags and source directories. Distinguish
local editable files, generated files and dependency sources. Missing metadata
or sources are visible limitations, not a silently substituted recursive glob.
Coordinate a read-only metadata contract with thc mba/thc linux, reusing THC's
existing plan/build-info knowledge without invoking Core acquisition just to
browse a project. Opening a file must not require a project build.

## Humor

Keep the original menu vocabulary and earnest dialog presentation. Place jokes
in About, help and idle messages: “640K ought to be enough for any thunk”,
“Destination: Weak Head Normal Form”, and “Turbo Haskell — now with fewer
assignment statements.” Keep destructive prompts and diagnostics unambiguous.
Do not claim a compiler action or debugger feature works merely for the joke.

## Delivery and checks

1. Build the terminal desktop and a widget gallery: menus, buttons, lists,
   scrollbars, radio/check controls, input fields and modal dialogs. Exercise
   mouse capture, focus restoration, resize, tile/cascade and keyboard access.
2. Add complete local editing and shared-buffer views, Haskell highlighting,
   file dialogs, safe saves, find/replace and both keymaps.
3. Add HLS-backed tooling and the Cabal project browser behind those established
   window/dialog conventions. Wire actual Run/Compile actions when their
   command and output handling are implemented.

Keep state transitions testable without a terminal. Replay deterministic input
sequences and compare character/attribute grids for key reference screens at
80x25 and larger sizes. Test click-through prevention, drag capture, clipped
hit targets, split-buffer edits, undo, Unicode positions and failed/conflicting
saves. Use a small fake LSP server for framing, delayed responses and failures,
then demonstrate real HLS diagnostics and navigation on a compatible fixture.
Run actual terminal smoke checks on macOS and Linux with the two collaborating
THC chats. Report tested behavior separately from planned features.

The first milestone is a faithful, interactive desktop harness, not a static
blue mockup. Core desktop and editing behavior must be sound before additional
novelty commands expand the surface area.

## References

- UI reference: https://ilyabirman.net/meanwhile/all/ui-museum-turbo-pascal-7-1/
- Vty: https://github.com/jtdaugherty/vty
- Platform selection: https://github.com/jtdaugherty/vty-crossplatform
- HLS configuration: https://haskell-language-server.readthedocs.io/en/latest/configuration.html
- Alternative considered: https://github.com/magiblot/turbo
