# Development

Build the editor, run the checks and exercise the frontend affected by a change.
The source stays separate from THC's compiler and runtime.

## Build and check

From the repository root:

```sh
cabal build all
cabal test
```

Native windows, browser support and embedded terminals are enabled by
default. [Installation](install.md) covers their native dependencies. To omit
components, use the same opt-out flags for build and test:

```sh
cabal build all -f-window -f-web -f-terminal
cabal test -f-window -f-web -f-terminal
```

The test suite replays the desktop's pure transitions and checks files using
real temporary directories, including encoding, permissions, symlinks and
external changes. The suite also covers session handling and live editor context.
Optional components add checks for browser transport, rendering assets and
embedded terminals. A passing model check and an interactive frontend check
establish different things; run the relevant frontend too.

## Source documentation

Each `Hide.*` module starts with an overview of its role, ownership and notable
control flow. Public API Haddocks describe caller obligations: coordinate units,
resource lifetime, worker/adoption boundaries, privacy checks and failure behavior.
Keep those contracts beside the owning operation when changing it; avoid repeating
the type signature in prose. The [architecture guide](architecture.md) connects
the subsystems.

Generate the initial API reference and linked source with:

```sh
cabal haddock lib:hide --haddock-html --haddock-hyperlink-source
```

Coverage is preliminary, especially the model's internal helpers and native FFI
exports. A documentation build checks parsing and links, not the truth of the
contracts: review those against implementation and the relevant behavioral tests.

## Plugin command implementation

`Hide.Plugin.Command` is the first implemented part of the
[plugin design](design/haskell-plugins.md). Define a typed `CommandDef` with input
and output codecs and a narrow host-supplied context. `withRegistry` scopes live
registrations; `registerCommand` rejects duplicate names. Native callers use the
typed handle with `invoke`. Wire adapters capture a `CommandRef` before queueing
and use `invokeJSON`, which validates input and prepares the complete JSON result
on the calling worker.

Retirement and scope closure refuse later admission. Re-registering the same
name does not redirect old handles or queued calls. Work already admitted may
finish; the host's task owner is responsible for cancellation. Codecs and handlers
run outside the registry lock. Errors are forced before return; successful typed
values remain lazy and their consumers own later evaluation. Registration grants no authority and does not
automatically expose an MCP tool.

The first consumer is `hide.docs.read` in `Hide.Documentation`. Its context only
resolves a documentation corpus root; it neither imports nor receives `Desktop`.
`Hide.DocsMCP` owns the session registration and adapts the explicit `docs_read`
tool to it, retaining permission checks and deferred filesystem work. Listing and
search are not yet registered commands.

## Immutable plugin buffer reads

`Hide.Plugin.Buffer` provides opaque `BufferRef`, `BufferRead` and `ContentVersion` values.
The host adapter `Hide.Plugin.BufferHost` captures immutable measured trees without
retaining the separate saved baseline, Undo or Redo roots. Deleted provenance
leaves in the live tree remain retained but are invisible to reads.
Public reads use distinct character, byte and
zero-based line coordinates, validate their full range and reject the wrong
representation. `readLines` preserves source line terminators; `readLine` gives
one editor row without its terminator. Whole-buffer reads are explicit worker
operations. Capture and version checks never flatten or compare contents.

`ContentVersion` combines the revision with immutable buffer identity. A new
buffer with the same revision and contents invalidates the captured version.
The identity check is conservative: replacing the immutable buffer to establish
a new saved baseline also requires a fresh version. The live `read_buffer` MCP
consumer now captures through `Hide.BufferReads` during a narrowly admitted
`MCPPermissions.permissionReadCall` callback, then formats measured reads on its
reply worker. ACP source freshness tracking uses the same version check.

`BufferRef` combines the running session namespace with its once-allocated
logical document ID. It survives ordinary edits and reload of that document;
close/reopen and a new daemon/recovery lifetime cannot reuse the reference.
`ContentVersion` separately identifies the immutable content captured from it.
The namespace belongs to the existing Permissions session lifetime, so frontend
detach does not create a new scope.

A read receipt exists only after ordinary MCP policy dispatch or an accepted
approval, and expires when its capture callback returns. Queued authenticated
reads resolve their token and active actor again before capture; anonymous
inspection remains guest input. Privacy comes from the existing GuestAccess
owner, including private-buffer rejection and Conversation masks. Already granted
immutable reads may outlive the receipt and cannot be recalled. Ordinary capture
forces only the cheap measured-content constructor, releasing the separate
Buffer/Undo thunk; Conversation redaction and tree construction stay on the
reply worker. The receipt does not grant edit authority or create a public
plugin CallContext.

This is an implementation slice, not a complete plugin SDK. Plugin
activation/task scopes, public checked buffer edits, menu contributions
and custom widget/window types remain tracked in
[the delivery plan](https://github.com/ekmett/hide/issues/1).

## Host checked edit ownership

`Hide.BufferEdits` owns opaque `PreparedEdit` values, worker preparation
through `Hide.Buffer.replaceRanges`, and all-target checked atomic adoption.
`Hide.Tooling` is the live HLS consumer; it retains protocol parsing, canonical
project admission, task cancellation/session ordering and reply delivery.
Preparation forces replacement trees, output content and selection spans on the
worker. It deliberately retains the file baseline and ordinary Undo state that
will be installed; these owned edits differ from immutable read images.

Under the session lock, `commitEdits` rejects duplicate, closed, replaced,
ambiguous or private targets before changing any buffer. Open targets require
matching buffer version and file-baseline identity. Success installs worker
values, preserves unrelated navigation and rebases current selections. Changed
buffers receive one ordinary Undo entry; saving stays explicit and a stale
result never rebases the proposed edits.

These are host operations, not the public scoped plugin edit service. A caller
must already own the session transition, admit the project/path/source baseline,
and revalidate its task's admission. Public edit admission, cancellation, approval-once continuations and plugin
authority remain part of [the buffer service work](https://github.com/ekmett/hide/issues/4).

## Frontend command routing

Browser shortcuts and menu packets use canonical `hide.*` IDs from `Hide.Commands`.
Short browser spellings are not accepted. Main, native and browser menus share
`menuCommandAvailable`; queued input checks availability again when consumed.
Native menu tokens refer to catalogue commands rather than menu occurrences. Each
native menu lifetime stamps its queued events, so rebuilding the menu rejects old
events; repeated occurrences of an action share enablement updates.

Source context popups capture the window, buffer, revision and character selection.
A changed focus, caret or source revision dismisses the action rather than using a
new target. Source edits and reloads derive from the original buffer and advance
its revision; read-only output/transcript replacement is not an editable source
target. Moving input ownership to the sidebar also refuses a source choice.
Agent choices retain the conversation target. Other parameterized
context actions retain their own arguments. These checks do not replace host input
origin and policy checks. `Hide.Plugin.Menu` composes contributions in declared named slots by group,
order and namespaced entry ID. `withMenus` bounds snapshots to 256 entries;
unknown slots, duplicate IDs and stale lifetimes fail explicitly. `menuAction`
retains typed command arguments and a worker-side presentation adapter. The
metadata snapshot contains no plugin callback or buffer payload.

The live `Hide.MenuCommands` host contributes **Help > Contents** through a
thin `hide.help.contents` command calling the existing scoped `hide.docs.read`
reader. F1, the popup, native catalogue and transported menu packets resolve the
same retained contribution. Documentation reading and Markdown preparation run
on one session worker; adoption preserves read-only Help styling, relative links
and shell-block/navigation metadata. Browser/native frames carry entry IDs and
exact generations plus a fresh non-secret registry nonce. A native catalogue rebuild also stamps a new Cocoa incarnation.

Install linked contributions before publishing the session snapshot. Published
retirement goes through `retireMenuFromHost`, which the serialized session owner
drains before admission and adoption. Do not retire a published command/menu from
an unrelated worker. Shutdown cancels and joins work before closing its registry
scopes. Plugin agent-enable metadata cannot grant authority: the live host only
permits its exact first-party Help ref and retains existing protected-control
policy checks. Origin comes from host dispatch, never frontend JSON.

This slice supports the existing main-menu slots and prepared documentation
replies. Context-menu contributions, runtime activation/publication and full
first-party routing through the typed registry remain open in #2.

`sh tools/check-native.sh` tests the real SDL event queue without a window. On
macOS it also checks an unshown application menu for duplicate enablement and
retained old menu-item stamps. Neither native check starts an editor session.

## Native Windows terminals

With the [pinned Ghostty installation](install.md#embedded-terminal) on `PATH`,
set `$ghosttyPrefix` to its prefix and run these checks from the repository root.
Use the `clang` supplied with native GHC (its `mingw/bin` directory):

```powershell
New-Item -ItemType Directory -Force build/native-terminal | Out-Null
clang -Wall -Wextra "-I$ghosttyPrefix/include" -Icbits cbits/terminal.c test/native-terminal.c "-L$ghosttyPrefix/lib" -lghostty-vt -lshell32 -o build/native-terminal/native-conpty.exe
./build/native-terminal/native-conpty.exe
./build/native-terminal/native-conpty.exe --parent-control
./build/native-terminal/native-conpty.exe --no-console
ghc --make -threaded -XGHC2021 -DWITH_TERMINAL -isrc -itest -outputdir build/native-terminal/hs test/native-terminal.hs cbits/terminal.c "-optc-I$ghosttyPrefix/include" -optc-Icbits "-L$ghosttyPrefix/lib" -lghostty-vt -o build/native-terminal/wrapper-check.exe
./build/native-terminal/wrapper-check.exe (Resolve-Path build/native-terminal/native-conpty.exe)
```

The C fixture exercises real console input, resizing, process-tree termination,
bounded queues and final output. The two additional modes check that starting a
terminal preserves an installed parent handler and works without a parent
console. The Haskell companion checks Unicode paths, argument quoting and
Ctrl-C in `cmd.exe` and PowerShell. Each command must exit successfully.
`cabal test` also checks retained editor terminal windows and ACP terminal requests;
the ACP fixtures require a working `python3` on `PATH`.

## Rendering previews

These previews use the actual Vty rendering output at 80 by 25 without starting
an interactive terminal:

```sh
cabal run -v0 hide -- --demo --snapshot
cabal run -v0 hide -- --demo --scene menu --snapshot-html > menu.html
cabal run -v0 hide -- --demo --scene help --snapshot-html > help.html
```

The HTML is a static preview. Available scenes are `desktop`, `menu`, `about`,
`gallery`, `split`, `open`, `tree`, `help`, `diff` and `preferences`.
**Tools > Widget gallery** exercises the dialog controls interactively.

## Documenting keyboard shortcuts

Verify bindings against `nativeMenuShortcut` in `src/Hide/Window.hs`,
`cbits/menu.m`, `cbits/window.c`, `src/Hide/Frontend.hs`,
`src/Hide/Model.hs` and `assets/web/editor.js`. Native menus, browser
shortcuts and terminal input are different entry points; do not infer a Mac
binding by replacing Ctrl with Command.

Use separate platform columns when bindings differ. For Mac, follow
[Apple’s modifier order](https://developer.apple.com/design/human-interface-guidelines/keyboards):
Fn, Control, Option, Shift, Command. Write symbol combinations without plus
signs (⇧⌘Z, ⌥⌘F), or full key names joined by hyphens (Shift-Command-Z).
Use Return and Esc in Mac columns. Write Ctrl+Shift+Z in Windows/Linux columns.
Repeat modifiers for each alternative; avoid ambiguous forms such as Ctrl/Cmd+G.
The README and [editing guide](editing.md#text-and-selection) carry the main
shortcut tables; update the relevant feature guide when its bindings change.

## Documentation screenshots

The site uses actual Metal captures of the editor running over this checkout,
including its modal dialogs. On macOS, build the window backend and run:

```sh
cabal build lib:hide
mkdir -p build/docs-capture/objects
cabal exec -- ghc -threaded -XGHC2021 -package hide -itools tools/docs-screenshots.hs -outputdir build/docs-capture/objects -o build/docs-capture/screenshots
build/docs-capture/screenshots
make docs
make docs-check
```

`tools/EditorDriver.hs` supplies reusable commands, input events (including their
effects), text entry, condition-based waits, and Metal captures with a colorless
text companion. The screenshot file contains scene recipes and crop selection;
new UI workflows can reuse the driver without adding another event loop or
capture implementation. These are trusted local development scripts. Agents
controlling a live session use the permission-checked MCP tools described in
[Session tools](session-tools.md), including their protected-input rules.

Pass scene names to refresh only affected images, for example
`build/docs-capture/screenshots preferences debug-launch`. The tool loads the real
Cabal file and `src/Hide/Buffer.hs`, invokes the normal editor commands,
and draws hidden Metal windows at 3× scale, up to 100×32 cells, with **CRT filter** and
**Pixelate Unicode** enabled. It needs a macOS graphical login for Metal, but
shows no window and starts no editor session. Personal provider settings are
excluded from the default scenes; no source file is saved and no build or commit
is submitted.

The conversation illustration replays a recorded exchange through the current
renderer by default. Request it explicitly; an optional provider configuration
selects a new live exchange instead. Source stepping requires a live session:

```sh
build/docs-capture/screenshots conversation
# Optional live exchange (contacts the configured provider):
THC_DOCS_AGENT_CONFIG="$HOME/.config/thc-edit/agents.json" build/docs-capture/screenshots conversation
THC_DOCS_DAP_PORT=4730 build/docs-capture/screenshots debug-step
```

Set `THC_DOCS_CAPTURE_DIR` to a temporary directory to preview captures before
replacing the site assets. Recorded replay does not read provider configuration
or start an ACP connection. The optional live conversation asks the configured
provider to read `src/Hide/Buffer.hs` and explain it, then asks a short
follow-up about edit costs. Use a provider permitted to read this checkout; its
configuration is copied only into ignored capture scratch space. The debugger attaches to an
already-running, suspended local DAP endpoint, steps into the program, captures
the source, Debug menu and call-stack picker, then continues it to termination.
Use a disposable toy program that terminates, not an interactive debugging
session. Live modes close their connections on exit. Their compact desktops
keep the status-bar instructions visible. Never stage provider configuration or
session records with the images.

Dialog and menu images are cropped to their actual UI rectangles plus the shadow
and a small margin. Keep full desktops only when window arrangement is the subject.
Avoid repeating the same source background for unrelated controls.

PNG artifacts live in `docs/site/screenshots/`; BMP intermediates stay under
`build/docs-capture/`. Source comments beside the dialog definitions identify
which images and guide pages need refreshing. Changes to shared dialog frames,
fields, buttons or shadows require refreshing all dialog images. Keep the scene
list in `tools/docs-screenshots.hs` and the site's screenshot allowlist in
`tools/docs/Main.hs` together when adding or removing an artifact.

Inspect regenerated images for clipped fields, wrong selections or stale labels
before committing them. Check that each image still illustrates its adjacent
instructions. The site copies only the allowlisted images and validates their
links, including deployment below `/hide/`.

## Live language-server checks

With a compatible HLS and GHC on `PATH`:

```sh
cabal exec -- ghc -threaded -package hide test/HLSLive.hs -o /tmp/thc-hls-live
/tmp/thc-hls-live
cabal exec -- ghc -threaded -package hide test/EditorLive.hs -o /tmp/hide-live
/tmp/hide-live
```

The source also includes native-input, terminal, DAP, browser and remote-session
harnesses under `test/` and `tools/`. Their setup depends on the component under
test; consult the harness before running it against a live session.

## Live debugger checks

`test/DebuggerLive.hs` drives the real editor debugger through the shared
`EditorDriver`, using both normal commands and MCP operations. Compile it after
building the library:

```sh
cabal exec -- ghc -Wall -threaded -XGHC2021 -package hide -itools test/DebuggerLive.hs -outputdir build/debugger-live -o build/debugger-live/check
build/debugger-live/check launch 4734 /absolute/path/to/toy 10 > build/debugger-live/trace.jsonl
```

Use a disposable, terminating THC Cabal executable with debug source notes. The
last argument selects a breakpoint line in the entry source which execution
will encounter after the initial stop (10 is only an example).
The program must have work after that breakpoint for a trace-in stop; the
qualification toy uses nested primitive mutable-variable operations. Choose an
unused port. Launch uses the normal saved **Build target** settings, so select
THC first and provide its compiler, runtime and LLVM tools on `PATH`. To keep
personal settings out of a run, set `XDG_CONFIG_HOME` to a fresh scratch directory;
THC is the default toolchain. Builds still write into the selected project.

The check verifies embedded-source retrieval, a verified breakpoint at the
requested line, a fresh stop on that breakpoint, trace-in/step-over, and
termination. It also exercises step-out when step-over leaves the toy stopped.
It rejects stale suspension generations and saves state/output as JSON lines.
Step-over or step-out may legitimately terminate an optimized toy; the trace
records which controls were exercised. Otherwise the check continues it. It
fails if the program exits before the breakpoint hit or initial trace-in stop.
Each wait is bounded, and failure closes the owned debugger process.

Use `attach PORT PROJECT BREAKPOINT_LINE` instead of `launch` to qualify an
already-suspended local runtime. That process belongs to the caller: record its exit status and
clean it up if the test fails. A successful DAP termination check does not prove
its operating-system exit status. Run separate instances for each THC backend;
this check does not establish locals inspection, interactive stdin, exception
stops or another adapter's compatibility.

The source-linked [GHC debugging assessment](design/ghc-debugging-options.md)
covers candidate adapters, GHCi facilities and separate profiling/heap workflows.

## Finding your way around

| Location | Responsibility |
| --- | --- |
| `app/Main.hs`, `src/Hide/App.hs` | Startup, command-line options and effects |
| `Model.hs`, `Render.hs`, `Buffer.hs` | Desktop interaction, drawing and document state |
| `Files.hs`, `Reconcile.hs` | File access and external changes |
| `Tooling.hs`, `LSP.hs` | HLS integration |
| `Git.hs`, `GitOperations.hs` | Review, commits and background Git commands |
| `Conversation.hs`, `ACP.hs`, `AgentFiles.hs`, `EditorMCP.hs` | Conversations, provider protocol, editor-mediated files and live buffer context |
| `Terminal.hs`, `Consoles.hs` | Embedded terminal and process management |
| `Debugger.hs`, `DAP.hs` | Debugger interaction and protocol |
| `Window.hs`, `Web.hs`, `BrowserServer.hs` | Native and browser frontends |
| `Session.hs`, `Remote.hs`, `RemoteEndpoint.hs` | Session records, processes and connections |
| `RemoteTerminal.hs`, `RemoteWindow.hs`, `RemoteWeb.hs` | Terminal, native and browser session frontends |
| `Protocol.hs`, `Font.hs`, `cbits/`, `assets/` | Display transport, fonts and native support |

The module filenames above are under `src/Hide/` unless a directory is shown.
[Architecture](architecture.md) explains how they fit together.

## Documentation and attribution

Build the documentation site with Pandoc and the separate Haskell generator:

```sh
make docs
make docs-check
```

The site is written to `build/site`. The generator builds independently of the
editor and checks local links, fragments and revision-pinned source links.
The Documentation workflow rebuilds and publishes every push to `main` at
[ekmett.github.io/hide](https://ekmett.github.io/hide/). It can also be
started manually from GitHub Actions.

Keep the [README](../README.md) useful both on GitHub and as F1 Help. Put detailed
workflows in the [user guide](README.md), and keep design history in `design/`
and `plans/`. User documentation should name the action, menu or key needed to
perform it.

Bundled font terms and provenance are in [assets/fonts](../assets/fonts/README.md).
Skylighting and its bundled grammar set are GPL-2 licensed; skylighting-core is
BSD-3-Clause. These are editor dependencies. The
[Turbo Pascal UI museum](https://ilyabirman.net/meanwhile/all/ui-museum-turbo-pascal-7-1/)
records the interface reference.

Run `sh test/launchers.sh` after changing the adjacent `thc-edit` or `th` launchers.
