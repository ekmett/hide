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
cabal run -v0 thc-edit -- --demo --snapshot
cabal run -v0 thc-edit -- --demo --scene menu --snapshot-html > menu.html
cabal run -v0 thc-edit -- --demo --scene help --snapshot-html > help.html
```

The HTML is a static preview. Available scenes are `desktop`, `menu`, `about`,
`gallery`, `split`, `open`, `tree`, `help`, `diff` and `preferences`.
**Tools > Widget gallery** exercises the dialog controls interactively.

## Documentation screenshots

The site uses actual Metal captures of the editor running over this checkout,
including its modal dialogs. On macOS, build the window backend and run:

```sh
cabal build lib:thc-edit
mkdir -p build/docs-capture/objects
cabal exec -- ghc -threaded -XGHC2021 -package thc-edit -itools tools/docs-screenshots.hs -outputdir build/docs-capture/objects -o build/docs-capture/screenshots
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
Cabal file and `src/THC/Edit/Buffer.hs`, invokes the normal editor commands,
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
provider to read `src/THC/Edit/Buffer.hs` and explain it, then asks a short
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
links, including deployment below `/thc-edit/`.

## Live language-server checks

With a compatible HLS and GHC on `PATH`:

```sh
cabal exec -- ghc -threaded -package thc-edit test/HLSLive.hs -o /tmp/thc-hls-live
/tmp/thc-hls-live
cabal exec -- ghc -threaded -package thc-edit test/EditorLive.hs -o /tmp/thc-editor-live
/tmp/thc-editor-live
```

The source also includes native-input, terminal, DAP, browser and remote-session
harnesses under `test/` and `tools/`. Their setup depends on the component under
test; consult the harness before running it against a live session.

## Live debugger checks

`test/DebuggerLive.hs` drives the real editor debugger through the shared
`EditorDriver`, using both normal commands and MCP operations. Compile it after
building the library:

```sh
cabal exec -- ghc -Wall -threaded -XGHC2021 -package thc-edit -itools test/DebuggerLive.hs -outputdir build/debugger-live -o build/debugger-live/check
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
| `app/Main.hs`, `src/THC/Edit/App.hs` | Startup, command-line options and effects |
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

The module filenames above are under `src/THC/Edit/` unless a directory is shown.
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
[ekmett.github.io/thc-edit](https://ekmett.github.io/thc-edit/). It can also be
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
