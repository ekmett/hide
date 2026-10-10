<!-- SPDX-FileCopyrightText: 2026 Edward Kmett
SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 -->

# GHC debugging options for hide

Date: 2026-10-02

Status: research and source inspection only. No debugger was installed, built,
or run for this report. “Verified” below means documented upstream or visible
in source, not a successful hide integration test. The editor baseline was
`53c134add77ad3a45a8001b3c478cc1ed41b9c0d`. The inspected `hdb` source was
`af22571abc9d4316ad592815f46d9b430f278014` (2026-09-19); release packages can differ.

The selected follow-on is [a shared debugger interface with hdb integration](hdb-integration.md).
That design records subsequent implementation and live qualification separately.

## Recommendation

Make **GHC source debugging through `hdb` and the existing DAP client** the next
practical experiment. Keep GHCi in an ordinary terminal as the immediately
useful fallback. Treat native debugging, profiling, eventlogs and heap analysis
as separate workflows with different questions and preparation requirements.
Do not embed the GHC API in hide or build a new GHCi-output parser first.

The expected useful first slice is: load one Cabal component, break in project
source, step, stop on an exception, inspect unforced values, continue, and
terminate cleanly. Existing UI and MCP operations cover most of that surface.
The two concrete integration gaps to investigate first are **launch timing** and
**interactive program input**, not a new debugging UI.

| User question | Best initial route | Fit in hide |
| --- | --- | --- |
| Why does this function return the wrong result? | GHCi or `hdb`, interpreted project code | Existing source/breakpoint/scopes UI through DAP; terminal fallback now |
| Where did this exception come from? | Exception breaks; `hdb` stack information; trace history in GHCi | Existing exception filters and stack view, subject to adapter semantics |
| Why did this native executable or FFI call crash? | GDB/LLDB with usable debug information | Existing adapter-config path; not equivalent to Haskell variable inspection |
| Where are time and allocation going? | GHC cost-centre profiling | Run/build recipes and report artifacts, not stepping controls |
| Why are threads blocked or GC dominating? | Eventlog and ThreadScope | Separate timeline/report workflow |
| What retains this large object graph? | `ghc-debug` | Instrumented executable and heap snapshot analysis; separate from DAP |

These are recommendations from the evidence below, not a claim that any one
backend provides all six experiences.

## What the editor already has

[Running and debugging](../running.md), [Debugger.hs](../../src/Hide/Debugger.hs),
[DAP.hs](../../src/Hide/DAP.hs) and [Build.hs](../../src/Hide/Build.hs)
show a largely reusable debugger client:

- Adapter configuration accepts either a subprocess speaking DAP over stdio or
  a loopback TCP endpoint, with adapter-specific `launch`/`attach` arguments.
  It does not interpolate VS Code variables such as `${workspaceFolder}`.
- Continue, pause, step-in/over/out, source breakpoints, advertised exception
  filters, threads, frames, scopes, variables and source retrieval already
  reach the same session from the Debug menu and structured MCP tools.
  Generation checks expire stale frame/variable selections.
- The current client does **not** expose `evaluate`, watch expressions, forcing,
  reverse stepping, conditional/function breakpoints, variable assignment or
  debugger-specific commands. These are not made available just by installing
  an adapter.
- `initialize` explicitly advertises `supportsRunInTerminalRequest: false`.
  The transport rejects reverse requests rather than dispatching them.
  Debug output is an output view, not a program-input channel.
- Every pending DAP request has a 15-second deadline in `tickDebugger`. The
  managed THC path builds before its DAP connection becomes ready. A GHC adapter
  may do significant cradle discovery/compilation *inside* its `launch` request,
  so it can exceed that deadline even while functioning correctly.
- GHC build selection currently means `cabal build`/`cabal run`, or standalone
  `ghc --make`/`runghc`. It does not select a matching debug adapter, construct
  a REPL cradle or enable profiling/debug information. “Select GHC” is therefore
  not yet “Debug with GHC”.

The daemon already owns debugger processes and keeps them across frontend
reattachment. Worktree editor sessions have their own debugger instance. Keep
that ownership model; changing displays should not spawn or reload a debugger.

## GHCi: useful now, with explicit semantic limits

GHCi supplies `:break`, `:continue`, `:step`, `:steplocal`, `:trace`, `:history`,
`:back` and `:forward`. Breakpoints and source stepping require interpreted
modules; compiled dependencies are not made steppable by loading their object
files. `:add *Module` can request interpretation. History requires tracing and
has a bounded size (`-fghci-hist-size`); traversing it is inspection of recorded
evaluation positions, not rollback of I/O. `:sprint` displays without forcing;
`:print` can name unevaluated subterms; `:force` evaluates thunks and may throw,
loop or change what later inspection sees. Exception stops distinguish
`-fbreak-on-exception` from uncaught-error `-fbreak-on-error`.
[GHCi 9.14.1 guide](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/ghci.html#the-ghci-debugger).

A terminal session running the project's `cabal repl` is the lowest-effort way
to make these facilities available with interactive input. It does not give
structured source-following or MCP variable handles. A plain terminal should
remain a manual workflow, rather than being quietly scraped into a second
partial debugger protocol.

GHC 9.14 improves breakpoint placement in `do`, interpreter performance and
APIs, and introduces `:stepout` as a technology preview. Those improvements are
reasons to start qualification on 9.14, not to promise identical behavior on
older compilers. [GHC 9.14.1 release notes](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/9.14.1-notes.html#ghci).

## DAP adapters

### Preferred: Well-Typed `haskell-debugger` / `hdb`

The project documents source/exception breakpoints, step-in/over/out, thunk-aware
inspection and a stopped REPL. Its supported stable starting point is GHC 9.14;
installation differs on Windows. It is an external DAP application, so hide
does not need to acquire a GHC-library dependency.
[Project installation and configuration](https://well-typed.github.io/haskell-debugger/).

Unlike the older GHCi wrapper, `hdb` uses GHC's API and `hie-bios` for project
configuration. The adapter and runtime compiler must match; the project's
bindist wrapper also checks compiler/boot-library compatibility. The README
notes a multiple-home-unit workaround using a cradle. A broad Cabal version
bound is not evidence that every compiler, package database, platform and
multi-component project works.
[Repository documentation](https://github.com/well-typed/haskell-debugger).

Maintenance is active: the changelog records 0.12's external interpreter,
exception/call-stack improvements and Windows support, then 0.13's terminal
integration and later breakpoint/forcing fixes. This is stronger evidence than
an abandoned adapter, but also a reason to pin a tested release rather than
assuming all versions behave alike. Compiled frames can appear through IPE
information; that does not imply source-stepping compiled dependencies.
[Changelog](https://github.com/well-typed/haskell-debugger/blob/master/CHANGELOG.md).

**Source-verified transport detail:** this revision provides `hdb server --port
PORT` for TCP DAP; the default command is a CLI debugger. Thus `"command":
["hdb"]` is not a valid stdio-DAP recipe. Start a qualified server separately
for the first experiment and use an adapter configuration with `request:
"launch"`; the bare Debug > Attach action sends different arguments.
[CLI parser](https://github.com/well-typed/haskell-debugger/blob/af22571abc9d4316ad592815f46d9b430f278014/hdb/Development/Debug/Options/Parser.hs).

Illustrative configuration, **not runtime-validated**:

```json
{
  "host": "127.0.0.1",
  "port": 4712,
  "request": "launch",
  "arguments": {
    "projectRoot": "/absolute/project",
    "entryFile": "app/Main.hs",
    "entryPoint": "main",
    "entryArgs": [],
    "extraGhcArgs": []
  }
}
```

Choose an unused port and verify the adapter itself binds only to the intended
local interface. The editor's loopback connection restriction does not constrain
an independently started server's listener. The example selects a source entry
point, not the native executable produced by `cabal build`.

**Input is a qualification boundary, not an automatic blocker.** At the inspected
revision `hdb` reads the client's `supportsRunInTerminalRequest`; false selects
a fallback rather than requiring a reverse request. Windows explicitly disables
this terminal route upstream. In the external-interpreter fallback, stdout and
stderr are forwarded as DAP output, while stdin is a pipe with no forwarding
path in that implementation. Therefore an output-only first test is plausible;
interactive `getLine` is not a supported hide workflow established by this
research. [Capability selection](https://github.com/well-typed/haskell-debugger/blob/af22571abc9d4316ad592815f46d9b430f278014/hdb-dap/Development/Debug/Adapter/Server.hs),
[interpreter I/O paths](https://github.com/well-typed/haskell-debugger/blob/af22571abc9d4316ad592815f46d9b430f278014/hdb-dap/Development/Debug/Adapter/DAPDebuggee.hs).

**Engineering recommendation:** first qualify that fallback using the current
client. Then add owned server launch/readiness and a bounded, cancellable launch
phase suitable for cradle loading. For interactive debugging, implement the
standard reverse request using the existing terminal backend only where the
adapter supports it. Validate argv, cwd, environment and process ownership;
never treat a reverse request as permission to execute arbitrary shell text.
Do not advertise the capability before its cleanup and response path exist.
[DAP specification](https://microsoft.github.io/debug-adapter-protocol/specification).

### Compatibility option: Phoityne `haskell-debug-adapter` + `ghci-dap`

This is a DAP frontend around a modified GHCi, with separate `haskell-dap`,
`ghci-dap` and adapter packages. Its documentation explicitly describes the
implementation as experimental and lists `.hs`-only source and unavailable
stdin while debugging. It is a plausible optional older-GHC route for
noninteractive examples, not the recommended basis for new terminal support.
[Adapter README](https://github.com/phoityne/haskell-debug-adapter/blob/master/README.md).

Do not describe it as abandoned or assume it stops at 9.12: the current
`ghci-dap` README advertises GHC 9.10, 9.12 and 9.14 and includes version-specific
implementation directories. Pin and test the complete compiler/package tuple;
there is no verified hide compatibility matrix here.
[`ghci-dap`](https://github.com/phoityne/ghci-dap).

## Native executables: DWARF, GDB and LLDB

GHC emits DWARF with `-g`/`-g2`; `-g1` provides less unwinding detail and `-g3`
adds GHC-specific information. Debug info does not disable optimization.
Reliable unwinding requires suitable information throughout dependencies,
including foreign code. Source locations can be approximate and inspecting
Haskell heap bindings remains difficult. The 9.14.1 manual's architecture claim
is x86-64/i386; do not extrapolate that into a tested Apple Silicon or Windows
support promise. macOS documentation also calls for `dsymutil`.
[GHC compiled-program debugging](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/debug-info.html).

Recommendation: use this route for native crashes, RTS/FFI investigations and
machine-level stops. Qualify actual source mapping and unwinding separately
from whether an adapter launches. “LLDB can launch it” is insufficient evidence
for pleasant Haskell source debugging.

`lldb-dap` already matches hide's subprocess-DAP configuration shape. GDB
also documents a DAP interpreter (`--interpreter=dap`), so a new GDB/MI client
is unnecessary for an initial native experiment. Actual installed binaries,
platform attach restrictions, core dumps and debug symbols still need checking.
[LLDB DAP documentation](https://lldb.llvm.org/use/lldbdap.html),
[GDB interpreters](https://www.sourceware.org/gdb/current/onlinedocs/gdb.html/Interpreters.html).

## Performance and heap investigations

**Profiling:** use cost-centre time/allocation reports for expensive paths and
heap profiles for residency questions. Profiling builds require appropriate
profiled libraries; automatic cost centres and their placement affect results.
`-fprof-late` reduces interference with Core optimization, with attribution
tradeoffs for inlining. A `.prof` report from `+RTS -p` is an artifact, not a
stopped stack frame. Keep profiling flags in a separate build profile rather
than silently changing the user's ordinary build.
[GHC profiling guide](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/profiling.html).

**Eventlog:** collect scheduling, GC and user events from native runs. Runtime
options control logging, destination and flushing; `ghc-events show` can provide
a textual inspection path. [RTS eventlog controls](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/runtime_control.html#rts-options-for-tracing),
[event format](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/eventlog-formats.html).
Use [ThreadScope](https://github.com/haskell/ThreadScope) for parallel timelines
and [eventlog2html](https://github.com/mpickering/eventlog2html) for shareable
reports. Recommendation: capture and open artifacts first; do not implement a
live timeline before users need one. Event logs cannot answer arbitrary local
variable queries or rewind side effects.

**`ghc-debug`:** instrument the application with `withGhcDebug`, connect to its
socket, pause it and inspect heap roots/retainers or save a heap snapshot.
Source attribution benefits from `-finfo-table-map` and optionally distinct
constructor tables. This addresses retained objects and heap structure, not
source stepping. Snapshots allow later offline analysis.
[Project guide](https://ghc.gitlab.haskell.org/ghc-debug/).
Maintenance continues: the maintainers report 0.8.0.0, bounds updates, FreeBSD
fixes and non-moving-GC work in 2026. That evidence does not establish a current
Windows or every-GHC compatibility matrix.
[Maintainer activity report](https://www.well-typed.com/blog/2026/06/haskell-ecosystem-report-march-may-2026/#ghc-debug).
Keep heap dumps private: they may contain application credentials and user data.
Prefer an explicit snapshot workflow over an unauthenticated remote heap port.

## UI, MCP, remote and laziness constraints

These are integration recommendations, not new implemented features:

- Keep inspection non-forcing by default. If `evaluate`/forcing is added, give it
  a distinct executing operation, explicit intent, cancellation and limits.
  A timeout cannot undo I/O or thunk evaluation. Do not classify arbitrary
  debugger expressions as read-only MCP inspection.
- Treat variable expansion/custom visualizers as adapter-dependent behavior.
  Verify with a thunk whose evaluation has an observable marker; do not infer
  non-forcing semantics from a button named “Scopes”. Handle adapter
  invalidation and refreshed references before exposing forcing.
- Preserve actual adapter capability and error reporting. GHCi history is not
  necessarily a DAP stack, and a DAP stack is not evidence of reversible
  execution. Do not enable Step Back merely because GHCi has `:back`.
- Run adapter, compiler, cradle and debuggee on the editor session host. Existing
  SSH display transport then needs no extra exposed debugger port; paths in
  configuration refer to that host. Native remote debugging on a different host
  is a separate adapter-specific setup, not the current loopback attach path.
- Opening dirty source does not make those unsaved bytes the loaded program.
  Save explicitly or reject a launch against a changed source snapshot. Retain
  loaded-source identity, and expire breakpoints/frames appropriately after
  reload. Never silently save for debugging.
- Keep adapter execution under the existing tool permissions. A debugger can
  execute project code even without an evaluation tool. A recovered editor
  checkpoint must not pretend a dead debugger process is still attached.

## Staged qualification plan

1. **No new client architecture:** on GHC 9.14, pin an `hdb` release and matching
   compiler. Use a separately owned local server and the existing TCP adapter
   configuration. Record versions, cradle, OS and architecture. Test a saved
   one-component executable with no stdin, including lazy locals and an
   exception. If launch exceeds 15 seconds, capture that failure instead of
   calling the adapter incompatible.
2. **Small production integration:** provide explicit GHC-debug launch selection,
   owned server startup and cancellation, useful cold-load progress/deadlines,
   and source-version checks. Reuse existing DAP views/MCP tools. Qualify Cabal
   libraries plus executables, multi-component projects and generated sources.
3. **Interactive input:** implement and test reverse terminal requests on
   supported adapters/platforms. Exercise stdin, Unicode, Ctrl+C, resize,
   disconnect during input and frontend detach/reattach. Upstream Windows
   terminal fallback requires its own answer; POSIX success is not Windows proof.
4. **Deliberate advanced operations:** add evaluation/forcing only with explicit
   execution semantics and stale-reference tests. Keep older-GHC Phoityne
   support optional. Offer native/profiling/eventlog recipes independently.
5. **Heap tooling when demanded:** qualify a `ghc-debug` snapshot recipe and
   bounded reports before designing a live heap browser or new MCP protocol.

Acceptance must include clean termination with no adapter/interpreter leaks,
exception filtering, stale frame rejection, Unicode/path handling, unforced
inspection, and remote session reattachment. Verify macOS, Linux and native
Windows independently. None of those runtime checks was performed for this
report; the useful next action is a bounded `hdb` compatibility experiment, not
an unconditional “GHC debugging supported” label.
