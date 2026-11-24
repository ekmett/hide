<!-- SPDX-FileCopyrightText: 2026 Edward Kmett
SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 -->

# GHC debugging through hdb

Date: 2026-10-02. Selected direction: use `hdb`, sharing the editor's DAP
session, source windows and MCP tools. Saved-source launch, breakpoints,
inspection, stepping and explicit scalar Watches have live qualification;
the remaining work is listed below. Upstream inspected:
[`af22571`](https://github.com/well-typed/haskell-debugger/tree/af22571abc9d4316ad592815f46d9b430f278014),
package version 0.14.0.0. See the [options assessment](ghc-debugging-options.md)
for alternatives and platform qualification.

## Shared interface first

Use one Debug menu, breakpoint editor, stack/scopes browser, expression console
and MCP vocabulary for THC, GHC and other DAP adapters. Describe semantic
capabilities (inspection, explicit evaluation, exception details, conditional
breakpoints, thread controls) rather than presenting separate hdb and THC modes.
Launch configuration is backend-specific; ordinary debugging is not.

Distinguish advertised DAP support from qualified behavior. Known adapter gaps
such as hdb's no-op Pause need an internal compatibility restriction, not a
broken generic button. Put new facilities into the common interface when both
backends could implement the same contract. Only expose a backend-specific
addition when it answers a useful question that the shared contract cannot.

Track improvements independently:

| Shared contract | GHC/hdb work | THC work |
| --- | --- | --- |
| Locals and value tree without implicit execution | Classify lazy references and validate visualizers | Add useful scopes and lazy-safe values to runtime DAP |
| Explicit force/evaluate | Expose existing execution behavior with invalidation | Assess runtime evaluation semantics; do not imitate hdb by assumption |
| Source stepping and breakpoints | Cradle/component mapping and interpreted source coverage | Improve source spans and readable runtime frame names across backends |
| Exception details | Surface hdb's exceptionInfo | Qualify Graal exception details for Haskell failures |
| Threads and execution control | Qualify GHC 9.14.3+ thread support | Qualify THC thread identity, suspension and continuation behavior |
| Program I/O and lifecycle | Owned interpreter terminal, completion ordering | Managed run process, detach/stop and reliable completion |

These are independent backlog items, not prerequisites for exposing the common
features already verified on each backend. Keep an unsupported capability
visible as unavailable where that helps the user understand their session;
never label unimplemented behavior as supported just because DAP has a request.

## The user experience

Select **GHC** in the status-bar dropdown. The matching external hdb owns its
GHC version; the selected compiler must agree with Cabal's project compiler.
A custom compiler command requires an explicit adapter configuration.

| Launch path | Entry and cradle | Live qualification |
| --- | --- | --- |
| **Debug > Launch > Selected target** | Current saved Haskell source, saved compiler and program arguments; hdb discovers its governing cradle | Direct and bios cradles on macOS |
| Cabal sidebar **Debug** | Captured runnable component and checked `main-is`; an owned explicit Cabal cradle selects the component. A governing `hie.yaml` or `.hie-bios` requires Adapter config | Library plus executable on macOS, including custom-cradle refusal |
| **Debug > Launch > Adapter config** | Configuration supplies project, entry, cradle and adapter arguments; custom cradle files stay owned by the project | Direct and explicit custom Cabal cradles on macOS |

The sidebar action preserves the saved run target and the source that was active
when it was invoked. Conditional or ambiguous entry paths still require Adapter
config. A normal GHC native build remains available independently.

Use the same Breakpoints, Threads, Call stack and Scopes controls as THC.
Expanding an ordinary value shows its children. An unevaluated value displays
`<thunk>`; read-only expansion refuses to force it. Persistent **Watches** now
have explicit human-only **Evaluate watch** actions with stopped-frame and
expression-revision provenance. Root Force is implemented for a known lazy root
result, but remains fixture-qualified; hdb Evaluate does not supply that lazy hint.
Nested lazy watch children have a human-only Force action; it retires the old
subtree and requires explicit expression refresh. This route has live hdb list-tail
qualification. The expression console remains planned.
An agent can inspect in the background and reveal the same source/frame to the
user with `debug_present`; it never gets a second hidden debugger state.

## Capability map

Editor exposure below distinguishes implemented behavior, live qualification
and planned controls.

| Facility | Upstream at inspected revision | Editor exposure |
| --- | --- | --- |
| Source breakpoints, step in/over/out, continue | Implemented | Line breakpoint UI/MCP and stepping implemented; saved-source and Cabal entry loading qualified on macOS |
| Threads, frames, locals/module/globals | Implemented | Existing pickers; preserve frame/thread identity and paging bounds |
| Ordinary variable children | Implemented | Existing expansion after validating reference provenance |
| Thunk inspection/forcing | Lazy child presentation hint; fetching lazy children forces | Read-only expansion refuses lazy references; nested-watch Force has live hdb list-tail qualification; root Force is fixture-tested, executing MCP tool remains planned |
| Expression evaluation | `evaluate` with optional frame; returns value/type/reference without a lazy root hint | Manually evaluated Watches have live scalar/list qualification; console, history and executing MCP tool remain planned |
| Source conditions, hit counts, logpoints | Implemented; hit count is parsed as an integer | Shared human editor and executing MCP conditions/hit counts preserve requested fields, disabled state and restart; logpoints pending program-terminal source privacy |
| Function breakpoints | Implemented, including condition/hit count | Planned; no function-breakpoint request or editor yet |
| Exception filters and details | Implemented | Existing filters plus exception details panel/tool |
| Valid breakpoint locations | Single-position query returns an executable span | Current-source chooser and read-only inspection; captured-source and UTF-16 validation; no range enumeration or breakpoint mutation |
| Value formatting | Advertised; `format.hex` ignored by `variables` and `evaluate` at this pin | Live audit confirms decimal-only machine values; formatting controls await adapter implementation |
| Program input | POSIX reverse terminal route; Windows route disabled | Shared owned terminal; macOS stdin, Unicode and Ctrl+C qualified |
| Stop | Terminate and disconnect destroy debuggee | Stop; no misleading “detach and leave running” for hdb |
| Pause, request cancellation | Pause handler is a no-op; cancel unsupported | Disable Pause for hdb; Stop/relaunch is recovery, not resumable cancellation |
| Reverse step, restart frame, assign value, memory/disassembly | Unsupported | Do not offer these controls for hdb |
| Concurrent thread stepping | Conditional on GHC >=9.14.3 | Gate single-thread controls; do not promise this on local GHC 9.14.1 |

Primary implementation:
[dispatch/capabilities](https://github.com/well-typed/haskell-debugger/blob/af22571abc9d4316ad592815f46d9b430f278014/hdb-dap/Development/Debug/Adapter/Server.hs),
[variables](https://github.com/well-typed/haskell-debugger/blob/af22571abc9d4316ad592815f46d9b430f278014/hdb-dap/Development/Debug/Adapter/Stopped.hs),
[evaluation](https://github.com/well-typed/haskell-debugger/blob/af22571abc9d4316ad592815f46d9b430f278014/hdb-dap/Development/Debug/Adapter/Evaluation.hs),
[breakpoints](https://github.com/well-typed/haskell-debugger/blob/af22571abc9d4316ad592815f46d9b430f278014/hdb-dap/Development/Debug/Adapter/Breakpoints.hs).

## Launch and ownership

`hdb server --port PORT` serves TCP; it is not a stdio adapter. Its default
listener is `0.0.0.0`, so every editor-owned launch must set **`DAP_HOST=127.0.0.1`**
and explicitly set `DAP_PORT` in the child environment as well as argv. Do not
mutate the daemon's global environment. Check for an occupied port, retain
startup output, own the server process group and clean up its interpreter and
terminal on stop or failed launch. No listener is exposed through SSH: remote
sessions start hdb on the session host and reuse the editor's display transport.

Use DAP `launch`, never `attach`: hdb explicitly rejects attach. Supply absolute
`projectRoot`, project-relative `entryFile`, `entryPoint`, `entryArgs` and
`extraGhcArgs`. Resolve Cabal component selection through its cradle rather than
assuming every `Main.hs` belongs to the first executable. Report compiler/cradle
mismatches with actionable output. Preserve the general adapter-JSON route.

The launch response arrives after cradle loading; the subsequent `initialized`
event opens configuration. Give launch its own bounded deadline (initially two
minutes), progress output and immediate Stop. Keep ordinary inspection deadlines
short. Expiring a deadline destroys the owned session; it must not suggest the
operation was rolled back. A slow build needs a clear timeout, not an indefinitely
spinning UI or a blanket increase of all request deadlines.

Install all breakpoint sets and exception filters before `configurationDone`.
Source files are local to the session host; hdb's `sourceReference` retrieval is
not implemented. Track the saved revision loaded for execution and mark dirty
editor source as differing from the debuggee. Do not silently save it.

## Inspection and execution must remain distinct

In hdb, a `variables` request can execute code: a variable with
`presentationHint.lazy=true` is a forcing handle. A read-only MCP annotation
cannot make that request safe. Before enabling hdb:

1. Register references returned by scopes, variables and evaluation, recording
   their session generation and lazy/executing classification. Unobserved or
   stale references cannot be guessed through MCP.
2. `debug_inspect` may expand known non-lazy references only. Preserve the lazy
   hint in its response so the agent can choose a separate operation.
3. Add `debug_force` and `debug_evaluate` as execution tools with independent
   Agent Permissions entries. Force targets a known lazy reference; evaluation
   requires an explicit expression and selected stopped frame. No hover-triggered
   or automatic watch evaluation.
4. Route the UI through those same checks. A lazy variable uses a clearly named
   Force action; ordinary Enter/Expand does not accidentally evaluate it.
5. Handle `invalidated` events. Forcing can update shared thunks outside the
   selected variable, so invalidate variable views/references and refetch scopes.
   A resumed frame, failed evaluation or replaced session cannot retain usable
   stale selections. Preserve source position when only variables change.

Both forcing and arbitrary expressions may block, throw or perform I/O. Report
returned exceptions as evaluation outcomes; do not treat every successful DAP
response as a successful Haskell result. Terminating a stuck evaluation ends the
session and cannot undo its effects. Custom visualizers also need qualification
with an observable unevaluated thunk; do not infer their safety from the word
“inspection”.

Conditions can execute while the program runs. Their
editing belongs under executing breakpoint permissions, not read-only context.
`debug_configure_breakpoints` has a separate executing permission; the existing
integer `debug_set_breakpoints` grant cannot introduce or enable expressions.
Existing integer-line breakpoint calls remain supported and preserve metadata
for retained lines. Rich source records carry `condition`, `hitCondition`,
and `enabled`; unsupported drafts remain visible without installing
an unconditional fallback. A rich breakpoint toggles enabled state without losing
its expressions. Ordinary line-only toggles retain their add/remove behavior.
File breakpoint drafts survive adapter restart; running debugger state is not a
part of daemon recovery checkpoints.

## Input and completion of a session

Implement `runInTerminal` through the existing terminal service, with argv/cwd
and environment passed structurally. Return its process identifiers, route
Ctrl+C into it and keep it alive across frontend detach. Advertise support only
when this path exists; on Windows test the upstream fallback separately.
Do not confuse debugger expression input with debuggee stdin.

Keep `terminated`, `exited` and transport failure distinct. hdb sends `terminated`
before `exited` in normal and exceptional completion, so immediate destruction
on the first event can lose the exit result. Retain the terminal outcome long
enough to collect available exit information without permitting further debug
commands. A missing exit code remains unknown. Show compile failure, exception,
user Stop and natural completion accurately; none is simply “success because
the socket closed”.

## Delivery and checks

- [x] Qualify a pinned hdb with GHC 9.14.1 on the current macOS host using an
      isolated toy/cradle; record exact versions and actual protocol behavior.
- [x] Add owned loopback server launch and launch deadlines through the shared
      DAP transport and process cleanup. DAP/debugger fixtures cover occupied
      ports, missing executables and cold-load deadlines; real macOS hdb
      qualification covers failed cradles, Stop during launch and repeated runs.
- [x] Protect read-only scopes with observed-reference provenance, lazy/unknown
      handle refusal and invalidation. Fixtures cover stale handles and watch
      ownership; real hdb checks cover inert lazy values and explicit child Force.
- [x] Qualify observable thunk side effects, cyclic value navigation and bounded
      Stop during explicit nonterminating human Watch Evaluate on pinned macOS hdb.
- [x] Qualify caught-exception stops and exact exception details with hdb's
      **All exceptions** filter through read-only inspection and the human view.
- [ ] Qualify uncaught-error filter behavior and live root Force. Root Force is
      fixture-only because hdb Evaluate results omit the lazy-root hint.
- [x] Expose GHC source/component launch, exception filters/details and human
      Watch Evaluate/Force through shared debugger state and permission handling.
- [x] Add source breakpoint conditions and hit counts through the
      shared editor and executing MCP replacement operation. Retain metadata on
      ordinary toggles, integer replacement and adapter restart; show unsupported
      features as unavailable. Fixtures cover stale edits and source privacy.
- [ ] Add logpoints with retained program-terminal source privacy and final-publication checks.
- [x] Add capability-gated single-position executable-location discovery through
      a human source chooser and read-only inspection, preserving source ownership.
- [ ] Add function breakpoints.
- [x] Audit advertised value formatting on the live pin: ordinary variables and
      explicit Evaluate ignore `format.hex`; keep ineffective controls absent.
- [ ] Add formatting controls after an adapter implementation honors the choice.
- [ ] Add an expression console/history and executing Evaluate/Force MCP tools.
- [x] Add terminal reverse requests; qualify macOS stdin, Unicode, Ctrl+C and
      owned cleanup. Shared terminal fixtures cover resize and retained output.
- [x] Qualify debugger terminal frontend reconnect end to end on macOS with
      a saved GHC source target.
- [ ] Test the upstream Windows fallback separately.
- [x] Capture actual shared debugger dialogs and live hdb Watches in Metal and
      document their workflows. The qualification below also records a real
      source/pinned-terminal Metal capture.
- [x] Qualify successful and exceptional program completion on real hdb
      (exit 0 and exit 42).
- [x] Qualify agent background inspection and explicit reveal on real hdb.

Do not add a GHC API dependency to hide. The external hdb installation owns
that compiler coupling. Keep upstream capability gaps explicit; a local GHC
9.14.1 test cannot establish 9.14.3 multithreaded behavior.

## Qualification checkpoint

On 2026-10-02, hdb `af22571` built with GHC 9.14.1 on macOS arm64. An
isolated direct-cradle program launched through the existing editor adapter
configuration, verified/hit a line-6 breakpoint, exposed Locals/Module/Globals,
reported a lazy `[Int]` value, rejected a read-only attempt to expand that
lazy handle, stepped, and terminated. No new debugger UI was
needed for that sequence. This does not yet qualify Cabal component discovery,
stdin, forcing, exception stops or other platforms.

The first attempt used `/tmp` as projectRoot while GHC reported `/private/tmp`,
and hdb failed to find the entry module. Canonical projectRoot fixed that
reproduction. Preserve canonical source/root identity in owned launch.

The shared debugger now records variable-reference provenance from UI and MCP
responses, denies ordinary expansion of lazy/unknown references, advertises and
handles variable invalidation, and keeps the stopped source frame when only
values expire. On 2026-10-04, the captured Watches provider route also verified
explicit stopped-frame scalar evaluation and nonlazy list-result expansion with hdb 0.14
and GHC 9.14.1 on macOS arm64. Lazy children stayed inert; these Evaluate replies
did not expose a root Force action. Explicit nested-child Force also works on a
lazy list tail: hdb invalidates the old variable references, and a subsequent
explicit watch refresh exposes fresh children. Root Force remains fixture-tested.

On 2026-11-24, the pinned hdb 0.14.0.0 and GHC 9.14.1 pair passed further live
qualification on macOS arm64 through the shared debugger owner and actual sidebar
watch provider. A `NOINLINE` thunk with an observable file-writing side effect
stayed inert through Locals/Module/Globals inspection, refusal of all observed
lazy references and saving its watch. Explicit human **Evaluate watch** returned
its scalar result and performed the side effect exactly once. A cyclic `Ring`
value exposed a lazy self-tail; read-only expansion refused to force it. Human
**Force lazy child**, followed by explicit watch refresh, exposed a non-lazy tail.
Four successive read-only navigation pages each returned two children with fresh
references. This qualifies bounded user-directed cyclic navigation through four
followed edges.

A separate human Watch Evaluate wrote an execution/PID marker before entering
an explicitly recursive nonterminating computation. With that watch still
loading, **Stop** returned to the owner within one second and disconnected at
about one second. Asynchronous retirement then closed the adapter listener and
ended the exact evaluating process. The same unchanged probe passed twice with
the pinned setup. These checks used the human watch actions for execution and
the existing read-only inspection/page paths for observation. They add no
executing agent tool. Live uncaught-error-filter and root-Force qualification remain open.

The same watch route on THC AST and bytecode reached embedded source stops but
returned a bounded error for explicit Haskell expression evaluation. Continue
produced the expected result and exited normally. THC currently has no stopped
lexical-frame expression evaluator; neither source stepping nor its global
export scope establishes watch evaluation or Force support.

Owned-server launch is now available through the shared adapter configuration's
`server` argv field. The existing process transport sets child-only DAP_HOST and
DAP_PORT, rejects an occupied endpoint, forwards output, and stops its process
group on cancellation or completion. Readiness is bounded to five minutes and
launch requests to two minutes; ordinary requests retain fifteen seconds.
A real hdb run through this path repeated breakpoint, scopes, lazy inspection
protection, stepping and termination, and left no listener on its selected port.
Later checkpoints below qualify Cabal component selection and failed-cradle
cleanup. Live uncaught-error-filter qualification remains open.

The shared status bar now selects THC/GHC for build, run and Selected target
debug launch. GHC Selected target starts hdb on PATH using the saved source
entry file, canonical project root, `main` and the configured program arguments.
Advanced launches still use the adapter configuration. A real selected-GHC
launch repeated the same live breakpoint/inspection/step/termination check.
`exceptionInfo` is now exposed through the shared read-only inspection tool and
Debug > Exception details, gated by the adapter capability. Structured fixture
checks cover nested causes and stack text; real exception outcomes remain to
be qualified independently.

A real GHC `error "demo exception"` probe enabled hdb's `break-on-error` filter
and confirmed the setExceptionBreakpoints response, but hdb 0.14.0.0/GHC 9.14.1
ran to termination without an exception stop. Its output contained the error
and an `exited` event after `terminated`. This does not qualify live exception
stops/details; investigate the upstream filter behavior and preserve the exit
code before claiming that path reliable. Fixture-backed exception presentation
remains separate evidence.

On 2026-11-24, the exact pinned adapter source grounded a supported live scenario.
It advertises `break-on-exception` (**All exceptions**) and `break-on-error`
(**Uncaught exceptions**); its DAP integration test selects the former for caught
exceptions. An isolated program using qualified `Control.Exception.catch` and
`throwIO` stopped before the catch, then the editor's actual exception-filter
dialog enabled **All exceptions** alone. Continue reached the caught exception.
Read-only `debug_inspect` `exceptionInfo` returned `exceptionId: IOException`,
`breakMode: always`, the exact `user error` message, a fully qualified
`ghc-internal:GHC.Internal.IO.Exception.IOException` type, and a stack trace at
the throw call. **Debug > Exception details** opened the same message in its
read-only document. Stop ended the session, and the unchanged probe passed
twice. This qualifies caught-exception stops/details on macOS arm64 with the
pinned hdb/GHC pair. The earlier uncaught-error-filter case remains unqualified.


Completion retains the final output and adapter-reported exit code for up to
one second after `terminated`, while rejecting further execution/inspection.
Both event orders are covered, including output arriving after `exited`;
missing codes remain null. Real selected-GHC runs qualified normal exit 0 and
`error "demo exception"` exit 42 with the pinned hdb/GHC pair. These are program
outcomes, not proof of exception-breakpoint behavior. The live checks used the
same saved-source launch, breakpoint, scopes, protected lazy inspection, step
and continue flow as the editor.


On 2026-10-02, the official hdb 0.14.0.0 bindist with GHC 9.14.1 on macOS
arm64 passed interactive qualification through **Selected target**. An isolated
direct-cradle program accepted `typed λ` through the editor's ordinary terminal
keyboard effects, printed its Unicode echo and separate stderr output, then
completed with DAP exit 0. A second run received Ctrl+C while waiting in
`getLine`; hdb ended that session with exit 42. This establishes interruption,
not a resumable exception stop. Both probe processes finished cleanup. A real
Metal capture showed the source above its pinned terminal while awaiting input.
Later checkpoints below qualify Cabal component selection and terminal
frontend reconnect. Windows input remains open.

The bindist's reverse request names its inner executable, bypassing the wrapper
that locates matching GHC shared libraries. Owned hdb sessions therefore run
only the known `external-interpreter`/`proxy` commands through the original
launcher, retaining argv, cwd and environment. Generic adapter commands remain
unchanged. A fixture covers both the launcher route and that negative control.
Terminal preparation and process retirement run on owned workers; replacing a
session waits for old listener cleanup on the transport worker. On macOS,
terminal cleanup drains the available output tail and closes the PTY master
before waiting for the killed process, avoiding an observed terminal-exit wait.
Focused DAP, debugger, terminal and console checks cover literal argv, environment
unsetting, process IDs, failed reverse requests, Unicode input, Ctrl+C, retained
output and exactly-once delivery of the final drained tail. Output-only adapters
also update the shared live Debugger output view.

On 2026-11-24, the official hdb 0.14.0.0 bindist with GHC 9.14.1 on macOS
arm64 passed a complete terminal frontend detach/resume qualification. The
existing managed acquisition path verified the catalog-pinned archive hash,
layout and selected-compiler startup. **Debug > Launch > Selected target**
launched a saved loose-source GHC target with a direct cradle. The program
accepted `typed λ` and printed its first echo, then waited for another line.
Ctrl+] detached the display while the debugger stayed active.
`--terminal --resume` returned to the same editor session identity, debugger
generation, terminal window and terminal buffer, retaining the first output.
The resumed frontend delivered `resumed 界`; its echo and separate stderr output
were retained, and DAP reported exit 0. The qualification then closed its editor
session. This covers macOS terminal frontend reconnect for the saved-source
launch path. The following checkpoint covers a Cabal library/executable project;
the upstream Windows fallback remains open.

A further 2026-11-24 qualification exercised the real pinned hdb 0.14.0.0 and
GHC 9.14.1 through the shared debugger owner on macOS arm64. A failing bios
cradle surfaced its failure and closed the session and listener. A second bios
cradle published readiness and its process identity, then held the launch
request. **Debug > Disconnect** returned to the owner within one second;
the identified cradle child exited and the listener was released. Two subsequent
sessions reused that endpoint, each produced its own distinct output token and
DAP exit 0, and their debugger generations advanced. These checks required no
production correction. The deadline and occupied-port claims above remain
covered by the DAP/debugger fixtures. The following checkpoint qualifies Cabal
component/cradle selection; the upstream Windows fallback remains open.


The same pinned hdb/GHC pair passed live Cabal qualification on macOS arm64
using a minimal package with a library and an executable that imports it. The
actual sidebar **Debug** action captured the qualified executable and its
`app/Main.hs` entry while another saved source was active and the saved run
target named a different component. hdb consumed the owned explicit Cabal
cradle, loaded the library dependency and produced its expected result with
DAP exit 0. The saved run settings stayed unchanged, and the scoped cradle was
retired. Governing `hie.yaml` and `.hie-bios` files each caused preflight
refusal without a new debugger generation or modification of those files.
An **Adapter config** launch then used an explicit custom Cabal cradle to run
the same library-backed executable, retained that cradle byte-for-byte and
completed with exit 0. These checks cover a library/executable dependency and
the three existing launch lanes. Broader multi-component, generated-source and
other-platform qualification remains open.


Live background/reveal qualification on the same pinned hdb/GHC pair kept a
separate edited source window in front while a second breakpoint stop arrived.
The shared `debug_inspect` tool handler read stack, scopes and variables while
preserving that window, its draft, selection and the stopped generation.
Explicit `debug_present` source and stack requests revealed the same paused
source location and preserved the draft. hdb allocated a fresh stack handle for
that location on a new stack request; the selected frame and generation stayed
unchanged. Continue then produced the expected output and exit 0. This passed
with both explicit matching GHC environment and bindist-wrapper compiler
discovery. Existing source-policy and debugger-sidebar checks cover stale and
private refusal. The visual workflows continue to use the verified Metal
captures described above.


Single-position executable-location discovery passed live qualification with the
same pinned hdb/GHC pair on macOS arm64. While the toy remained stopped, the
read-only query returned the span `5:3–5:24`. The actual registered source
**Executable locations…** action displayed it and selected the corresponding
source region, preserving the breakpoint records and stopped generation. Stop
then ended the session. The unchanged probe passed twice. The pin implements a
line with optional column query; its handler does not enumerate an end range.
Shared transport regressions separately cover supplementary-Unicode UTF-16
conversion, empty/malformed/oversized replies, source edits, private source,
retired caller admission, and one-shot chooser cancellation. Source-coordinate conversion and reply validation run on the waiting worker;
interaction checks retain small source/version receipts and never compare buffer
contents.


Value-formatting qualification on 2026-11-24 used an owned real hdb 0.14.0.0
adapter with GHC 9.14.1 on macOS arm64. It advertised
`supportsValueFormattingOptions: true`. Already-evaluated `Int` values `4660`
and `-42`, and a `Word` value `255`, stayed decimal when the same stopped frame
and Locals reference received `variables` requests with no format, `hex: true`
and `hex: false`. Explicit `evaluate` of the same `Int` on the exact frame likewise
returned `4660` for all three requests. The stopped source location stayed the
same, and no continue, stop or termination event occurred during the probes.
Both runs completed with owned process cleanup; the repeated run also confirmed
that the adapter process group had no live members.

The inspected `commandVariables` handler forwards only thread/frame/reference
through `GetVariables` and copies the runtime-rendered `varValue` string.
`commandEvaluate` forwards expression/frame through `DoEval` and copies
`resultVal`. Neither path uses its decoded formatting argument. Thus successful
responses and the advertised capability do not qualify hexadecimal rendering
at this pin. The audit used direct owned DAP requests, with evaluation explicitly
requested for qualification; no executing read-only agent tool or ineffective
editor control was added.
