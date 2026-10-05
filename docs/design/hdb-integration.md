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

Select **GHC** in the status-bar dropdown, then **Debug > Launch > Selected
target**. This starts `hdb` from PATH using the saved source entry, canonical
project root, `main` and configured program arguments. Use **Adapter config**
for explicit cradle/entry arguments. The installed hdb owns its GHC version;
a custom compiler command requires an explicit adapter configuration. Automatic
Cabal component/cradle selection and richer launch fields remain planned.
A normal GHC native build remains available independently.

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

All rows below are planned exposure. Existing editor support is identified
separately so upstream features are not confused with finished integration.

| Facility | Upstream at inspected revision | Editor exposure |
| --- | --- | --- |
| Source breakpoints, step in/over/out, continue | Implemented | Reuse existing UI/MCP; qualify disk sources and cradle loading |
| Threads, frames, locals/module/globals | Implemented | Existing pickers; preserve frame/thread identity and paging bounds |
| Ordinary variable children | Implemented | Existing expansion after validating reference provenance |
| Thunk inspection/forcing | Lazy child presentation hint; fetching lazy children forces | Read-only expansion refuses lazy references; nested-watch Force has live hdb list-tail qualification; root Force is fixture-tested, executing MCP tool remains planned |
| Expression evaluation | `evaluate` with optional frame; returns value/type/reference without a lazy root hint | Manually evaluated Watches have live scalar/list qualification; console, history and executing MCP tool remain planned |
| Source conditions, hit counts, logpoints | Implemented; hit count is parsed as an integer | Breakpoint editor; preserve all fields when replacing a source's set |
| Function breakpoints | Implemented, including condition/hit count | Function breakpoint rows and separate replace operation |
| Exception filters and details | Implemented | Existing filters plus exception details panel/tool |
| Valid breakpoint locations | Implemented | Resolve executable spans and show relocation/unverified state |
| Value formatting | Advertised | Offer only formatting verified to affect actual output |
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

Conditions and logpoint expressions can execute while the program runs. Their
editing belongs under executing breakpoint permissions, not read-only context.
Existing integer-line breakpoint calls remain supported; richer breakpoint
records must preserve conditions/log text when another line is toggled.

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
- [ ] Add owned loopback server launch and launch deadlines, using the existing
      DAP transport and process cleanup. Test occupied port, missing executable,
      failed cradle, cold load, Stop during launch and repeated sessions.
- [ ] Protect lazy references and implement invalidation before exposing hdb
      scopes to read-only agents. Test an observable thunk, cyclic structures,
      stale/unknown handles, exceptions and a nonterminating evaluation.
- [ ] Expose GHC launch selection, exception details, Evaluate/Force and rich
      breakpoints through the UI and MCP, sharing permission/state handling.
- [x] Add terminal reverse requests; qualify macOS stdin, Unicode, Ctrl+C and
      owned cleanup. Shared terminal fixtures cover resize and retained output.
- [ ] Qualify debugger terminal frontend reconnect end to end and test the
      upstream Windows fallback separately.
- [ ] Capture actual dialogs and stopped code in Metal, document workflows, and
      qualify both successful and exceptional exits plus agent background/reveal.

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
Component discovery and failed-cradle/exception-stop qualification remain open
items above.

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
Cabal component discovery, a complete display-detach/reconnect cycle and Windows
input remain separate qualification items.

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
