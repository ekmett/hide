# DAP client implementation plan

Goal: attach the editor to Graal's stock loopback DAP instrument, preserving ACP,
Ghostty terminals, file safety, and the classic editor UI.

Approved contract: Content-Length UTF-8 DAP over TCP; asynchronous bounded queues;
initialize/attach/configurationDone sequencing based on actual capabilities;
breakpoints, pause/continue/steps, threads, frames, scopes, explicit variable
expansion, exception filters, and sourceReference retrieval. No evaluation,
variable mutation, thunk forcing, or reconstructed guest stack. THC launch flags
and runtime hooks remain upstream work; do not imply that current thc run has them.

- [x] Add DAP.hs and DAPCheck.hs: loopback transport, correlation, framing bounds,
  split/coalesced UTF-8 frames, failed requests, disconnect and cancellation.
- [x] Add Debugger.hs and DebuggerCheck.hs: session sequencing, request generation
  guards after resume/disconnect, source requests, explicit lazy inspection,
  breakpoint replacement, advertised exception filters, failures/timeouts.
- [x] Connect Debug commands/dialogs in Model and runtime in App. Reuse existing
  source navigation and read-only views. Keep Launch explicit about upstream limits.
- [x] Verify a deterministic server and stock Graal when locally available; build
  native and terminal-only configurations, review, commit and sync owned checkouts.

Review focus: stale frame/variable references after resume; failed initialization;
source without a real path; dirty source text versus running code; bounded server
output; disconnect during pending requests. Test these in the owning transport or
session checks. Guest I/O never travels through the debugger socket.

References: https://www.graalvm.org/latest/tools/dap/ and
https://microsoft.github.io/debug-adapter-protocol/overview .

Validation: full editor tests pass with SDL/Metal + Ghostty and with both optional
backends disabled. Deterministic DAP regressions cover early initialized events,
stale frame/variable replies, dialog row identity, reordered breakpoint responses,
reconnect cleanup and nonterminating disconnect. Restoring four identified bugs
individually makes their regression fail. The actual Haskell client also attaches
to stock GraalJS dap-tool 25.3.4.1, verifies a breakpoint, steps, expands scopes and
variables, retrieves source and detaches; it sends no evaluation or mutation.
This is stock Graal qualification, not proof of THC runtime debugger support.

The same milestone adds the requested hex editor: NUL/invalid UTF-8 detection,
manual text/hex toggle, 16-byte offset/hex/ASCII grid, byte edits, exact-byte save,
encoding-aware undo, and binary exclusion from text tooling. Full-suite checks
include external binary reload/conflict handling and ACP saves after a mode change.
Native rendering was inspected at 80x25. Typed numeric/endian preview is deferred.
