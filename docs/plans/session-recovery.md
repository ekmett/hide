# Daemon startup and crash recovery

Start an editor without a display using `thc-edit --daemon [path]`. Print its
session ID and resume command only after it accepts connections. `--sessions`
lists running or recoverable desktops; `--resume [ID]` attaches using any frontend.
The existing detached session loop continues agents, HLS, builds and debugging.
Human approval requests remain pending without a display.

## Recovery contract

- Store versioned, atomic, owner-private checkpoints separately from transient
  sockets. Keep buffer bytes (including hex), saved baseline, full undo/redo,
  split view identity, selections, scrolling, window layout and project location.
- Capture source/untitled buffers and conversation draft/transcript. Do not
  serialize pending approvals, dialogs, input gestures, transport journals or
  live process handles. A recovered session starts with fresh transport state.
- Preserve source disk baselines so existing reconciliation/save conflict checks
  detect external changes; recovery never writes source files itself.
- Persist periodically after edits, with a visible error if checkpoints cannot
  be written. Clean File > Exit removes recovery; process loss retains it.
- Recover an unavailable local/remote session only from an existing valid
  checkpoint. Never silently turn an ended session into a new empty one.
- Private session files and checkpoints remain inaccessible through agent file,
  UI and screen tools. Never restore captured permissions over current policy.
- Restore the desktop after a crash; terminals and debugger processes are ended.
  Retain conversation resume information, and let the user reconnect the provider.

## Validation

Round-trip text/hex, saved/dirty baselines, undo/redo, split layout and draft.
Reject malformed/unknown-version checkpoints safely. Exercise startup/discovery,
SIGKILL after unsaved edit, resume with another frontend and disk-conflict save.
Check clean exit removes recovery, concurrent resume does not split ownership,
and daemon agent polling continues while no frontend is attached.

## Remaining platform qualification

- [x] Native Windows pending-inspector shutdown: verified at `57cc730` with
  native GHC 9.12.4. `RemoteCheck` covers deferred client EOF, cancellation while
  bridge input remains open, a subsequent request, and explicit Exit with an
  attached display and pending inspections. Bound the inspector's native Handle
  readiness wait to 100 ms so cancellation can run even when successful local
  socket shutdown does not wake that foreign call. Token bridge fixtures close
  their owned input peer before joining the reader. All assertions retain their
  original deadlines. Native headless build and real SSH crash/recovery checks
  also pass; this does not qualify the Windows embedded-terminal backend.

- [ ] Investigate intermittent macOS stdio relay closure after attachment.
  The Ctrl-C regression twice received EOF after hello/assets and before its
  first input acknowledgement; the daemon remained alive. The retained first
  failure occurred on the initial attachment, so rapid reattachment is not a
  necessary trigger. Its daemon log matches the failed harness timestamp and
  contains two uncaught `AsyncCancelled` exceptions, without a connection stage.
  Cancellation also occurs normally when `race_` stops the losing receive/send
  worker; this log alone does not establish that cancellation caused the EOF.
  Seven subsequent runs with exception-stage tracing, twelve fresh-daemon input
  runs, and 120 rapid attachments passed. A bounded shared-render cancellation
  probe did not reproduce an exception on subsequent evaluation. These negative
  results do not diagnose or fix the failure; no speculative transport change
  was made.

  `test/interrupt-session.py` owns its resumed daemon and relay processes, retains
  failed fixture directories, relay stderr, failure stage and relay exit status
  before and after cleanup, and checks actual daemon exit,
  SIGTERM detachment, and recovery/save of an unsaved edit. On the next failure,
  use the retained relay status alongside both logs to distinguish relay stdin/socket
  EOF, daemon input failure, frame/output failure, and the enclosing attachment
  exception. A cancellation recorded only in the losing worker is insufficient;
  identify the first terminating worker and whether its exception propagates to
  the attachment before changing cleanup or shared rendering. Temporary stage
  evidence from this investigation is indexed by `/tmp/interrupt-trace-2.log`
  through `/tmp/interrupt-trace-8.log`; the original failure is retained in
  `/tmp/thc-interrupt-green.log` (2026-10-01 14:01:57).
