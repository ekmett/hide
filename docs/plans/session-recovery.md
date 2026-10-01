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

- [ ] Native Windows: while an MCP inspection is waiting for a user answer or
  other deferred reply, close its stdio client and exercise editor shutdown.
  Verify EOF cancels the wait, retires approvals, releases the desktop/endpoint
  and exits without a blocked socket or pipe reader. Repeat with a display
  attached and detached. Earlier Windows display detach/Exit checks do not
  establish this pending-inspector shutdown behavior.

- [ ] Investigate intermittent macOS stdio relay closure during rapid reattachment.
  The Ctrl-C regression twice received EOF before the first input acknowledgement;
  the daemon remained alive and logged `AsyncCancelled`. Eight traced reruns and
  the final uninstrumented run passed. Do not treat this as a diagnosed/fixed
  transport issue. `test/interrupt-session.py` now checks actual daemon exit,
  SIGTERM detachment, and recovery/save of an unsaved edit.
