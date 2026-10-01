# Sessions

Leave the desktop where it is and return to the same buffers, windows and
running tools. Every frontend attaches to an editor session in a separate
process, whether the project is local or reached over SSH.

## Return to your work

```sh
thc-edit --resume
```

With one unfinished session, the editor opens it directly. With several, it
lists their hosts and working directories and asks you to choose a number in
the launching terminal. If there is no interactive terminal, select the session
explicitly:

```sh
thc-edit --resume ID
```

Use the ID printed when detaching, or any prefix that identifies it uniquely.
The saved record supplies the project and, for SSH sessions, the host. Do not
add a path or `--ssh` to `--resume`.

## Change displays

Choose the frontend as usual when returning:

```sh
thc-edit --terminal --resume
thc-edit --window --resume
thc-edit --web --resume
```

`--metal` and `--vulkan` also work. The native and browser displays are included unless disabled at build time. Only one frontend controls a session at a time, so detach
the current one before opening another. Resuming keeps the existing desktop;
the new frontend supplies its display and dimensions.

## Detach or finish

**Ctrl+]** detaches in the terminal, native window and browser frontends. The
native window closes; the browser confirms detachment and stops reconnecting.
The launching terminal prints the session ID and a command to resume it.
Buffers, including unsaved changes, remain in the session process.

**Ctrl+C in the launching terminal** stops the display and its editor daemon,
after checkpointing unsaved buffers. The printed resume command restores that
checkpoint; running terminal jobs, agents and debugger connections stop with the
daemon. Inside an embedded terminal, **Ctrl+C** instead goes to that terminal's
foreground program. Use **Ctrl+]** when you want those programs to keep running.

On POSIX systems, SIGTERM or SIGHUP sent to the frontend still detaches and prints
the resume command while the editor session continues. In browser mode, closing
a tab leaves the local frontend process running; use **Ctrl+]** in the page to release the session for another
frontend.
Reloading the page reconnects through the running frontend.

**File > Exit** finishes the session through the normal save prompts. Closing a
connected native window follows that same exit workflow. Closing a native window
while disconnected detaches without queuing an Exit for later delivery.

## Run without a display

```sh
thc-edit --daemon .
thc-edit --sessions
thc-edit --window --resume ID
```

`--daemon` starts the desktop, waits until it is ready, prints its ID and returns.
Agents, HLS, builds and debugger polling continue while detached. Human permission
requests remain pending until you attach; being headless grants no permissions.
Use `--daemon --resume ID` to continue an existing or recoverable session without
opening a display. `--sessions` lists running, recoverable and remote sessions;
live local entries also show attachment, replies, queued work and waiting state.

## Recover after a process dies

The editor checkpoints its desktop once per second after changes. `--resume`
reattaches to a live daemon or restores its last complete checkpoint after a
crash. Recovery includes unsaved text and hex buffers, saved disk baselines,
undo/redo history, split views, window placement, Files, display preferences and
the conversation draft. Source files are never overwritten during recovery.
If a file changed on disk, the usual conflict checks still apply when saving.

Conversation transcripts and their provider resume IDs are retained per editor
session. Resume the provider explicitly to reconnect. Terminal output is restored
as an ended view; live terminal processes and debugger connections are not
recreated. Pending questions and approvals are not restored or accepted.

Checkpoints and the session catalog live under the private
`$XDG_DATA_HOME/thc-edit/sessions` directory (normally `~/.local/share/thc-edit/sessions`
on Unix; the platform application-data directory on Windows). Atomic replacement
keeps a partial write from replacing the previous checkpoint. Up to a second of
recent work can be lost on abrupt termination; this is process-crash recovery,
not a guarantee against power loss. A checkpoint over 256 MiB reports an error
and retains the previous complete version rather than trimming your history.
Save important files normally.

**File > Exit** removes the completed session's checkpoint. A crash or lost
frontend leaves it available. Session ownership uses an OS-held lock so concurrent
resume attempts cannot start independent daemons for the same desktop.

These editor sessions are separate from a provider's conversation sessions.
**Tools > New session** and **Tools > Resume session** manage the latter; see
[conversations](conversations.md). [Remote editing](remote.md) covers connecting
to another machine for the first time.
