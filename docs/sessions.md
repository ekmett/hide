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

`--metal` and `--vulkan` also work. The native and browser displays need their
usual build flags. Only one frontend controls a session at a time, so detach
the current one before opening another. Resuming keeps the existing desktop;
the new frontend supplies its display and dimensions.

## Detach or finish

**Ctrl+]** detaches in the terminal, native window and browser frontends. The
native window closes; the browser confirms detachment and stops reconnecting.
The launching terminal prints the session ID and a command to resume it.
Buffers, including unsaved changes, remain in the session process.

On POSIX systems, sending SIGINT, SIGTERM or SIGHUP to the frontend also detaches
and prints the resume command. This stops the display process while the editor
session continues. In browser mode, closing a tab leaves the local frontend
process running; use **Ctrl+]** in the page to release the session for another
frontend.
Reloading the page reconnects through the running frontend.

**File > Exit** finishes the session through the normal save prompts. Closing a
connected native window follows that same exit workflow. Closing a native window
while disconnected detaches without queuing an Exit for later delivery.

The session process owns the working state. It survives detachment or a lost frontend or
SSH connection, but not its own crash or a host reboot. Save files
normally; session reattachment is a way to continue a running editor.

These editor sessions are separate from a provider's conversation sessions.
**Tools > New session** and **Tools > Resume session** manage the latter; see
[conversations](conversations.md). [Remote editing](remote.md) covers connecting
to another machine for the first time.
