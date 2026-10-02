# Remote editing

Open a project on another machine while using a terminal, native window or
browser on your own. The files, language server, Git, program commands and conversation
provider run on the project host. Display, clipboard and downloads stay local.

## Install on both machines

The remote host needs `thc-edit` on the `PATH` used by SSH commands. Sessions
and remote editing are always included. A minimal server installation omits
client frontends and embedded terminals:

```sh
cabal install exe:thc-edit -f-window -f-web -f-terminal
```

It does not need SDL, a browser, a graphical session or the THC compiler for
editing. Install HLS, Git, THC and a conversation provider there for the
corresponding workflows.

The default local build includes terminal, native-window and browser clients.
Keep both installations on compatible revisions of the remote protocol.

The remote endpoint has POSIX and native Windows implementations. Native window
and browser frontend dependencies belong to the client. For embedded terminals,
keep `terminal` enabled and install libghostty-vt on the server; omit only
`window` and `web`. Native Windows servers use ConPTY, so commands run directly
in Windows without WSL.

## Open a project

Use an ordinary SSH host, username or configured alias:

```sh
thc-edit --terminal buildbox:projects/example
thc-edit --window buildbox:projects/example
thc-edit --web user@buildbox:projects/example
```

The path names a file or directory on the remote host. Use your shell's quoting
for spaces:

```sh
thc-edit --window --ssh buildbox 'projects/example package'
thc-edit --web --ssh buildbox '~/projects/example'
```

`--ssh HOST PATH` is equivalent to `HOST:PATH`; leaving out PATH opens the remote
working directory. A leading `~/` expands to the remote user's home. Use
`--ssh buildbox 'C:\Users\name\project'` for an explicit Windows path.

One client opens one remote project at a time. Prefix a local filename containing
a colon with `./` to distinguish it from a remote target.

The client starts the fixed remote command `thc-edit --remote` through SSH without
a PTY. Paths and startup options travel in the protocol, not as pieces of a
remote shell command. Test that the remote command can find the executable if
connection reports that `thc-edit` is missing. SSH host aliases, authentication
and host-key handling use your normal SSH configuration.

`--terminal` uses the same session connection as the graphical frontends; it
does not require an SSH PTY. **Ctrl+]** detaches in all three frontends,
including while the remote connection is recovering.

## Work where the project lives

Open and save files as usual. HLS sees the remote package and its toolchain. Git
operates on that repository, and **Run**, terminals, debugger attachment and
conversation processes execute there. A loopback debugger address refers to the
remote machine.

The native and browser displays use the local clipboard. In the terminal
display, Copy writes to the clipboard through OSC 52 when the terminal permits
it. Use your terminal's paste shortcut for text from other applications; the
editor's Paste command reuses the last text copied in that frontend.

Dropping a local file into a graphical display uploads it into a new unsaved
remote buffer, with a limit of 16 MiB per file. Choose a remote location with
Save as. Browser **File > Download** brings the current buffer, including
unsaved changes, back to the client machine.

## Reconnect to the same session

If the connection drops, the remote session keeps its buffers and the client
attempts to reconnect. After detaching, return from the same client machine with:

```sh
thc-edit --resume
```

The session record remembers the SSH host and project. With several unfinished
sessions, choose one from the numbered list or use `--resume ID` with its full
ID or a unique prefix. You can also change displays, for example with
`thc-edit --web --resume`. Do not combine `--resume` with `--ssh` or a path.

To attach from a client machine without that saved record, use the remote
session's full ID and host:

```sh
thc-edit --window --ssh buildbox --remote-session ID
```

Only one client controls a session at a time. **File > Exit** ends it through
the normal save prompts. Detaching leaves it running. A session survives an SSH
disconnect; after a daemon crash, resume restores its last complete checkpoint.
Live terminal processes and debugger connections cannot be restored. Save files normally.

[Sessions](sessions.md) covers detachment and frontend switching.
The [architecture reference](architecture.md#sessions-and-remote-editing)
describes the transport and host-local endpoint.

Native menus use named commands negotiated with the server. When either side
predates named-menu support, native menu items are disabled rather than mapped
to a different command. Keyboard shortcuts and the editor menus drawn in the
remote screen remain available, and existing sessions can still be resumed.
