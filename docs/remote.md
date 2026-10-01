# Remote editing

Open a project on another machine while using a terminal, native window or
browser on your own. The files, language server, Git, program commands and conversation
provider run on the project host. Display, clipboard and downloads stay local.

## Install on both machines

The remote host needs `thc-edit` on the `PATH` used by SSH commands. Sessions
and remote editing are included in the default build:

```sh
cabal install exe:thc-edit
```

It does not need SDL, a browser, a graphical session or the THC compiler for
editing. Install HLS, Git, THC and a conversation provider there for the
corresponding workflows.

The default local build includes the terminal client. For a native window:

```sh
cabal install exe:thc-edit -fwindow
```

For a browser client, replace `-fwindow` with `-fweb`. Keep the two installations
on compatible revisions of the remote protocol.

The remote endpoint has POSIX and native Windows implementations. Native window
and browser frontend dependencies belong to the client. Embedded terminals on
the server additionally need `-fterminal` and the current POSIX PTY backend.

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
does not require an SSH PTY. **Ctrl+]** detaches to your local shell.

## Work where the project lives

Open and save files as usual. HLS sees the remote package and its toolchain. Git
operates on that repository, and **Run**, terminals, debugger attachment and
conversation processes execute there. A loopback debugger address refers to the
remote machine.

Copy and Paste use the local clipboard. Dropping a local file uploads it into a
new unsaved remote buffer, with a limit of 16 MiB per file. Choose a remote
location with Save as. Browser **File > Download** brings the current buffer,
including unsaved changes, back to the client machine.

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
disconnect, but not a daemon crash or host reboot; save files normally.

[Sessions](sessions.md) covers detachment and frontend switching.
The [architecture reference](architecture.md#sessions-and-remote-editing)
describes the transport and host-local endpoint.
