# Running and debugging

Keep program output beside the source, use a shell for project commands, and
attach a debugger when you need to stop and inspect execution.

## Run the package

Save modified source buffers, then choose **Run > Run** or press **Ctrl+F9**.
The editor runs `thc run --project-dir DIR` in an embedded terminal. THC selects
the package's runnable component.

**Run > Target** lets you choose:

| Field | Purpose |
| --- | --- |
| THC executable | The command to run; defaults to `thc` |
| Cabal target | An optional target such as `package:exe:program` |
| THC root | An optional THC source/build root; defaults from `THC_ROOT` |
| Runtime | An optional runtime path |

The target is associated with the project directory. Settings are saved in the
user configuration directory. This command uses the current THC CLI with a
positional Cabal target and `--project-dir`; installations using the older
`--exe` interface need updating. Which programs can run is determined by the
installed THC compiler and runtime.

Run needs the optional [terminal build](install.md#embedded-terminal). The
Compile and Make menu entries are reserved for the integrated build workflow;
use Run or project commands in a terminal for the working paths.

## Shells and output

**File > Terminal** opens the configured shell in the project directory.
Ordinary keys and paste go to the focused terminal. F5, F6, F10, numbered-window
shortcuts and menu controls stay with the editor. Resizing the terminal window
resizes its PTY; output supports colors, attributes and Unicode.

Closing a terminal window leaves its command running. **Run > Stop terminal**
stops the selected process. **File > Exit** cleans up the session's terminal
processes. Detaching the frontend leaves them running; return with `--resume`.
ACP terminal requests use the same backend, with approval before execution.

## Attach a debugger

Start a compatible DAP server, then choose **Debug > Attach**. Enter its loopback
host and TCP port; the default is `127.0.0.1:4711`. The editor uses Content-Length
DAP over TCP, separate from the program's input and output.

Attachment is the current debugger entry point. **Debug > Launch** is reserved
for the THC launch integration. A stock Graal DAP server can provide the protocol,
but forwarding its options through `thc run` requires the corresponding runtime
integration.

| Action | Keys or menu |
| --- | --- |
| Toggle a source breakpoint | Ctrl+F8 |
| List or remove breakpoints | Debug > Breakpoints |
| Continue | F4 |
| Pause | Debug > Pause |
| Trace into | F7 |
| Step over | F8 |
| Step out | Ctrl+F7 |
| Choose a thread | Debug > Threads |
| Inspect frames and variables | Debug > Call stack, then Scopes |
| Choose advertised exception filters | Debug > Exceptions |
| Read debugger output | Debug > Output |
| Detach | Debug > Disconnect |

Choose a frame to inspect its scopes and expand variables explicitly. Values
come from the runtime; the editor does not automatically evaluate expressions
or change variables. Frame and variable selections expire when execution
resumes. Source supplied by the server opens read-only if no disk file is
available.

Disconnect detaches without asking the program to terminate. Only loopback
endpoints are accepted. In a [remote editor session](remote.md), loopback is the
remote host, so its debugger stays beside the running program.
