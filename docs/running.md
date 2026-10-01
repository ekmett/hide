# Running and debugging

Keep program output beside the source, use a shell for project commands, and
attach a debugger when you need to stop and inspect execution.

## Compile, make, run

Choose **Run > Target** (also **Compile > Target**) to select **THC** or **GHC**,
the compiler executable, and an optional Cabal target such as
`package:exe:program`. Save your source files, then use:

| Action | Shortcut | What it does |
| --- | --- | --- |
| Compile | Alt+F9 | Compile the selected package target; check a standalone GHC source file |
| Make | F9 | Build the selected package target or standalone executable |
| Run | Ctrl+F9 | Build and run the selected program |

These actions are also clickable in the ordinary status bar. Compile and Make
open an output window; output arrives while you keep editing. Compiler errors
and warnings appear in **Messages**, where **Alt+F8** and **Alt+F7** move between
source locations. **Compile > Stop build** stops the current job.

For THC, Compile and Make invoke `thc acquire`: Cabal builds the component and
THC captures its dependency closure. Run invokes `thc run`. For GHC projects,
the editor uses `cabal build` and `cabal run` with the selected compiler.
Outside a Cabal project, Compile checks the current `.hs` or `.lhs` file with
`ghc --make -fno-code`, Make compiles the module (and links an executable for
`Main`), and Run uses
`runghc` with the selected GHC.

The target dialog also accepts an optional THC root and runtime path, and
program arguments as a JSON array, for example `["input.txt", "--verbose"]`.
Arguments go directly to the process; shell syntax is not interpreted. Settings
live in the user configuration directory. The Cabal target belongs to the
selected project, while the compiler installation is shared.

Run uses an embedded terminal when the [terminal build](install.md#embedded-terminal)
is available. Basic builds capture output without interactive input; **Run >
Stop build/run** stops a captured run. All commands execute on the session host,
including over SSH. Detaching the display leaves an active build or run intact.

## Shells and output

**File > Terminal** opens the configured shell in the project directory.
Ordinary keys and paste go to the focused terminal. **Ctrl+C** interrupts its
foreground job; raw-mode programs receive the control byte themselves.
F5, F6, F10, numbered-window
shortcuts and menu controls stay with the editor. Resizing the terminal window
resizes its PTY; output supports colors, attributes and Unicode.

Click the terminal title bar's cyan **[ ]** control, or choose **Window >
Pin / unpin terminal**, to dock it at the bottom. Messages and pinned terminals
share one panel with tabs. Click a tab or use the existing numbered-window/F6
shortcuts to focus a terminal; hidden tabs keep receiving output. Drag an unused
part of the panel's top border to resize the shared panel. The cyan **[P]**
control restores the terminal's floating window, constrained to the current
screen and Files panel. Pinning preserves the process, window number, buffer
and selection. Tile and Cascade arrange the floating windows only.

The Messages collapse arrow hides its tab; pinned terminals keep the panel
open. A recovered checkpoint retains these tabs as **Ended Terminal** views
without restarting their processes.

Closing a terminal window leaves its command running. **Run > Stop terminal**
stops the selected process. **File > Exit** cleans up the session's terminal
processes. Detaching the frontend leaves them running; return with `--resume`.
ACP terminal requests use the same backend, with approval before execution.

## Launch or attach a debugger

**Debug > Launch** offers **THC target** and **Adapter config**. THC target
uses the THC installation and package selected in **Run > Target**. Choose an
unused loopback port (default `4711`); the editor starts `thc run --dap-port PORT`,
waits for its debugger, and configures your breakpoints before execution.
This requires a THC build with the DAP instrument and `--dap-port` support.
Build output is available in **Debug > Output** while it starts. The first
launch can spend several minutes building and capturing dependencies.
**Debug > Disconnect** stops that work; protocol response timing starts when
the debugger connects.

THC initially stops at an instrumented source location. Embedded source opens
read-only, where **Ctrl+F8** sets a breakpoint in the running program. Both
the AST and bytecode runtimes carry source locations; optimized Core can
repeat or combine locations, so a step need not advance to a different line.
Scopes and values depend on the runtime: THC source stepping is available,
while lexical scopes and value inspection remain runtime work.

For another debugger, put its command and DAP arguments in `.thc-debug.json`
in the project directory, or choose another configuration path:

```json
{
  "command": ["lldb-dap"],
  "request": "launch",
  "arguments": {"program": "/absolute/path/to/program", "args": []}
}
```

The adapter owns the meaning of `arguments`. Use `"request": "attach"` for an
adapter-specific attach configuration. A TCP configuration replaces `command`
with `"host": "127.0.0.1", "port": 4711`; only loopback endpoints are accepted.
Adapter commands and arguments are passed without a shell.

**Debug > Attach** connects directly to an already-running DAP endpoint,
defaulting to `127.0.0.1:4711`. The debugger protocol stays separate from program
output. Disconnecting an attached session leaves its program running;
disconnecting an editor-launched session requests termination and releases its
owned adapter. Detaching the editor display preserves the debugger, so `--resume`
returns to the same breakpoints and stopped state. With THC, continuing to
normal termination avoids a known early-detach shutdown race in the Graal DAP
instrument.

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
| Disconnect | Debug > Disconnect |

Choose a frame to inspect its scopes and expand variables explicitly. Values
come from the runtime; the editor does not automatically evaluate expressions
or change variables. Frame and variable selections expire when execution
resumes. Source supplied by the server opens read-only if no disk file is
available.

In a [remote editor session](remote.md), loopback is the remote host, so the
debugger stays beside the running program.

## Debugging with an agent

The conversation agent uses the same DAP session as the Debug menu. Its tools
control the debugger directly and return structured results. Automatic source
following starts enabled. The agent can set `debug_present` to `follow: false`
to investigate without moving your source window at each stop, then reveal a
source frame, stack, scopes or output view when there is something to show.
Changing presentation does not resume or restart the program.

See [debugging skills](agent-skills.md#debug-a-program) and the
[debugging operation reference](agent-tools.md#debugging).
