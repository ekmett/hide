# Work with the editor

The agent in a conversation can use the same editor you do: read an unsaved buffer, ask HLS for its type, rename a symbol, move a window, inspect a stopped program, or run a build and read the result. The built-in `editor` MCP server is supplied automatically when an ACP conversation starts or resumes.

Try requests such as:

- “Find the warnings in this package and show me the first one.”
- “Rename this function and show me the diff before saving.”
- “Tile these windows, hide Files, and open the executable in hex mode at byte 256.”
- “Show what the last two undo steps would change.”
- “Break at this line, run, and inspect the caller.”

For task recipes, use the [agent skills catalog](agent-skills.md). The
[operation reference](agent-tools.md) lists arguments, results and constraints by
category. Both are also available through `docs_read` in the `editor` corpus.

## Files, windows and history

`list_windows`, `list_buffers`, `read_buffer` and `read_selection` expose live contents, paths, dirty flags and revisions. `workspace_project` identifies the project root and package file. `workspace_search` searches disk files and substitutes unsaved buffer contents; `trackedOnly` limits it to Git-tracked files. `workspace_git` reports disk changes separately from unsaved buffers.

`editor_file` opens, saves or closes files. Save and close require the current revision. Closing a dirty buffer requires an explicit save or discard choice. `buffer_apply_diff` checks a unified diff against that revision and applies it as one undoable edit, without saving. `workspace_files` creates files/directories, renames paths, or deletes files and empty directories inside the project.

`editor_layout` reports screen and panel dimensions in character cells. `editor_arrange` focuses, moves, resizes, tiles, cascades, splits or zooms windows through the normal window manager. Its `pin`/`unpin` actions move the same terminal window into or out of the shared Messages/terminal bottom panel; focusing a hidden terminal selects its tab. `editor_panels` shows, hides and resizes Files and Messages. `editor_mode` selects text or hex; `editor_navigate` jumps to a line/column or a hex byte offset.

`editor_history` previews undo or redo steps as diffs, including hex changes. `editor_undo` applies the requested number of steps after checking the buffer revision. It changes the live buffer; saving remains a separate action.

## Haskell and diagnostics

`workspace_diagnostics` reads Messages and HLS/build diagnostics with paths, positions and revisions. The `lsp_` tools provide hover/type information, definitions, type definitions, references, document symbols and rename. They synchronize unsaved source text with the running language server. Rename updates buffers without saving and rejects results if the source changed while HLS was working.

Editor positions are 1-based Unicode character positions. Raw LSP results retain LSP's 0-based UTF-16 positions; each response identifies the buffer revision it describes. Byte offsets are 0-based.

## Build, test and run

`build_start` uses the selected THC/GHC configuration for compile, make or run. It requires saved source buffers and captures output in an editor window. `build_status` reports the output buffer and actual exit code; `build_stop` stops the job. A run can request a shared terminal instead.

`test_start` runs Cabal tests with GHC; `test_status` reports explicit Cabal suite outcomes alongside the process result and compiler diagnostic count. A successful process without suite output is reported as completed, not as invented individual passing tests. Test and build commands share one job slot.

`terminal_list`, `terminal_start`, `terminal_output`, `terminal_input` and `terminal_stop` use the same Ghostty terminals as the editor and ACP. Output is bounded and reports truncation and exit status. Execution tools can run programs with the editor's account and receive their inputs; choose Enable, Prompt or Disable under **Options > Agent Permissions**.

## Debugging

`debug_status` reports the adapter, stop state, frame, breakpoints and current generation. At termination, `active` becomes false immediately; `finishing` remains true while the editor drains final output for up to one second. `exitCode` is the adapter-reported program result, or null when unavailable. Launch the selected THC/GHC target with `debug_launch`, supply an adapter configuration for another DAP implementation, or attach to a loopback adapter with `debug_attach`.

Use `debug_set_breakpoints`, `debug_control` and `debug_inspect` to set source breakpoints, continue/step/pause, and inspect threads, stack frames, scopes, variables, adapter-owned source and `exceptionInfo`. Exception inspection requires a stopped target and an adapter advertising `supportsExceptionInfoRequest`; its structured response includes nested causes when supplied. Handles belong to a debug generation; the tool rejects them after execution advances. An accepted control command does not imply that the program has reached its next stop. Variable references must come from current scopes/variables responses. Lazy handles are not expanded by read-only inspection: adapters such as hdb force the thunk when those handles are requested. Variable invalidation expires the old handles without moving the source selection.

`debug_present` controls visibility independently of execution. Set `follow: false`
for background stops, then reveal `source`, `stack`, `scopes` or `output` using
the current generation. Set `follow: true` to follow future stops. The user and
agent share the same debugger; revealing a stop does not restart the program.

The [debugging skill](../skills/debug-editor/SKILL.md) is also available to MCP clients through `resources/read` at `thc-edit://debugging`.

## See what is on screen

`editor_screen` returns the full character grid without colors. Set `image: true` to include a PNG of the same frame. This gives the agent a small text representation for navigation and a visual representation for borders, colors and overlapping windows.

The PNG uses the bundled bitmap fonts and the current screen-mode aspect ratio. It includes the editor cursor but excludes OS window chrome, CRT effects and platform font shaping; complex Unicode clusters use a bitmap approximation.

## Work with several agents

Ask the main conversation to delegate a task to a named child. Agents can list
sessions with `agent_directory`, spawn a child with `agent_spawn`, rename
sessions, queue attributed messages, wait for a task ticket, and read or search
retained history. A child takes instructions from its parent; another agent's
message remains a peer message. Only you or an ancestor can cancel or end it.
Names help locate work, while stable IDs keep messages routed correctly.

A shared child uses the parent's editor. A worktree child starts from committed
Git source in a separate checkout and editor session, with its own buffers,
builds, terminals and debugger. Parent changes that have not been committed are
not copied. The new editor stays hidden until you open it, and ending the agent
preserves the checkout for review. Separate worktree sessions can build or debug
at the same time; shared children use the same editor job slots.

Open **Tools > Agents** or **Window > Agents** to browse names, status and parent
relationships. **Conversation** opens that agent's live messages in the existing
conversation window. Enter sends or queues a human message; Escape cancels its
reply. For a parent-controlled child, your message is attributed as a human peer
message, not as its parent's instructions. Each conversation keeps its own draft
and scroll position, including across editor recovery. **Tools > Conversation**
switches back to Primary. Click the child conversation title to change its
advertised model or effort while it is idle and has no queued messages. These
settings belong to that child. Ctrl+Enter steers an active reply only when its
provider advertises support; rejected steering keeps the draft. Reported child
context usage appears in the lower-left frame; otherwise it shows `--`. Expand a tool row to inspect its retained details. The view shows
the latest 100 events; history tools can read older retained events.

**Workspace** opens the associated editor. After recovery, select a child and
choose **Reconnect** to load its saved provider conversation. This keeps its
identity, history, workspace and editor session, issues a fresh connection token,
and sends no task or queued message. If its workspace daemon died, Reconnect
restores that editor checkpoint first; it never substitutes an empty session.
Current limits and provider load/resume support are checked again; failed
attempts leave the child available to retry.
Recovery itself never starts child providers. Ended children cannot reconnect.

Workspace windows open on the editor host. A build without native windows shows
`thc-edit --resume SESSION_ID` to run in a terminal instead. When the editor runs
over SSH, use a display attachment from your SSH client; launching a window on
the remote host does not open one on your local desktop.

The main conversation receives the authenticated `editor` MCP server. Children
receive an `editor` server for their workspace and a separate `agents` server
for coordination. The host supplies their identities; they cannot impersonate
you or another agent through tool arguments. Model/effort choices come from the
provider, and context forks require actual provider support.

Set limits in the shared configuration:

```toml
[editor.agents]
max_agents = 8
max_subagents = 4
```

The total includes the main conversation; the second limit counts direct
children per agent. Project `thc.toml` can lower either ceiling. Lowering limits
does not end existing work, and agents cannot raise them. See
[agent limits](configuration.md#agent-limits) and the
[coordination tool reference](agent-tools.md#agent-sessions-and-coordination).

## Questions and tool activity

An agent can use `ask_user` to ask a question with choices and a free-text answer inside the conversation. Your draft stays where you left it. A single tool call has a chevron and description. Consecutive calls share a double-chevron row with their count and any running or failed calls. Expand the run to see individual calls, then expand a call to inspect its retained request and reply JSON. Collapsing the run keeps your individual expansions.

## Controls, settings and documentation

`editor_input` operates the editor through mouse, key and paste events in character-cell coordinates. It follows the same menus and editing rules as direct input. Its input origin is assigned by the host. It cannot type as you in a conversation,
answer its own questions or approvals, change agent settings or permissions,
or turn off Streamer mode. Each batch starts without your clipboard, key prefix
or drag state; agent gestures can span events within that batch.

`editor_settings` reads current display/editing settings as JSON. Supply `settings` to change this session or `defaults` to save startup defaults for future sessions. These cover appearance, screen mode and dimensions, WordStar keys, cursor blinking, CRT filtering and Unicode/icon rendering; startup defaults also include backend and scale. `streamerMode` is reported read-only. `agent_settings` exposes the provider
executable, environment variable names, model choices and context usage;
argument values, environment values and session keys are omitted. It also reports
the global/project context supplied by the user. **Options > Agent Context** opens
the selected TOML for editing; saved changes reach the next query or steering
message. See [configuration](configuration.md#agent-context).

`docs_list`, `docs_search` and `docs_read` provide the editor manual and Turbo Haskell compiler documentation. Select the `editor` or `thc` corpus. Compiler documentation comes from the configured THC checkout (`THC_ROOT` or the project's THC build configuration). Reads return line numbers and heading information, so the agent can cite a particular section.

## What the agent can see and control

`editor_screen` includes separate readable/clickable cell masks and current
command/key permissions. Conversation drafts and unanswered input, sensitive
fields and session keys are redacted from text and image views. Agent settings
can be read but not changed. Generic buffer/selection reads follow the same
privacy rules, so switching tools does not reveal hidden values.

`clipboard_write` copies text supplied by the agent into the editor clipboard
and queues a copy for the attached frontend. It never reads your existing
clipboard. Browser or terminal clipboard permissions can prevent that final
system copy; the tool reports that it was queued rather than claiming the OS
accepted it.

These are boundaries on editor services. A separately enabled terminal execution
tool still runs programs with the editor account's access.

## Agent Permissions

Open **Options > Agent Permissions** to choose each tool's policy:

- **Enable** lets the agent use it directly.
- **Prompt** asks you before each invocation.
- **Disable** refuses the invocation.

Read-only tools start enabled; tools that can change the session or execute programs start with Prompt. Policies apply to every call, including tools an agent discovered before you disabled them. Cancelling a pending approval removes the request; approval does not revive a cancelled call. Tools that already ran are not reversed by cancellation.

Policies live in `[editor.mcp.permissions]` in the shared [configuration file](configuration.md). They apply to all editor sessions using that file. The editor preserves other sections, including future compiler configuration.

## Connect another MCP client

Register a stdio server with this command and the session ID printed when the display detaches:

```sh
thc-edit --mcp-editor SESSION_ID
```

The bridge connects to that session's private endpoint without taking its display connection. Tools keep working while the display is detached. Use `tools/list` for the current schemas. Long-running protocol requests and questions wait outside the desktop lock, so the person using the editor can continue working. Cancelling an MCP request cancels its wait; it does not reverse an operation that already ran.
