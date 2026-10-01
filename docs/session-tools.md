# Work with the editor

The guest in a conversation can use the same editor you do: read an unsaved buffer, ask HLS for its type, rename a symbol, move a window, inspect a stopped program, or run a build and read the result. The built-in `editor` MCP server is supplied automatically when an ACP conversation starts or resumes.

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

`editor_layout` reports screen and panel dimensions in character cells. `editor_arrange` focuses, moves, resizes, tiles, cascades, splits or zooms windows through the normal window manager. `editor_panels` shows, hides and resizes Files and Messages. `editor_mode` selects text or hex; `editor_navigate` jumps to a line/column or a hex byte offset.

`editor_history` previews undo or redo steps as diffs, including hex changes. `editor_undo` applies the requested number of steps after checking the buffer revision. It changes the live buffer; saving remains a separate action.

## Haskell and diagnostics

`workspace_diagnostics` reads Messages and HLS/build diagnostics with paths, positions and revisions. The `lsp_` tools provide hover/type information, definitions, type definitions, references, document symbols and rename. They synchronize unsaved source text with the running language server. Rename updates buffers without saving and rejects results if the source changed while HLS was working.

Editor positions are 1-based Unicode character positions. Raw LSP results retain LSP's 0-based UTF-16 positions; each response identifies the buffer revision it describes. Byte offsets are 0-based.

## Build, test and run

`build_start` uses the selected THC/GHC configuration for compile, make or run. It requires saved source buffers and captures output in an editor window. `build_status` reports the output buffer and actual exit code; `build_stop` stops the job. A run can request a shared terminal instead.

`test_start` runs Cabal tests with GHC; `test_status` reports explicit Cabal suite outcomes alongside the process result and compiler diagnostic count. A successful process without suite output is reported as completed, not as invented individual passing tests. Test and build commands share one job slot.

`terminal_list`, `terminal_start`, `terminal_output`, `terminal_input` and `terminal_stop` use the same Ghostty terminals as the editor and ACP. Output is bounded and reports truncation and exit status. Execution tools can run programs with the editor's account and receive their inputs; choose Enable, Prompt or Disable under **Options > Agent Permissions**.

## Debugging

`debug_status` reports the adapter, stop state, frame, breakpoints and current generation. Launch the selected THC target with `debug_launch`, supply an adapter configuration for another DAP implementation, or attach to a loopback adapter with `debug_attach`.

Use `debug_set_breakpoints`, `debug_control` and `debug_inspect` to set source breakpoints, continue/step/pause, and inspect threads, stack frames, scopes, variables and adapter-owned source. Handles belong to a debug generation; the tool rejects them after execution advances. An accepted control command does not imply that the program has reached its next stop.

The [debugging skill](../skills/debug-editor/SKILL.md) is also available to MCP clients through `resources/read` at `thc-edit://debugging`.

## See what is on screen

`editor_screen` returns the full character grid without colors. Set `image: true` to include a PNG of the same frame. This gives the guest a small text representation for navigation and a visual representation for borders, colors and overlapping windows.

The PNG uses the bundled bitmap fonts and the current screen-mode aspect ratio. It includes the editor cursor but excludes OS window chrome, CRT effects and platform font shaping; complex Unicode clusters use a bitmap approximation.

## Questions and tool activity

A guest can use `ask_user` to ask a question with choices and a free-text answer inside the conversation. Your draft stays where you left it. Tool activity appears as a compact chevron and description; expand it to inspect the complete request and reply JSON.

## Controls, settings and documentation

`editor_input` operates the editor through mouse, key and paste events in character-cell coordinates. It follows the same menus and editing rules as direct input. Its input origin is assigned by the host. It cannot type as you in a conversation,
answer its own questions or approvals, change agent settings or permissions,
or turn off Streamer mode. Each batch starts without your clipboard, key prefix
or drag state; guest gestures can span events within that batch.

`editor_settings` reads current display/editing settings as JSON. Supply `settings` to change this session or `defaults` to save startup defaults for future sessions. These cover appearance, screen mode and dimensions, WordStar keys, cursor blinking, CRT filtering and Unicode/icon rendering; startup defaults also include backend and scale. `streamerMode` is reported read-only. `agent_settings` exposes the provider
executable, environment variable names, model choices and context usage;
argument values, environment values and session keys are omitted.

`docs_list`, `docs_search` and `docs_read` provide the editor manual and Turbo Haskell compiler documentation. Select the `editor` or `thc` corpus. Compiler documentation comes from the configured THC checkout (`THC_ROOT` or the project's THC build configuration). Reads return line numbers and heading information, so the guest can cite a particular section.

## What the guest can see and control

`editor_screen` includes separate readable/clickable cell masks and current
command/key permissions. Conversation drafts and unanswered input, sensitive
fields and session keys are redacted from text and image views. Agent settings
can be read but not changed. Generic buffer/selection reads follow the same
privacy rules, so switching tools does not reveal hidden values.

`clipboard_write` copies text supplied by the guest into the editor clipboard
and queues a copy for the attached frontend. It never reads your existing
clipboard. Browser or terminal clipboard permissions can prevent that final
system copy; the tool reports that it was queued rather than claiming the OS
accepted it.

These are boundaries on editor services. A separately enabled terminal execution
tool still runs programs with the editor account's access.

## Agent Permissions

Open **Options > Agent Permissions** to choose each tool's policy:

- **Enable** lets the guest use it directly.
- **Prompt** asks you before each invocation.
- **Disable** refuses the invocation.

Read-only tools start enabled; tools that can change the session or execute programs start with Prompt. Policies apply to every call, including tools a guest discovered before you disabled them. Cancelling a pending approval removes the request; approval does not revive a cancelled call. Tools that already ran are not reversed by cancellation.

Policies live in `[editor.mcp.permissions]` in the shared [configuration file](configuration.md). They apply to all editor sessions using that file. The editor preserves other sections, including future compiler configuration.

## Connect another MCP client

Register a stdio server with this command and the session ID printed when the display detaches:

```sh
thc-edit --mcp-editor SESSION_ID
```

The bridge connects to that session's private endpoint without taking its display connection. Tools keep working while the display is detached. Use `tools/list` for the current schemas. Long-running protocol requests and questions wait outside the desktop lock, so the person using the editor can continue working. Cancelling an MCP request cancels its wait; it does not reverse an operation that already ran.
