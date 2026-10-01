# Agent operation reference

Compact reference for the editor MCP server. Start with the
[skills catalog](agent-skills.md) for task workflows or [session tools](session-tools.md)
for the user guide. `tools/list` is authoritative for the running version’s
schemas; fields below omit JSON type boilerplate.

**Notation:** `?` marks an optional argument; `{}` means no arguments. **R** is
read-only, **W** changes editor/files/settings, **X** can execute/control programs,
**Q** asks the user. These are descriptive categories: actual policy is selected
per tool in **Options > Agent Permissions**. Read-only tools default to Enable;
other tools default to Prompt. `editor_settings` can write, so even its read form
uses that tool’s policy.

## Buffers, files and search

| Tool | Kind | Arguments | Result / contract |
| --- | --- | --- | --- |
| `list_buffers` | R | `{}` | Buffer IDs, paths, revisions, dirty state; includes untitled buffers |
| `list_windows` | R | `{}` | Window IDs, titles, buffer IDs, rectangles, active window and panels |
| `read_buffer` | R | `bufferId?`, `startLine?`, `lineCount?`, `byteOffset?` | Live text; 200 lines default, 1000 maximum. Binary: up to 4096 hex bytes. Defaults to active buffer |
| `read_selection` | R | `windowId?` | Selected content and cursor offsets; defaults to active window |
| `workspace_project` | R | `{}` | Project root, Cabal package file, active source, unsaved buffers, existing Cabal component/dependency graph |
| `workspace_search` | R | `query`, `trackedOnly?`, `offset?`, `limit?` | Literal line matches with live-buffer substitution; tracked-only or ignore-respecting workspace search |
| `editor_file` | W | `action`, `bufferId?`, `windowId?`, `path?`, `revision?`, `dirtyAction?` | `open`, `save`, `close`; save/close require current revision. Dirty close requires `save` or `discard` |
| `buffer_apply_diff` | W | `bufferId`, `revision`, `diff` | Strict unified diff, atomic and undoable, no save; returns new revision |
| `workspace_files` | W | `operation`, `path`, `to?` | `mkdir`, `create_file` (empty), `delete`, `rename`; only rename takes `to` |

Search: `query` is 1–256 characters on one line; `offset` 0–9999; `limit` 1–1000
(default 100). Skips binary/files over 1 MiB; caps the scan at 32 MiB and 10,000
files/matches. Inspect truncation flags. Filesystem mutations stay inside the
project, refuse overwrite/dirty descendants, protect Git metadata and symlink
endpoints, and do not recursively delete directories. Save uses disk-conflict
checks. Private authority files and protected conversation contents are excluded
or redacted across these surfaces.

`workspace_project.cabalPlan` reads only `dist-newstyle/cache/plan.json`; it does
not invoke Cabal. `status` distinguishes `available`, `missing`, `invalid`,
`too-large`, and `unavailable`. Available results include compiler/Cabal versions,
plan modification time and age, package unit IDs/names/versions/type/style,
local component references, and Cabal's `depends`/`exeDepends` edges. Nested
Custom Setup components retain their separate dependencies. Package source roots are
workspace-relative; external/protected paths, repository URLs, flags and compiler
arguments are omitted.

`freshness.status` is `stale` when known manifests are newer or have unsaved
edits, otherwise `unknown`: timestamps do not establish that a plan matches the
current source or configuration. Check `dependenciesKnown`, `graphComplete`,
`omittedUnits` and `truncatedUnits` before treating the result as complete. Reads
stop at 8 MiB and inspect at most 4096 units; graph output is capped at 512 KiB
and the full response at 1 MiB. Truncation removes whole units, retaining every
reported unit's emitted dependency edges, which may refer to omitted units.

## History

| Tool | Kind | Arguments | Result / contract |
| --- | --- | --- | --- |
| `editor_history` | R | `bufferId`, `direction?`, `offset?`, `limit?` | Diff previews, newest first, for steps that would be applied; no mutation |
| `editor_undo` | W | `bufferId`, `revision`, `direction?`, `steps?` | Apply undo/redo atomically; no save; returns revision and remaining counts |

`direction` is `undo` (default) or `redo`. Preview offset 0–100, limit 1–10
(default 5); apply 1–100 steps (default 1). Insufficient history/stale revision
fails without edits. Binary previews describe byte changes.

## Haskell language service and Messages

| Tool | Kind | Arguments | Result / contract |
| --- | --- | --- | --- |
| `workspace_diagnostics` | R | `offset?`, `limit?` | HLS/build/Messages snapshot with positions and reported/current revisions |
| `lsp_hover` | R | `bufferId`, `line`, `column`, `revision?` | Hover text and type information |
| `lsp_definition` | R | `bufferId`, `line`, `column`, `revision?` | Definition locations |
| `lsp_type_definition` | R | `bufferId`, `line`, `column`, `revision?` | Type definition locations |
| `lsp_references` | R | `bufferId`, `line`, `column`, `revision?`, `includeDeclaration?` | Reference locations; declarations included by default |
| `lsp_document_symbols` | R | `bufferId`, `revision?` | Document symbol structure |
| `lsp_rename` | W | `bufferId`, `revision`, `line`, `column`, `newName` | Apply HLS rename to live buffers; reject stale source; no save |
| `lsp_code_actions` | R | `bufferId`, `revision`, `line`, `column`, `endLine?`, `endColumn?` | List up to 128 action IDs with title/kind/preference and disabled reason; includes current intersecting diagnostics |
| `lsp_apply_code_action` | W | `bufferId`, `revision`, `actionId` | Consume a listed action once; resolve advertised text edits, then apply atomically to buffers; no save |

Calls synchronize unsaved Haskell text. Editor input positions use 1-based
Unicode code points; raw LSP results use 0-based UTF-16. Responses identify the
source revision. Diagnostic messages are capped at 8192 characters. Code-action
ranges default to the cursor; supply both end coordinates for a selection.
A new action list expires previous IDs. Application requires the original source
revision and unchanged affected files, excludes protected files and other
projects, and never accepts caller-supplied edits or commands. Command-only and
edit-plus-command actions are listed disabled; resource operations are refused.
`workspace/executeCommand` and unsolicited `workspace/applyEdit` are unsupported.

## Build, test and execution

| Tool | Kind | Arguments | Result / contract |
| --- | --- | --- | --- |
| `build_status` | R | `{}` | Active job/last completion, output buffer ID, actual exit code |
| `build_start` | X | `action`, `toolchain?`, `target?`, `arguments?`, `terminal?` | `compile`, `make`, `run`; `THC` or `GHC`; overrides apply to this job |
| `build_stop` | X | `{}` | Stop captured build/run; keep output and completion status |
| `test_start` | X | `target?`, `toolchain?` | Cabal test job; supported toolchain `GHC` |
| `test_status` | R | `{}` | Explicit suite outcomes, process status and compiler diagnostics |
| `terminal_list` | R | `{}` | Shared Ghostty terminal IDs, buffer IDs and exit codes |
| `terminal_start` | X | `command`, `args?`, `cwd?`, `outputByteLimit?` | Executable plus argument array; no implicit shell; opens window and returns terminal ID |
| `terminal_output` | R | `terminalId`, `offset?`, `limit?` | Retained output and exit code; at most 128 KiB per call |
| `terminal_input` | X | `terminalId`, `text` | UTF-8 input, including control characters |
| `terminal_stop` | X | `terminalId` | Terminate process; keep output window |

Builds require saved source buffers. Build/tests share one job slot. Read build
output through `read_buffer`. Terminal output offsets refer to the retained tail;
`truncated` reports discarded earlier output. Execution runs with the editor
account’s access. UI privacy masks are not an OS execution sandbox.

## Debugging

| Tool | Kind | Arguments | Result / contract |
| --- | --- | --- | --- |
| `debug_status` | R | `{}` | Adapter readiness, stopped state, generation, selected frame, follow mode, capabilities, breakpoints, recent output |
| `debug_present` | W | `follow?`, `view?`, `generation?` | Set automatic UI following; reveal `source`, `stack`, `scopes` or `output` from the shared session |
| `debug_launch` | X | `adapterConfig?`, `port?` | Configured THC target or general DAP JSON config; refuses an already active session |
| `debug_attach` | X | `host?`, `port?` | Loopback adapter; defaults to `127.0.0.1:4711` |
| `debug_control` | X | `generation`, `command` | `continue`, `next`, `stepIn`, `stepOut`, `pause`, `disconnect`; acceptance is not a stop |
| `debug_set_breakpoints` | W | `generation`, `bufferId`, `lines` | Replace that buffer’s source breakpoint set; pending/verified state, `sourceModified` |
| `debug_inspect` | R | `generation`, `request`, `threadId?`, `frameId?`, `variablesReference?`, `sourceReference?`, `start?`, `count?` | `threads`, `stackTrace`, `scopes`, `variables`, `source`; handles depend on request |

`follow: false` keeps automatic stops in the background. `follow: true` follows
future stops. An explicit `view` requires current `generation`; source/stack/scopes
require a stopped, configured session. Reveal a view without changing follow
mode by omitting `follow`. This does not launch or resume a separate debugger.

Refresh generation after execution changes. Stack/variable inspection requires
a stopped target. Inspection pages default to 100 entries, maximum 1000.
Breakpoints use 1-based lines, at most 1000. See the
[debugging skill](agent-skills.md#debug-a-program), also published as
`thc-edit://debugging` through MCP resources.

## Git

| Tool | Kind | Arguments | Result / contract |
| --- | --- | --- | --- |
| `workspace_git` | R | `view`, `path?` | `status` or `diff`; disk changes and unsaved buffers reported separately |
| `git_fetch` | W | — | Fetch the selected repository’s configured default remote; returns `accepted` and `jobId` |
| `git_operation_status` | R | `jobId?` | `busy` and the requested fetch job’s `running`, `succeeded`, or `failed` state and actual `exitCode` |

Diff text is capped at 128 Ki characters. A path filter is a literal file path,
not a Git glob/magic pathspec. Whole-repository diffs omit private authority files and Git-detected rename/copy
destinations derived from them; `omittedFiles` reports a count without their names. Agent mouse/key input cannot
open the unrestricted human Git review or commit dialog; use `workspace_git`
for filtered review. `workspace_search` with
`trackedOnly: true` searches tracked source. Pull, merge, commit, and commit-history
search are not exposed as agent tools.

`git_fetch` and `git_operation_status` have separate permission policies. Fetch
shares the human Git operation slot, permits unsaved editor changes, and does not
change working files. It accepts no remote URL, refspec, shell, or configuration
overrides. Acceptance does not imply success: poll status for completion. The
latest 16 agent jobs remain queryable for this session; omit `jobId` for the latest.
`busy` also includes human Git operations. A launch failure has `state: "failed"`
and a null exit code; a process exit reports its actual code. Transport output,
URLs, and raw errors are not returned or placed in agent fetch output buffers.
Closing the session cancels and reaps the active Git process.

## Windows, panels and binary navigation

| Tool | Kind | Arguments | Result / contract |
| --- | --- | --- | --- |
| `editor_layout` | R | `{}` | Character-cell resolution, window rectangles, modes, cursors and docks |
| `editor_navigate` | W | `windowId?`, `bufferId?`, `path?`, `line?`, `column?`, `byteOffset?` | Focus/open and navigate; no save; byte offsets require hex mode |
| `editor_arrange` | W | `action`, `windowId?`, `x?`, `y?`, `width?`, `height?` | `tile`, `cascade`, `split_vertical`, `split_horizontal`, `focus`, `move`, `resize`, `zoom` |
| `editor_panels` | W | `files?`, `messages?`, `filesWidth?`, `messagesHeight?` | Set visibility/dock dimensions; panel must be visible to resize |
| `editor_mode` | W | `mode`, `windowId?`, `bufferId?` | `text` or `hex`; preserve bytes; refuse invalid UTF-8/NUL in text mode |

Coordinates are 0-based character cells, text locations 1-based, byte offsets
0-based. Geometry obeys screen limits and docking; use returned rectangles.
Files width is at least 16 cells; Messages height at least 3, within available
screen space. Split windows share their buffer and undo history.

## Screen and input

| Tool | Kind | Arguments | Result / contract |
| --- | --- | --- | --- |
| `editor_screen` | R | `image?` | Colorless text grid, optional PNG, readable/clickable cell masks, command/key permissions |
| `editor_input` | W/X | `events` | 1–64 sequential input events; returns applied count, exit flag, error, text copied by the agent and settings |
| `clipboard_write` | W | `text` | Write supplied text and queue frontend copy; never read existing clipboard |

Input event forms:

```json
{"type":"key","key":"F3","mods":[]}
{"type":"paste","text":"supplied text"}
{"type":"mouse","action":"down","x":10,"y":5,"button":0,"clicks":1,"mods":[]}
{"type":"mouse","action":"up","x":10,"y":5,"button":0}
{"type":"modifiers","mods":["ctrl"]}
{"type":"blur"}
```

Mouse actions: `down`, `up`, `move`, `wheel-up`, `wheel-down`; button 0 left,
2 right. Modifiers: `ctrl`, `alt`, `shift`. Keys: characters, `Enter`, `Escape`,
`Tab`, `ArrowUp/Down/Left/Right`, `Home`, `End`, `PageUp`, `PageDown`, `Backspace`,
`Delete`, `Insert`, `F1`–`F24`. Paste is capped at 64 Ki characters per event.
Input is not transactional: inspect partial results before retrying.

The host identifies input from an agent. Each batch starts without human clipboard,
drag or key-prefix state; gestures may span events within the batch. Agent input
cannot submit chat as the person, answer its own questions/approvals, modify agent
settings/permissions or toggle Streamer mode. Generic reads and screen captures
also protect drafts, authority files and session keys.

Screen capture is capped at 32,768 cells; PNG at 4 megapixels/2 MiB. PNG uses the
bundled bitmap font, excluding OS chrome, CRT and native Unicode shaping.
Clipboard text is capped at 1 MiB UTF-8 with no NUL; frontend permission can
prevent OS export even after queueing succeeds.

## Agent sessions and coordination

The primary conversation receives these tools through its authenticated `editor`
server. Children receive an `editor` server for their own workspace and an
`agents` server for coordination. Caller identity is assigned by the host; tool
arguments cannot select another sender or redirect editor calls to another
workspace.

| Tool | Kind | Arguments | Result / contract |
| --- | --- | --- | --- |
| `agent_directory` | R | `{}` | Named agents, stable IDs, parents, state, workspace, limits and advertised model/effort choices |
| `agent_spawn` | X | `name`, `task`, `context?`, `sourceAgentId?`, `model?`, `effort?`, `workspace?` | Create child, queue its task, return metadata and message ticket; no display focus change |
| `agent_rename` | W | `agentId`, `name` | Rename self or another agent; stable ID and rename attribution remain |
| `agent_message` | X | `agentId`, `text` | Queue attributed message; return ticket |
| `agent_wait` | R | `agentId`, `ticket`, `timeoutMs?` | Wait 0–60,000 ms, default 30,000; timeout reports running without cancelling |
| `agent_cancel` | X | `agentId` | Cancel current/queued messages; human or ancestor only; keep session |
| `agent_end` | X | `agentId` | End session and descendants; human or ancestor only; retain history and worktrees |
| `agent_history` | R | `agentId`, `after?`, `limit?` | Retained events; `nextAfter` continues, `dropped` counts discarded older events |
| `agent_search` | R | `agentId`, `query`, `after?`, `limit?` | Literal, case-insensitive search of retained events |

Names are unique ignoring case, at most 80 characters without controls. Tasks
and messages accept at most 65,536 characters. History pages default to 50
entries, maximum 100; search queries accept at most 4096 characters. Agent IDs
remain stable when names change. A parent occupies its child's user seat;
messages from other agents remain peer messages, not human instructions.

`context` defaults to `fresh`. `fork` requires `sourceAgentId`, caller ownership
and actual provider fork support; it never silently starts a fresh conversation.
Choose `model` and `effort` only from provider-advertised choices.

`workspace` defaults to `{"mode":"shared"}`. Use `{"mode":"worktree"}` with
optional `ref`, `branch` and `name` for a separate Git checkout from committed
source. Unsaved/uncommitted parent edits are not copied. Its editor runs without
a display until opened, with separate buffers, build jobs, terminals and debugger.
Shared children use their parent's editor and its existing job slots. Ending an
agent does not delete its worktree.

Global `[editor.agents]` sets `max_agents` (default 8, range 1–64, including the
primary) and `max_subagents` (default 4, range 0–64 direct children per agent).
Project `thc.toml` may lower these ceilings; agents cannot raise them. Both limits
are checked at spawn. See [configuration](configuration.md#agent-limits).

**Tools > Agents** or **Window > Agents** opens the directory. History is a
read-only view of the latest retained events; Workspace opens the associated
editor. The directory currently provides browsing, not a child conversation
composer or child-provider reconnect control.

## Conversation and settings

| Tool | Kind | Arguments | Result / contract |
| --- | --- | --- | --- |
| `ask_user` | Q | `question`, `choices?`, `allowMultiple?` | One inline question, optional single-choice answers, always free text; waits for human |
| `agent_settings` | R | `{}` | Public provider/model/config choices, connection/steering state, context usage and global/project guidance |
| `editor_settings` | W | `settings?`, `defaults?` | Read current state, change session settings or merge future startup defaults |

Questions: 1–4096 characters; up to 12 choices of 1–256 characters;
`allowMultiple` must be false. One pending question, no human-answer timeout.
Agent settings omit argument/environment values and session keys, returning only
argument count and environment names; secret-labelled settings are redacted.

`settings`: `appearance` (`light`, `dark`, `system`), `screenMode` (3, 259),
`columns` (40–512), `rows` (12–256), `wordStar`, `blinkCursor`, `crtFilter`,
`pixelateUnicode`, `materialIcons`. Selecting a mode defaults to its 80×25/80×50
grid unless dimensions are supplied. `defaults` additionally accepts `backend`
(`terminal`, `auto`, `metal`, `vulkan`, `web`, `remote`) and `scale` (1–8).
Omitted fields remain unchanged. `streamerMode` is returned read-only. Current
backend/pixel scale and agent settings are not changed by this tool.

## Documentation

| Tool | Kind | Arguments | Result / contract |
| --- | --- | --- | --- |
| `docs_list` | R | `corpus?`, `offset?`, `limit?` | Paths, titles, heading line numbers and pagination |
| `docs_search` | R | `query`, `corpus?`, `path?`, `offset?`, `limit?`, `caseSensitive?` | Literal matches with line numbers and enclosing headings |
| `docs_read` | R | `path`, `corpus?`, `startLine?`, `lineCount?` | Document range, heading metadata, total lines, continuation flags |

`corpus` is `editor` (default) or `thc`; compiler docs use the configured THC
checkout. Paths are relative listed documentation paths. List/search page limit
1–100; read 1–500 lines, default 200. Search examines at most 256 files/16 MiB;
reads accept UTF-8 documents up to 1 MiB and return at most 128 Ki characters.
Use truncation and continuation flags. Start at `docs/agent-skills.md` for skills
or this document for calls. Both are bundled for offline/site consumption.

## Permissions and lifecycle

The host applies Enable/Prompt/Disable per invocation, including previously
discovered tools. Policies live in `[editor.mcp.permissions]` in
`$XDG_CONFIG_HOME/thc/config.toml`, falling back to `~/.config/thc/config.toml`.
Startup defaults use `[editor.defaults]`; precedence is CLI → environment →
project defaults → global configuration. Project `thc.toml` can set defaults
and agent context, but cannot override global permissions. See
[configuration](configuration.md).

A pending approval can be cancelled; cancellation does not undo an operation
already executed. Long-running questions, HLS and debugger replies release the
desktop lock while waiting. After a disconnect or uncertain reply, inspect current
state before retrying a mutation. IDs belong to the editor session.

External clients connect with `thc-edit --mcp-editor SESSION_ID`. The bridge
shares the session without taking over its display. Skills describe workflows;
MCP tool schemas and host policy govern the actual calls.
