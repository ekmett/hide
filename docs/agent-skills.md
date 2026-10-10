# Agent skills

Work with the person’s live editor: the same buffers, windows, language server,
terminals and debugger. A skill describes how to finish a task with those tools.
The [operation reference](agent-tools.md) describes each call.

## Choose a skill

| Skill | Use when | Evidence of completion |
| --- | --- | --- |
| [Explore a project](#explore-a-project) | Locate code, understand the package or follow a symbol | Relevant files, symbols and current revisions identified |
| [Read a conversation or text window](#read-a-conversation-or-text-window) | Inspect published window content without screen scraping | Window ID, readable text and pagination accounted for |
| [Edit and review](#edit-and-review) | Make a focused change or apply a patch | Intended buffer diff, with saved/unsaved state explicit |
| [Diagnose and refactor Haskell](#diagnose-and-refactor-haskell) | Explain types, fix diagnostics or rename a symbol | Current HLS result and reviewed edits |
| [Build, test and run](#build-test-and-run) | Check a change or execute the project | Actual completion status and relevant output |
| [Debug a program](#debug-a-program) | Investigate execution with THC or another DAP adapter | Observed stop, stack or variable evidence |
| [Review repository changes](#review-repository-changes) | Understand saved changes alongside live edits | Disk diff and unsaved changes accounted for separately |
| [Organize the desktop](#organize-the-desktop) | Show source, Messages, hex data or a particular layout | Returned geometry and mode |
| [Inspect and reproduce a UI problem](#inspect-and-reproduce-a-ui-problem) | Check a dialog, menu, focus or layout problem | Masked before/after screen and exact applied input |
| [Coordinate agents](#coordinate-agents) | Delegate work or inspect another agent’s progress | Attributed task ticket, actual completion and reviewed workspace changes |
| [Consult the user](#consult-the-user) | A decision needs the person’s answer | Submitted answer, not a draft or guessed choice |
| [Consult documentation and settings](#consult-documentation-and-settings) | Learn a feature or inspect/configure the editor | Cited documentation or confirmed settings |

## Shared rules

- Apply the single [protected content policy](agent-tools.md#protected-content-policy)
  across reads, UI interaction and mutations; changing tools does not grant access.
- Discover `tools/list` for this running version. Read only the skill needed for
  the task; tool schemas supply exact arguments. A skill grants no permissions.
- Use `bufferId`/`windowId` from this session. Read the live buffer before editing:
  the file on disk may be older. Refresh a rejected revision rather than retrying
  an old patch. Save is an explicit operation.
- Editor lines/columns are 1-based Unicode code points; byte offsets and screen
  cell coordinates are 0-based. Raw LSP positions are 0-based UTF-16.
- Report observed results. Submission is not completion; a queued clipboard
  export is not proof of a system copy. Read truncation/pagination flags.
- Use structured tools for ordinary operations. Use screen/input tools when the
  task concerns the UI. Honor the current readable/clickable masks and input
  permissions. Hidden values, user drafts and approval controls stay private.

## Explore a project

**Use:** find the relevant package, code and symbols before changing anything.

**Do:** `workspace_project` → `list_buffers` → `workspace_search`. Use
`trackedOnly: true` for Git-tracked files. Read matching live buffers with
`read_buffer`; open/navigate unopened source with `editor_navigate`. For Haskell,
use `lsp_document_symbols`, `lsp_definition`, `lsp_type_definition` and
`lsp_references` to follow structure instead of guessing from text matches.

**Why these tools:** shell search sees saved files. These tools see live buffers,
including the person’s selected text through `read_selection`, and enforce the
same resource permissions across source and symbol results.

**Check:** record the paths, buffer IDs and revisions used. Search substitutes
unsaved buffer contents; its default search also includes untitled buffers.

**Recover:** page results and inspect skipped/truncated counts before treating a
missing match as absent. Project context identifies the root/package. The existing
Cabal component/dependency graph is available through `workspace_project` and
**Tools > Project browser**.

## Read a conversation or text window

**Use:** inspect published conversation text or another prepared text window.

**Do:** `list_windows` → `read_window` with the chosen `windowId`. Page by
`startLine` and `lineCount`. For a source window, use its `bufferId` with
`read_buffer` instead.

**Why these tools:** read the logical text directly without drawing or decoding a
screenshot. The installed body declares whether it is readable; protected spans
are masked. Human drafts and approval controls are not conversation history.

**Check:** coordinates belong to `window-text`, not source offsets or screen
cells. Check `redacted`, `truncated`, `lineCount` and `totalLines`. Use
`editor_screen` when the task concerns placement, clipping or controls.

**Recover:** a body refreshed or closed before capture requires a new request.
A stopped conversation may remain readable; that does not restore its actions or
agent connection. Private windows and Rows/Details projections refuse this read.

## Edit and review

**Use:** make a source change, create a file, or undo a known edit.

**Do:** `read_buffer` → `buffer_apply_diff` with that revision → `read_buffer`.
Use one strict unified diff for the target buffer. Create an empty file with
`workspace_files`, then open and edit it. Preview history with `editor_history`;
apply undo/redo with `editor_undo` and the current revision.

**Why these tools:** edits join the editor’s undo history and use exact captured
versions. A disk rewrite cannot safely replace an unsaved buffer.

**Check:** inspect the resulting change, run the relevant check, then use
`editor_file` to save when saving is part of the task. Name what remains dirty.

**Recover:** stale revision or mismatched context means reread and reconstruct
the patch. Disk conflicts require review; do not silently overwrite them. A
failed atomic patch/history request leaves the buffer unchanged. Closing a dirty
view requires an explicit save/discard choice; another view may share its buffer.

## Diagnose and refactor Haskell

**Use:** explain a type, fix a diagnostic, or refactor source across the package.

**Do:** `workspace_diagnostics` → read/navigate the relevant source → `lsp_hover`
or symbol/location tools. For rename, call `lsp_rename` with `bufferId`, current
`revision`, `line`, `column`, and `newName`. For a quick fix or refactoring, call
`lsp_code_actions` for the current source range, inspect titles and disabled
reasons, then pass the chosen ID and original source revision to
`lsp_apply_code_action`. Review the changed buffers.

**Why these tools:** reuse the project’s live HLS session and unsaved source,
with checked edit application instead of asking a second language server about
older files on disk.

**Check:** distinguish the diagnostic’s reported version from the current
buffer revision. HLS receives unsaved source. Rename and code actions change live buffers;
saving and rebuilding remain separate steps.

**Recover:** if source changes while HLS works, refresh and retry against the new
revision. If HLS is unavailable, report its result and inspect build output.
Refresh expired or consumed action IDs with another list. HLS command actions
and lazy resolution use the same session; successful commands do not restart it.
Cancellation asks HLS to settle before a bounded restart fallback. Resource file
operations remain unsupported; read the action result for partial effects before
retrying. See the [command contract](agent-tools.md#haskell-language-service-and-messages).

## Build, test and run

**Use:** compile, test or run the selected THC/GHC project.

**Do:** inspect unsaved buffers; save the intended source changes.
`build_start` chooses `compile`, `make` or `run`, with optional job-local
`toolchain`, `target` and `arguments`. Poll `build_status`; page combined stdout/stderr with `build_output` and its
exact `jobId`. Offsets count Unicode characters; each read is limited to 32768
characters. A new job expires the previous ID; closing its window does not stop
the job or discard the last retained output.
Use `test_start` for GHC/Cabal tests and `test_status` for suite outcomes and explicit TAP 13 cases. For an
interactive program, use a shared terminal: `terminal_start`, `terminal_input`,
`terminal_output`; `terminal_list` finds existing terminals.

**Why these tools:** builds, tests and terminals remain visible in the workspace
that owns them. They share its environment and diagnostics; terminal input goes
to its existing PTY rather than an unrelated shell. Execution still requires its
own permission.

**Check:** report the exit code, explicit suite/case outcomes and relevant diagnostics.
For TAP output, check stream completeness and result truncation before claiming
that every case was observed.
A successful process with no parsed test results is not evidence that individual
tests passed. Build and test jobs share a slot.

**Environment:** for missing executables/libraries, read `environment_get`
with the relevant names (usually `PATH` or `PKG_CONFIG_PATH`). Preserve existing
search paths and use `environment_set` with session or project scope. Start a
new job; do not prescribe shell exports or restarting the editor. Existing
terminal/provider processes retain their old environment, so restart only those
if needed. Prefer a repository build-configuration fix for shared dependencies.
Values are literal; credentials and editor authority variables are protected.

**Recover:** inspect an existing job before starting another. `build_stop` stops
a captured job; `terminal_stop` stops a terminal process. Retained output can
lose its earlier bytes; read its truncation flag. `terminal_start` takes an
executable and argument array, with no implicit shell.

## Debug a program

**Use:** explain a runtime failure, inspect values or follow control flow.

**Do:** `debug_status` → reuse the active adapter, or `debug_launch` /
`debug_attach` → check readiness. Use `debug_set_breakpoints` with the current
`generation`.
A breakpoint call replaces the entire set for its source buffer: retain desired
existing lines. Once stopped, use `debug_inspect` for threads → stack trace →
scopes → variables.
Use `debug_control` to continue, step, pause or disconnect, then refresh status.

**Why these tools:** inspection and the person’s debugger UI use the same stopped
process and generation. A separate debugger would not have that state.

**Present:** use `debug_present` with `follow: false` to investigate without
moving the person's source window on each stop. At an interesting stop, send
`view: "source"` and the current `generation`; `stack`, `scopes` and `output`
reveal those views. Set `follow: true` to follow future stops automatically.
The agent and the UI share one debugger, breakpoints and stopped state.
`debug_status.sourcePreparation` reports the background source worker: `preparing`
while it reads or decodes, `ready` once its result awaits the UI, and `idle` when
no preparation remains. A human dialog can hold a ready result. Idle alone does
not prove that a source opened; inspect the resulting window or error. Adapter
requests that have not returned a source are separate from this worker state.

**Check:** accepted execution commands are submissions. Wait for the next
observed stop before drawing conclusions. Report pending versus verified
breakpoints and actual values returned by the adapter.

**Recover:** refresh generation after execution changes; do not reuse stale
thread/frame/variable handles. Inspect state after an uncertain transport
failure before relaunching. Unsaved source does not rebuild the running program.
Empty scopes mean the adapter supplied no lexical values.

The packaged debugging skill is also available through MCP `resources/read` at
`hide://debugging`.

## Review repository changes

**Use:** compare the working tree with the change the person intended.

**Do:** `workspace_git` with `view: "status"`, then `view: "diff"`; use a literal
`path` to focus the diff. Inspect `list_buffers` for unsaved edits and read them
separately. Use `workspace_search` with `trackedOnly: true` for repository source
search.

**Check:** Git describes on-disk changes. It does not include unsaved buffer
edits, and tracked-file search does not search commit history.

**Synchronize:** when requested, start `git_fetch`, `git_pull` or `git_merge` and
poll `git_operation_status`. Pull is fast-forward only. Pull/merge require saved
buffers and a clean worktree; inspect failures instead of falling back to a shell
command that bypasses the editor’s checks.

**Commit:** `git_review` → poll for a complete `reviewId` → inspect its diff →
`git_commit` with that ID and message → poll completion. A stale or incomplete
review cannot authorize a commit. Check the resulting HEAD and
`reviewedTreeMatched`, since ordinary Git hooks can change the tree.

**Why these tools:** use the person’s selected repository and operation slot,
protect authority paths, and reconcile saved changes with open buffers. They do
not replace arbitrary Git history queries or confer permission to publish.

**Recover:** narrow a truncated diff to safe literal files. A commit refusal
requires a fresh complete review. Failed hooks can leave files staged; inspect
status before retrying.

## Organize the desktop

**Use:** show related source, arrange panels or inspect binary data.

**Do:** `editor_layout` / `list_windows` → `editor_arrange`, `editor_panels` or
`editor_navigate`. Use `editor_mode` for hex and then navigate by `byteOffset`.

**Why these tools:** they operate the editor’s live windows and docking rules.
Shell commands cannot select its buffers, move its views or preserve their
cursor/selection state. Prefer these commands to simulated mouse input.

**Check:** use returned constrained rectangles, not assumed placement. Split
views share buffers. Text mode refuses invalid UTF-8 or NUL-containing bytes;
stay in hex mode for those files.

## Inspect and reproduce a UI problem

**Use:** investigate a menu, dialog, focus or layout problem, or demonstrate an
interaction that has no structured editor command.

**Do:** capture `editor_screen` and inspect its readable/clickable masks and
command/key permissions. Request `image: true` when colors, borders or overlap
matter; use the text grid for labels and coordinates. Submit a short
`editor_input` batch for one interaction, then capture again before continuing.

**Why these tools:** the screen and admitted input describe this editor’s actual
UI state. Use them for UI work; use buffer, layout and language-service tools
for ordinary editing. Input does not acquire human authority from its coordinates.

**Check:** report the observed before/after state and applied event count. The
PNG shows the editor grid, not OS chrome or the native CRT pass. Do not infer
that a hidden control was activated from a successful input submission.

**Recover:** events are sequential, not transactional. After partial failure,
inspect `appliedEvents` and recapture. Never use input to operate approval,
provider credentials, human drafts or agent permission controls.

## Coordinate agents

**Use:** delegate a scoped task, work on a separate feature branch, or inspect
another agent's progress without taking the person's display focus.

**Why this service:** a child can own a separate editor, buffers, build slot,
terminals and debugger in its worktree. That isolation is the useful distinction
from ordinary provider subagents or a message queue. Use provider orchestration
when no separate editor workspace is needed.

**Do:** read `agent_directory` for IDs, workspace paths, limits and advertised
model/effort choices. Call `agent_spawn` with a provisional `name` and concrete
`task`. Omitted `workspace` creates a separate worktree and hidden editor from the
caller's committed `HEAD`, with independent buffers, build jobs, terminals and
debugger. Unsaved/uncommitted parent edits are not copied. It requires Git;
failure does not fall back to shared edits. Specify optional `ref`, `branch` or
`name` in `workspace: {"mode":"worktree"}` when needed. Use
`workspace: {"mode":"shared"}` only for intentional shared edits in the parent's
editor.
Use a fresh context unless the provider supports a real fork and the source is
yours to fork. After understanding a task, choose a descriptive name with
`agent_rename`; names can also be assigned to other agents without changing IDs.

**Check:** wait on the returned ticket with `agent_wait`; a timeout is still
running. Inspect `agent_history` or `agent_search`, then review the actual file
changes and build/test results in that agent's workspace. Use `agent_message`
for follow-ups. Sender attribution is host-controlled: a peer message does not
occupy the human or controlling parent's user seat.

**Recover:** refresh the directory when a name or state changes. Only the human
or an ancestor can cancel/end a session. `agent_cancel` stops the current reply;
`agent_end` also ends descendants and
preserves their worktrees; it does not merge their edits. Spawn limits come from
`[editor.agents]` in global/project TOML and cannot be raised through tools.
A recovered directory is not proof that its providers are running again.

The primary agent uses its authenticated `editor` server. Children use their
workspace's `editor` server for editor operations and `agents` for coordination.
A shared child uses the parent's editor; a worktree child has a separate hidden
editor session. **Tools > Agents** / **Window > Agents** lets the person open a
live child conversation, send or queue a human message, cancel its reply, open
its workspace, or explicitly reconnect a recovered provider. The human remains
a peer when the child's user seat belongs to its parent. Drafts stay private to
the person and survive switching conversations.

## Consult the user

**Use:** ask for a missing decision without blocking independent work or replacing
the person’s draft.

**Do:** `ask_user` with one clear `question` and optional single-choice `choices`.
A free-text answer is always available. The result supplies `questionId` and
`status: "pending"`; continue independent work and retrieve the result by calling
`ask_user` with only `questionId`. Do not repeatedly poll while nothing depends
on the answer. Only one question may be pending in the conversation.

**Why this tool:** the question appears inline with explicit human submission and
owned result delivery. Neither terminal input nor reading the chat draft provides
that evidence. `clipboard_write` separately exports supplied text when the person
asks for something to copy; it never reads their existing clipboard.

**Check:** pending is not an answer. An explicit submission is queued once to the
original live Primary provider, even if its previous reply has ended. A caller
without a live provider must retrieve its result. Current child editor bridges
lack attributed question callers and refuse the operation.

**Recover:** a cancellation is not consent; an expired result needs a new question
if the decision is still required. No elapsed timeout invents an answer. Results
belong to the original caller/provider incarnation, remain in memory only and are
bounded to 64 completed/cancelled questions. Never answer through UI input.
Clipboard export remains subject to frontend permission; queuing it is not proof
that the operating system clipboard changed.

## Consult documentation and settings

**Use:** learn the editor/compiler workflow or inspect the current environment.

**Do:** `docs_list` → `docs_search` → `docs_read`, with `corpus: "editor"` or
`"thc"`. Read `agent_settings` for public provider/model choices, context
usage and the global/project guidance supplied by the person. Read `editor_settings` for display/editing state; change `settings` for
this session or `defaults` for future launches when requested.

**Why these tools:** documentation comes from the declared editor/compiler
corpus; settings reads reflect the running session and project/global precedence.
They avoid guessing from defaults or disclosing protected configuration wholesale.

**Check:** cite the document and relevant section. Public agent settings are
read-only; argument/environment values and session keys are withheld. Streamer
mode is readable by the agent but only the person can change it.

**Recover:** compiler documentation needs the configured THC checkout. Refresh
`docs_list` as compiler sections are added. Settings merges preserve omitted
fields and unrelated shared configuration sections.

## Load these skills through the documentation service

The canonical playbooks are this document; the site and offline service share
that source. The editor corpus indexes its headings and searches its text.

```json
{"name":"docs_read","arguments":{"corpus":"editor","path":"docs/agent-skills.md"}}
```

Use `docs_list` for heading line numbers, then `docs_read` with `startLine` and
`lineCount` to load one category. Exact calls are in `docs/agent-tools.md`.

The ACP conversation supplies a compact skill catalog with the first query on
each connection, including resumed conversations. Agents fetch the chosen
playbook on demand. External MCP clients receive a catalog pointer in server
initialization instructions. Merely listing an MCP resource does not
ensure the host loads it. Explicit user-selected workflows can also be exposed
as MCP prompts. Neither mechanism changes the tool permission policy.
