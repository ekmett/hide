# Agent skills

Work with the person’s live editor: the same buffers, windows, language server,
terminals and debugger. A skill describes how to finish a task with those tools.
The [operation reference](agent-tools.md) describes each call.

## Choose a skill

| Skill | Use when | Evidence of completion |
| --- | --- | --- |
| [Explore a project](#explore-a-project) | Locate code, understand the package or follow a symbol | Relevant files, symbols and current revisions identified |
| [Edit and review](#edit-and-review) | Make a focused change or apply a patch | Intended buffer diff, with saved/unsaved state explicit |
| [Diagnose and refactor Haskell](#diagnose-and-refactor-haskell) | Explain types, fix diagnostics or rename a symbol | Current HLS result and reviewed edits |
| [Build, test and run](#build-test-and-run) | Check a change or execute the project | Actual completion status and relevant output |
| [Debug a program](#debug-a-program) | Investigate execution with THC or another DAP adapter | Observed stop, stack or variable evidence |
| [Review repository changes](#review-repository-changes) | Understand saved changes alongside live edits | Disk diff and unsaved changes accounted for separately |
| [Organize and operate the desktop](#organize-and-operate-the-desktop) | Show source, Messages, hex data or a particular layout | Returned geometry/mode and a fresh screen snapshot |
| [Consult the user](#consult-the-user) | A decision needs the person’s answer | Submitted answer, not a draft or guessed choice |
| [Consult documentation and settings](#consult-documentation-and-settings) | Learn a feature or inspect/configure the editor | Cited documentation or confirmed settings |

## Shared rules

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

**Check:** record the paths, buffer IDs and revisions used. Search substitutes
unsaved buffer contents; its default search also includes untitled buffers.

**Recover:** page results and inspect skipped/truncated counts before treating a
missing match as absent. Project context identifies the root/package; a complete
Cabal component/dependency graph is a separate planned extension.

## Edit and review

**Use:** make a source change, create a file, or undo a known edit.

**Do:** `read_buffer` → `buffer_apply_diff` with that revision → `read_buffer`.
Use one strict unified diff for the target buffer. Create an empty file with
`workspace_files`, then open and edit it. Preview history with `editor_history`;
apply undo/redo with `editor_undo` and the current revision.

**Check:** inspect the resulting change, run the relevant check, then use
`editor_file` to save when saving is part of the task. Name what remains dirty.

**Recover:** stale revision or mismatched context means reread and reconstruct
the patch. Disk conflicts require review; do not silently overwrite them. A
failed atomic patch/history request leaves the buffer unchanged. Closing a dirty
view requires an explicit save/discard choice; another view may share its buffer.

## Diagnose and refactor Haskell

**Use:** explain a type, locate an error, or rename a symbol across the package.

**Do:** `workspace_diagnostics` → read/navigate the relevant source → `lsp_hover`
or symbol/location tools. For rename, call `lsp_rename` with `bufferId`, current
`revision`, `line`, `column`, and `newName`; review the changed buffers.

**Check:** distinguish the diagnostic’s reported version from the current
buffer revision. HLS receives unsaved source. Rename changes live buffers;
saving and rebuilding remain separate steps.

**Recover:** if source changes while HLS works, refresh and retry against the new
revision. If HLS is unavailable, report its result and inspect build output.
General code actions, including extracting/inserting type signatures where HLS
supports them, are planned; hover/type lookup is available now.

## Build, test and run

**Use:** compile, test or run the selected THC/GHC project.

**Do:** inspect unsaved buffers; save the intended source changes.
`build_start` chooses `compile`, `make` or `run`, with optional job-local
`toolchain`, `target` and `arguments`. Poll `build_status`; read its output buffer.
Use `test_start` for GHC/Cabal tests and `test_status` for suite outcomes. For an
interactive program, use a shared terminal: `terminal_start`, `terminal_input`,
`terminal_output`; `terminal_list` finds existing terminals.

**Check:** report the exit code, explicit suite outcomes and relevant diagnostics.
A successful process with no parsed test results is not evidence that individual
tests passed. Build and test jobs share a slot.

**Recover:** inspect an existing job before starting another. `build_stop` stops
a captured job; `terminal_stop` stops a terminal process. Retained output can
lose its earlier bytes; read its truncation flag. `terminal_start` takes an
executable and argument array, with no implicit shell.

## Debug a program

**Use:** explain a runtime failure, inspect values or follow control flow.

**Do:** `debug_status` → reuse the active adapter, or `debug_launch` /
`debug_attach` → check readiness. Set breakpoints with the current `generation`.
A breakpoint call replaces the entire set for its source buffer: retain desired
existing lines. Once stopped, inspect threads → stack trace → scopes → variables.
Use `debug_control` to continue, step, pause or disconnect, then refresh status.

**Present:** use `debug_present` with `follow: false` to investigate without
moving the person's source window on each stop. At an interesting stop, send
`view: "source"` and the current `generation`; `stack`, `scopes` and `output`
reveal those views. Set `follow: true` to follow future stops automatically.
The agent and the UI share one debugger, breakpoints and stopped state.

**Check:** accepted execution commands are submissions. Wait for the next
observed stop before drawing conclusions. Report pending versus verified
breakpoints and actual values returned by the adapter.

**Recover:** refresh generation after execution changes; do not reuse stale
thread/frame/variable handles. Inspect state after an uncertain transport
failure before relaunching. Unsaved source does not rebuild the running program.
Empty scopes mean the adapter supplied no lexical values.

The packaged debugging skill is also available through MCP `resources/read` at
`thc-edit://debugging`.

## Review repository changes

**Use:** compare the working tree with the change the person intended.

**Do:** `workspace_git` with `view: "status"`, then `view: "diff"`; use a literal
`path` to focus the diff. Inspect `list_buffers` for unsaved edits and read them
separately. Use `workspace_search` with `trackedOnly: true` for repository source
search.

**Check:** Git describes on-disk changes. It does not include unsaved buffer
edits, and tracked-file search does not search commit history.

**Recover:** narrow a truncated or protected whole-repository diff to safe
literal files. Dedicated fetch/pull/merge/commit MCP operations are planned.
Terminal execution, when enabled, follows the person’s requested Git operation;
it does not acquire authority from this skill.

## Organize and operate the desktop

**Use:** show related files, arrange panels, inspect binary data or operate UI.

**Do:** `editor_layout` / `list_windows` → `editor_arrange`, `editor_panels` or
`editor_navigate`. Use `editor_mode` for hex and then navigate by `byteOffset`.
Read `editor_screen` for the whole text grid; request `image: true` when colors,
borders or overlap matter. For raw interaction, inspect its access/command/key
permissions and submit a short `editor_input` batch, then recapture.

**Check:** use returned constrained rectangles, not assumed placement. Tiling,
splits and dock resizing follow normal editor rules. Split views share buffers.
The screen PNG represents the editor grid, not OS chrome or the native CRT pass.

**Recover:** input events are sequential, not transactional. On a partial failure,
inspect `appliedEvents` and the current screen before continuing. Human authority
controls cannot be operated by agent input. Text mode refuses invalid UTF-8 or
NUL-containing bytes; stay in hex mode for those files.

## Consult the user

**Use:** ask for a missing decision while preserving the person’s draft.

**Do:** `ask_user` with one clear `question` and optional single-choice `choices`.
A free-text answer is always available. Wait for the submitted response; only
one question can be pending. Use `clipboard_write` when asked to provide text
for copying.

**Check:** a pending question is not an answer. Tool activity stays compact in
the conversation and can be expanded to inspect the request/reply JSON.

**Recover:** cancellation ends the wait; do not answer the question through UI
input. Clipboard writing accepts supplied text only and queues frontend export;
it does not read the person’s clipboard or confirm frontend permission.

## Consult documentation and settings

**Use:** learn the editor/compiler workflow or inspect the current environment.

**Do:** `docs_list` → `docs_search` → `docs_read`, with `corpus: "editor"` or
`"thc"`. Read `agent_settings` for public provider/model choices, context
usage and the global/project guidance supplied by the person. Read `editor_settings` for display/editing state; change `settings` for
this session or `defaults` for future launches when requested.

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
