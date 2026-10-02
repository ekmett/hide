# Protocol and terminal integration

Use the existing Haskell desktop, measured buffers, undo history and character-grid
renderer. Keep optional runtime programs outside the compiler's dependency graph.

## Deliverables

- [x] Preserve the supplied Unicode hover formatting as a separate verified commit.
- [x] ACP v1 stdio client: newline-delimited JSON-RPC, bounded messages/stderr,
  asynchronous requests/notifications, bidirectional calls and bounded cleanup.
  Verify against the published codex-acp adapter without assuming LSP framing.
- [x] Options > Agents configures an executable, argument array and environment
  overrides. A numbered Conversation window streams Markdown replies; tool
  activity and permission prompts remain structured. Support cancellation,
  source/selection/diagnostic context and capability-gated session resumption.
- [x] Filesystem requests read unsaved buffers. Writes enter existing undo history,
  check captured revisions/baselines, and never silently replace intervening edits.
- [x] Background external-change detection observes open files and expanded project
  directories. Poll metadata on a bounded cadence, read changed files off the UI
  thread, and ignore stale results. Clean files reload; dirty/deleted files retain
  both versions and offer Compare, Reload, Keep and Save as. Keep the baseline and
  remember acknowledged disk versions; save-time checks remain authoritative.
- [x] Parse Markdown with a maintained parser, render headings/lists/emphasis/links
  on the grid and fenced code using Skylighting. Preserve raw reply text for copy;
  partial streamed Markdown must remain readable.
- [x] Add an optional libghostty-vt C shim and Haskell wrapper, with a terminal window
  and process lifecycle shared by ACP terminal requests and Run.
- [x] Run invokes the current `thc run [TARGET] --project-dir DIR` interface.
- [x] DAP launch/attach, source breakpoints and stepping share the editor debugger
  and agent tools. THC launches with `--dap-port`; other adapters use a DAP
  configuration. Runtime lexical scopes and lazy-value inspection remain separate
  compiler work; see the [current debugger guide](../running.md#launch-or-attach-a-debugger).

## File boundaries

`ACP.hs` owns subprocess JSON-RPC transport only. `External.hs` owns asynchronous
filesystem observation only. `Terminal.hs` and `cbits/terminal.c` own terminal state
and processes. Conversation orchestration and UI effects reuse Model/App/Render;
new state must preserve split-buffer identity and existing modal focus behavior.

## Verification

Use local fake ACP subprocesses for framing, cancellation, permissions, filesystem
races and disconnects; negotiate with the real adapter separately. Test filesystem
replacement/deletion/recreation and edits arriving while a result is pending. Test
terminal escape sequences and resize through libghostty-vt. Use isolated native
captures, never type into or close existing editor instances. Run the full suite
before commits and keep the durable Mac/Linux editor checkouts synchronized.

## Validation

- ACP adapter 2.0.1 initialized protocol v1 and created a session with local Codex
  0.156.1; no model prompt was sent during the compatibility check.
- Fake-provider checks exercise streaming, permission review, cancellation,
  resumed sessions, stale file writes and Ghostty terminal lifecycle.
- CommonMark, filesystem observer/reconciliation and Run argument checks pass.
- Native terminal parsing passes AddressSanitizer checks; PTY tests cover output
  draining, resize, Unicode, terminal replies and foreground-job cleanup.
- Metal preview uses the same grid renderer for Markdown and terminal cells.

## Docking behavior

Files docks only on the left; Messages only on the bottom. Editor windows float
within the remaining workspace. Terminals can float or join Messages in one
bottom panel with tabs. Dock resizing pushes adjacent floating windows until they
reach the opposite workspace edge, then resizes them; expanding the workspace
pulls abutting windows with the dock boundary.

Pinning or unpinning preserves the terminal, process, scrollback and window
identity. Use a single-cell monochrome pin in the SDL font if available;
the text fallback is `[P]` with a cyan P when pinned and `[ ]` when floating.
The status hint describes the action: “Unpin window” or “Dock window at bottom”.
Only windows that support both states expose an interactive pin control.
