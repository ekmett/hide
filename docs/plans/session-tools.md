# Session tools

The conversation agent should work with the same files, windows and running programs as the person using the editor. Tools use stable session IDs, live buffer revisions and the existing editor commands. Long-running replies must not block the display or language/debug protocol polling.

## Work list

- [x] Debugger status, launch/attach, breakpoints, control, stack/scopes/variables/source, and packaged debugging skill.
- [x] HLS hover/types, definitions, references, symbols, rename; live diagnostics and Messages.
- [x] Project context and saved/dirty buffer inventory.
- [x] Build/run control and shared Ghostty terminal services.
- [x] Structured test status with suite outcomes and compiler diagnostics.
- [x] Git status/diff and workspace/Git file search, including live unsaved text.
- [x] Open/save/close files, checked diff edits, create/rename/delete files and directories.
- [x] Window geometry, focus, movement, resize, tile/cascade/split and Files/Messages visibility.
- [x] Text/hex mode and byte-offset navigation.
- [x] Undo/redo history as diff-style previews and revision-checked application.
- [x] Screen capture as colorless text and an optional PNG from the same frame.
- [x] Inline questions with choices and free text.
- [x] Compact tool-call descriptions with expandable full JSON details.

- [x] In-app mouse, typing and key-combination control.
- [x] JSON display/editing settings and persistent startup defaults.
- [x] Per-tool Enable/Prompt/Disable in Options > Agent Permissions and shared THC TOML config.
- [x] Editor/compiler documentation listing, search and reads.

Check items after integration and verification, not just after their implementation compiles. Keep execution outcomes distinct from acceptance, diagnostics distinct from test results, and on-disk Git changes distinct from unsaved buffers.

Verification: full editor suite and live MCP bridge passed on macOS. The live
check exercised 50 discovered tools, startup precedence, unsaved diff/undo,
configuration preservation, disabled built-ins, question cancellation, screen
capture and immediate exit. Prompted exit response ordering is covered by transport regressions.
Debugger follow/reveal behavior is exercised with a real fake-DAP transport.

## Further hooks

- [x] Origin-aware agent input, readable/clickable cell masks, and protected conversation/approval/settings controls.
- [x] Secret redaction for agent views, plus optional human-facing Streamer mode.
- [x] Explicit clipboard write without reading the user's clipboard.
- [x] Dedicated read-only agent settings snapshot.
- [x] Dedicated Git fetch, complete review and one-time checked commit operations.
- [ ] Dedicated Git pull/merge operations with protected-path checks.
- [x] Edit-based HLS quick fixes/refactorings: chooser, bounded opaque handles,
  current diagnostics, advertised lazy resolution and checked atomic buffer edits.
- [x] Command-based HLS actions and scoped workspace/executeCommand application.
- [x] Cabal component/dependency graph context from the existing generated plan.
- [x] Individual top-level TAP 13 test results from stdout, with planned/observed counts,
  skip/TODO outcomes, failure precedence and explicit truncation/completeness.

- [x] Shared/background debugger presentation from the same stopped DAP session.
- [x] Global/project agent context, UI editing and on-demand workflow catalog.
