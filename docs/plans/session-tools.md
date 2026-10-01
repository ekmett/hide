# Session tools

The conversation guest should work with the same files, windows and running programs as the person using the editor. Tools use stable session IDs, live buffer revisions and the existing editor commands. Long-running replies must not block the display or language/debug protocol polling.

## Work list

- [ ] Debugger status, launch/attach, breakpoints, control, stack/scopes/variables/source, and packaged debugging skill.
- [ ] HLS hover/types, definitions, references, symbols, rename; live diagnostics and Messages.
- [ ] Project context and saved/dirty buffer inventory.
- [ ] Build/run control and shared Ghostty terminal services.
- [ ] Structured test status with suite outcomes and compiler diagnostics.
- [ ] Git status/diff and workspace/Git file search, including live unsaved text.
- [ ] Open/save/close files, checked diff edits, create/rename/delete files and directories.
- [ ] Window geometry, focus, movement, resize, tile/cascade/split and Files/Messages visibility.
- [ ] Text/hex mode and byte-offset navigation.
- [ ] Undo/redo history as diff-style previews and revision-checked application.
- [ ] Screen capture as colorless text and an optional PNG from the same frame.
- [ ] Inline questions with choices and free text.
- [ ] Compact tool-call descriptions with expandable full JSON details.

- [ ] In-app mouse, typing and key-combination control.
- [ ] JSON display/editing settings and persistent startup defaults.
- [ ] Per-tool Enable/Prompt/Disable in Options > Agent Permissions and shared THC TOML config.
- [ ] Editor/compiler documentation listing, search and reads.

Check items after integration and verification, not just after their implementation compiles. Keep execution outcomes distinct from acceptance, diagnostics distinct from test results, and on-disk Git changes distinct from unsaved buffers.
