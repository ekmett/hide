# Turbo desktop and local editing implementation plan

> Execute inline with superpowers:executing-plans. Check each task against the approved design.

**Goal:** Deliver a runnable `thc-edit` with the classic desktop, functional widgets and safe local editing.
**Architecture:** Pure buffer and desktop state transitions produce file-operation requests. A cell renderer shares window/control geometry with hit testing; Vty owns terminal I/O and cleanup.
**Tech stack:** Haskell, Vty/crossplatform, boot libraries. No Brick or native editor framework.
**Spec:** ../specs/2026-09-30-thc-edit-design.md

## Constraints and review focus

- Preserve the Borland palette, menu order, window borders, shadows and key legend.
- Keep the independent executable usable without THC or HLS.
- Modal dialogs must block clicks/typing behind them, including after terminal resize.
- Unicode display columns differ from buffer character offsets; tabs and wide characters need explicit mapping.
- Save errors/conflicts must retain the original file and dirty buffer; clean up temporary files.
- Views of one buffer share text and undo history; selection/cursors must remain valid after other-view edits.
- Quit/close cannot silently discard modified buffers.

## Task 1: Editing model

Files: `thc-edit.cabal`, `cabal.project`, `src/THC/Edit/Buffer.hs`, `src/THC/Edit/Syntax.hs`, `test/Main.hs`.
Interfaces: character-indexed `Buffer`, `Selection`, `replaceSelection`, `undo`, `redo`, line/column conversion and lexical `highlight`.
- [x] Add runnable checks for insertion, selection replacement, undo/redo, shared edits, tab/wide-character navigation and nested Haskell comments. Observe failure before implementation.
- [x] Implement using Text and bounded undo snapshots; document the large-file ceiling.
- [x] Run `cabal test`; record results and commit.

## Task 2: Desktop and widgets

Files: `src/THC/Edit/Model.hs`, `src/THC/Edit/Render.hs`, extend `test/Main.hs`.
Interfaces: `Desktop`, `Window`, `Rect`, `Command`, `Effect`; `handleEvent :: Event -> Desktop -> (Desktop, [Effect])`; `renderDesktop :: Desktop -> Picture`.
- [x] Add event-replay checks for topmost hit testing, modal interception, menu keys, drag capture, resize clamps, tiled/shared views, WordStar prefixes and dirty close cancellation; observe failures.
- [x] Implement menus, window frames, scrollbars, keyboard/mouse control, help/about, file/search dialogs and an interactive widget gallery.
- [x] Render the 80x25 palette and geometry; inspect terminal screenshots. Add deterministic snapshot output for repeatable rendering checks.
- [x] Run full tests and commit.

## Task 3: Files and executable

Files: `src/THC/Edit/Files.hs`, `src/THC/Edit/App.hs`, `app/Main.hs`, extend `test/Main.hs`.
Interfaces: UTF-8 load, identity-aware checked save; terminal loop executes `Effect` and feeds results back into Desktop.
- [x] Add real temporary-file tests for round trips, CRLF/final-newline preservation, external-write conflicts, symlinks, unsupported UTF-8 and failed saves; observe failures.
- [x] Implement same-directory temporary writes/replacement, permissions preservation and errors that keep buffers dirty.
- [x] Implement direct CLI paths, `--`, help, snapshot mode, Vty mouse/bracketed-paste and bracketed terminal cleanup.
- [x] Run `cabal build all`, `cabal test`, CLI and PTY smoke checks. Commit.

## Task 4: Review and delivery

Files: `README.md`, design status, plan checklist.
- [x] Review whole branch, fix meaningful defects with regression checks, run the full suite.
- [x] Document actual commands, supported keys, terminal limitations and currently disabled capabilities.
- [ ] Synchronize tested commits into `/Users/ekmett/thc-edit` and coordinate Linux smoke testing.

## Subsequent milestone

HLS transport/tooling and Cabal component browsing get a separate implementation plan after this usable desktop. They remain required direction from the design, not claimed capabilities of the first executable. Run/Compile/Debug entries explain disabled actions until backed by actual services.

## Execution record

- User approved the design and said to proceed. Execute inline; no additional implementation-approval round.
- Working checkout: `/private/tmp/thc-edit-work`; durable repository: `/Users/ekmett/thc-edit`.

- Implemented the pure buffer/desktop, Vty renderer and CLI; a parallel file-I/O
  task supplied checked saves and temporary-file tests.
- Independent review found and prompted fixes for dirty-quit cancellation,
  shared-view rebase with repeated text, WordStar block markers, resize-grip
  hit testing, wrapped search, CRLF movement and control-character width.
- Final small-window checks reproduced and fixed undersized tiled frames and
  invisible modal controls receiving border clicks.
- `cabal build all --offline` and `cabal test --offline` pass on GHC 9.14.1,
  macOS arm64, with `-Wall`. No warnings in the final build.
- Inspected 80x25 desktop and widget-gallery HTML generated from the actual
  Vty display operations. Menus retain the classic gray/green palette.
- Real PTY smoke: opened a temporary source file, bracketed-pasted a comment,
  saved with Ctrl+S, opened File with SGR mouse input, and clicked Exit.
  Verified exact disk contents and normal terminal shutdown (exit 0).
- Linux source transfer awaits explicit destination approval after automatic
  approval review blocked copying the private bundle to castlemeadow.
