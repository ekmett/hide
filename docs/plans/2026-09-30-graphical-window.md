# Graphical Window Implementation Plan


Goal: `thc-edit --window` presents the existing editor on Metal/macOS and Vulkan/Linux with tightly rendered IBM VGA cells.
Spec: ../design/2026-09-30-graphical-window-design.md
Constraints: optional Cabal flag, no changes to THC, preserve terminal behavior, plain UI copy, real selected GPU driver, included font licensing.
Review focus: HiDPI pointer alignment; double-inserted text; dirty window close; clipping at tiny window dimensions; resource cleanup and backend errors.

- [x] Font: bundle credited IBM VGA bitmap and Unicode fallback with deterministic glyph lookup. Interface `loadFont :: IO Font`, `glyph :: Font -> Char -> Glyph`, `Glyph { glyphWidth :: Int, glyphRows :: [Word16] }` with bit 15 the leftmost pixel, 16 rows, width 8 or 16. Tests check representative glyphs, box-edge joins and missing-glyph fallback.
- [x] Backend: add optional SDL3 C bridge for window/resource lifecycle, frame upload, input polling and pixel-aligned geometry. Keep event ABI explicit and bounded. Native checks cover rendering geometry and resource errors; runtime checks report renderer name and capture a frame.
- [x] Haskell integration: consume actual Vty spans; translate input to existing events; preserve file effects and close confirmation; expose `--window`, `--scale`. Test translation and frame cursor/clipping without requiring a display.
- [x] Validate both flag configurations, macOS Metal runtime and Linux build/runtime where available. Fresh whole-branch review, fix consequential findings, document invocation and limits, sync durable checkout and notify coordination chats.

Execution: the user's explicit request authorizes implementing the optional window and bundled font. Continue inline in the existing isolated checkout without additional approval rounds. Alternative considered: independent native Metal/Vulkan renderers would duplicate platform code; SDL3 is the smaller maintained bridge. CPU bitmap composition is a deliberate first implementation; move composition to a glyph atlas only if profiling warrants it.


Integration record: optional SDL3 Metal/Vulkan window, IBM VGA and Unicode fonts,
native Mac menus, character dimensions (`--size`, default 80x25), red shortcut
labels, glyph-preserving gray-on-black popup shadows, file Open browser, docked
tree, rendered Markdown Help and Git review/commit UI are implemented. Linux
SDL3 was built in an isolated dependency prefix; its Vulkan capture smoke test
and editor tests passed. Mac Metal frame captures verified Open, tree, Help,
menus and diffs without interacting with the user's running editor.

Review fixed duplicate native Command input, Option/AltGr text entry, tree focus
leaking edits and keyboard Open ignoring a typed filename. Git review snapshots
reject changes before staging/commit; normal Git hooks remain authoritative and
may transform the commit, with a visible report if the committed tree differs.
This intentionally follows Git hook semantics rather than replacing hooks.
HLS and Cabal-plan component browsing remain the next backend milestones.

Runtime Debug actions will eventually use THC's Truffle debugger; HLS supplies static editor tooling.

The final Linux frame revealed a startup resize race despite a successful render
return code: X11 had not completed the requested window resize. Calling
SDL_SyncWindow after SDL_SetWindowSize fixes it. Verified actual Metal and Vulkan
BMP dimensions at 800x512 for `--size 100x32 --scale 1`, with correct glyphs,
status bar and shadows. The forwarded X11 Vulkan driver still emits a DRI3
warning, but the captured frame is correct; native Linux desktop interaction
has not been manually exercised.

Follow-up: replaced the bespoke lexer with Skylighting 0.15's KDE/Kate grammar
set, selected by filename and cached per document across split views. Tests
cover source preservation, CRLF/Unicode/incomplete input, Python selection,
unknown extensions, edits, undo and filename changes. Benchmark command and
measurements (36 ms/1,001-line retokenization, 2 ms cached redraw) are in README.
The bundled Skylighting grammar package is GPL-2; this stays outside THC.

Added Borland-numbered Mode 3 (80x25) and Mode 259 (C80+Font8x8,80x50), exposed
in Preferences and --mode. Per the user's preference, both retain the existing
8x16 bitmap and change its vertical aspect ratio. Native tests cover geometry,
cursor/mouse placement, runtime changes, custom dimensions and failed resize
rollback; mode changes preserve buffers and scale the existing pane layout.

HLS integration now runs asynchronously in both frontends, with versioned
buffer synchronization, status-bar types, completion, definition, diagnostics
and rename. Real HLS 2.15 with GHC 9.14.1 verified these actions through the
editor model. Rename snapshots closed project source files before requesting
edits, validates all affected files, and leaves changes in undoable buffers.
The Problems dock and source chevrons track current diagnostics.

Buffers now use measured line finger trees with persistent undo states and a
cached text projection. Mouse controls include Open-dialog double-click,
button hover/pressed feedback, and a source context menu. Startup opens the
current package in the file explorer. Desktop dither and scrollbars use their
distinct colors; dragging uses a cyan single-line frame. The graphical mouse
uses the DOS text-cursor color mask rather than a graphical arrow.
