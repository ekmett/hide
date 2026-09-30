# Graphical Window Implementation Plan

> Use superpowers:executing-plans for integration. The font asset task is independent and can be delegated under dispatching-parallel-agents.

Goal: `thc-edit --window` presents the existing editor on Metal/macOS and Vulkan/Linux with tightly rendered IBM VGA cells.
Spec: ../specs/2026-09-30-graphical-window-design.md
Constraints: optional Cabal flag, no changes to THC, preserve terminal behavior, plain UI copy, real selected GPU driver, included font licensing.
Review focus: HiDPI pointer alignment; double-inserted text; dirty window close; clipping at tiny window dimensions; resource cleanup and backend errors.

- [x] Font: bundle credited IBM VGA bitmap and Unicode fallback with deterministic glyph lookup. Own `src/THC/Edit/Font.hs`, `assets/`, `tools/` and `test/FontCheck.hs`. Interface `loadFont :: IO Font`, `glyph :: Font -> Char -> Glyph`, `Glyph { glyphWidth :: Int, glyphRows :: [Word16] }` with bit 15 the leftmost pixel, 16 rows, width 8 or 16. Tests check representative glyphs, box-edge joins and missing-glyph fallback.
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

Coordination: “thc mba” owns THC repository/interface decisions; “thc linux”
is a worker for delegated implementation and validation. The editor stays in
its own repository. Runtime Debug actions will eventually use THC's Truffle
debugger; HLS supplies static editor tooling.

The final Linux frame revealed a startup resize race despite a successful render
return code: X11 had not completed the requested window resize. Calling
SDL_SyncWindow after SDL_SetWindowSize fixes it. Verified actual Metal and Vulkan
BMP dimensions at 800x512 for `--size 100x32 --scale 1`, with correct glyphs,
status bar and shadows. The forwarded X11 Vulkan driver still emits a DRI3
warning, but the captured frame is correct; native Linux desktop interaction
has not been manually exercised.
