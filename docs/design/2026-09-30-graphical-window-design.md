# Graphical editor window

User request: an optional Metal/Vulkan window with the existing UI, tightly joined cells, preferably a classic IBM font. Play the interface straight. Terminal mode stays available and its build need not acquire graphical dependencies.

`hide --window` uses an optional Cabal `window` flag and SDL3 >=3.2. macOS explicitly selects Metal; Linux and Windows select Vulkan. A failed backend is an actionable error, never a silent software fallback. SDL handles the native window, events, HiDPI and presentation through a small C FFI bridge; editor state, widgets and rendering layout remain Haskell.

The graphical renderer consumes the same Vty display spans used by the terminal and snapshot renderer. Bitmap glyphs and cell backgrounds are composed into a compact pixel buffer and uploaded to a nearest-filtered texture. This deliberately simple renderer redraws on input/exposure, not continuously. No shaders or custom Vulkan device management are needed.

Bundle the IBM VGA 8x16 remake from VileR's font pack with attribution and its font license; include Unicode bitmap fallback where practical. Draw on an 8x16 cell grid, with integer physical pixel scale and no inter-cell spacing. Default window is 80x25 with an appropriate HiDPI scale. Resizes change row/column count; leftover pixels form an outer margin. Pointer positions use the same scale/origin as drawing. `--scale N` chooses an integer physical-pixel scale.

SDL keyboard/text, mouse, wheel and resize events become existing Vty events. Text input is distinct from shortcut keydown so it cannot insert twice. Dragging captures mouse movement until release; focus loss clears capture. Window close routes through the existing dirty-buffer confirmation. Use the main OS thread for SDL. Clipboard integration is bounded to native paste/copy shortcuts while preserving existing internal editing commands.

Validation: preserve terminal build/tests; test cell geometry, glyph joins, frame/span equivalence and input translation; compile the graphical build on macOS/Linux; run a real Metal window and capture its framebuffer; exercise editing, mouse and cancel-on-close. Report Linux display/Vulkan availability honestly.
