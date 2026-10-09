#!/bin/sh
set -eu
case "$(uname -s)" in
  Darwin) fonts="-framework CoreFoundation -framework CoreGraphics -framework CoreText"; packages="sdl3 libutf8proc" ;;
  *) fonts=""; packages="sdl3 libutf8proc pangocairo" ;;
esac
mkdir -p .deps
cc -O2 -Wall -Wextra test/power-mode.c -o .deps/power-mode-check
.deps/power-mode-check
cc -Wall -Wextra $(pkg-config --cflags $packages) cbits/window.c cbits/unicode.c cbits/unicode-window.c test/native-input.c $(pkg-config --libs $packages) $fonts -lm -o .deps/native-input
cc -Wall -Wextra $(pkg-config --cflags $packages) cbits/window.c cbits/unicode.c cbits/unicode-window.c test/native-atlas.c $(pkg-config --libs $packages) $fonts -lm -o .deps/native-atlas
.deps/native-atlas software
if [ -n "${HIDE_TEST_GPU:-}" ]; then
  HIDE_POWER_MODE=1 .deps/native-atlas "$HIDE_TEST_GPU"
  HIDE_POWER_MODE=2 .deps/native-atlas "$HIDE_TEST_GPU"
  cc -Wall -Wextra $(pkg-config --cflags $packages) cbits/window.c cbits/unicode.c cbits/unicode-window.c test/native-canvas.c $(pkg-config --libs $packages) $fonts -lm -o .deps/native-canvas
  .deps/native-canvas "$HIDE_TEST_GPU"
fi
.deps/native-input

if [ "$(uname -s)" = Darwin ]; then
  cc -Wall -Wextra -fobjc-arc cbits/menu.m test/native-menu.m -framework Cocoa -o .deps/native-menu
  .deps/native-menu
  cc -Wall -Wextra -fobjc-arc $(pkg-config --cflags $packages) cbits/window.c cbits/unicode.c cbits/unicode-window.c cbits/menu.m cbits/accessibility.m test/native-accessibility.m $(pkg-config --libs $packages) $fonts -framework Cocoa -lm -o .deps/native-accessibility
  .deps/native-accessibility
  if [ -n "${HIDE_TEST_GPU:-}" ]; then .deps/native-accessibility "$HIDE_TEST_GPU"; fi
fi
