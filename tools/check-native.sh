#!/bin/sh
set -eu
case "$(uname -s)" in
  Darwin) fonts="-framework CoreFoundation -framework CoreGraphics -framework CoreText"; packages="sdl3 libutf8proc" ;;
  *) fonts=""; packages="sdl3 libutf8proc pangocairo" ;;
esac
mkdir -p .deps
cc -Wall -Wextra -DWITH_WINDOW $(pkg-config --cflags $packages) cbits/window.c cbits/unicode.c test/native-input.c $(pkg-config --libs $packages) $fonts -lm -o .deps/native-input
cc -Wall -Wextra -DWITH_WINDOW $(pkg-config --cflags $packages) cbits/window.c cbits/unicode.c test/native-atlas.c $(pkg-config --libs $packages) $fonts -lm -o .deps/native-atlas
.deps/native-atlas software
if [ -n "${HIDE_TEST_GPU:-}" ]; then .deps/native-atlas "$HIDE_TEST_GPU"; fi
.deps/native-input

if [ "$(uname -s)" = Darwin ]; then
  cc -Wall -Wextra -fobjc-arc cbits/menu.m test/native-menu.m -framework Cocoa -o .deps/native-menu
  .deps/native-menu
  cc -Wall -Wextra -fobjc-arc -DWITH_WINDOW $(pkg-config --cflags $packages) cbits/window.c cbits/unicode.c cbits/menu.m cbits/accessibility.m test/native-accessibility.m $(pkg-config --libs $packages) $fonts -framework Cocoa -lm -o .deps/native-accessibility
  .deps/native-accessibility
  if [ -n "${HIDE_TEST_GPU:-}" ]; then .deps/native-accessibility "$HIDE_TEST_GPU"; fi
fi
