#!/bin/sh
set -eu
case "$(uname -s)" in
  Darwin) fonts="-framework CoreFoundation -framework CoreGraphics -framework CoreText"; packages="sdl3 libutf8proc" ;;
  *) fonts=""; packages="sdl3 libutf8proc pangocairo" ;;
esac
native_test_dir=$(mktemp -d "${TMPDIR:-/tmp}/hide-native-checks.XXXXXX")
trap 'rm -rf "$native_test_dir"' 0
trap 'exit 1' HUP INT TERM
cc -O2 -Wall -Wextra test/power-mode.c -o "$native_test_dir/power-mode-check"
"$native_test_dir/power-mode-check"
cc -Wall -Wextra $(pkg-config --cflags $packages) cbits/window.c cbits/unicode.c cbits/unicode-window.c test/native-input.c $(pkg-config --libs $packages) $fonts -lm -o "$native_test_dir/native-input"
cc -Wall -Wextra $(pkg-config --cflags $packages) cbits/window.c cbits/unicode.c cbits/unicode-window.c test/native-atlas.c $(pkg-config --libs $packages) $fonts -lm -o "$native_test_dir/native-atlas"
"$native_test_dir/native-atlas" software "$native_test_dir/atlas.bmp"
if [ -n "${HIDE_TEST_GPU:-}" ]; then
  HIDE_POWER_MODE=1 "$native_test_dir/native-atlas" "$HIDE_TEST_GPU" "$native_test_dir/atlas.bmp"
  HIDE_POWER_MODE=2 "$native_test_dir/native-atlas" "$HIDE_TEST_GPU" "$native_test_dir/atlas.bmp"
  cc -Wall -Wextra $(pkg-config --cflags $packages) cbits/window.c cbits/unicode.c cbits/unicode-window.c test/native-canvas.c $(pkg-config --libs $packages) $fonts -lm -o "$native_test_dir/native-canvas"
  "$native_test_dir/native-canvas" "$HIDE_TEST_GPU" "$native_test_dir/canvas.bmp"
fi
"$native_test_dir/native-input"

if [ "$(uname -s)" = Darwin ]; then
  cc -Wall -Wextra -fobjc-arc cbits/menu.m test/native-menu.m -framework Cocoa -o "$native_test_dir/native-menu"
  if [ "${HIDE_TEST_DOCK_RESTORE:-0}" = 1 ]; then
    "$native_test_dir/native-menu" --dock-restore
  else
    "$native_test_dir/native-menu"
  fi
  cc -Wall -Wextra -fobjc-arc $(pkg-config --cflags $packages) cbits/window.c cbits/unicode.c cbits/unicode-window.c cbits/menu.m cbits/accessibility.m test/native-accessibility.m $(pkg-config --libs $packages) $fonts -framework Cocoa -lm -o "$native_test_dir/native-accessibility"
  "$native_test_dir/native-accessibility"
  if [ -n "${HIDE_TEST_GPU:-}" ]; then "$native_test_dir/native-accessibility" "$HIDE_TEST_GPU"; fi
fi
