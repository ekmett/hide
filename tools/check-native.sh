#!/bin/sh
set -eu
case "$(uname -s)" in
  Darwin) fonts="-framework CoreFoundation -framework CoreGraphics -framework CoreText"; packages="sdl3 libutf8proc" ;;
  *) fonts=""; packages="sdl3 libutf8proc pangocairo" ;;
esac
mkdir -p .deps
cc -Wall -Wextra -DWITH_WINDOW $(pkg-config --cflags $packages) cbits/window.c cbits/unicode.c test/native-input.c $(pkg-config --libs $packages) $fonts -lm -o .deps/native-input
.deps/native-input
