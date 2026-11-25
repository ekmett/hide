# SPDX-FileCopyrightText: 2026 Edward Kmett
# SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{ pkgs, accesskitC }:

pkgs.runCommand "hide-native-menu-check" {
  nativeBuildInputs = [ pkgs.stdenv.cc pkgs.pkg-config ];
  buildInputs = [ pkgs.sdl3 pkgs.pango pkgs.utf8proc accesskitC ];
} ''
  mkdir -p "$out/bin" "$out/libexec/hide-atspi-menu"
  cc -O2 -DWITH_ACCESSKIT -I${../cbits} \
    $(pkg-config --cflags sdl3 pangocairo libutf8proc accesskit-c) \
    ${../cbits/window.c} ${../cbits/unicode.c} ${../cbits/unicode-window.c} \
    ${../cbits/accessibility-menu.c} ${../cbits/accessibility-accesskit.c} \
    ${../cbits/accessibility-unix.c} ${../test/native-accessibility-unix.c} \
    $(pkg-config --libs sdl3 pangocairo libutf8proc accesskit-c) \
    -lpthread -ldl -lm -o "$out/bin/hide-atspi-menu-host"
  install -m755 ${../test/atspi-menu.sh} "$out/libexec/hide-atspi-menu/atspi-menu.sh"
  install -m644 ${../test/atspi-editor-menu.py} "$out/libexec/hide-atspi-menu/atspi-editor-menu.py"
  install -m644 ${../test/atspi-menu.py} "$out/libexec/hide-atspi-menu/atspi-menu.py"
''
