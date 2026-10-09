# SPDX-License-Identifier: UPL-1.0 AND BSD-3-Clause
{ lib, haskell, haskellPackages, ghosttyVt, utf8proc, sdl3, pango }:

haskell.lib.overrideCabal
  (haskell.lib.justStaticExecutables
    (haskellPackages.callCabal2nix "hide" (lib.cleanSource ../.) {
      libutf8proc = utf8proc;
      libghostty-vt = ghosttyVt;
      inherit sdl3 pango;
    }))
  (old: {
    # Interactive integration checks run against the installed package in NixOS.
    doCheck = false;
    postInstall = (old.postInstall or "") + ''
      install -m755 bin/th bin/thc-edit "$out/bin/"
    '';
  })
