# SPDX-FileCopyrightText: 2026 Edward Kmett
# SPDX-License-Identifier: UPL-1.0 AND BSD-3-Clause
{ lib, haskell, haskellPackages, ghosttyVt, utf8proc, sdl3, pango }:

let
  package = haskell.lib.justStaticExecutables
    (haskellPackages.callCabal2nix "hide" (lib.cleanSource ../.) {
      libutf8proc = utf8proc;
      libghostty-vt = ghosttyVt;
      inherit sdl3 pango;
    });
  # Cabal queries private pkg-config dependencies even for shared native libraries.
  # Keep that metadata visible without passing SDL/Pango's entire build closure
  # as C include/link directories, which exceeds the compiler's argv limit.
  pkgConfigDependencies = package.getBuildInputs.allPkgconfigDepends;
  pkgConfigPath = lib.makeSearchPathOutput "dev" "lib/pkgconfig" pkgConfigDependencies
    + ":" + lib.makeSearchPathOutput "dev" "share/pkgconfig" pkgConfigDependencies;
in haskell.lib.overrideCabal package (old: {
  __propagatePkgConfigDepends = false;
  env = (old.env or {}) // { PKG_CONFIG_PATH = pkgConfigPath; };
  # Interactive integration checks run against the installed package in NixOS.
  doCheck = false;
  postInstall = (old.postInstall or "") + ''
    install -m755 bin/th bin/thc-edit "$out/bin/"
  '';
})
