# SPDX-FileCopyrightText: 2026 Edward Kmett
# SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{ lib, haskell, haskellPackages, ghosttyVt, utf8proc, sdl3, pango, accesskitC }:

let
  package = haskell.lib.justStaticExecutables
    (haskellPackages.callCabal2nixWithOptions "hide" (lib.cleanSource ../.) "-faccesskit" {
      libutf8proc = utf8proc;
      libghostty-vt = ghosttyVt;
      accesskit-c = accesskitC;
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
