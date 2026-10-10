# SPDX-FileCopyrightText: 2026 Edward Kmett
# SPDX-License-Identifier: UPL-1.0 AND BSD-3-Clause
{
  description = "hide — Haskell IDE";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/7c8764b7c7b09b34f632464276218ef9090eaa11";
    ghostty = {
      url = "github:ghostty-org/ghostty/76895d97b74ff6b24c2b1543bcd69ccc18048a4d";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ghostty }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      haskellPackages = pkgs.haskell.packages.ghc9141.override {
        overrides = final: old: {
          # Build the runtime dependency closure. Hide's installed-package check
          # below owns acceptance; upstream suites, docs and profiling are not
          # part of producing an editor executable.
          mkDerivation = args: old.mkDerivation (args // {
            doCheck = false;
            doHaddock = false;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
          });
          # Linked packages use this same compiler and dependency set.
          hide-agent-api = final.callCabal2nix "hide-agent-api"
            (pkgs.lib.cleanSource ./packages/hide-agent-api) {};
          hide-plugin-api = final.callCabal2nix "hide-plugin-api"
            (pkgs.lib.cleanSource ./packages/hide-plugin-api) {};
          hide-acp = final.callCabal2nix "hide-acp"
            (pkgs.lib.cleanSource ./plugins/hide-acp) {};
          hide-agents = final.callCabal2nix "hide-agents"
            (pkgs.lib.cleanSource ./plugins/hide-agents) {};
          # Match the Unicode 17 tables used by GHC 9.14.
          unicode-data = old.unicode-data_0_8_0;
          # These releases satisfy hide.cabal but postdate the stable package set.
          commonmark = final.callHackageDirect {
            pkg = "commonmark"; ver = "0.3";
            sha256 = "sha256-Kn0amBHt3dEmr8J1b+z7I5z3OkC7+tC8he3N/kSzPHs=";
          } {};
          commonmark-extensions = final.callHackageDirect {
            pkg = "commonmark-extensions"; ver = "0.2.7.1";
            sha256 = "sha256-C7qHFVHTF5KQe4BBQ/oL+ozCPNr4CF0Fe+PMAqutxzI=";
          } {};
          skylighting = final.callHackageDirect {
            pkg = "skylighting"; ver = "0.15";
            sha256 = "sha256-hZ/Yo78ZdgWeNa0svxvVYkE6eXB/4XYGepTt5lt9ubI=";
          } {};
          skylighting-core = final.callHackageDirect {
            pkg = "skylighting-core"; ver = "0.15";
            sha256 = "sha256-omkFy0GsBSsl9CcjKGqzZqpPxNkO/CCZcIzjqBM09uk=";
          } {};
        };
      };
      ghosttyVt = pkgs.callPackage "${ghostty}/nix/libghostty-vt.nix" {
        optimize = "ReleaseFast";
        revision = "76895d97";
      };
      hide = import ./nix/package.nix {
        inherit (pkgs) lib haskell utf8proc sdl3 pango;
        inherit ghosttyVt haskellPackages;
      };
    in {
      packages.${system} = { inherit hide; default = hide; };
      checks.${system}.nixos = import ./nix/check.nix { inherit pkgs hide; };
      apps.${system}.default = {
        type = "app";
        program = "${hide}/bin/hide";
        meta.description = "Haskell IDE";
      };
      devShells.${system}.default = haskellPackages.shellFor {
        packages = hp: [ hide hp.hide-agent-api hp.hide-plugin-api hp.hide-acp hp.hide-agents ];
        genericBuilderArgsModifier = args: args // {
          __propagatePkgConfigDepends = false;
          env = (args.env or {}) // { inherit (hide.env) PKG_CONFIG_PATH; };
        };
        nativeBuildInputs = [ pkgs.cabal-install pkgs.pkg-config ];
      };
    };
}
