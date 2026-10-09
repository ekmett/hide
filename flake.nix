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
      hide = pkgs.callPackage ./nix/package.nix { inherit ghosttyVt haskellPackages; };
    in {
      packages.${system} = { inherit hide; default = hide; };
      checks.${system}.nixos = import ./nix/check.nix { inherit pkgs hide; };
      apps.${system}.default = {
        type = "app";
        program = "${hide}/bin/hide";
        meta.description = "Haskell IDE";
      };
      devShells.${system}.default = haskellPackages.shellFor {
        packages = _: [ hide ];
        nativeBuildInputs = [ pkgs.cabal-install pkgs.pkg-config ];
      };
    };
}
