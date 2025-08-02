{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    commonmark-initial-src = {
      url = "github:henrytill/commonmark-initial";
      flake = false;
    };
    dwergaz-src = {
      url = "github:henrytill/dwergaz";
      flake = false;
    };
  };

  nixConfig = {
    extra-substituters = [ "https://henrytill.cachix.org" ];
    extra-trusted-public-keys = [
      "henrytill.cachix.org-1:EOoUIk8e9627viyFmT6mfqghh/xtfnpzEtqT4jnyn1M="
    ];
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      commonmark-initial-src,
      dwergaz-src,
      ...
    }:
    let
      ghcName = "ghc948";
      overlay = final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcName} = prev.haskell.packages.${ghcName}.override {
              overrides = hfinal: hprev: {
                commonmark-initial = hfinal.callCabal2nix "commonmark-initial" commonmark-initial-src { };
                dwergaz = hfinal.callCabal2nix "dwergaz" dwergaz-src { };
                hbt = hfinal.callCabal2nix "hbt" (builtins.path {
                  path = ./.;
                  name = "hbt-src";
                }) { };
              };
            };
          };
        };
      };
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
      in
      {
        packages.hbt = pkgs.haskell.packages.${ghcName}.hbt;
        packages.default = self.packages.${system}.hbt;
      }
    );
}
