{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
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
      ...
    }:
    let
      ghcName = "ghc948";
      overlay = final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcName} = prev.haskell.packages.${ghcName}.override {
              overrides = hfinal: hprev: {
                dwergaz = hfinal.callCabal2nix "dwergaz" (final.fetchFromGitHub {
                  owner = "henrytill";
                  repo = "dwergaz";
                  rev = "13c00c1a9bb32fc468c260735b00e2152d71b056";
                  sha256 = "sha256-tL0zA+af1u8VedK2SXAS7YSbqUfxGMdvnbp7O8bo/k4=";
                }) { };
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
