{
  inputs = {
    self.submodules = true;
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    # hbt-data's own flake, read from the corpus submodule: a relative path
    # input locks relative to this flake, not by hash, so the submodule stays
    # the one pin on the harness and the corpus it checks.
    hbt-data = {
      url = "path:./test/data";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    commonmark-initial-src = {
      url = "github:henrytill/commonmark-initial";
      flake = false;
    };
    dwergaz-src = {
      url = "github:henrytill/dwergaz";
      flake = false;
    };
    uri-bytestring-src = {
      url = "github:Soostone/uri-bytestring/0.4.0.1";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      hbt-data,
      commonmark-initial-src,
      dwergaz-src,
      uri-bytestring-src,
      ...
    }:
    let
      ghcName = "ghc9103";
      # Stamp the revision the executable is built from into project.h, the
      # header cli/exe/Version.hs includes.  A source with no git metadata
      # carries neither attribute, and the header in the repository defaults the
      # suffix to empty, so the executable then reports the cabal version alone.
      stampCliRevision =
        final: drv:
        let
          rev = self.shortRev or self.dirtyShortRev or null;
        in
        final.haskell.lib.overrideCabal drv (
          _:
          final.lib.optionalAttrs (rev != null) {
            postPatch = ''
              echo '#define HBT_COMMIT_SUFFIX "-${rev}"' > exe/project.h
            '';
          }
        );
      # pkgsStatic links executables statically by default.  Dropping the
      # library component keeps its static archive out of the output.
      maybeStaticExecutable =
        isStatic: final: drv:
        final.haskell.lib.overrideCabal drv (
          _:
          final.lib.optionalAttrs isStatic {
            isLibrary = false;
            isExecutable = true;
          }
        );
      overlay = isStatic: final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcName} = prev.haskell.packages.${ghcName}.override {
              overrides = hfinal: hprev: {
                commonmark-initial = hfinal.callCabal2nix "commonmark-initial" commonmark-initial-src { };
                dwergaz = hfinal.callCabal2nix "dwergaz" dwergaz-src { };
                uri-bytestring = hfinal.callCabal2nix "uri-bytestring" uri-bytestring-src { };
                hbt-attic = hfinal.callCabal2nix "hbt-attic" (builtins.path {
                  path = ./attic;
                  name = "hbt-attic-src";
                }) { };
                hbt-cli = maybeStaticExecutable isStatic final (
                  stampCliRevision final (
                    hfinal.callCabal2nix "hbt-cli" (builtins.path {
                      path = ./cli;
                      name = "hbt-cli-src";
                    }) { }
                  )
                );
                hbt-core = hfinal.callCabal2nix "hbt-core" (builtins.path {
                  path = ./core;
                  name = "hbt-core-src";
                }) { };
                hbt-pinboard-client = maybeStaticExecutable isStatic final (
                  hfinal.callCabal2nix "hbt-pinboard-client" (builtins.path {
                    path = ./pinboard-client;
                    name = "hbt-pinboard-client-src";
                  }) { }
                );
                hbt-pinboard-types = hfinal.callCabal2nix "hbt-pinboard-types" (builtins.path {
                  path = ./pinboard-types;
                  name = "hbt-pinboard-types-src";
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
        pkgs = nixpkgs.legacyPackages.${system}.extend (overlay false);
        pkgsStatic = nixpkgs.legacyPackages.${system}.pkgsStatic.extend (overlay true);
      in
      {
        packages = rec {
          hbt-attic = pkgs.haskell.packages.${ghcName}.hbt-attic;
          hbt-cli = pkgs.haskell.packages.${ghcName}.hbt-cli;
          hbt-cli-static = pkgsStatic.haskell.packages.${ghcName}.hbt-cli;
          hbt-pinboard-client = pkgs.haskell.packages.${ghcName}.hbt-pinboard-client;
          hbt-pinboard-client-static = pkgsStatic.haskell.packages.${ghcName}.hbt-pinboard-client;
          all = pkgs.symlinkJoin {
            pname = "hbt-all";
            version = "0.1.0.0";
            paths = [
              hbt-attic
              hbt-cli
              hbt-pinboard-client
            ];
          };
          all-static = pkgsStatic.symlinkJoin {
            pname = "hbt-all-static";
            version = "0.1.0.0";
            paths = [
              hbt-attic
              hbt-cli-static
              hbt-pinboard-client-static
            ];
          };
          default = all;
        };
        checks.conformance = hbt-data.lib.${system}.check {
          binary = "${self.packages.${system}.hbt-cli}/bin/hbt";
          waivers = ./conformance.waivers;
        };
        devShells.default = pkgs.haskell.packages.${ghcName}.shellFor {
          packages = hpkgs: [
            hpkgs.hbt-attic
            hpkgs.hbt-cli
            hpkgs.hbt-core
            hpkgs.hbt-pinboard-client
            hpkgs.hbt-pinboard-types
          ];
          withHoogle = true;
          nativeBuildInputs = with pkgs; [
            cabal-install
            ghcid
            haskell.packages.${ghcName}.fourmolu
            haskell.packages.${ghcName}.ghc-tags
            haskell.packages.${ghcName}.hlint
            haskell.packages.${ghcName}.weeder
            yaml-language-server
            hbt-data.packages.${system}.python
          ];
        };
      }
    );
}
