{
  inputs = {
    self.submodules = true;
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
    uri-bytestring-src = {
      url = "github:Soostone/uri-bytestring/0.4.0.1";
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
      uri-bytestring-src,
      ...
    }:
    let
      ghcName = "ghc9103";
      overlay = isStatic: final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcName} = prev.haskell.packages.${ghcName}.override {
              overrides = hfinal: hprev: {
                ghc = hprev.ghc.override (
                  prev.lib.optionalAttrs isStatic {
                    enableNuma = false;
                  }
                );
                commonmark-initial = hfinal.callCabal2nix "commonmark-initial" commonmark-initial-src { };
                dwergaz = hfinal.callCabal2nix "dwergaz" dwergaz-src { };
                uri-bytestring = hfinal.callCabal2nix "uri-bytestring" uri-bytestring-src { };
                hbt =
                  let
                    hbt_ = hfinal.callCabal2nix "hbt" (builtins.path {
                      path = ./.;
                      name = "hbt-src";
                    }) { };
                  in
                  final.haskell.lib.overrideCabal hbt_ (
                    _:
                    # Partially adapted from:
                    # https://sigkill.dk/blog/2024-05-22-static-linking-on-nix-with-ghc96.html
                    #
                    # Linking libdw's dependencies by hand allows us to build GHC with DWARF support.
                    final.lib.optionalAttrs isStatic {
                      isLibrary = false;
                      isExecutable = true;
                      enableSharedExecutables = false;
                      enableSharedLibraries = false;
                      configureFlags =
                        let
                          bzip2 = prev.bzip2.override {
                            enableStatic = true;
                          };
                          gmp = prev.gmp.overrideAttrs (_: {
                            dontDisableStatic = true;
                          });
                          libffi = prev.libffi.overrideAttrs (_: {
                            dontDisableStatic = true;
                          });
                          xz = prev.xz.override {
                            enableStatic = true;
                          };
                          zlib = final.zlib.static;
                          zstd = prev.zstd.override {
                            enableStatic = true;
                          };
                        in
                        [
                          "--ghc-option=-split-sections"
                          "--ghc-option=-optl=-static"
                          "--ghc-option=-optl=-lbz2"
                          "--ghc-option=-optl=-lelf"
                          "--ghc-option=-optl=-llzma"
                          "--ghc-option=-optl=-lz"
                          "--ghc-option=-optl=-lzstd"
                          "--extra-lib-dirs=${bzip2.out}/lib"
                          "--extra-lib-dirs=${gmp}/lib"
                          "--extra-lib-dirs=${libffi}/lib"
                          "--extra-lib-dirs=${xz.out}/lib"
                          "--extra-lib-dirs=${zlib}/lib"
                          "--extra-lib-dirs=${zstd.out}/lib"
                        ];
                    }
                  );
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
        pkgsMusl = nixpkgs.legacyPackages.${system}.pkgsMusl.extend (overlay true);
      in
      {
        packages.hbt = pkgs.haskell.packages.${ghcName}.hbt;
        packages.hbt-static = pkgsMusl.haskell.packages.${ghcName}.hbt;
        packages.default = self.packages.${system}.hbt;
      }
    );
}
