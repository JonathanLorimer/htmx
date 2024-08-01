{
  description = "htmx";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    forAllSystems = function:
      nixpkgs.lib.genAttrs ["x86_64-linux" "aarch64-linux"] (system:
        function rec {
          inherit system;
          compilerVersion = "ghc964";
          pkgs = nixpkgs.legacyPackages.${system};
          hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
            overrides = hfinal: hprev:
              with pkgs.haskell.lib; {
                # Internal Packages
                htmx = hfinal.callCabal2nix "htmx" ./htmx {};
                htmx-lucid = hfinal.callCabal2nix "htmx-lucid" ./htmx-lucid {};
                htmx-servant = hfinal.callCabal2nix "htmx-servant" ./htmx-servant {};
              };
          };
        });
  in {
    formatter = forAllSystems ({pkgs, ...}: pkgs.alejandra);

    # nix build
    packages = forAllSystems (
      {hsPkgs, ...}: {
        inherit hsPkgs;
        htmx = hsPkgs.htmx;
        default = hsPkgs.htmx;
      }
    );

    checks = {};

    # nix develop
    devShells = forAllSystems ({
      system,
      hsPkgs,
      pkgs,
      ...
    }: let
      hkg = pkgs.writeShellScriptBin "hkg" (builtins.readFile ./scripts/hkg.sh);
      packages = ["htmx" "htmx-lucid" "htmx-servant"];
      format = pkgs.writeShellScriptBin "format" (
        pkgs.lib.strings.concatMapStringsSep "\n" (
          package: ''
            fourmolu -i ./${package}
            cabal-fmt -i ./${package}/${package}.cabal
          ''
        )
        packages
      );
    in {
      default = hsPkgs.shellFor {
        name = "htmx";
        shellHook = ''
          export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
          export LC_ALL=C.UTF-8
        '';
        packages = p: [
          p.htmx
          p.htmx-lucid
          p.htmx-servant
        ];
        buildInputs = with pkgs; [
          hsPkgs.haskell-language-server
          cabal2nix
          haskellPackages.ghcid
          haskellPackages.fourmolu
          haskellPackages.cabal-fmt
          haskellPackages.cabal-install
          hlint
          hkg
          format
        ];
      };
    });
  };
}
