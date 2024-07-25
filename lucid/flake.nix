{
  description = "lucid-htmx";

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
                lucid-htmx = overrideCabal (hfinal.callCabal2nix "lucid-htmx" ./. {}) (drv: {checkPhase = "true";});
              };
          };
        });
  in {
    formatter = forAllSystems ({pkgs, ...}: pkgs.alejandra);

    # nix build
    packages = forAllSystems (
      {hsPkgs, ...}: {
        inherit hsPkgs;
        lucid-htmx = hsPkgs.lucid-htmx;
        default = hsPkgs.lucid-htmx;
      }
    );

    checks = {};

    # nix develop
    devShells = forAllSystems ({
      system,
      hsPkgs,
      pkgs,
      ...
    }: {
      default = hsPkgs.shellFor {
        name = "lucid-htmx";
        shellHook = ''
          export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
          export LC_ALL=C.UTF-8
        '';
        packages = p: [
          p.lucid-htmx
        ];
        buildInputs = with pkgs; [
          hsPkgs.haskell-language-server
          cabal2nix
          haskellPackages.ghcid
          haskellPackages.fourmolu
          haskellPackages.cabal-fmt
          haskellPackages.cabal-install
          hlint
        ];
      };
    });
  };
}
