{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-root.url = "github:srid/flake-root";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
      ];

      perSystem = {
        self',
        config,
        pkgs,
        ...
      }: {
        haskellProjects.default = {
          packages = {};
          settings = {};

          devShell = {
            enable = true;
            tools = hp:
              {
                treefmt = config.treefmt.build.wrapper;
              }
              // config.treefmt.build.programs;

            hlsCheck.enable = true;
          };
        };

        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;
          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;

          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = ["--ghc-opt" "-XImportQualifiedPost"];
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.heffal;
        apps.default = self'.apps.heffal;
      };
    };
}
