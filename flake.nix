{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    ## Use Juspay upstream after PR merged - https://github.com/juspay/beam-mysql/pull/40
    beam-mysql.url = "github:arjunkathuria/beam-mysql/GHC-927";
    beam-mysql.inputs.nixpkgs.follows = "nixpkgs";
    beam-mysql.inputs.haskell-flake.follows = "haskell-flake";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ... }: {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', pkgs, lib, config, ... }: {
        haskellProjects.default = {
          projectFlakeName = "sequelize";
          basePackages = pkgs.haskell.packages.ghc927;
          imports = [
            inputs.beam-mysql.haskellFlakeProjectModules.output
          ];
          settings = {
            beam-postgres.check = false;
            beam-mysql.jailbreak = true;
            generic-lens.check = false;
          };
          autoWire = [ "packages" "checks" "devShells" "apps"];
        };
      };
    });
}
