{
  inputs = {
    common.url = "github:juspay/nix-common";
    beam.url = "github:juspay/beam/b5f14b640110bcfab6bc86f07f864516b2d7ffd8";
    beam-mysql.url = "github:juspay/beam-mysql/895e10ffadb122a3393ce1274bb6b114435ba2fe";
    common.inputs.beam.follows = "beam";
    common.inputs.beam-mysql.follows = "beam-mysql";

  };
  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      perSystem = { self', pkgs, pkgs-latest, config, filter, ... }: {

        haskellProjects.default = let fs = pkgs-latest.lib.fileset; in {
          projectRoot = builtins.toString (fs.toSource {
            root = ./.;
            fileset = fs.unions [
              ./src
              ./test
              ./sequelize.cabal
              ./README.md
            ];
          });

          autoWire = [ "packages" ];
          packages = {
            # Dependencies
          };
          settings = {
            sequelize.check = false;
          };
        };

        packages.default = self'.packages.sequelize;

        devShells.default = pkgs.mkShell {
          name = "haskell-sequelize";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.devShells.common
          ];
        };
      };
    };
}
