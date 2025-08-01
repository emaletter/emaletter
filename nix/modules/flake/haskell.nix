{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  perSystem = { lib, config, ... }: {
    haskellProjects.default = {
      imports = [
        inputs.ema.haskellFlakeProjectModules.output
      ];
      projectRoot = builtins.toString (lib.fileset.toSource {
        root = root;
        fileset = lib.fileset.unions [
          (root + /emaletter-website)
          (root + /emaletter-crawler)
          (root + /cabal.project)
          (root + /LICENSE)
        ];
      });
      autoWire = [ "packages" "apps" "checks" ];
    };

    packages.default = config.packages.emaletter-website;
  };
}
