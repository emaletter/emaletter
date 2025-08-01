{
  perSystem = { config, pkgs, lib, ... }:
    let
      tailwind = pkgs.haskellPackages.tailwind;
    in
    {
      devShells.default = pkgs.mkShell {
        name = "emaletter-website";
        meta.description = "emaletter-website development environment";
        packages = [
          tailwind
          pkgs.just
          pkgs.nixd
        ];
        inputsFrom = [
          config.haskellProjects.default.outputs.devShell
          config.pre-commit.devShell
        ];
      };
    };
}
