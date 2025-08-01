{ inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];
  perSystem = { config, pkgs, lib, ... }:
    let
      tailwind = pkgs.haskellPackages.tailwind;
      buildEmaSiteWithTailwind = { baseUrl }:
        pkgs.runCommand "site"
          { }
          ''
            mkdir -p $out
            pushd ${inputs.self}/emaletter-website
            ${lib.getExe config.packages.emaletter-website} \
              --base-url=${baseUrl} gen $out
            ${lib.getExe tailwind} \
              -o $out/tailwind.css 'src/**/*.hs'
          '';
    in
    {
      process-compose."ema-tailwind-run" = {
        cli.environment.PC_DISABLE_TUI = true;
        settings = {
          processes = {
            haskell.command = "cd ./emaletter-website && ghcid";
            tailwind = {
              command = "cd ./emaletter-website && ${lib.getExe tailwind} -w -o ./static/tailwind.css './src/**/*.hs'";
              is_tty = true;
            };
          };
        };
      };

      packages = {
        site = buildEmaSiteWithTailwind { baseUrl = "/"; };
        site-github = buildEmaSiteWithTailwind { baseUrl = "/emaletter/"; };
      };
    };
}
