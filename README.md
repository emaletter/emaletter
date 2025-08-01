# emaletter

> [!NOTE]
> **Goal**: Like https://haskellweekly.news/ - but a general "X weekly" (or "X monthly") software powered by https://ema.srid.ca/

## Repo structure

- Haskell
    - `./emaletter-website`: [Ema](https://ema.srid.ca/) static site generator[^tmpl]
    - `./emaletter-crawler`: [Flat Data](https://githubnext.com/projects/flat-data/) style crawler for updating data (used by `emaletter-website`)
- Nix
    - `./nix/modules/flake/*.nix`: [flake.parts](http://flake.parts/) Nix modules, that get automatically imported by `flake.nix`
    - `justfile`: Run `just` in Nix devShell to see what's available.
    - We use pre-commit hooks to enforce formatting and hlint health.

[^tmpl]: Based on [ema-template](https://github.com/srid/ema-template) with structure inspired by [haskell-template](https://github.com/srid/haskell-template).

## Roadmap

TBD

## License

AGPL 
