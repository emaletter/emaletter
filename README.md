# emaletter

> [!NOTE]
> **Goal**: Like https://haskellweekly.news/ - but a general "X weekly" (or "X monthly") software powered by https://ema.srid.ca/

## Repo structure

> [!TIP]
> **Relude**: Please familiarize yourself with [Relude](https://hackage.haskell.org/package/relude), our favoured Haskell prelude, whilst working on the Haskell codebase.

- Haskell
    - `./emaletter-website`: [Ema](https://ema.srid.ca/) static site generator[^tmpl]
    - `./emaletter-crawler`: [Flat Data](https://githubnext.com/projects/flat-data/) style crawler for updating data (used by `emaletter-website`)
- Nix
    - `./nix/modules/flake/*.nix`: [flake.parts](http://flake.parts/) Nix modules, that get automatically imported by `flake.nix`
    - `justfile`: Run `just` in Nix devShell to see what's available.
    - We use pre-commit hooks to enforce formatting and hlint health.

[^tmpl]: Based on [ema-template](https://github.com/srid/ema-template) with structure inspired by [haskell-template](https://github.com/srid/haskell-template).

## Development

First, enter the Nix devShell ideally via [direnv](https://nixos.asia/en/direnv).

We use `ghcid` for fast re-compile/ re-run loop.

To run the Ema website:

```sh
# In Nix devShell
just run-website
```

To run the crawler:

```sh
# In Nix devShell
just run-crawler
```

## Roadmap

TBD

## License

AGPL 
