default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ARGS}}

# Run the Ema dev server (ghcid + tailwind)
run-website:
    nix run .#ema-tailwind-run

# Run the crawler (ghcid)
run-crawler:
    cd emaletter-crawler && \
    ghcid --warnings -T Main.main
