Emaletter is a Haskell application designed to create newsletter for any technology stack.

Repo structure is documented in ./README.md. Read it.

To build the project, you can run `cabal build all` in the Nix devShell. The VSCode terminal is often activated in direnv, so `nix develop -c` is usually not required.

## Haskell coding guidelines

- Library preferences
    - We use relude as prelude
    - Prefer optics over lens
    - Use NeatInterpolation for multiline strings.
- Imports
    - Prefer qualified imports unless it impacts readability.
    - Use explicit imports unless it makes sense (as with preludes, or when importing optparse-applicative in a CLI module)
- Modules
    - Generally every module should have explicit exports, unless it makes sense otherwise (e.g.: when all toplevels need to be exported)