module Main where

import Data.Default (def)
import Ema qualified
import EmaLetter.Website.CLI qualified as CLI
import EmaLetter.Website.Site ()
import EmaLetter.Website.Types (Route)

-- Main entrypoint
-- ---------------

main :: IO ()
main = do
  cliArgs <- CLI.parseCliArgs
  let cfg = Ema.SiteConfig (CLI.cliArgsEmaCli cliArgs) def
  void $ Ema.runSiteWith @Route cfg cliArgs
