{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module must be imported to make the 'EmaSite' instance available.
module EmaLetter.Website.Site () where

import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import EmaLetter.Website.CLI
import EmaLetter.Website.Render
import EmaLetter.Website.Types
import Optics.Core ((%))

instance EmaSite Route where
  type SiteArg Route = CliArgs
  siteInput cliAct args = do
    staticRouteDyn <- siteInput @StaticRoute cliAct ()
    pure $ Model (cliArgsBaseUrl args) <$> staticRouteDyn
  siteOutput rp m = \case
    Route_Html r ->
      pure $ Ema.AssetGenerated Ema.Html $ renderHtmlRoute rp m r
    Route_Static r ->
      siteOutput (rp % (_As @"Route_Static")) (modelStatic m) r
