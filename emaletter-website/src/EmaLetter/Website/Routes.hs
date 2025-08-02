module EmaLetter.Website.Routes where

import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import EmaLetter.Website.Types
import Lucid
import Optics.Core (Prism', (%))

routeTitle :: HtmlRoute -> Text
routeTitle r = case r of
  HtmlRoute_Index -> "Home"
  HtmlRoute_About -> "About"

routeLink :: Prism' FilePath Route -> HtmlRoute -> Html () -> Html ()
routeLink rp r =
  a_
    [ href_ $ routeUrl rp $ Route_Html r
    , class_ "text-rose-400"
    ]

staticRouteUrl :: (IsString r) => Prism' FilePath Route -> Model -> FilePath -> r
staticRouteUrl rp m =
  SR.staticRouteUrl (rp % (_As @"Route_Static")) (modelStatic m)
