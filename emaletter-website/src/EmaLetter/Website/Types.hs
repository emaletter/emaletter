{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module EmaLetter.Website.Types where

import Ema.Route.Generic.TH
import Ema.Route.Lib.Extra.StaticRoute qualified as SR

data Model = Model
  { modelBaseUrl :: Text
  , modelStatic :: SR.Model
  }
  deriving stock (Eq, Show, Generic)

data HtmlRoute
  = HtmlRoute_Index
  | HtmlRoute_About
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)

deriveGeneric ''HtmlRoute
deriveIsRoute ''HtmlRoute [t|'[]|]

type StaticRoute = SR.StaticRoute "static"

data Route
  = Route_Html HtmlRoute
  | Route_Static StaticRoute
  deriving stock (Eq, Show, Ord, Generic)

deriveGeneric ''Route
deriveIsRoute
  ''Route
  [t|
    [ -- To render a `Route` we need `Model`
      WithModel Model
    , -- Override default sub-route encoding (to avoid the folder prefix in encoded URLs)
      WithSubRoutes [HtmlRoute, StaticRoute]
    ]
    |]
