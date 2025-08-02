-- | Common UI widgets for the EmaLetter website
module EmaLetter.Website.Widgets where

import Ema
import EmaLetter.Website.Routes
import EmaLetter.Website.Types
import Lucid
import Optics.Core (Prism')

{- | Primary color scheme for consistent theming
| Main brand colors using blue-purple gradient theme
-}
primaryColorScheme :: Text
primaryColorScheme = "bg-gradient-to-r from-blue-600 via-purple-600 to-indigo-600"

{- | Secondary color scheme for accents and interactive elements
| Includes text colors, hover states, and secondary UI elements
-}
secondaryColorScheme :: Text
secondaryColorScheme = "text-blue-600 hover:text-blue-800"

-- | Main application layout with header, body, and footer
appLayout :: Prism' FilePath Route -> Model -> HtmlRoute -> Html () -> Html ()
appLayout rp model currentRoute content = do
  headerBar rp model currentRoute
  main_ [class_ "flex-1 container mx-auto px-4 py-8 max-w-4xl"] content
  footerBar

-- | Header bar with logo, breadcrumbs, and navigation
headerBar :: Prism' FilePath Route -> Model -> HtmlRoute -> Html ()
headerBar rp model currentRoute =
  header_ [class_ $ primaryColorScheme <> " text-white shadow-lg"] $ do
    div_ [class_ "container mx-auto px-4 py-4"] $ do
      div_ [class_ "flex items-center justify-between"] $ do
        logoSection rp model
        titleSection currentRoute
        navigationSection rp currentRoute

-- | Logo section with site branding
logoSection :: Prism' FilePath Route -> Model -> Html ()
logoSection rp model =
  a_ [href_ $ routeUrl rp $ Route_Html HtmlRoute_Index, class_ "flex items-center space-x-3 hover:opacity-90 transition-opacity"] $ do
    img_ [src_ $ staticRouteUrl rp model "logo.svg", class_ "w-8 h-8", alt_ "Ema Logo"]
    span_ [class_ "text-xl font-bold"] "Emaletter"

-- | Navigation section with breadcrumbs and menu
navigationSection :: Prism' FilePath Route -> HtmlRoute -> Html ()
navigationSection rp currentRoute =
  nav_ [class_ "flex items-center space-x-6"] $ do
    forM_ (universe @HtmlRoute) $ \r ->
      let isActive = r == currentRoute
          activeClass = if isActive then "bg-white bg-opacity-20 text-white" else "text-blue-100 hover:text-white hover:bg-white hover:bg-opacity-10"
       in a_
            [ href_ $ routeUrl rp $ Route_Html r
            , class_ $ "px-3 py-2 rounded-md font-medium transition-all duration-200 " <> activeClass
            ]
            $ toHtml
            $ routeTitle r

-- | Title section for header bar (only shown on non-root pages)
titleSection :: HtmlRoute -> Html ()
titleSection currentRoute =
  case currentRoute of
    HtmlRoute_Index -> mempty
    _ -> h1_ [class_ "text-xl font-bold text-white"] $ toHtml $ routeTitle currentRoute

-- | Footer bar with additional links and info
footerBar :: Html ()
footerBar =
  footer_ [class_ "bg-gray-800 text-white mt-12"] $ do
    div_ [class_ "container mx-auto px-4 py-6"] $ do
      div_ [class_ "text-center text-sm text-gray-400"] $ do
        "Built with "
        a_ [href_ "https://ema.srid.ca", class_ "text-blue-400 hover:text-blue-300 transition-colors"] "Ema"
        " and "
        a_ [href_ "https://haskell.org", class_ "text-purple-400 hover:text-purple-300 transition-colors"] "Haskell"

-- | Content card wrapper for main content areas
contentCard :: Html () -> Html ()
contentCard =
  div_ [class_ "bg-white rounded-lg shadow-md border border-gray-200 p-6"]

-- | Stylized link component
styledLink :: Text -> Html () -> Html ()
styledLink href =
  a_ [href_ href, class_ $ secondaryColorScheme <> " font-medium transition-colors duration-200"]
