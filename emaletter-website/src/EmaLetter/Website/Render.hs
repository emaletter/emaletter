module EmaLetter.Website.Render (
  renderHtmlRoute,
) where

import Ema
import EmaLetter.Website.Routes
import EmaLetter.Website.Types
import EmaLetter.Website.Widgets
import Lucid
import Optics.Core (Prism')

renderHtmlRoute :: Prism' FilePath Route -> Model -> HtmlRoute -> LByteString
renderHtmlRoute rp m r = do
  renderBS $ doctypehtml_ $ do
    head_ $ renderHead rp m r
    body_ [class_ "bg-gray-50 font-sans min-h-screen flex flex-col"] $ do
      appLayout rp m r $ renderPageContent rp m r

renderHead :: Prism' FilePath Route -> Model -> HtmlRoute -> Html ()
renderHead rp model r = do
  meta_ [charset_ "UTF-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  title_ $ toHtml $ routeTitle r <> " - Emaletter"
  base_ [href_ $ modelBaseUrl model]
  link_ [rel_ "icon", type_ "image/svg+xml", href_ $ staticRouteUrl rp model "logo.svg"]
  link_ [rel_ "stylesheet", href_ $ staticRouteUrl rp model "tailwind.css"]

renderPageContent :: Prism' FilePath Route -> Model -> HtmlRoute -> Html ()
renderPageContent rp model r = do
  contentCard $ do
    case r of
      HtmlRoute_Index -> do
        p_ [class_ "text-gray-700"] $ do
          "Emaletter is work in progress. See "
          styledLink (routeUrl rp $ Route_Html HtmlRoute_About) "About"
          " for details."
      HtmlRoute_About -> do
        p_ [class_ "text-gray-700 mb-4"] $ do
          "Check out the "
          a_ [href_ "https://github.com/srid/emaletter", target_ "_blank", class_ "text-blue-600 hover:text-blue-800 font-medium transition-colors duration-200"] "GitHub repository"
          " for the source code."
        p_ [class_ "text-gray-700"] $ do
          "Join our "
          a_ [href_ "https://discord.gg/y9mb7uwD7W", target_ "_blank", class_ "text-purple-600 hover:text-purple-800 font-medium transition-colors duration-200"] "Discord community"
          " for discussions and support."
