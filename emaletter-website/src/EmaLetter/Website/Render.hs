module EmaLetter.Website.Render (
  renderHtmlRoute,
) where

import Ema
import EmaLetter.Website.Routes
import EmaLetter.Website.Types
import Lucid
import Optics.Core (Prism')

renderHtmlRoute :: Prism' FilePath Route -> Model -> HtmlRoute -> LByteString
renderHtmlRoute rp m r = do
  renderBS $ doctypehtml_ $ do
    head_ $ renderHead rp m r
    body_ [class_ "bg-gray-50"] $ renderBody rp m r

renderHead :: Prism' FilePath Route -> Model -> HtmlRoute -> Html ()
renderHead rp model r = do
  meta_ [charset_ "UTF-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  title_ $ toHtml $ routeTitle r <> " - Ema Template"
  base_ [href_ $ modelBaseUrl model]
  link_ [rel_ "stylesheet", href_ $ staticRouteUrl rp model "tailwind.css"]

renderBody :: Prism' FilePath Route -> Model -> HtmlRoute -> Html ()
renderBody rp model r = do
  div_ [class_ "container mx-auto mt-8 p-4 max-w-prose border-2 bg-white rounded-lg shadow"] $ do
    renderNavbar rp r
    h1_ [class_ "text-3xl font-bold"] $ toHtml $ routeTitle r
    case r of
      HtmlRoute_Index -> do
        "You are on the index page. Want to see "
        routeLink rp HtmlRoute_About "About"
        "?"
      HtmlRoute_About -> do
        "You are on the about page."
    a_ [href_ $ staticRouteUrl rp model "logo.svg", target_ "_blank"] $ do
      img_ [src_ $ staticRouteUrl rp model "logo.svg", class_ "py-4 w-32", alt_ "Ema Logo"]

renderNavbar :: Prism' FilePath Route -> HtmlRoute -> Html ()
renderNavbar rp currentRoute =
  nav_ [class_ "w-full text-xl font-bold flex space-x-4  mb-4"] $ do
    forM_ (universe @HtmlRoute) $ \r ->
      let extraClass = if r == currentRoute then "bg-rose-400 text-white" else "text-gray-700"
       in a_
            [ href_ $ routeUrl rp $ Route_Html r
            , class_ $ "rounded p-2 " <> extraClass
            ]
            $ toHtml
            $ routeTitle r
