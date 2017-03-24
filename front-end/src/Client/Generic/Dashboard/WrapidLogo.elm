module Client.Generic.Dashboard.WrapidLogo exposing (logo)


-- import Svg exposing (..)
-- import Svg.Attributes exposing (..)
import Html exposing (Html, div, img)
import Html.Attributes exposing (style, src)


logo : Html msg
logo =
  img [src "../../../Assets/Icons/wrapid_logo.png"
      , style [("height", "30px"), ("margin-left", "39px")]] []




-- svg [ enableBackground "new 0 0 218 35", id "Layer_1", viewBox "0 0 218 35", x "0px", y "0px" ]
--   [ g []
--       [ g [ id "Landing-Page" ]
--           [ g [ id "App-Landing", transform "translate(-146.000000, -76.000000)" ]
--               [ g [ id "header", transform "translate(-6.000000, -1.000000)" ]
--                   [ g [ id "nav", transform "translate(87.000000, 69.000000)" ]
--                       [ g [ id "logo" ]
--                           [ g [ id "Group-3", transform "translate(65.000000, 8.000000)" ]
--                               [ Svg.path [ d "M81.5,18l5-15.4H90L95,18l5.7-15.4h4.1L97,24h-3.8L88.4,9.3h-0.3L83.4,24h-3.8L71.7,2.6 h4.1L81.5,18z M128.2,9.5c0,3.5-1.6,5.7-4.9,6.7l5.9,7.8h-4.9l-5.4-7.2h-5V24h-3.8V2.6h8.4c3.5,0,5.9,0.6,7.4,1.7 S128.2,7.1,128.2,9.5z M123.2,12.6c0.8-0.6,1.2-1.6,1.2-3s-0.4-2.4-1.2-2.9c-0.8-0.5-2.2-0.8-4.3-0.8H114v7.6h4.8 C120.9,13.5,122.4,13.2,123.2,12.6z M138.7,19.1l-2.3,4.9h-4.1l10-21.4h4.1l10,21.4h-4.1l-2.3-4.9H138.7z M148.5,15.8 L144.4,7l-4.1,8.8H148.5z M176.6,4.5c1.6,1.2,2.4,3.2,2.4,5.7s-0.8,4.5-2.4,5.7c-1.6,1.2-4.1,1.8-7.4,1.8h-4V24h-3.8V2.6h7.8 C172.5,2.6,175,3.2,176.6,4.5z M173.9,13.3c0.8-0.8,1.1-1.9,1.1-3.4s-0.5-2.5-1.5-3.1s-2.5-0.9-4.6-0.9h-3.9v8.6h4.4 C171.7,14.4,173.2,14,173.9,13.3z M185.3,2.6h3.8V24h-3.8V2.6z M214.3,5.4c2.2,1.9,3.3,4.5,3.3,7.8s-1.1,6-3.2,7.9 c-2.1,1.9-5.4,2.9-9.7,2.9h-7.5V2.6h7.8C209,2.6,212.1,3.5,214.3,5.4z M213.7,13.3c0-4.9-3-7.3-8.9-7.3H201v14.6h4.2 c2.7,0,4.8-0.6,6.3-1.9C213,17.5,213.7,15.7,213.7,13.3z", fill "#FFFFFF", id "WRAPID" ]
--                                   []
--                               , text "           "
--                               , node "rect" [ fill "#FFFFFF", height "3.4", id "Rectangle-4", width "19.8", x "78.5", y "31.5" ]
--                                   []
--                               , text "           "
--                               ]
--                           ]
--                       ]
--                   ]
--               ]
--           ]
--       , g [ id "Symbols" ]
--           [ g [ id "Wrapid-logo", transform "translate(-1.000000, -4.000000)" ]
--               [ g [ id "W-logo", transform "translate(1.000000, 4.000000)" ]
--                   [ node "rect" [ fill "#FF4600", height "24", id "Rectangle-3-Copy-4", transform "matrix(-0.7071 -0.7071 0.7071 -0.7071 57.293 49.4602)", width "12", x "32.9", y "0.9" ]
--                       []
--                   , text "      "
--                   , Svg.path [ d "M0.4,9.4L8.9,1l17,17l-8.5,8.5L0.4,9.4z M17.2,9.2l8.5-8.5l17,17l-8.5,8.5L17.2,9.2z", fill "#FFFFFF"]
--           []
--                   , text "      "
--                   ]
--               ]
--           ]
--       ]
--   ]
