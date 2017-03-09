module View exposing (view)

import Types exposing (..)
import Client.Generic.Authentication.Login.View as Login
import Client.ExtraPortal.ExtraPortal as ExtraPortal
import Client.PAPortal.View as PAPortal
import Client.WardrobePortal.View as WardrobePortal
import Common.Navbar exposing (navbar)
import Html exposing (Html, a, button, div, h1, h4, img, li, p, text, ul)
import Html.Attributes exposing (href, src, style)
import Html.Events exposing (onClick)


view : Model -> Html Msg
view model =
    div [ style [ ( "height", "100vh" ) ] ]
        [ case model.currentViewState of
            Login loginModel ->
                Html.map LoginMsg (Login.loginView loginModel)

            ExtraPortal epModel ->
                Html.map (\b -> (ChildMsg (ExtraPortalMsg b))) (ExtraPortal.viewExtraPortal epModel)

            PAPortal paModel ->
                div
                    []
                    [ navbar
                        model.mdl
                        Mdl
                        (Just "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/cizlua8ye0s1s0187rsrj1yn5")
                        [ "Check in at 9:00", "Lunch at 12:30" ]
                    , Html.map (\b -> (ChildMsg (PAPortalMsg b))) (PAPortal.viewPAPortal paModel)
                    ]

            WardrobePortal wardrobeModel ->
                Html.map (\b -> (ChildMsg (WardrobePortalMsg b))) (WardrobePortal.view wardrobeModel)
        , if model.shouldShowPortalSwitcher then
            viewPortalSwitcher
          else
            div [ onClick (ShowPortalSwitcher True), style [ ( "position", "fixed" ), ( "bottom", "0px" ), ( "left", "0px" ), ( "min-height", "20px" ), ( "width", "100%" ), ( "background", "transparent" ) ] ] []
        ]


viewPortalSwitcher : Html Msg
viewPortalSwitcher =
    div
        [ style [ ( "position", "fixed" ), ( "bottom", "0px" ), ( "left", "0px" ), ( "border", "1px solid black" ) ]
        ]
        [ button [ style [ ( "background", "orange" ) ], onClick (ShowPortalSwitcher False) ] [ text "-" ]
        , button [ onClick (ChangeView ExtraPortalView) ] [ text "Extra Portal" ]
        , button [ onClick (ChangeView PAPortalView) ] [ text "PA Portal" ]
        , button [ onClick (ChangeView LoginView) ] [ text "Login" ]
        , button [ onClick (ChangeView WardrobePortalView) ] [ text "Wardrobe Portal" ]
        ]
