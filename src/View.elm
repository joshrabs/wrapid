module View exposing (view)

import Types exposing (..)
import Client.Generic.Authentication.Login.Login as Login
import Client.ExtraPortal.ExtraPortal as ExtraPortal
import Client.PAPortal.View as PAPortal
import Html exposing (Html, a, button, div, h1, h4, img, li, p, text, ul)
import Html.Attributes exposing (href, src, style)
import Html.Events exposing (onClick)
import Navigation as Nav


view : Model -> Html Msg
view model =
    div []
        [ case model.currentViewState of
            LoginView ->
                Html.map LoginMsg (Login.loginView (Login.initModel Nothing Nothing))

            ExtraPortalView ->
                Html.map ExtraPortalMsg (ExtraPortal.viewExtraPortal model.extraPortalModel)

            PAPortalView ->
                Html.map PAPortalMsg
                    (PAPortal.viewPAPortal model.paPortalModel)
        , div [ style [ ( "position", "fixed" ), ( "top", "0px" ), ( "right", "0px" ), ( "border", "1px solid black" ) ] ]
            [ button [ onClick (ChangeView ExtraPortalView) ] [ text "Extra Portal" ]
            , button [ onClick (ChangeView PAPortalView) ] [ text "PA Portal" ]
            , button [ onClick (ChangeView LoginView) ] [ text "Login" ]
            ]
        ]


viewLink : String -> Html msg
viewLink name =
    li [] [ a [ href ("#" ++ name) ] [ text name ] ]


viewLocation : Nav.Location -> Html msg
viewLocation location =
    li [] [ text (location.pathname ++ location.hash) ]
