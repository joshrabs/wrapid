module View exposing (view)

import Types exposing (..)
import Client.Generic.Authentication.Login.View as Login
import Client.ExtraPortal.ExtraPortal as ExtraPortal
import Client.PAPortal.View as PAPortal
import Common.Navbar exposing (navbar)
import Html exposing (Html, a, button, div, h1, h4, img, li, p, text, ul)
import Html.Attributes exposing (href, src, style)
import Html.Events exposing (onClick)
import Navigation as Nav


{-| For the PAPortal view, the navbar has been moved to the top level view
    instead of being managed as part of the component's view. This has several
    advantages, the main one being that the navbar may need access to parts of
    the model like the user object, login state, etc. that aren't part of any
    one subcomponent's model.

    Eventually I want to move the navbar to the very top of this view function,
    but for now other views are still using the original version.
-}
view : Model -> Html Msg
view model =
    let
        navbarConfig =
            { toggleMenuMsg = ToggleNotifications
            , selectItemMsg = SelectNotification
            }

        navbarContext =
            { isOpen = False
            }
    in
        div []
            [ case model.currentViewState of
                LoginView ->
                    Html.map LoginMsg (Login.loginView (Login.initModel Nothing Nothing))

                ExtraPortalView ->
                    Html.map ExtraPortalMsg (ExtraPortal.viewExtraPortal model.extraPortalModel)

                PAPortalView ->
                    div
                        []
                        [ navbar
                            model.mdl
                            navbarConfig
                            navbarContext
                            Nothing
                            [ "Check in at 9:00", "Lunch at 12:30" ]
                        , Html.map PAPortalMsg
                            (PAPortal.viewPAPortal model.paPortalModel)
                        ]
            , div [ style [ ( "position", "fixed" ), ( "top", "0px" ), ( "right", "0px" ), ( "border", "1px solid black" ) ] ]
                [ button [ onClick (ChangeView ExtraPortalView) ] [ text "Extra Portal" ]
                , button [ onClick (ChangeView PAPortalView) ] [ text "PA Portal" ]
                , button [ onClick (ChangeView LoginView) ] [ text "Login" ]
                ]
            ]


{-| Not sure what these are for. They're not used in the view function, and
    they're not exported, so no other modules should be using them.
-}
viewLink : String -> Html msg
viewLink name =
    li [] [ a [ href ("#" ++ name) ] [ text name ] ]


viewLocation : Nav.Location -> Html msg
viewLocation location =
    li [] [ text (location.pathname ++ location.hash) ]
