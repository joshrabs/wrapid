module Client.Generic.Authentication.Login.View exposing (loginView, initModel)

import Client.Generic.Authentication.Login.Types exposing (Model, Msg, Mdl)
import Client.Generic.Dashboard.Dashboard as Dashboard
import Html exposing (..)
import Html.Attributes exposing (..)
import Material
import Material.Textfield as Textfield
import Material.Scheme


loginView : Model -> Html Msg
loginView model =
    div []
        [ Dashboard.view { navbar = { rightItems = Nothing } }
        , viewLoginPanel model
        ]



-- TODO: Refactor this (ND)


initModel : Maybe String -> Maybe String -> Model
initModel email password =
    { email = email
    , password = password
    , mdl = Material.model
    }


viewLoginPanel : Model -> Html Msg
viewLoginPanel model =
    let
        panelHeader =
            Just { title = "Login", rightItem = Nothing }

        panelBody =
            div [ style [ ( "display", "flex" ), ( "flex-direction", "column" ) ] ]
                [ Textfield.render Mdl
                    [ 10, 0 ]
                    model.mdl
                    [ Textfield.label "Enter email"
                    , Textfield.floatingLabel
                    , Textfield.email
                    ]
                    []
                , Textfield.render Mdl
                    [ 10, 1 ]
                    model.mdl
                    [ Textfield.label "Enter password"
                    , Textfield.floatingLabel
                    , Textfield.password
                    ]
                    []
                ]
                |> Material.Scheme.top

        footer =
            Nothing
    in
        Dashboard.makePanel panelHeader panelBody footer
