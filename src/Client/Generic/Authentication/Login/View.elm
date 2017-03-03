module Client.Generic.Authentication.Login.View exposing (loginView)

import Client.Generic.Authentication.Login.Types exposing (Model, Msg(..), ViewState(..))
import Client.Generic.Dashboard.Dashboard as Dashboard
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Material
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Scheme


loginView : Model -> Html Msg
loginView model =
    div []
        [ Dashboard.view { navbar = { rightItems = Nothing } }
        , div [style [
            ("display", "flex")
            ,("flex-direction", "column")
            , ("justify-content", "center")
            , ("align-items", "center")
            , ("margin", "16px")
          ]]
          [span [style headerTitleStyle] [text "Welcome"]
          ,span
              [ style
                  [ ( "font-family", "Roboto-Regular" )
                  , ( "font-size", "16px" )
                  , ( "margin", "8px 0px 8px 0px" )
                  , ( "color", "#282C35" )
                  , ( "letter-spacing", "0" )
                  ]
              ]
              [ text "RUNABETTERSET Productions" ]
          ]
        ,viewLoginPanel model
        ]

headerTitleStyle : List ( String, String )
headerTitleStyle =
  [
  ("font-family", "Roboto-Bold")
  ,("font-size", "32px")
  ,("color", "#282C35")
  ,("letter-spacing", "0")
  ,("margin", "4px 0px 8px 0px")
  ]


viewLoginPanel : Model -> Html Msg
viewLoginPanel model =
    let
        panelHeader =
            Just { title = "Login", rightItem = Nothing }

        panelBody =
            div [ style [ ( "display", "flex" ), ( "flex-direction", "column" ), ("margin", "8px") ] ]
                [ Textfield.render Mdl
                    [ 10, 0 ]
                    model.mdl
                    [ Textfield.label "Enter email"
                    , Textfield.floatingLabel
                    , Textfield.email
                    , Options.onInput Email
                    ]
                    []
                , Textfield.render Mdl
                    [ 10, 1 ]
                    model.mdl
                    [ Textfield.label "Password"
                    , Textfield.floatingLabel
                    , Textfield.password
                    , Options.onInput Password
                    ]
                    []
                ]
                |> Material.Scheme.top

        footer =
            Just (div [style [("display", "flex"), ("flex-direction", "row-reverse")]]
            [
              div [style loginButtonStyle]
              [case model.viewState of
                Submitting ->
                  div [] [text "Logging you in..."]
                InUse ->
                  span [onClick SubmitLogin, style loginButtonTextStyle] [text "Sign In"]
              ]
            ])
    in
        div [style [("margin", "8px"), ("width", "360px")]] [Dashboard.makePanel panelHeader panelBody footer]



loginButtonStyle : List ( String, String )
loginButtonStyle =
  [
     ("display", "flex")
    ,("justify-content", "center")
    ,("align-items", "center")
    ,("background", "#50E3C2")
    ,("box-shadow", "0 2px 2px 0 #C3C6CF")
    ,("border-radius", "2px")
    ,("border-color", "transparent")
    ,("height", "48px")
    ,("width", "114px")
    ,("margin", "8px")
  ]

loginButtonTextStyle : List ( String, String )
loginButtonTextStyle =
  [
     ("font-family", "Roboto-Medium")
    ,("font-size", "16px")
    ,("color", "#FFFFFF")
    ,("margin", "12px 8px 12px 8px")
  ]
