port module Main exposing (main)

import Html exposing (Html, a, button, div, h1, img, li, p, text, ul)
import Html.Attributes exposing (href, src, style)
import Html.Events exposing (onClick)
import Navigation as Nav
import List.Extra exposing (find)
import Maybe exposing (andThen)

import Client.Generic.Authentication.Login.Login as Login exposing (loginView, Msg)
import Client.ExtraPortal.ExtraPortal as ExtraPortal exposing (..)
import Client.PAPortal.PAPortal as PAPortal exposing (..)

main : Program Never Model Msg
main =
    Nav.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { history : List Nav.Location
    , profiles : List Profile
    , currentImg : Maybe String
    , currentView : ViewState
    }

type alias Url =
    String

type alias Profile =
    { id : String
    , firstName : String
    , url : Maybe String
    }


type ViewState = LoginView | ExtraPortalView | PAPortalView

init : Nav.Location -> ( Model, Cmd Msg )
init location =
    ( Model [ location ] [] Nothing LoginView
    , Cmd.none
    )

-- UPDATE


type Msg
    = UrlChange Nav.Location
    | LoginMsg Login.Msg
    | GetAllProfiles
    | Profiles (List Profile)
    | ShowAvatar String
    | ChangeView ViewState
    | ExtraPortalMsg ExtraPortal.Msg
    | PAPortalMsg PAPortal.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeView viewState -> ({model | currentView = viewState}, Cmd.none)
        UrlChange location ->
            ( { model | history = location :: model.history }
            , Cmd.none
            )

        GetAllProfiles ->
            ( model
            , getAllProfiles ()
            )

        Profiles list ->
            ( { model | profiles = list }
            , Cmd.none
            )

        ShowAvatar id ->
            let
                clickedUser =
                    find (\usr -> usr.id == id) model.profiles

                url =
                    andThen .url clickedUser
            in
                ( { model | currentImg = url }
                , Cmd.none
                )
        LoginMsg loginMsg -> (model, Cmd.none)
        ExtraPortalMsg epMsg -> (model, Cmd.none)
        PAPortalMsg paMsg -> (model, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    div []
    [
      case model.currentView of
          LoginView -> Html.map LoginMsg (Login.loginView Nothing)
          ExtraPortalView -> Html.map ExtraPortalMsg (ExtraPortal.viewExtraPortal {profile = {firstName = "Steve"}})
          PAPortalView -> Html.map PAPortalMsg (PAPortal.viewPAPortal (PAPortal.defaultModel "ciykqvsynnqo60127o3illsce"))
      , div [style [("position", "fixed"), ("bottom", "0px"), ("border", "1px solid black")]]
      [
          div [] [
                ul [] (List.map viewLink [ "bears", "cats", "dogs", "elephants", "fish" ])
              , h1 [] [ text "History" ]
              , ul [] (List.map viewLocation model.history)
              , h1 [] [ text "Data" ]
              , button [ onClick GetAllProfiles ] [ text "Get All Profiles" ]
              ]
        , button [onClick (ChangeView ExtraPortalView)] [text "Extra Portal"]
        , button [onClick (ChangeView PAPortalView)] [text "PA Portal"]
        , button [onClick (ChangeView LoginView)] [text "Login"]
      ]
    ]


viewLink : String -> Html msg
viewLink name =
    li [] [ a [ href ("#" ++ name) ] [ text name ] ]


viewLocation : Nav.Location -> Html msg
viewLocation location =
    li [] [ text (location.pathname ++ location.hash) ]







-- PORTS


port getAllProfiles : () -> Cmd msg


port receiveNames : (List Profile -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveNames Profiles
