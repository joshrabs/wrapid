port module Main exposing (main)

import Html exposing (Html, a, button, div, h1, img, li, p, text, ul)
import Html.Attributes exposing (href, src)
import Html.Events exposing (onClick)
import Navigation as Nav
import List.Extra exposing (find)
import Maybe exposing (andThen)

import Client.Generic.Authentication.Login.Login as Login exposing (loginView, Msg)
import Client.Generic.Dashboard.Dashboard as Dashboard exposing (..)


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
    , currentView : ViewStates
    }

type alias Url =
    String


type alias Profile =
    { id : String
    , firstName : String
    , url : Maybe String
    }

type ViewStates = LoginView | ExtraPortalView

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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


-- VIEW

view : Model -> Html Msg
view model =
    case model.currentView of
      LoginView -> Html.map LoginMsg (Login.loginView Nothing)
      ExtraPortalView ->
        div []
            [
              let
                  rightItems = {avatar = Just model.currentImg}
              in
                Dashboard.view {navbar = {rightItems = Just rightItems}}
            , ul [] (List.map viewLink [ "bears", "cats", "dogs", "elephants", "fish" ])
            , h1 [] [ text "History" ]
            , ul [] (List.map viewLocation model.history)
            , h1 [] [ text "Data" ]
            , button [ onClick GetAllProfiles ] [ text "Get All Profiles" ]
            , ul [] (List.map viewProfile model.profiles)
            ]


viewLink : String -> Html msg
viewLink name =
    li [] [ a [ href ("#" ++ name) ] [ text name ] ]


viewLocation : Nav.Location -> Html msg
viewLocation location =
    li [] [ text (location.pathname ++ location.hash) ]


viewProfile : Profile -> Html Msg
viewProfile profile =
    li
        [ onClick (ShowAvatar profile.id) ]
        [ text profile.firstName ]






-- PORTS


port getAllProfiles : () -> Cmd msg


port receiveNames : (List Profile -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveNames Profiles
