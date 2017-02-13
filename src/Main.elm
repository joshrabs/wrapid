port module Main exposing (main)

import Html exposing (Html, a, button, div, h1, h4, img, li, p, text, ul)
import Html.Attributes exposing (href, src, style)
import Html.Events exposing (onClick)
import Navigation as Nav
import Maybe exposing (andThen)

import Client.Generic.Authentication.Login.Login as Login exposing (loginView, Msg)
import Client.ExtraPortal.ExtraPortal as ExtraPortal exposing (..)
import Client.PAPortal.PAPortal as PAPortal exposing (..)

import Client.Generic.Animation.FadeInUp as FadeInUp exposing (..)

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
    , currentImg : Maybe String
    , currentViewState : ViewState
    , extraPortalModel: ExtraPortal.Model
    , paPortalModel: PAPortal.Model
    , title: String
    }

type ViewState = LoginView | ExtraPortalView | PAPortalView


type alias Url =
    String

type alias Profile =
    { id : String
    , firstName : String
    , url : Maybe String
    }




init : Nav.Location -> ( Model, Cmd Msg )
init location =
    ( Model [ location ] Nothing LoginView ExtraPortal.initModel (PAPortal.initModel "word") "Yo"
    , Cmd.none
    )

-- UPDATE


type Msg
    = NoOp
    | UrlChange Nav.Location
    | LoginMsg Login.Msg
    | ChangeView ViewState
    | ExtraPortalMsg ExtraPortal.Msg
    | PAPortalMsg PAPortal.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        ChangeView viewState ->
          case viewState of
            LoginView ->
              ({model | currentViewState=viewState}, Cmd.none)
            ExtraPortalView ->
              ({model | extraPortalModel = ExtraPortal.initModel, currentViewState=viewState}, Cmd.none)
            PAPortalView ->
              ({model | currentViewState=viewState}, Cmd.none)
        UrlChange location ->
            ( { model | history = location :: model.history }
            , Cmd.none
            )
        LoginMsg loginMsg -> (model, Cmd.none)
        ExtraPortalMsg epMsg ->
          let
            (extraPortalModel, epCmd) = ExtraPortal.update epMsg model.extraPortalModel
          in
            ({model | extraPortalModel = extraPortalModel}, Cmd.map (\b -> ExtraPortalMsg b) epCmd)
        PAPortalMsg paMsg ->
          let
            (paPortalModel, paCmd) = PAPortal.update paMsg model.paPortalModel
          in
            ({model | paPortalModel = paPortalModel}, Cmd.map (\b -> PAPortalMsg b) paCmd)


view : Model -> Html Msg
view model =
    div []
    [
      case model.currentViewState of
          LoginView -> Html.map LoginMsg (Login.loginView (Login.initModel Nothing Nothing))
          ExtraPortalView -> Html.map ExtraPortalMsg (ExtraPortal.viewExtraPortal model.extraPortalModel)
          PAPortalView -> Html.map PAPortalMsg
                  (PAPortal.viewPAPortal model.paPortalModel)
      , div [style [("position", "fixed"), ("top", "0px"), ("right", "0px"), ("border", "1px solid black")]]
      [button [onClick (ChangeView ExtraPortalView)] [text "Extra Portal"]
      , button [onClick (ChangeView PAPortalView)] [text "PA Portal"]
      , button [onClick (ChangeView LoginView)] [text "Login"]
      ]
      , Html.map (\_ -> NoOp) (FadeInUp.view (Tuple.first FadeInUp.init))
    ]



viewLink : String -> Html msg
viewLink name =
    li [] [ a [ href ("#" ++ name) ] [ text name ] ]


viewLocation : Nav.Location -> Html msg
viewLocation location =
    li [] [ text (location.pathname ++ location.hash) ]



-- PORTS


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [
      Sub.map (\pas -> PAPortalMsg pas) (PAPortal.subscriptions model.paPortalModel)
    ]
