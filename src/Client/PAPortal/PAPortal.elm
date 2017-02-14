port module Client.PAPortal.PAPortal exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)

import Client.Generic.Dashboard.Dashboard as Dashboard exposing (..)
import Client.PAPortal.HorizontalCalendar exposing (..)

-- MODEL

type alias Model =
  {
    user: Profile
  , extras: Maybe (List Profile)
  }

type alias Profile =
    { id : String
    , firstName : String
    , url : Maybe String
    }

initModel: String -> Model
initModel userId =
  let
    user =
      {id=userId
      , firstName="Jeff"
      , url=Just "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"
      }
  in
  { user = user
  , extras = Nothing
  }

type ViewState = ProfileWizard | FormStatus

-- UPDATE
type Msg =
    ChangeView ViewState
  | GetAllProfiles
  | Profiles (List Profile)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ChangeView view ->
      (model, Cmd.none)
    GetAllProfiles ->
      let _ = Debug.log "Running get all profiles!"
      in
        ( {model | user = {id=model.user.id, firstName="Bob", url=model.user.url}}
        , getAllProfiles ()
        )

    Profiles list ->
        ( { model | extras = Just list }
        , Cmd.none
        )



--VIEW
viewPAPortal: Model -> Html Msg
viewPAPortal model =
    div []
        [
          let
              rightItems = {avatar = Just model.user.url}
          in
            Dashboard.view {navbar = {rightItems = Just rightItems}}
        , viewHeader
        , viewCalendar Nothing
        , button [ onClick GetAllProfiles ] [ text "Get All Profiles" ]
        , case model.extras of
            Just extras ->
              ul [] (List.map viewExtras extras)
            Nothing -> div [] [text "No extras!"]
        ]

viewHeader: Html msg
viewHeader =
  viewHeaderInfo


viewHeaderInfo: Html msg
viewHeaderInfo =
  div [style [("background-color", "#FFFFFF")]]
  [
    div [style [
        ("margin", "16px")
      , ("display", "inline-flex")
      , ("flex-direction", "column")
    ]]
    [
       span [style [
        ("font-family", "Roboto-Regular")
        ,("font-size", "12px")
        ,("color", "#6D717A")
       ]]
      [text "Monday May 25, 2016"]
      ,span [style [
        ("font-family", "Roboto-Regular")
        ,("font-size", "16px")
        ,("margin", "8px 0px 8px 0px")
        ,("color", "#282C35")
        ,("letter-spacing", "0")
        ]]
        [text "RUNABETTERSET Productions"]
    ]
  ]


viewExtras : Profile -> Html Msg
viewExtras profile =
        li  []
            [ text profile.firstName ]


-- PORTS


port getAllProfiles : () -> Cmd msg

port receiveNames : (List Profile -> msg) -> Sub msg

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveNames Profiles
