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
  , currentView: ViewState
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
  , currentView = LiveMonitor
  }

type ViewState = LiveMonitor | SkinManager

-- UPDATE
type Msg =
    ChangeView ViewState
  | GetAllProfiles
  | Profiles (List Profile)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ChangeView view ->
      ({model | currentView = view}, Cmd.none)
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
        , viewHeader model.currentView
        , viewCalendar Nothing
        ,
          case model.currentView of
            LiveMonitor ->
              div []
              [
                button [ onClick GetAllProfiles ] [ text "Get All Profiles" ]
              , case model.extras of
                  Just extras ->
                    ul [] (List.map viewExtras extras)
                  Nothing -> div [] [text "No extras!"]
              ]
            SkinManager ->
              div []
              [
                text "Skin manager!"
              ]
        ]

viewHeader: ViewState -> Html Msg
viewHeader currentView =
  let
      getTabStyle tabType =
        if currentView == tabType then selectedTabStyle
          else baseTabStyle
  in
    div [style [("background-color", "#FFFFFF")]]
    [
      viewHeaderInfo
      , div [style [("display", "flex"), ("border-top", "1px solid #EBF0F5")]]
        [
          div [onClick (ChangeView LiveMonitor)
            , style (getTabStyle LiveMonitor)]
            [text "Live Monitor"]
          ,div [
              onClick (ChangeView SkinManager)
              , style (getTabStyle SkinManager)
            ]
            [text "Skin Manager"]
        ]
    ]


baseTabStyle = [
  ("padding", "8px"), ("font-size", "14px"),("font-family", "Roboto-Regular")
    ,("font-family", "Roboto-Regular")
    ,("color", "#6D717A")]

selectedTabStyle = List.concat
    [baseTabStyle,
      [("border-bottom", "2px solid black")
      ,("color", "black")
      ,("font-family", "Roboto-Medium")
      ]
    ]

viewHeaderInfo: Html msg
viewHeaderInfo =
  div [style []]
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
