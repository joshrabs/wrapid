module Client.PAPortal.Pages.LiveMonitor exposing (..)

import Html exposing (Html, div, text, input, button, img, span)
import Html.Attributes exposing (style, src, placeholder)
import Html.Events exposing (onClick, onInput)

import Regex exposing (contains)

import Client.Generic.Dashboard.Dashboard as Dashboard exposing (makePanel)
import Assets.Icons.SearchIcon exposing (viewSearchIcon)
import Material
import Material.Icon as Icon
import Material.Textfield as Textfield
import Material.Options as Options

import Client.ExtraPortal.Types exposing (ScheduleItem)

type alias Profile =
    { id : String
    , firstName : String
    , lastName: String
    , avatarSrc : Maybe String
    }


type alias Model =
    { isAddingTask: Bool
    , mdl : Material.Model
    , tableFilter: Filter
    }


type alias LiveExtraTable =
    List ExtraInfoItem


type alias ExtraInfoItem =
    { firstName : String
    , lastName: String
    , imgSrc : Maybe String
    , isClockedIn : Bool
    }


type alias ExtrasSnapStatModel =
    { totalExtras : Int
    , clockedIn : Int
    , holdClothes : Int
    , missingForms : Int
    }


fakeSnapStateModel : ExtrasSnapStatModel
fakeSnapStateModel =
    { totalExtras = 3
    , clockedIn = 1
    , holdClothes = 2
    , missingForms = 2
    }


fakeImg =
    "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"


type alias Filter = String
liveTable: Maybe (List Profile) -> Filter -> LiveExtraTable
liveTable profs tableFilter =
  case profs of
    Just extras ->
      extras
        |> tableFilterMatch tableFilter
        |> (List.map (\prof ->
              {firstName=prof.firstName, lastName=prof.lastName, imgSrc=prof.avatarSrc, isClockedIn=True}
            ))
    Nothing -> []


tableFilterMatch: Filter -> List Profile -> List Profile
tableFilterMatch tFilter profs =
  let
      sanProfs prof =
            (prof.firstName ++ prof.lastName) |> String.toLower |> String.trim

      sanFilter =
        tFilter |> String.toLower |> String.split " " |> String.concat
  in
      List.filter (\prof -> Regex.contains (Regex.regex sanFilter) (sanProfs prof)) profs

initModel : Material.Model -> Model
initModel mdlModel =
    { isAddingTask = False
    , mdl = mdlModel
    , tableFilter = ".*"
    }



--UPDATE
type Msg =
    Mdl (Material.Msg Msg)
  | ViewAddingTask
  | SubmitTaskByRole ScheduleItem
  | SetTableFilter Filter

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Mdl msg_ ->
        Material.update Mdl msg_ model

    ViewAddingTask ->
      ({model | isAddingTask = True}, Cmd.none)

    SubmitTaskByRole scheduleItem ->
      (model, Cmd.none)

    SetTableFilter tFilter ->
      ({model | tableFilter = tFilter}, Cmd.none)

--VIEW


viewLiveMonitor : Model -> Maybe (List Profile) -> Html Msg
viewLiveMonitor model extraProfiles =
    div [style [("margin", "8px 4px 8px 4px")]]
        [ viewExtrasSnapStats fakeSnapStateModel
        , viewLiveTable (liveTable extraProfiles model.tableFilter) model.mdl model.isAddingTask

        ]


viewLiveTable : LiveExtraTable -> Material.Model -> Bool -> Html Msg
viewLiveTable table mdlModel isAddingTask =
    let
        panelHeader =
            Just { title = "Extras", rightItem = Nothing }

        panelBody =
            div [style [("margin", "8px")]]
                [ viewSearchTaskBar mdlModel
                , if isAddingTask then viewTaskPanel else div [] []
                , (viewLiveTableItems table)
                ]

        footer =
            Nothing
    in
        Dashboard.makePanel panelHeader panelBody footer


viewTaskPanel: Html Msg
viewTaskPanel =
  div [ style [
        ( "display", "flex" )
        , ("align-items", "center")
        , ( "justify-content", "space-between" )
        , ( "padding", "8px 4px 8px 4px" )
        , ("height", "74px")
        , ("background", "yellow")
      ]]
      [div [] [input [placeholder "Role"] []]
      ,div []
        [input [placeholder "Start Tm"] []
        ,input [placeholder "End Tm"] []
        ]
      , button [onClick (SubmitTaskByRole defaultScheduleItem)] [text "Submit!"]
      ]

defaultScheduleItem: ScheduleItem
defaultScheduleItem =
  {startTm = "5:30 PM"
  ,category ="Shoot"
  ,endTm = Nothing
  ,name="Zombie shot"
  }

viewSearchTaskBar: Material.Model -> Html Msg
viewSearchTaskBar mdlModel =
  div [style
    [
      ("display", "flex")
    , ("justify-content", "space-between")
    , ("align-items", "center")
    , ("height", "71px")
    , ("width", "100%")
    , ("background", "#FFFFFF")
    , ("border-bottom", "1px solid #EFF3F7")
    ]]
    [
        viewSearch mdlModel
      , div [onClick ViewAddingTask,
          style [
            ("display", "flex")
            ,("justify-content", "center")
            ,("align-items", "center")
            ,("background", "#FFFFFF")
            ,("box-shadow", "0 2px 4px 0 rgba(155,158,167,0.50)")
            ,("border-radius", "2px")
            ,("font-family", "Roboto-Regular")
            ,("font-size", "12px")
            ,("color", "#0000FF")
            ,("width", "72px")
            ,("height", "32px")
            ,("margin", "8px")
            ,("letter-spacing" , "0")
        ]]
        [text "Schedule"]
    ]



viewSearch : Material.Model -> Html Msg
viewSearch mdlModel =
  div [style [("margin-left", "16px")]]
  [ div [style [("display", "flex"), ("align-items", "center")]]
      [
        span [style [("margin-top", "4px")]] [viewSearchIcon]
       ,span [style [("margin-left", "4px"), ("width", "170px")]]
        [Textfield.render Mdl
             [ 3, 0 ]
             mdlModel
             [ Textfield.label "Search"
             , Textfield.floatingLabel
             , Options.onInput SetTableFilter
             ]
             []
        ]
      ]

  ]

viewLiveTableItems items =
    div [style [("max-height", "580px"), ("overflow-y", "scroll")]]
    (List.map (\item -> viewLiveTableItem item) items)

getImgSrc: Maybe String -> String
getImgSrc imgSrc =
  case imgSrc of
    Just src -> src
    Nothing -> fakeImg

viewLiveTableItem : ExtraInfoItem -> Html msg
viewLiveTableItem item =
    div [ style [
          ( "display", "flex" )
          , ("align-items", "center")
          , ( "justify-content", "space-between" )
          , ( "padding", "8px 4px 8px 4px" )
        ]]
        [ div [ style [
            ( "display", "flex" )
            ,( "align-items", "center" )
            , ("margin", "8px 8px 8px 8px")
            ] ]
            [ img
                  [ src (getImgSrc item.imgSrc)
                  , style
                      [ ( "border-radius", "50%" )
                      , ( "height", "36px" )
                      ]
                  ]
                  []
            , div [style [
                ("display", "flex")
                , ("flex-direction", "column")
                , ("justify-content", "center")
                ,("margin", "4px 8px 2px 8px")
                ,("padding", "4px")
              ]]
              [
                span [style [
                  ("font-size", "14px")
                  ,("font-family", "Roboto-Regular")
                  ,("color", "#282C35")
                ]]
                [text (item.firstName ++ " " ++ item.lastName)]
                , span [style [
                    ("font-size", "12px")
                    ,("font-family", "Roboto-Regular")
                    ,("color", "#9B9EA7")
                    ,("margin", "2px 0px 2px 0px")
                  ]]
                  [text "Extra"]
              ]
            ]
          ,div [style [
              ("margin-right", "8px")
              ,("display", "flex")
            ]]
            [span [style [("margin", "0px 8px 0px 8px")]] [Icon.view "watch_later" [Options.css "color" "rgb(255, 93, 0)"]]
            ,span [] [Icon.i "loyalty"]
            ]
        ]


viewExtrasSnapStats : ExtrasSnapStatModel -> Html msg
viewExtrasSnapStats model =
    let
        totalExtras =
            (model.totalExtras |> toString)

        clockRatio =
            (model.clockedIn |> toString) ++ " / " ++ totalExtras

        clothingRatio =
            (model.holdClothes |> toString) ++ " / " ++ totalExtras

        formRatio =
            (model.missingForms |> toString) ++ " / " ++ totalExtras

        allIcons =
          [{num=clockRatio, text ="Clocked In"}
          , {num=clothingRatio, text ="Hold Clothes"}
          , {num=formRatio, text ="Missing Forms"}
          ]
    in
        div [ style [ ( "display", "flex" ) ] ]
          (List.map
            (\r ->
              div [style [
                  ("display", "flex")
                  , ("flex-direction", "column")
                  , ("padding-right", "24px")
                  , ("border-right", "1px solid #EFF3F7")
                ]]
                [ span [style snapRatioStyle] [text r.num]
                , span [style snapTextStyle] [text r.text]
                ]
            )
            allIcons)


snapRatioStyle: List (String, String)
snapRatioStyle =
  [("color", "#50E3C2")
  ,("font-family", "Roboto-Regular")
  ,("font-size", "20px")
  ]

snapTextStyle: List (String, String)
snapTextStyle =
  [("color", "#282C35")
  ,("font-family", "Roboto-Regular")
  ,("font-size", "12px")
  ]

snapStatStyle: String -> List (String, String)
snapStatStyle color =
  [
    ("background-image", "radial-gradient(0% 50%, #9DFFF3 1%, #65F9DD 100%)")
    ,("box-shadow", "0 8px 30px 0 rgba(0,0,0,0.04)")
    ,("margin", "0px 8px 0px 8px")
  ]
