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
import Client.PAPortal.Types exposing (..)


fakeSnapStateModel : ExtrasSnapStatModel
fakeSnapStateModel =
    { totalExtras = 3
    , clockedIn = 1
    , holdClothes = 2
    , missingForms = 2
    }


fakeImg =
    "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"


liveTable: AllLiveExtraInfo -> LiveTableFilter -> LiveExtraTable
liveTable extraInfo tableFilter =
    extraInfo
      |> tableFilterMatch tableFilter
      |> (List.map (\extra ->
            {firstName=extra.info.firstName
            , lastName=extra.info.lastName
            , part = extra.info.role
            , imgSrc=extra.info.avatar.url
            , isClockedIn=True
            }
          ))

tableFilterMatch: LiveTableFilter -> AllLiveExtraInfo -> AllLiveExtraInfo
tableFilterMatch tFilter extras =
  let
      sanProfs prof =
            (prof.firstName ++ prof.lastName) |> String.toLower |> String.trim

      sanFilter =
        tFilter |> String.toLower |> String.split " " |> String.concat
  in
      extras
        |> List.filter (\extra -> Regex.contains (Regex.regex sanFilter) (sanProfs extra.info))

initState : Material.Model -> LiveMonitorState
initState mdlModel =
    { isAddingTask = False
    , mdl = mdlModel
    , tableFilter = ".*"
    , roleScheduler = defaultItemScheduler
    }

type alias AllLiveExtraInfo = List LiveExtraInfo
type alias LiveExtraInfo =
  {extraId: String
  ,activity: Maybe ExtraActivity
  ,info: ExtraInfo
  }

getLiveExtraInfo: List ExtraActivity -> List ExtraInfo -> AllLiveExtraInfo
getLiveExtraInfo activity info =
  info
    |> List.map (mergeActivityItem activity)


mergeActivityItem: List ExtraActivity -> ExtraInfo -> LiveExtraInfo
mergeActivityItem activity info =
  let
      matchingRecord =
        activity
          |> List.filter (\ai -> ai.extraId == info.extraId)
          |> List.head
  in
      case matchingRecord of
        Just r ->
          {extraId = info.extraId, info=info, activity = Just r}
        Nothing ->
          {extraId = info.extraId, info=info, activity = Nothing}


defaultItemScheduler: RoleScheduler
defaultItemScheduler =
  {role = ""
  ,scheduleItem = {startTm = "", endTm = Nothing, category="", name=""}
  }

--UPDATE


setSchedulerRole: LiveMonitorState -> String -> LiveMonitorState
setSchedulerRole model role =
  let
    oldScheduler = model.roleScheduler
    newScheduler = {oldScheduler | role = role}
  in
    ({model | roleScheduler = newScheduler})

setSchedulerTime: LiveMonitorState -> ScheduleTimeParam -> String -> LiveMonitorState
setSchedulerTime model param time =
  let
    oldScheduler = model.roleScheduler
    oldItem = oldScheduler.scheduleItem
    newItem =
      case param of
        StartTm -> {oldItem | startTm = time}
        EndTm -> {oldItem | endTm = Just time}
    newSchedulerItem = {oldScheduler | scheduleItem = newItem}
  in
    ({model | roleScheduler = newSchedulerItem})

setSchedulerCategory: LiveMonitorState -> String -> LiveMonitorState
setSchedulerCategory model category =
  let
    oldScheduler = model.roleScheduler
    oldItem = oldScheduler.scheduleItem
    newItem = {oldItem | category = category}
    newSchedulerItem = {oldScheduler | scheduleItem = newItem}
  in
    ({model | roleScheduler = newSchedulerItem})

setSchedulerName: LiveMonitorState -> String -> LiveMonitorState
setSchedulerName model name =
  let
    oldScheduler = model.roleScheduler
    oldItem = oldScheduler.scheduleItem
    newItem = {oldItem | name = name}
    newRoleScheduler = {oldScheduler | scheduleItem = newItem}
  in
    ({model | roleScheduler = newRoleScheduler})

update: LiveMonitorMsg -> LiveMonitorState -> (LiveMonitorState, Cmd LiveMonitorMsg)
update msg model =
  case msg of
    Mdl msg_ ->
        Material.update Mdl msg_ model

    ToggleAddingTask ->
      ({model | isAddingTask = not model.isAddingTask}, Cmd.none)

    SetSchedulerRole role ->
      (setSchedulerRole model role, Cmd.none)

    SetSchedulerTime timeType time ->
      (setSchedulerTime model timeType time, Cmd.none)

    SetSchedulerCategory category ->
      (setSchedulerCategory model category, Cmd.none)

    SetSchedulerName name ->
      (setSchedulerName model name, Cmd.none)

    SubmitTaskByRole scheduleItem ->
      (model, Cmd.none)

    SetTableFilter tFilter ->
      ({model | tableFilter = tFilter}, Cmd.none)

--VIEW


viewLiveMonitor : LiveMonitorState -> List ExtraActivity -> List ExtraInfo -> Html LiveMonitorMsg
viewLiveMonitor model extraActivity extraInfo =
    div [style [("margin", "8px 4px 8px 4px")]]
        [ viewExtrasSnapStats fakeSnapStateModel
        , viewLiveTable (liveTable (getLiveExtraInfo extraActivity extraInfo) model.tableFilter) model.mdl model.isAddingTask

        ]


viewLiveTable : LiveExtraTable -> Material.Model -> Bool -> Html LiveMonitorMsg
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


viewTaskPanel: Html LiveMonitorMsg
viewTaskPanel =
  div [ style [
        ( "display", "flex" )
        , ("align-items", "center")
        , ( "justify-content", "space-between" )
        , ( "padding", "8px 4px 8px 4px" )
        , ("height", "74px")
        , ("background", "yellow")
      ]]
      [div []
        [ input [onInput SetSchedulerRole, placeholder "Role"] []
        , input [onInput SetSchedulerCategory, placeholder "Category"] []
        , input [onInput SetSchedulerName, placeholder "Name"] []
        ]
      ,div []
        [input [onInput (SetSchedulerTime StartTm), placeholder "Start Tm"] []
        ,input [onInput (SetSchedulerTime EndTm), placeholder "End Tm"] []
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

viewSearchTaskBar: Material.Model -> Html LiveMonitorMsg
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
      , div [onClick ToggleAddingTask,
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



viewSearch : Material.Model -> Html LiveMonitorMsg
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
                      , ( "width", "36px" )
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
                  [text item.part]
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
          [{num=clockRatio, text ="Clocked In", background = "rgb(0,150,136)", icon="watch_later"}
          , {num=clothingRatio, text ="Hold Clothes", background="#ff5252", icon="loyalty"}
          , {num=formRatio, text ="Missing Forms", background="#ab47bc", icon="assignment"}
          ]
    in
        div [ style [ ( "display", "flex" ) ] ]
          (List.map
            (\r ->
              div [style [
                  ("display", "flex")
                  , ("flex-direction", "column")
                  , ("justify-content", "center")
                  , ("flex", "1")
                  , ("align-items", "center")
                  , ("padding", "24px")
                  , ("border-right", "1px solid #EFF3F7")
                  , ("background", r.background)
                ]]
                [ span [] [Icon.view r.icon [Options.css "color" "white"]]
                , span [style snapRatioStyle] [text r.num]
                , span [style snapTextStyle] [text r.text]

                ]
            )
            allIcons)


snapRatioStyle: List (String, String)
snapRatioStyle =
  [("color", "white")
  ,("font-family", "Roboto-Bold")
  ,("font-size", "28px")
  ,("margin", "8px 0px 8px 0px")
  ]

snapTextStyle: List (String, String)
snapTextStyle =
  [("color", "white")
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
