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

import Client.ExtraPortal.Types exposing (ScheduleItem, TimeCard)
import Client.PAPortal.Types exposing (..)



defaultProfile =
    "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"


liveTable: AllLiveExtraInfo -> LiveTableFilter -> LiveExtraTable
liveTable extraInfo tableFilter =
    extraInfo
      |> tableFilterMatch tableFilter
      |> (List.map (\extra ->
            {firstName=extra.firstName
            , lastName=extra.lastName
            , part = extra.role
            , imgSrc=extra.avatar.url
            , isClockedIn=isClockedIn extra.timecard
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
        |> List.filter (\extra -> Regex.contains (Regex.regex sanFilter) (sanProfs extra))

initState : Material.Model -> LiveMonitorState
initState mdlModel =
    { isAddingTask = False
    , mdl = mdlModel
    , tableFilter = ".*"
    , roleScheduler = defaultItemScheduler
    }

type alias AllLiveExtraInfo = List ExtraInfo
-- type alias AllLiveExtraInfo = List LiveExtraInfo
-- type alias LiveExtraInfo =
--   {extraId: String
--   -- ,activity: Maybe ExtraActivity
--   ,info: ExtraInfo
--   }

getLiveExtraInfo: List ExtraInfo -> AllLiveExtraInfo
getLiveExtraInfo info =
  info
    -- |> List.map (mergeActivityItem activity)


-- mergeActivityItem: List ExtraActivity -> ExtraInfo -> LiveExtraInfo
-- mergeActivityItem activity info =
--   let
--       matchingRecord =
--         activity
--           |> List.filter (\ai -> ai.extraInfo.extraId == info.extraId)
--           |> List.head
--
--       z = Debug.log "MATCHING RECORD!" info
--   in
--       case matchingRecord of
--         Just r ->
--           {extraId = info.extraId, info=info, activity = Just r}
--         Nothing ->
--           {extraId = info.extraId, info=info, activity = Nothing}


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
getSnapStatFromInfo: List ExtraInfo -> ExtrasSnapStatModel
getSnapStatFromInfo info =
  let
      totalExtras = List.length info
      clockedInQt =
        info
          |> List.map (\ei -> ei.timecard)
          |> List.filter isClockedIn
          |> List.length
      holdingClothes = 0
      missingForms = 0
  in
        { totalExtras = totalExtras
        , clockedIn = clockedInQt
        , holdClothes = holdingClothes
        , missingForms = missingForms
        }


isClockedIn: TimeCard -> Bool
isClockedIn timecard =
  case timecard.clockinTs of
    Just clockin -> True
    Nothing -> False

viewLiveMonitor : LiveMonitorState -> List ExtraInfo -> Html LiveMonitorMsg
viewLiveMonitor model extraInfo =
    div [style [("margin", "8px 4px 8px 4px")]]
        [ viewExtrasSnapStats (getSnapStatFromInfo extraInfo)
        , viewLiveTable (liveTable (getLiveExtraInfo extraInfo) model.tableFilter) model.mdl model.isAddingTask

        ]


viewLiveTable : LiveExtraTable -> Material.Model -> Bool -> Html LiveMonitorMsg
viewLiveTable table mdlModel isAddingTask =
    let
        panelHeader =
            Just { title = "Extras", rightItem = Nothing }

        panelBody =
            div [style [("margin", "8px")]]
                [ viewSearchTaskBar mdlModel
                , (viewLiveTableItems table)
                ]

        footer =
            Nothing
    in
        Dashboard.makePanel panelHeader panelBody footer



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
    Nothing -> defaultProfile

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
            [span [style [("margin", "0px 8px 0px 8px")]]
                [Icon.view "watch_later"
                  [Options.css "color"
                    (if item.isClockedIn then validIconColor else badIconColor)
                ]]
            ,span [style [("margin", "0px 8px 0px 8px")]] [Icon.view "loyalty" [Options.css "color" badIconColor]]
            ,span [style [("margin", "0px 8px 0px 8px")]] [Icon.view "assignment" [Options.css "color" badIconColor]]
            ]
        ]

validIconColor = "#00e676"
badIconColor = "#ff3d00"

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
          [{num=clockRatio, text1 ="Clocked", text2 = "In", background = "rgb(0,150,136)", icon="watch_later"}
          , {num=clothingRatio, text1 ="Holding", text2="Clothes", background="#ff5252", icon="loyalty"}
          , {num=formRatio, text1 ="Missing", text2="Forms", background="#ab47bc", icon="assignment"}
          ]
    in
        div [ style [ ( "display", "flex" ) ] ]
          (List.map
            (\r ->
              div [style [
                  ("display", "flex")
                  , ("flex-direction", "column")
                  , ("justify-content", "center")
                  , ("align-items", "center")
                  , ("flex", "1")
                  , ("align-items", "center")
                  , ("padding", "16px")
                  , ("border-right", "1px solid #EFF3F7")
                  , ("background", r.background)
                ]]
                [ span [] [Icon.view r.icon [Options.css "color" "white"]]
                , span [style snapRatioStyle] [text r.num]
                , span [style snapTextStyle] [text r.text1]
                , span [style snapTextStyle] [text r.text2]
                ]
            )
            allIcons)


snapRatioStyle: List (String, String)
snapRatioStyle =
  [("color", "white")
  ,("font-family", "Roboto-Bold")
  ,("font-size", "28px")
  ,("margin", "12px 0px 12px 0px")
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
