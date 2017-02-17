module Client.ExtraPortal.ExtraPortal exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Client.Generic.Dashboard.Dashboard as Dashboard exposing (..)
import Client.ExtraPortal.ExtraWardrobeStatus exposing (..)
import Client.ExtraPortal.NotificationBar exposing (..)
import Client.ExtraPortal.Schedule exposing (..)

import Client.ExtraPortal.Pages.FormStatusPage as FormStatusPage exposing (viewFormStatusPage)
import Client.ExtraPortal.Pages.ProfileWizard as Wizard
import Client.ExtraPortal.Pages.DailyMonitor as DailyMonitor exposing (viewDailyMonitor)

import Client.ExtraPortal.Types exposing (..)

-- MODEL

type alias Model =
    { currentView: ViewState
    , wizardModel : Wizard.Model
    , extraInfo: ExtraInfo
    }



initModel: Model
initModel =
    { currentView = DailyMonitor
    , wizardModel = Wizard.init
    , extraInfo = {timecard = defaultTimeCard}
    }


type ViewState = ProfileWizard | FormStatus | DailyMonitor

-- UPDATE

type Msg =
      NoOp
    | ChangeView ViewState
    | WizardMsg Wizard.Msg
    | DailyMonitorMsg DailyMonitor.Msg

update: Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        ChangeView viewState ->
            ({model | currentView = viewState}, Cmd.none)

        WizardMsg subMsg ->
            let
                ( updatedWizardModel, wizardCmd ) =
                    Wizard.update subMsg model.wizardModel
            in
                ( { model | wizardModel = updatedWizardModel }
                , Cmd.none
                )
        DailyMonitorMsg dmMsg ->
              let
                  (updatedDailyMonitorModel, dmCmd) =
                    DailyMonitor.update dmMsg {timecard = model.extraInfo.timecard}
              in
                  ( { model | extraInfo = {timecard = updatedDailyMonitorModel.timecard} }
                  , Cmd.none
                  )
        NoOp -> (model, Cmd.none)





--VIEW
viewExtraPortal: Model -> Html Msg
viewExtraPortal model =
  div []
      [
       div [style [("margin-bottom", "8px"), ("background-color", "orange"), ("display", "inline-flex")]]
           [
            button [onClick (ChangeView ProfileWizard)] [text "Profile Wizard"]
           ,button [onClick (ChangeView FormStatus)] [text "Form Status"]
           ,button [onClick (ChangeView DailyMonitor)] [text "DailyMonitor"]
           ]
      , let
            rightItems = {avatar = Just defaultUrl}
       in
           Dashboard.view {navbar = {rightItems = Just rightItems}}
      , case model.currentView of
            DailyMonitor ->
              let
                  dmModel = {timecard = model.extraInfo.timecard}
              in
                Html.map DailyMonitorMsg (viewDailyMonitor dmModel)

            ProfileWizard ->
                div []
                    [ Html.map WizardMsg (Wizard.view model.wizardModel) ]

            FormStatus ->
                viewFormStatusPage (ChangeView DailyMonitor) defaultFormStatus

    ]


type alias Header = {firstName: String, production: String}
viewHeader: Header -> Html Msg
viewHeader header =
  div [style [("display", "flex"), ("flex-direction", "column"),("margin", "28px 4px 16px 16px")]]
  [
     span [style [
      ("font-family", "Roboto-Regular")
      ,("font-size", "12px")
      ,("color", "#6D717A")
      ,("letter-spacing", "0")
      ,("line-height", "20px")]] [text "Monday May 25th, 2017"]
    , span [style headerTitleStyle] [text ("Welcome " ++ header.firstName)]
    ,span [style [
        ("font-family", "Roboto-Regular")
        ,("font-size", "16px")
        ,("color", "#282C35")
        ,("letter-spacing", "0")
        ,("line-height", "20px")]]
      [text (header.production)]
  ]
headerTitleStyle : List ( String, String )
headerTitleStyle =
  [
  ("font-family", "Roboto-Bold")
  ,("font-size", "32px")
  ,("color", "#282C35")
  ,("margin", "4px 0px 4px 0px")
  ,("letter-spacing", "0")
  ]

headerProductionStyle : List ( String, String )
headerProductionStyle =
  [
  ("font-family", "Roboto-Regular")
  ,("font-size", "16px")
  ,("color", "#282C35")
  ,("letter-spacing", "0")
  ,("line-height", "20px")
  ]



type alias CrewInfoItem = {name: String, role: String}
viewCrewInfoItems: List CrewInfoItem -> Html Msg
viewCrewInfoItems prodContacts =
  div []
  [
    let
      listItems = List.map (\s -> (
          div [style [("display", "flex"), ("flex-direction", "column"), ("margin-left", "8px")]]
          [
            span [style [
               ("font-family", "Roboto-Medium")
              ,("font-size", "12px")
              ,("color", "#282C35")
              ,("letter-spacing", "0")
              ,("line-height", "24px")
            ]] [text s.name]
           , span [style [
              ("font-family", "Roboto")
             ,("font-size", "16px")
             ,("color", "#6D717A")
             ,("letter-spacing", "0")
             ,("line-height", "24px")
           ]][text s.role]
          ]
      )) prodContacts
    in
      div [style [("display", "flex"), ("flex-direction", "column")]] listItems
  ]



--SAMPLE data


defaultTimeCard: TimeCard
defaultTimeCard = {clockinTs = Nothing, clockoutTs = Nothing}

defaultUrl: Maybe String
defaultUrl = Just "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"

defaultNotificationItems: List NotificationBarItem
defaultNotificationItems =
  [
    {description="Lunch in 1 Hour", icon=LunchIcon, startTm="12:00 PM", endTm="1:00 PM"}
    ,{description="Shoot Zombie Set", icon=Default, startTm="4:00 PM", endTm="4:30 PM"}
  ]
defaultCrewInfoItems: List { name : String, role : String }
defaultCrewInfoItems =
  [{name = "Josh Weinberg", role="Lead PA"}
  ,{name = "Randy Lahey", role="Extra PA"}
  ,{name = "Patty Lebotomy", role="Wardrobe"}
  ]

defaultScheduleItems: Schedule
defaultScheduleItems =
  [
    {name="Start Time", startTm="8:00 AM"}
    ,{name="Break for Lunch", startTm="12:00 PM"}
    ,{name="Estimated End Time", startTm="6:00 PM"}
  ]

defaultFormStatus: FormStatusPage.FormStatuses
defaultFormStatus =
  [{formName = "Pence", completedDt = "11/12/2017", completedTs="8:00 AM", imgSrc = "meow"}
  ,{formName = "Emergency Contact", completedDt = "11/12/2017", completedTs="8:00 AM", imgSrc = "meow"}
  ,{formName = "EFS Voucher", completedDt = "11/12/2017", completedTs="8:00 AM", imgSrc = "meow"}
  ]
