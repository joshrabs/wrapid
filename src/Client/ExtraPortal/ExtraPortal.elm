port module Client.ExtraPortal.ExtraPortal exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Client.Generic.Dashboard.Dashboard as Dashboard exposing (..)
import Client.ExtraPortal.NotificationBar exposing (..)

import Client.ExtraPortal.Pages.FormStatusPage as FormStatusPage exposing (viewFormStatusPage)
import Client.ExtraPortal.Pages.ProfileWizard as Wizard
import Client.ExtraPortal.Pages.DailyMonitor as DailyMonitor exposing (viewDailyMonitor)

import Client.ExtraPortal.Types exposing (..)

-- MODEL
type RemoteData a = Loading | Success a

type alias Model =
    { currentView: Pages
    , wizardModel : Wizard.Model
    , userId: UserID
    , extraInfo: RemoteData ExtraInfo
    }

initModel: String -> Model
initModel userId =
  (
    { currentView = DailyMonitor
    , wizardModel = Wizard.init
    , userId = userId
    , extraInfo = Loading
    })




type Pages = ProfileWizard | FormStatus | DailyMonitor

-- UPDATE

type Msg =
      NoOp
    | ChangePage Pages
    | WizardMsg Wizard.Msg
    | DailyMonitorMsg DailyMonitor.Msg
    | LoadRemoteData
    | ExtraInfoRetrieved ExtraInfo
    | TimeCardUpdate TimeCard

update: Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        ChangePage page ->
            ({model | currentView = page}, Cmd.none)

        LoadRemoteData ->
            (model, getExtraInfo((model.userId, "2017-02-18")))

        ExtraInfoRetrieved extraInfo ->
            ({model | extraInfo = Success extraInfo}, Cmd.none)

        TimeCardUpdate timecard ->
          let
            oldInfo = model.extraInfo
            newExtraInfo =
              case oldInfo of
                Success a -> Success {a | timecard = timecard}
                Loading -> oldInfo
          in
            ({model | extraInfo = newExtraInfo}, Cmd.none)

        WizardMsg subMsg ->
            let
                ( updatedWizardModel, wizardCmd ) =
                    Wizard.update subMsg model.wizardModel
            in
                ( { model | wizardModel = updatedWizardModel }
                , Cmd.none
                )
        DailyMonitorMsg dmMsg ->
            ( model
            , clockinExtra("cizbke6ld32du0152funy1fe3", "10:00am")
            )
            -- ( model
            -- , createExtraSchedule("2017-02-18", "Schedule!", "08:00am")
            -- )
              -- let
              --     (updatedDailyMonitorModel, dmCmd) =
              --       DailyMonitor.update dmMsg model.extraInfo
              -- in

        NoOp -> (model, Cmd.none)



--VIEW
viewExtraPortal: Model -> Html Msg
viewExtraPortal model =
  case model.extraInfo of
    Loading -> div [] [text "Loading"]
    Success extraInfo ->
      div []
        [
         div [style [("margin-bottom", "8px"), ("background-color", "orange"), ("display", "inline-flex")]]
             [
              button [onClick (ChangePage ProfileWizard)] [text "Profile Wizard"]
             ,button [onClick (ChangePage FormStatus)] [text "Form Status"]
             ,button [onClick (ChangePage DailyMonitor)] [text "DailyMonitor"]
             ]
        , let
              rightItems = {avatar = Just defaultUrl}
         in
             Dashboard.view {navbar = {rightItems = Just rightItems}}
        , case model.currentView of
              DailyMonitor ->
                let
                    dmModel =
                      { timecard = extraInfo.timecard
                      , firstName=extraInfo.profile.firstName
                      , schedule=extraInfo.schedule
                      }
                in
                  Html.map DailyMonitorMsg (viewDailyMonitor dmModel)

              ProfileWizard ->
                  div []
                      [ Html.map WizardMsg (Wizard.view model.wizardModel) ]

              FormStatus ->
                  viewFormStatusPage (ChangePage DailyMonitor) defaultFormStatus

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


--PORTS
type alias UserId = String
type alias Day = String
port getExtraInfo : (String, Day) -> Cmd msg
port clockinExtra : (String, String) -> Cmd msg
port createExtraSchedule : (String, String, String) -> Cmd msg

port receiveExtraInfo : (ExtraInfo -> msg) -> Sub msg
port receiveTimecardUpdate : (TimeCard -> msg) -> Sub msg


--SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [receiveExtraInfo ExtraInfoRetrieved
    ,receiveTimecardUpdate TimeCardUpdate
    ]

--SAMPLE data


defaultUrl: Maybe String
defaultUrl = Just "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"

defaultCrewInfoItems: List { name : String, role : String }
defaultCrewInfoItems =
  [{name = "Josh Weinberg", role="Lead PA"}
  ,{name = "Randy Lahey", role="Extra PA"}
  ,{name = "Patty Lebotomy", role="Wardrobe"}
  ]


defaultFormStatus: FormStatusPage.FormStatuses
defaultFormStatus =
  [{formName = "Pence", completedDt = "11/12/2017", completedTs="8:00 AM", imgSrc = "meow"}
  ,{formName = "Emergency Contact", completedDt = "11/12/2017", completedTs="8:00 AM", imgSrc = "meow"}
  ,{formName = "EFS Voucher", completedDt = "11/12/2017", completedTs="8:00 AM", imgSrc = "meow"}
  ]
