port module Client.ExtraPortal.ExtraPortal exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Material

import Client.Generic.Dashboard.Dashboard as Dashboard exposing (..)

import Client.ExtraPortal.Pages.FormStatusPage as FormStatusPage exposing (viewFormStatusPage)
import Client.ExtraPortal.Pages.ProfileWizard as Wizard
import Client.ExtraPortal.Pages.DailyMonitor as DailyMonitor exposing (viewDailyMonitor, Msg(..))

import Client.ExtraPortal.Types exposing (..)
import Date exposing (Date)
import Task exposing (perform, succeed)
import Client.Generic.Status.Loading exposing (viewLoadingScreen)
import Client.Generic.WebForm.Utils exposing (..)

import Animation exposing (px)
import Time exposing (Time)

-- MODEL
type RemoteData a = Loading | Success a
type Pages = ProfileWizard | FormStatus | DailyMonitor

type alias Model =
    { currentDate: Maybe Date
    , currentView: Pages
    , wizardModel : Wizard.Model
    , userId: UserID
    , extraInfo: RemoteData ExtraInfo
    , animStyle: Animation.State
    , mdl: Material.Model
    }

initModel: String -> Maybe Date -> Material.Model -> (Model, Cmd Msg)
initModel userId currentDate mdlModel =
  (
    { currentDate = currentDate
    , currentView = DailyMonitor
    , wizardModel = Wizard.init
    , userId = userId
    , extraInfo = Loading
    , animStyle = initAnimStyle
    , mdl = mdlModel
    }
    , Task.perform (always LoadRemoteData) (Task.succeed ())
  )

initAnimStyle =
  Animation.style
      [ Animation.translate (px 0) (px 100)
      , Animation.opacity 0.0
      ]



-- UPDATE

type Msg =
      NoOp
    | ChangePage Pages
    | WizardMsg Wizard.Msg
    | DailyMonitorMsg DailyMonitor.Msg
    | LoadRemoteData
    | ExtraInfoRetrieved ExtraInfo
    | TimeCardUpdate TimeCard
    | Animate Animation.Msg
    | FadeInUpMsg
    | ClockIn Time


fadeInUpMsg: Cmd Msg
fadeInUpMsg = Task.perform (always FadeInUpMsg) (Task.succeed ())

submitClockin: Cmd Msg
submitClockin =
  Task.perform ClockIn Time.now

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangePage page ->
            ({model | currentView = page}, fadeInUpMsg)

        FadeInUpMsg ->
          let
            newStyle =
                Animation.interrupt
                    [ Animation.to
                          [ Animation.translate (px 0) (px 0)
                          , Animation.opacity 1.0
                          ]
                    ]
                    initAnimStyle
          in
              ({ model
                  | animStyle = newStyle
              }, Cmd.none
              )

        LoadRemoteData ->
            (model, getExtraInfo((model.userId, "2017-02-18")))

        ExtraInfoRetrieved extraInfo ->
            ({model | extraInfo = Success extraInfo}, fadeInUpMsg)

        Animate animMsg ->
          ({ model
              | animStyle = Animation.update animMsg model.animStyle
          }
          ,Cmd.none)

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
        ClockIn curTime ->
          ( model
          , case model.extraInfo of
              Success extraInfo -> clockinExtra(extraInfo.timecard.id, curTime |> toString)
              Loading -> Cmd.none
          )
        DailyMonitorMsg dmMsg ->
          case dmMsg of
            TimeCardMsg punchAction ->
              case punchAction of
                PunchIn -> (model, submitClockin)
                PunchOut ->
                  ( model
                  , clockinExtra("cizbke6ld32du0152funy1fe3", "10:00am")
                  )

        NoOp -> (model, Cmd.none)



--VIEW
viewExtraPortal: Model -> Html Msg
viewExtraPortal model =
  case model.extraInfo of
    Loading -> viewLoadingScreen
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
              avatar = extraInfo.profile.avatar.url
              rightItems = {avatar = Just avatar}
         in
             Dashboard.view {navbar = {rightItems = Just rightItems}}
        , case model.currentView of
              DailyMonitor ->
                let
                    dmModel =
                      { currentDate = model.currentDate
                      , timecard = extraInfo.timecard
                      , firstName=extraInfo.profile.firstName
                      , schedule=extraInfo.schedule
                      }
                in
                  Html.map DailyMonitorMsg (viewDailyMonitor dmModel (Animation.render model.animStyle))

              ProfileWizard ->
                  div []
                      [ Html.map WizardMsg (Wizard.view model.wizardModel) ]

              FormStatus ->
                viewFormStatusPage (ChangePage DailyMonitor) defaultFormStatus (Animation.render model.animStyle)

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
    ,Animation.subscription Animate [ model.animStyle ]
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
