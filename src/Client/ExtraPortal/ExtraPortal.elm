port module Client.ExtraPortal.ExtraPortal exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Material
import Client.Generic.Dashboard.Dashboard as Dashboard exposing (..)
import Client.ExtraPortal.Pages.FormStatusPage as FormStatusPage exposing (viewFormStatusPage)
import Client.ExtraPortal.Pages.ProfileWizard as Wizard exposing (Msg(..))
import Client.ExtraPortal.Pages.DailyMonitor as DailyMonitor exposing (viewDailyMonitor, Msg(..))
import Client.ExtraPortal.Types exposing (..)
import Date exposing (Date)
import Task exposing (perform, succeed)
import Client.Generic.Status.Loading exposing (viewLoadingScreen)
import Animation exposing (px)
import Time exposing (Time)

import Server.API.Queries.ExtraPortalQueries exposing (fetchReqExtraInfo, fetchReceiveExtraInfo)

-- MODEL


type RemoteData a
    = Loading
    | Success a


type ViewState
    = Initializing
    | PageView Page


type Page =
    ProfileWizard
  | FormStatus
  | DailyMonitor

type alias Model =
    { currentDate : Maybe Date
    , currentView : ViewState
    , extraInfo : Maybe ExtraInfo
    , wizardModel : Wizard.Model
    , userId : UserID
    , animStyle : Animation.State
    , mdl : Material.Model
    , shouldShowPortalSwitcher: Bool -- NOTE DEVELOPMENT ONLY!!!!
    }


initModel : String -> Maybe Date -> Material.Model -> ( Model, Cmd Msg )
initModel userId currentDate mdlModel =
    ( { currentDate = currentDate
      , currentView = Initializing
      , wizardModel = Wizard.init
      , userId = userId
      , extraInfo = Nothing
      , animStyle = initAnimStyle
      , mdl = mdlModel
      , shouldShowPortalSwitcher = True
      }
    , Task.perform (always LoadRemoteData) (Task.succeed ())
    )


initAnimStyle =
    Animation.style
        [ Animation.translate (px 0) (px 100)
        , Animation.opacity 0.0
        ]



-- UPDATE


type Msg
    = NoOp
    | ChangeView ViewState
    | WizardMsg Wizard.Msg
    | DailyMonitorMsg DailyMonitor.Msg
    | LoadRemoteData
    | ExtraInfoRetrieved ExtraInfo
    | TimeCardUpdate TimeCard
    | SubSchedule Schedule
    | Animate Animation.Msg
    | FadeInUpMsg
    | ClockIn Time
    | ShowPageSwitcher Bool --NOTE THIS IS DEVELOPMENT ONLY!


fadeInUpMsg : Cmd Msg
fadeInUpMsg =
    Task.perform (always FadeInUpMsg) (Task.succeed ())


submitClockin : Cmd Msg
submitClockin =
    Task.perform ClockIn Time.now

isProfileComplete : Profile -> Bool
isProfileComplete profile = False

requestNextView: ViewState -> Cmd Msg
requestNextView view =
  Task.perform (always (ChangeView view)) (Task.succeed ())

mapWizardCmd: Wizard.Msg -> Msg
mapWizardCmd msg =
  case msg of
    SubmitProfile profile -> ChangeView (PageView FormStatus)
    _ -> NoOp

updateSchedule: Schedule -> Model -> Model
updateSchedule schedule model =
  case model.extraInfo of
    Just extraInfo ->
      let
        updInfo = {extraInfo | schedule = schedule}
      in
        {model | extraInfo = Just updInfo}
    Nothing -> model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowPageSwitcher should ->
          ({model | shouldShowPortalSwitcher = should}, Cmd.none)
        ChangeView view ->
            ( { model | currentView = view }, fadeInUpMsg )

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
                ( { model
                    | animStyle = newStyle
                  }
                , Cmd.none
                )

        LoadRemoteData ->
            ( model, Cmd.batch[fetchReqExtraInfo (( model.userId, "2017-02-18" )), subExtraSchedule ()] )

        ExtraInfoRetrieved extraInfo ->
          let
              nextView =
                if isProfileComplete extraInfo.profile
                  then DailyMonitor else ProfileWizard
          in
            ( { model | currentView=PageView nextView, extraInfo = Just extraInfo }, fadeInUpMsg )

        SubSchedule schedule ->
          (updateSchedule schedule model, Cmd.none)

        Animate animMsg ->
            ( { model
                | animStyle = Animation.update animMsg model.animStyle
              }
            , Cmd.none
            )

        TimeCardUpdate timecard ->
            let
                oldInfo =
                    model.extraInfo

                newExtraInfo =
                    case oldInfo of
                        Just a ->
                            Just { a | timecard = timecard }
                        Nothing ->
                            oldInfo
            in
                ( { model | extraInfo = newExtraInfo }, Cmd.none )

        WizardMsg subMsg ->
            let
                ( updatedWizardModel, wizardCmd ) =
                    Wizard.update subMsg model.wizardModel

                log2 = Debug.log "Wizard CMD" wizardCmd
            in
                ( { model | wizardModel = updatedWizardModel }
                , Cmd.map mapWizardCmd wizardCmd
                )

        ClockIn curTime ->
            ( model
            , case model.extraInfo of
                Just extraInfo ->
                    clockinExtra ( extraInfo.timecard.id, curTime |> toString )

                Nothing ->
                    Cmd.none
            )

        DailyMonitorMsg dmMsg ->
            case dmMsg of
                TimeCardMsg punchAction ->
                    case punchAction of
                        PunchIn ->
                            ( model, submitClockin )

                        PunchOut ->
                            ( model
                            , clockinExtra ( "cizbke6ld32du0152funy1fe3", "10:00am" )
                            )

        NoOp ->
            ( model, Cmd.none )

--VIEW


viewExtraPortal : Model -> Html Msg
viewExtraPortal model =
    case model.currentView of
        Initializing ->
            viewLoadingScreen

        PageView page ->
          case model.extraInfo of
            Nothing -> viewLoadingScreen
            Just extraInfo ->
              div []
                  [if model.shouldShowPortalSwitcher then viewPageSwitcher
                    else
                      div [onClick (ShowPageSwitcher True), style [ ( "position", "fixed" ), ( "top", "0px" ), ( "left", "0px" ), ("min-height", "20px"), ("width", "100%"), ("background", "transparent") ]] []
                  , let
                      avatar =
                          extraInfo.profile.avatar.url

                      rightItems =
                          { avatar = Just avatar }
                    in
                      Dashboard.view { navbar = { rightItems = Just rightItems } }
                  , case page of
                      DailyMonitor ->
                        let
                          dmModel =
                             { currentDate = model.currentDate
                              , timecard = extraInfo.timecard
                              , firstName = extraInfo.profile.firstName
                              , schedule = extraInfo.schedule
                            }
                        in
                          Html.map DailyMonitorMsg (viewDailyMonitor dmModel (Animation.render model.animStyle))

                      ProfileWizard->
                          div []
                              [ Html.map WizardMsg (Wizard.view model.wizardModel) ]

                      FormStatus ->
                          viewFormStatusPage (ChangeView (PageView DailyMonitor)) defaultFormStatus (Animation.render model.animStyle)
                  ]

viewPageSwitcher: Html Msg
viewPageSwitcher =
  div [style [
      ("position", "fixed"), ("top", "0"), ("left", "0")
      , ( "margin-bottom", "8px" )
      , ( "background-color", "orange" )
      , ("display", "inline-flex")
      ]
    ]
    [ button [onClick (ShowPageSwitcher False)] [text "--"]
    , button [ onClick (ChangeView (PageView ProfileWizard)) ] [ text "Profile Wizard" ]
    , button [ onClick (ChangeView (PageView FormStatus)) ] [ text "Form Status" ]
    , button [ onClick (ChangeView (PageView DailyMonitor))][ text "DailyMonitor" ]
    ]
--PORTS

port clockinExtra : ( String, String ) -> Cmd msg


port createExtraSchedule : ( String, String, String ) -> Cmd msg


port receiveTimecardUpdate : (TimeCard -> msg) -> Sub msg

port subExtraSchedule: () -> Cmd msg
port subReceiveExtraSchedule: (Schedule -> msg) -> Sub msg

--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fetchReceiveExtraInfo ExtraInfoRetrieved
        , receiveTimecardUpdate TimeCardUpdate
        , subReceiveExtraSchedule SubSchedule
        , Animation.subscription Animate [ model.animStyle ]
        ]



--SAMPLE data


defaultUrl : Maybe String
defaultUrl =
    Just "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"


defaultCrewInfoItems : List { name : String, role : String }
defaultCrewInfoItems =
    [ { name = "Josh Weinberg", role = "Lead PA" }
    , { name = "Randy Lahey", role = "Extra PA" }
    , { name = "Patty Lebotomy", role = "Wardrobe" }
    ]


defaultFormStatus : FormStatusPage.FormStatuses
defaultFormStatus =
    [ { formName = "Pence", completedDt = "11/12/2017", completedTs = "8:00 AM", imgSrc = "meow" }
    , { formName = "Emergency Contact", completedDt = "11/12/2017", completedTs = "8:00 AM", imgSrc = "meow" }
    , { formName = "EFS Voucher", completedDt = "11/12/2017", completedTs = "8:00 AM", imgSrc = "meow" }
    ]
