module Client.PAPortal.Pages.Schedule exposing (..)

import Html exposing (Html, div, text, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Dict exposing (Dict, toList)

import Common.Styles.TextStyles exposing (regularLight, regularBolder, headerTitleStyle)
import Common.Styles.ButtonStyles exposing (blackButtonStyle)

import Material.Icon as Icon exposing (view)

--MODEL
type alias Model =
  {currentStep: StepNum
  ,allSteps: Dict StepNum Step
  }

type Step = ReviewAndSave | SignAll | Submit
type alias StepNum = Int

initModel: Model
initModel =
  {currentStep = 0
  ,allSteps = Dict.fromList (List.indexedMap (,) [ReviewAndSave, SignAll, Submit])
  }

--UPDATE
type Msg = NextStep | PrevStep | SetStep StepNum

update: Msg -> Model -> Model
update msg model =
  case msg of
    NextStep -> incrementStep model
    PrevStep -> incrementStep model
    SetStep stepNum -> {model | currentStep = stepNum}

incrementStep: Model -> Model
incrementStep model =
  let
    nextStepNum = model.currentStep + 1
    nextStep = Dict.get nextStepNum model.allSteps
  in
    case nextStep of
      Just nextStep -> {model | currentStep = nextStepNum}
      Nothing -> model

--VIEW
view: Model -> Html Msg
view model =
  div [style [
    ("margin", "4px 8px 24px 4px")
  ]]
  [div [style (List.concat [headerTitleStyle, [("margin", "16px 0px 16px 0px")]])] [text "Review"]
  ,viewWrapWizard model
  ]


viewWrapWizard: Model -> Html Msg
viewWrapWizard model =
  div [style [
    ("width", "387px")
    ,("background", "#FFFFFF")
    ,("box-shadow", "0 8px 8px 0 rgba(40,44,52,0.20)")
    ]]
    (List.map (\step -> viewWrapStep step model.currentStep) (Dict.toList model.allSteps))


viewWrapStep: (StepNum, Step) -> StepNum -> Html Msg
viewWrapStep stepItem currentStep =
  let
    stepNum = (stepItem |> Tuple.first)
    step = (stepItem |> Tuple.second)
    stepName = toString step
  in
    div [onClick (SetStep stepNum)]
    [viewWrapStepHeader stepNum stepName
    ,if stepNum == currentStep then viewWrapStepBody step
      else div [] []
    ]

viewWrapStepBody: Step -> Html msg
viewWrapStepBody step =
  case step of
    ReviewAndSave -> viewWrapStepSkin
    SignAll -> viewWrapStepSignAll
    Submit -> viewWrapSubmitDownload

viewWrapStepSkin: Html msg
viewWrapStepSkin =
  div [style [
    ("display", "flex")
    ,("flex-direction", "column")
    ,("background", "#F8FAFF")
    ,("padding", "16px")
  ]]
  [
    span [style (regularLight 12)] [text "Monday May 25, 2016"]
    ,span [style (regularBolder 16)] [text "RUNABETTERSET Productions"]
    ,div [style [("margin", "8px 0px 8px 0px"), ("display", "flex")]]
      [div [style (List.concat [(blackButtonStyle True), [("margin-right", "8px")]])] [text "View Skin"]
      ,div [style (blackButtonStyle False)] [text "Save and Wrap"]
      ]

  ]

viewWrapStepSignAll: Html msg
viewWrapStepSignAll =
  div [style [
    ("display", "flex")
    ,("flex-direction", "column")
    ,("background", "#F8FAFF")
    ,("padding", "16px")
  ]]
  [
    span [style (regularBolder 14)]
    [text "I have reviewed the document and the Information Supplied is True to the Best of my Knowledge"]
    ,div [style (blackButtonStyle True)] [text "Sign All Forms"]
  ]

viewWrapSubmitDownload: Html msg
viewWrapSubmitDownload =
  div [style [
    ("display", "flex")
    ,("flex-direction", "column")
    ,("align-items", "center")
    ,("background", "#F8FAFF")
    ,("padding", "16px")
  ]]
  [Icon.view "cloud_download" [Icon.size48]
  ,div [style (blackButtonStyle True)] [text "Download"]
  ]

viewWrapStepHeader: StepNum -> String -> Html msg
viewWrapStepHeader stepNum stepName =
    div [style [
      ("display", "flex")
      ,("align-items", "center")
      ,("justify-content", "space-between")
      ,("height", "64px")
      ,("border-bottom", "1px solid #EBF0F5")
    ]]
    [
      span [style [
        ("font-family", "Roboto-Regular")
        ,("font-size", "14px")
        ,("color", "#282C35")
        ,("margin-left", "20px")
      ]]
      [text ((stepNum |> toString) ++ ". " ++ stepName)]]
