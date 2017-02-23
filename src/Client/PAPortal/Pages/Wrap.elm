module Client.PAPortal.Pages.Wrap exposing (..)

import Html exposing (Html, div, text, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Dict exposing (Dict, toList)

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
viewWrap: Model -> Html Msg
viewWrap model =
  div [style [
    ("display", "flex")
    ,("justify-content", "center")
    ,("align-items", "center")
  ]]
  [viewWrapWizard model]


viewWrapWizard: Model -> Html Msg
viewWrapWizard model =
  div [style [
    ("width", "387px")
    ,("height", "286px")
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
  div [] [text "current step!!!"]

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
