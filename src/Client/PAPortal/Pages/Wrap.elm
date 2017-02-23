module Client.PAPortal.Pages.Wrap exposing (..)

import Html exposing (Html, div, text, span)
import Html.Attributes exposing (style)
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

--VIEW
viewWrap: Html msg
viewWrap =
  div [style [
    ("display", "flex")
    ,("justify-content", "center")
    ,("align-items", "center")
  ]]
  [viewWrapWizard initModel]


viewWrapWizard: Model -> Html msg
viewWrapWizard model =
  div [style [
    ("width", "387px")
    ,("height", "286px")
    ,("background", "#FFFFFF")
    ,("box-shadow", "0 8px 8px 0 rgba(40,44,52,0.20)")
    ]]
    (List.map (\step -> viewWrapStep step) (Dict.toList model.allSteps))


viewWrapStep: (StepNum, Step) -> Html msg
viewWrapStep step =
  div []
  [viewWrapStepHeader step]


viewWrapStepHeader: (StepNum, Step) -> Html msg
viewWrapStepHeader step =
  let
    stepNum = (step |> Tuple.first |> toString)
    stepName = (step |> Tuple.second |> toString)
  in
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
      [text (stepNum ++ ". " ++ stepName)]]
