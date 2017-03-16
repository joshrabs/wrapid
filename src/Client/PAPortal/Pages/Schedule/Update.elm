module Client.PAPortal.Pages.Schedule.Update exposing (..)

import Client.PAPortal.Pages.Schedule.Model exposing (..)
import Dict exposing (Dict)
import Dict

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

