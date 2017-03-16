module Client.PAPortal.Pages.Schedule.Model exposing (..)

import Dict exposing (Dict)

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
