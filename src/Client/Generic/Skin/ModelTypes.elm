module Client.Generic.Skin.ModelTypes exposing (..)

import Client.Generic.Types.ModelTypes exposing (Part, Pay, Email, CallStartTs)

-----Main Types-------
type alias Skin =
  {effectiveDt: EffectiveDt
  ,skinItems: List SkinItem
  }

type alias SkinItem =
  {email: Email
  ,firstName: String
  ,lastName: String
  ,part: Part
  ,pay: Pay
  ,callStart: CallStartTs
  }
