module Client.Generic.Skin.BulkRowCreator exposing (..)

import Client.Generic.Skin.DataTypes exposing (Skin, SkinItem)

----MODEL/State-------
type alias Model =
  { numRows: Int
  , part: Part
  , pay: Pay
  }


--UPDATE
type Msg = AddBulkRows Model
type alias ViewBulkRowCreator = BulkRowValues -> Html Msg


--VIEW
-----view: State -> SaveSkinMsg -> Html Msg
