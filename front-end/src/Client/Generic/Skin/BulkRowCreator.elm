module Client.Generic.Skin.BulkRowCreator exposing (..)

import Client.Generic.Skin.DataTypes exposing (Skin, SkinItem)
import Client.Generic.Types.ModelTypes exposing (Part, Pay, CallStart)

----MODEL/State-------
type alias Model =
  { numRows: Int
  , part: Part
  , pay: Pay
  , callStart: CallStart
  }


--UPDATE

init : Model
init =
    Model 10 "" "" ""

toRoles : Model -> List Role
toRoles model =
    let
        role = { emptyRole |
                     role = model.role,
                     callStart = model.callStart,
                     pay = model.pay
               }
    in
        List.repeat model.amount role

-- ACTION, UPDATE

type Msg
    = UpdPart String
    | UpdCallstart String
    | UpdPay String
    | UpdAmount String

-- type Filed
--     = .role

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdPart str ->
            ( { model | role = str }
            , Cmd.none)
        UpdCallstart str ->
            ( { model | callStart = str }
            , Cmd.none)
        UpdPay str ->
            ( { model | pay = str }
            , Cmd.none)
        UpdAmount str ->
            let
                newAmount = Result.withDefault 0 (String.toInt str)
            in
                ( { model | amount = newAmount}
                , Cmd.none)

-- View


view : Model -> Html Msg
view model =
    div [ ]
        [ p [] [ text "Add Fields"]
        , input [ placeholder "Amount", type_ "number", onInput UpdAmount, value (toString model.amount) ] []
        , input [ placeholder "Role", onInput UpdPart, value model.role ] []
        , input [ placeholder "Pay", onInput UpdPay, value model.pay ] []
        , input [ placeholder "Call Start", onInput UpdCallstart, value model.callStart ] []
        ]
