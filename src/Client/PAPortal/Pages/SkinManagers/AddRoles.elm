module Client.PAPortal.Pages.SkinManagers.AddRoles exposing (..)

import Html exposing (Html, text, input, div, p)
import Html.Attributes exposing (placeholder, style, value, type_)
import Html.Events exposing (onInput)
import Client.PAPortal.Pages.SkinManagers.Types exposing (Role, emptyRole)

-- MODEL

type alias Model =
    { role : String
    , callStart : String
    , pay : String
    , amount : Int
    }

init : Model
init =
    Model "" "" "" 10

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
    = UpdRole String
    | UpdCallstart String
    | UpdPay String
    | UpdAmount String

-- type Filed
--     = .role

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "role msg: " msg of
        UpdRole str ->
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
        , input [ placeholder "Role", onInput UpdRole, value model.role ] []
        , input [ placeholder "Pay", onInput UpdPay, value model.pay ] []
        , input [ placeholder "Call Start", onInput UpdCallstart, value model.callStart ] []
        ]
