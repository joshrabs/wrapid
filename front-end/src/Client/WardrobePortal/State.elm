module Client.WardrobePortal.State exposing (..)

import Json.Decode exposing (at, decodeValue, list)
import Client.WardrobePortal.Decoders exposing (wardrobeStatus)
import Client.WardrobePortal.Types exposing (..)
import Client.WardrobePortal.Ports as Ports
import Common.Helpers exposing (updateRecord)
import Material


init : Model
init =
    { statuses = Init
    , dateFilter = None
    , mdl = Material.model
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveWardrobeStatuses result ->
            { model | statuses = receiveStatuses result } ! []

        SelectWardrobeStatusPhoto uploadId ->
            ( model, Ports.selectWardrobePhoto(uploadId) )

        SelectDate date ->
            if date == "None" then
                { model | dateFilter = None } ! []
            else
                { model | dateFilter = Filter date } ! []

        Mdl msg ->
            Material.update Mdl msg model

        WardrobeStatusUpdate result ->
            { model | statuses = updateStatuses result model.statuses } ! []

        CheckIn id ->
            { model | statuses = waiting model.statuses id } ! [ Ports.checkInWardrobe id ]

        CheckOut id ->
            { model | statuses = waiting model.statuses id } ! [ Ports.checkOutWardrobe id ]


waiting : RemoteData (List WardrobeStatus) -> String -> RemoteData (List WardrobeStatus)
waiting statuses id =
    case statuses of
        Success list ->
            Success
                (List.map
                    (\i ->
                        if i.id == id then
                            { i | checkStatus = Updating }
                        else
                            i
                    )
                    list
                )

        _ ->
            statuses


receiveStatuses : Result String (List WardrobeStatus) -> RemoteData (List WardrobeStatus)
receiveStatuses val =
    case val of
        Err error ->
            Debug.log error Failure

        Ok result ->
            Success result


updateStatuses : Result String WardrobeStatus -> RemoteData (List WardrobeStatus) -> RemoteData (List WardrobeStatus)
updateStatuses new list =
    let
        old =
            case list of
                Success wardrobeStatusList ->
                    wardrobeStatusList

                _ ->
                    []
    in
        case new of
            Err error ->
                Debug.log error (Success old)

            Ok result ->
                Success (updateRecord result old)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.receiveWardrobeStatuses (decodeValue (at [ "data", "allExtraWardrobeStatuses" ] (list wardrobeStatus)) >> ReceiveWardrobeStatuses)
        , Ports.receiveWardrobeStatusUpdate (decodeValue (at [ "data", "setExtraWardrobePic", "extraWardrobeStatusExtraWardrobeStatus" ] wardrobeStatus) >> WardrobeStatusUpdate)
        , Ports.updateCheckStatus (decodeValue (at [ "data", "updateExtraWardrobeStatus" ] wardrobeStatus) >> WardrobeStatusUpdate)
        , Material.subscriptions Mdl model
        ]
