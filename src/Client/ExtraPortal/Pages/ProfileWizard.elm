module Client.ExtraPortal.Pages.ProfileWizard exposing (Model, init, Msg(..), initEmpty, update, view)

import Html exposing (Html, h3, text)
import Html.Attributes exposing (class, style)
import Material
import Material.Scheme
import Material.Options as Options
import Material.Elevation as Elevation
import Material.Textfield as Textfield
import Material.Card as Card
import Material.Button as Button
import Debug exposing (log)
import Task exposing (succeed)
import Dict exposing (fromList, Dict)
import Client.Generic.WebForm.Types exposing (Profile, FieldCategory(..), Field)
import Http exposing (..)
import Json.Encode as Encode exposing (encode, object, string)
import Json.Decode as Decode exposing (list, string)
import RemoteData exposing (sendRequest, WebData)


-- MODEL


type alias Model =
    { mdl : Material.Model
    , steps : List WizardStep
    }


type alias WizardStep =
    { step : Int
    , display : Display
    , title : String
    , fields : List WizardField
    }


type alias WizardField =
    { id : Int
    , label : String
    , value : String
    , category : FieldCategory
    }


type Display
    = Active
    | Hidden


initEmpty : Model
initEmpty =
    { mdl = Material.model
    , steps = []
    }


init : Model
init =
    { mdl = Material.model
    , steps =
        createStepsFromProfile defaultProfile
    }


createStepsFromProfile : Profile -> List WizardStep
createStepsFromProfile profile =
    profile
        |> Dict.values
        |> List.map (\v -> ( .category v |> toString, v ))
        |> (flip part) Dict.empty
        |> Dict.toList
        |> createSteps 0


part : List ( String, Field ) -> Dict String (List Field) -> Dict String (List Field)
part list d =
    case list of
        x :: xs ->
            addToDict d x
                |> part xs

        [] ->
            d


addToDict : Dict String (List Field) -> ( String, Field ) -> Dict String (List Field)
addToDict d k =
    let
        key =
            Tuple.first k

        v =
            Tuple.second k

        curVal =
            Dict.get key d
    in
        case curVal of
            Just val ->
                Dict.insert key (v :: val) d

            Nothing ->
                Dict.insert key [ v ] d


categoryOrder =
    [ Name, Address ]


defaultProfile : Profile
defaultProfile =
    Dict.fromList
        [ ( "firstName", { id = "firstName", label = "First Name", value = "", category = Name } )
        , ( "lastName", { id = "firstName", label = "Last Name", value = "", category = Name } )
        , ( "Street", { id = "Street", label = "Street Address", value = "", category = Address } )
        , ( "City", { id = "City", label = "City", value = "", category = Address } )
        ]


type alias FieldsByCategory =
    ( String, List Field )


createStepsFrom0 : List FieldsByCategory -> List WizardStep
createStepsFrom0 list =
    createSteps 0 list


createSteps : Int -> List FieldsByCategory -> List WizardStep
createSteps step list =
    case list of
        x :: [] ->
            [ createStep step x ]

        x :: xs ->
            [ createStep step x ] ++ createSteps (step + 1) xs

        [] ->
            []


createStep : Int -> FieldsByCategory -> WizardStep
createStep step ( title, fs ) =
    { step = step
    , title = title
    , display =
        if step == 0 then
            Active
        else
            Hidden
    , fields = List.indexedMap (\i x -> { id = i, label = x.label, value = x.value, category = x.category }) fs
    }



-- ACTION, UPDATE


profileFromWizard : List WizardStep -> Profile
profileFromWizard wizardSteps =
    wizardSteps
        |> List.map (\ws -> wizardStepToFields ws)
        |> List.concat
        |> Dict.fromList


wizardStepToFields : WizardStep -> List ( String, Field )
wizardStepToFields ws =
    ws.fields
        |> List.map
            (\wField ->
                ( wField.label, { label = wField.label, category = wField.category, value = wField.value, id = wField.label } )
            )


type Msg
    = Mdl (Material.Msg Msg)
    | Batch (List Msg)
    | Upd ( Int, Int ) String
    | NextStep
    | SubmitProfile Profile
    | ReceiveProfile (RemoteData.WebData String)


submitProfile : Profile -> Cmd Msg
submitProfile profile =
    Task.perform (always (SubmitProfile profile)) (Task.succeed ())


profileBody : profile -> Http.Body
profileBody profile =
    jsonBody
        (object
            [ ( "profile"
              , Encode.object
                    [ ( "first_name", Encode.string "Bruce" )
                    , ( "last_name", Encode.string "Williams" )
                    , ( "street_address", Encode.string "10th avenue" )
                    , ( "city", Encode.string "New York" )
                    ]
              )
            ]
        )


submitProfileHttp : Profile -> Cmd (RemoteData.WebData String)
submitProfileHttp profile =
    Http.post
        "http://35.157.165.22/profiles"
        (profileBody profile)
        (Decode.field "id" Decode.string)
        |> RemoteData.sendRequest



-- LoginTypes.SubmitLogin ->
--     (Server.loginUser loginModel.email loginModel.password)
--         |> Cmd.map ReceiveAuthentication
-- loginUser : Username -> Password -> Cmd (RemoteData.WebData String)
-- loginUser username password =
--     Http.post
--         "http://35.157.165.22/user_token"
--         (loginBody username password)
--         (Decode.field "jwt" Decode.string)
--         |> RemoteData.sendRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        Upd tuple str ->
            ( { model | steps = updateSteps tuple str model.steps }, Cmd.none )

        NextStep ->
            let
                checkNextStep =
                    nextStep model.steps

                allComplete =
                    areAllStepsComplete model.steps

                log =
                    Debug.log "ALL COMPLETE ??" allComplete

                cmd =
                    if allComplete then
                        submitProfile (profileFromWizard model.steps)
                        -- Cmd.map ReceiveProfile (submitProfileHttp (profileFromWizard model.steps))
                    else
                        Cmd.none
            in
                ( { model | steps = nextStep model.steps }, cmd )

        SubmitProfile profile ->
            let
                log2 =
                    Debug.log "CMD!" "SUBMITTING PROFILE!!!"
            in
                ( model, Cmd.none )

        Batch listOfMsg ->
            let
                ( finalModel, listOfFx ) =
                    List.foldl
                        (\msg ->
                            \( mdl, fxList ) ->
                                let
                                    ( newModel, newFx ) =
                                        update msg mdl
                                in
                                    ( newModel, fxList ++ [ newFx ] )
                        )
                        ( model, [] )
                        listOfMsg
            in
                ( finalModel, Cmd.batch listOfFx )

        _ ->
            ( model, Cmd.none )


updateSteps : ( Int, Int ) -> String -> List WizardStep -> List WizardStep
updateSteps ( st, i ) str steps =
    case steps of
        x :: xs ->
            if x.step == st then
                [ (updateWizardStep i str x) ] ++ (updateSteps ( st, i ) str xs)
            else
                [ x ] ++ (updateSteps ( st, i ) str xs)

        [] ->
            []


updateWizardStep : Int -> String -> WizardStep -> WizardStep
updateWizardStep i str wizardStep =
    { wizardStep | fields = updateFields i str wizardStep.fields }


updateFields : Int -> String -> List WizardField -> List WizardField
updateFields i str fields =
    case fields of
        x :: xs ->
            if x.id == i then
                [ { x | value = str } ] ++ updateFields i str xs
            else
                [ x ] ++ updateFields i str xs

        [] ->
            []


isFieldComplete : WizardField -> Bool
isFieldComplete field =
    let
        log =
            Debug.log "isFieldComplete" (field.value |> String.isEmpty |> not)
    in
        if String.isEmpty field.value then
            False
        else
            True


isStepComplete : WizardStep -> Bool
isStepComplete step =
    let
        firstMissing =
            step.fields
                |> List.filter (\field -> not (isFieldComplete field))
                |> List.head
    in
        case firstMissing of
            Nothing ->
                True

            Just missing ->
                False


areAllStepsComplete : List WizardStep -> Bool
areAllStepsComplete steps =
    let
        firstMissingStep =
            steps |> List.filter (\step -> not (isStepComplete step)) |> List.head
    in
        case firstMissingStep of
            Just step ->
                False

            Nothing ->
                True


getNextStep : List WizardStep -> Int
getNextStep wizard =
    let
        step =
            completedSteps wizard |> List.reverse |> List.head
    in
        case step of
            Just s ->
                s.step

            Nothing ->
                -1


completedSteps : List WizardStep -> List WizardStep
completedSteps steps =
    List.filter (\x -> isStepComplete x) steps


nextStep : List WizardStep -> List WizardStep
nextStep wizard =
    let
        cs =
            getNextStep wizard

        f x =
            if x.step == cs + 1 then
                { x | display = Active }
            else
                { x | display = Hidden }
    in
        List.map f wizard



-- View


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    Material.Scheme.top <|
        Options.div
            [ Options.center ]
            (List.indexedMap (\i x -> viewStep model x i) model.steps)


viewStep : Model -> WizardStep -> Int -> Html Msg
viewStep model ws step =
    Options.div
        [ if ws.display == Active then
            Options.css "display" "block"
          else
            Options.css "display" "none"
        ]
        [ h3
            []
            [ text "Please Enter Info" ]
        , Card.view
            [ Elevation.e8
            , Options.css "height" "500px"
            ]
            [ Card.title
                []
                [ text (toString (ws.step + 1) ++ ". " ++ ws.title) ]
            , Card.text
                [ Card.expand ]
                (List.indexedMap
                    (\i x ->
                        viewWizardField model
                            x.label
                            step
                            i
                            (Upd ( step, i ))
                    )
                    ws.fields
                )
            , Card.actions
                [ Options.css "text-align" "right" ]
                [ Button.render Mdl
                    [ 1, 0 ]
                    model.mdl
                    [ Button.ripple
                    , Button.accent
                    , Options.onClick NextStep
                    ]
                    [ text "Next"
                    ]
                ]
            ]
        ]


viewWizardField : Model -> String -> Int -> Int -> (String -> Msg) -> Html Msg
viewWizardField model label step n msg =
    Textfield.render Mdl
        [ step, n ]
        model.mdl
        [ Textfield.label label
        , Textfield.text_
        , Options.dispatch Batch
        , Options.onInput msg
        ]
        []
