module Client.ExtraPortal.Pages.ProfileWizard exposing (Model, init, Msg, initEmpty, update, view)

import Html exposing (Html, h3, text)
import Html.Attributes exposing (class, style)
import Material
import Material.Scheme
import Material.Options as Options
import Material.Elevation as Elevation
import Material.Textfield as Textfield
import Material.Card as Card
import Material.Button as Button



-- MODEL


type alias Model =
    { mdl : Material.Model
    , steps : List WizardStep
    }


type alias WizardStep =
    { step : Int
    , display : Display
    , title : String
    , last : Bool
    , fields : List Field
    }


type alias Field =
    { id : Int
    , label : String
    , value : String
    }


type Display
    = Active | Hidden

initEmpty : Model
initEmpty =
    { mdl = Material.model
    , steps = []
    }


init : Model
init =
    { mdl = Material.model
    , steps =
        createStepsFrom0
            [ ( "Personal Info", [ "First Name", "Last Name", "Ml", "Phone", "Email" ] )
            , ( "Address", [ "Address 1", "Address 2", "City", "ZIP", "Country" ] )
            , ( "Signature", [ "Type Digital Signature" ] )
            ]
    }


createStepsFrom0 : List ( String, List String ) -> List WizardStep
createStepsFrom0 list =
    createSteps 0 list


createSteps : Int -> List ( String, List String ) -> List WizardStep
createSteps step list =
    case list of
        x :: [] ->
            [ createStep step x True ]

        x :: xs ->
            [ createStep step x False ] ++ createSteps (step + 1) xs

        [] ->
            []


createStep : Int -> ( String, List String ) -> Bool -> WizardStep
createStep step ( title, fs ) last =
    { step = step
    , title = title
    , last = last
    , display =
        if step == 0 then
            Active
        else
            Hidden
    , fields = List.indexedMap (\i x -> { id = i, label = x, value = "" }) fs
    }



-- ACTION, UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | Batch (List Msg)
    | Upd ( Int, Int ) String
    | NextStep


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        Upd tuple str ->
            ( { model | steps = updateSteps tuple str model.steps }, Cmd.none )

        NextStep ->
            ( { model | steps = nextStep model.steps }, Cmd.none )

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


updateFields : Int -> String -> List Field -> List Field
updateFields i str fields =
    case fields of
        x :: xs ->
            if x.id == i then
                [ { x | value = str } ] ++ updateFields i str xs
            else
                [ x ] ++ updateFields i str xs

        [] ->
            []


getNextStep : List WizardStep -> Int
getNextStep wizard =
    let
        step =
            List.head (List.filter (\x -> x.display == Active) wizard)
    in
        case step of
            Just s ->
                s.step

            Nothing ->
                -1


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
    Material.Scheme.top
        <| Options.div
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
                (List.indexedMap (\i x -> viewWizardField model x.label step i (Upd ( step, i ))) ws.fields)
            , Card.actions
                [ Options.css "text-align" "right" ]
                [ Button.render Mdl
                    [ 1, 0 ]
                    model.mdl
                    [ Button.ripple
                    , Button.accent
                    , Options.onClick NextStep
                    ]
                    [ text
                        (if ws.last == True then
                            "Finish"
                         else
                            "NEXT"
                        )
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
