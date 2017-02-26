module Client.PAPortal.Pages.SkinManager exposing (..)

import Client.PAPortal.Pages.SkinManagers.AddRoles as AddRoles
import Client.PAPortal.Pages.SkinManagers.Types exposing (Role, initRoles, addIdToRoles, roleToString, emptyRole)
import Html exposing (Html, Attribute, a, button, div, h1, img, li, p, text, ul, input)
import Html.Attributes exposing (href, src, placeholder, style, checked, type_)
import Html.Events exposing (onClick, onInput, onCheck, onDoubleClick, onBlur)
import List.Extra exposing (find, group, groupWhile)
import Maybe exposing (andThen)
import Table exposing (defaultCustomizations)
import Client.Generic.Dashboard.Dashboard as Dashboard exposing (..)
import Material
import Material.Scheme
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Button as Button


-- MODEL


type alias Model =
    { mdl : Material.Model
    , addRoles : AddRoles.Model
    , editRole : String
    , roles : List Role
    , tableState : Table.State
    , query : String
    , dialogOpened : Dialog
    , breakdown : Bool
    , editableField : ( String, String )
    }


initModel : Model
initModel =
    { mdl = Material.model
    , addRoles = AddRoles.init
    , editRole = ""
    , roles = initRoles
    , tableState = (Table.initialSort "Role")
    , query = ""
    , dialogOpened = NoDialog
    , breakdown = False
    , editableField = ( "", "" )
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | Batch (List Msg)
    | SetQuery String
    | ToggleSelected String
    | ToggleSelectedAll Bool
    | SetTableState Table.State
    | ToggleDialog Dialog
    | AddRoles
    | EditRoles String
    | ChangeEditableField ( String, String )
    | UpdateRoleField String String
    | EditConfirm
    | AddRolesMsg AddRoles.Msg
    | Breakdown


type Dialog
    = AddDialog
    | EditDialog
    | NoDialog


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

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

        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )

        ToggleDialog dialog ->
            let
                toggleDialog =
                    if dialog == model.dialogOpened then
                        NoDialog
                    else
                        dialog
            in
                ( { model | dialogOpened = toggleDialog }
                , Cmd.none
                )

        ToggleSelected id ->
            ( { model | roles = List.map (toggle id) model.roles }
            , Cmd.none
            )

        ToggleSelectedAll bool ->
            ( { model | roles = toggleAll bool model.roles }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        AddRolesMsg subMsg ->
            let
                ( updatedAddRolesModel, addRolesCmd ) =
                    AddRoles.update subMsg model.addRoles
            in
                ( { model | addRoles = updatedAddRolesModel }
                , Cmd.map AddRolesMsg addRolesCmd
                )

        AddRoles ->
            let
                rs =
                    addIdToRoles (model.roles ++ AddRoles.toRoles (model.addRoles))
            in
                ( { model | roles = rs }
                , Cmd.none
                )

        EditRoles string ->
            ( { model | editRole = string }
            , Cmd.none
            )

        ChangeEditableField ( id, fieldName ) ->
            ( { model | editableField = ( id, fieldName ) }
            , Cmd.none
            )

        UpdateRoleField id value ->
            let
                updateRoles =
                    List.map
                        (\x ->
                            if x.id == id then
                                { x | role = value }
                            else
                                x
                        )
                        model.roles
            in
                ( { model | roles = updateRoles }
                , Cmd.none
                )

        EditConfirm ->
            let
                updateRoles =
                    List.map
                        (\x ->
                            if x.selected == True && (String.length model.editRole > 0) then
                                { x | role = model.editRole }
                            else
                                x
                        )
                        model.roles
            in
                ( { model | roles = updateRoles }
                , Cmd.none
                )

        Breakdown ->
            ( { model
                | breakdown = not model.breakdown
                , roles = toggleAll False model.roles
              }
            , Cmd.none
            )



-- _ ->
--     (model, Cmd.none)


toggleAll : Bool -> List Role -> List Role
toggleAll bool roles =
    List.map (\x -> { x | selected = bool }) roles


toggle : String -> Role -> Role
toggle id role =
    if role.id == id then
        { role | selected = not role.selected }
    else
        role



-- VIEW


view : Model -> Html Msg
view model =
    Material.Scheme.top <|
        Dashboard.makePanel
            (Just { title = "Skin 2017-01-01", rightItem = Nothing })
            (panelBody model)
            (Just (panelFooter model.mdl))


panelFooter : Material.Model -> Html Msg
panelFooter mdl =
    div []
        [ Button.render Mdl
            [ 4 ]
            mdl
            [ Button.ripple
            , Button.accent
            , Options.onClick Breakdown
            ]
            [ text "Breakdown" ]
        , Button.render Mdl
            [ 5 ]
            mdl
            [ Button.ripple
            , Button.accent
            ]
            [ text "Export CSV" ]
        , Button.render Mdl
            [ 4 ]
            mdl
            [ Button.ripple
            , Button.accent
            ]
            [ text "Wrap Skin" ]
        ]


panelBody : Model -> Html Msg
panelBody model =
    div []
        [ viewAddRoles model.mdl model.dialogOpened model.addRoles
        , viewTableWithSearch model
        ]


viewAddRoles : Material.Model -> Dialog -> AddRoles.Model -> Html Msg
viewAddRoles mdl dialog addRolessModel =
    case dialog of
        AddDialog ->
            div []
                [ Html.map AddRolesMsg (AddRoles.view addRolessModel)
                , button [ onClick AddRoles ] [ text "ADD ROLES" ]
                ]

        EditDialog ->
            viewEditRoles mdl EditRoles

        _ ->
            div [] []


viewEditRoles : Material.Model -> (String -> Msg) -> Html Msg
viewEditRoles mdl msg =
    div []
        [ Textfield.render Mdl
            [ 1, 0 ]
            mdl
            [ Textfield.label "Edit role"
            , Textfield.floatingLabel
            , Options.dispatch Batch
            , Options.onInput msg
            ]
            []
        , Button.render Mdl
            [ 1 ]
            mdl
            [ Button.ripple
            , Button.accent
            , Options.onClick EditConfirm
            ]
            [ text "Confirm" ]
        ]


acceptableRoles : String -> List Role -> List Role
acceptableRoles query roles =
    let
        lowerQuery =
            String.join "" << String.words <| String.toLower query
    in
        roles
            |> List.filter
                (String.contains lowerQuery
                    << String.join ""
                    << String.words
                    << String.toLower
                    << roleToString
                )


viewTableWithSearch : Model -> Html Msg



-- viewTableWithSearch breakdown roles tableState query =


viewTableWithSearch model =
    let
        checkedAll =
            List.all (\x -> x.selected == True) model.roles
    in
        div []
            [ topButtons model.mdl checkedAll
            , viewTable model.editableField model.breakdown model.tableState (acceptableRoles model.query model.roles)
            ]


topButtons : Material.Model -> Bool -> Html Msg
topButtons mdl checkedAll =
    div []
        [ input
            [ type_ "checkbox"
            , onCheck ToggleSelectedAll
            , checked checkedAll
              -- Hackish style
              -- TODO: Use thead options in Customizations
            , style
                [ ( "position", "relative" )
                , ( "top", "30px" )
                , ( "left", "3px" )
                ]
            ]
            []
        , Textfield.render Mdl
            [ 0 ]
            mdl
            [ Textfield.label "Search by Role"
            , Textfield.floatingLabel
            , Options.dispatch Batch
            , Options.onInput SetQuery
            ]
            []
        , Button.render Mdl
            [ 1 ]
            mdl
            [ Button.ripple
            , Button.accent
            , Options.onClick (ToggleDialog AddDialog)
            ]
            [ text "ADD" ]
        , Button.render Mdl
            [ 2 ]
            mdl
            [ Button.ripple
            , Button.accent
            , Options.onClick (ToggleDialog EditDialog)
            ]
            [ text "Edit" ]
        ]


viewTable : ( String, String ) -> Bool -> Table.State -> List Role -> Html Msg
viewTable ( id, name ) bool tableState roles =
    if bool then
        viewTableBreakdown tableState roles
    else
        Table.view (config ( id, name )) tableState roles


sortBreakdown : List Role -> List Role
sortBreakdown roles =
    List.sortBy .pay <|
        List.sortBy .lunchStart <|
            List.sortBy .lunchLength <|
                List.sortBy .clockIn <|
                    List.sortBy .clockOut <|
                        roles


compareBreakdown : Role -> Role -> Bool
compareBreakdown x y =
    (x.pay == y.pay) && (x.lunchStart == y.lunchStart) && (x.lunchLength == y.lunchLength) && (x.clockIn == y.clockIn) && (x.clockOut == y.clockOut)


flatListRole : List (List Role) -> List Role
flatListRole listRoles =
    case listRoles of
        x :: xs ->
            let
                role =
                    Maybe.withDefault emptyRole (List.head x)
            in
                [ { role | sum = toString (List.length x) } ] ++ flatListRole xs

        [] ->
            []


viewTableBreakdown : Table.State -> List Role -> Html Msg
viewTableBreakdown tableState roles =
    let
        acceptableRole =
            groupWhile (compareBreakdown) (sortBreakdown roles)
    in
        Table.view configBreakdown tableState (flatListRole acceptableRole)



-- Pay, Lunch Start, Lunch Length, Clock In, Clock Out


configBreakdown : Table.Config Role Msg
configBreakdown =
    Table.customConfig
        { toId = .id
        , toMsg = SetTableState
        , columns =
            [ checkboxColumn
            , Table.stringColumn "Sum" .sum
            , Table.stringColumn "Pay" .pay
            , Table.stringColumn "Lunch Start" .lunchStart
            , Table.stringColumn "Lunch length" .lunchLength
            , Table.stringColumn "In" .clockIn
            , Table.stringColumn "Out" .clockOut
            ]
        , customizations =
            { defaultCustomizations | rowAttrs = toRowAttrs }
        }


config : ( String, String ) -> Table.Config Role Msg
config ( fid, fname ) =
    Table.customConfig
        { toId = .id
        , toMsg = SetTableState
        , columns =
            [ checkboxColumn
            , roleColumn "Role" ( fid, fname ) .role .role
              -- , roleEditColumn
              -- , Table.stringColumn "Role" .role
            , Table.stringColumn "First" .first
            , Table.stringColumn "Last" .last
            , Table.stringColumn "Call Start" .callStart
            , Table.stringColumn "Pay" .pay
            , Table.stringColumn "Email" .email
              -- , Table.stringColumn "Lunch Start" .lunchStart
              -- , Table.stringColumn "Lunch length" .lunchLength
              -- , Table.stringColumn "In" .clockIn
              -- , Table.stringColumn "Out" .clockOut
              -- , Table.stringColumn "Call End" .callEnd
            ]
        , customizations =
            { defaultCustomizations | rowAttrs = toRowAttrs }
        }


checkboxColumn : Table.Column Role Msg
checkboxColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = viewCheckbox
        , sorter = Table.unsortable
        }


viewCheckbox : Role -> Table.HtmlDetails Msg
viewCheckbox { id, selected } =
    Table.HtmlDetails [ onClick (ToggleSelected id) ]
        [ input
            [ type_ "checkbox"
            , checked selected
            ]
            []
        ]


roleColumn : String -> ( String, String ) -> (Role -> comparable) -> (Role -> String) -> Table.Column Role Msg
roleColumn name ( fid, fname ) toComparable toStr =
    Table.veryCustomColumn
        { name = "Role"
        , viewData = viewRoleColumn name ( fid, fname ) toStr
        , sorter = Table.increasingOrDecreasingBy toComparable
        }


viewRoleColumn : String -> ( String, String ) -> (Role -> String) -> Role -> Table.HtmlDetails Msg
viewRoleColumn name ( fid, fname ) toStr role =
    let
        _ =
            Debug.log "viewRoleColumn: " ( fid, fname )
    in
        if fid == role.id && fname == name then
            Table.HtmlDetails
                []
                [ input
                    [ onInput (UpdateRoleField role.id)
                    , Html.Attributes.value (toStr role)
                    , onBlur (ChangeEditableField ( "", "" ))
                    ]
                    []
                ]
        else
            Table.HtmlDetails
                [ onDoubleClick
                    (ChangeEditableField ( role.id, name ))
                ]
                [ p [] [ text (toStr role) ] ]



-- roleColumn : Table.Column Role Msg
-- roleColumn =
--     Table.veryCustomColumn
--         { name = "Role Editable"
--         , viewData = viewRoleColumn
--         , sorter = Table.unsortable
--         }
-- viewRoleColumn : Role -> Table.HtmlDetails Msg
-- viewRoleColumn { id, selected, role } =
--     Table.HtmlDetails [ onClick (EditRole id "role") ]
--         [ p [] [ text role ] ]


toRowAttrs : Role -> List (Attribute Msg)
toRowAttrs role =
    [ style
        [ ( "background"
          , if role.selected then
                "#CEFAF8"
            else
                "white"
          )
        ]
    ]


viewLink : String -> Html msg
viewLink name =
    li [] [ a [ href ("#" ++ name) ] [ text name ] ]



-- PORTS
-- SUBSCRIPTIONS
