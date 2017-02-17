module Client.PAPortal.Pages.SkinManager exposing (..)

import Client.PAPortal.Pages.SkinManagers.AddRoles as AddRoles
import Client.PAPortal.Pages.SkinManagers.Types exposing (Role, initRoles, addIdToRoles, roleToString, emptyRole)

import Html exposing (Html, Attribute, a, button, div, h1, img, li, p, text, ul, input)
import Html.Attributes exposing (href, src, placeholder, style, checked, type_)
import Html.Events exposing (onClick, onInput, onCheck)
import List.Extra exposing (find, group, groupWhile)
import Maybe exposing (andThen)
import Table exposing (defaultCustomizations)
import Client.Generic.Dashboard.Dashboard as Dashboard exposing (..)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }



-- MODEL


type alias Model =
    { addRoles : AddRoles.Model
    , roles : List Role
    , tableState : Table.State
    , query : String
    , dialogOpened : Dialog
    , breakdown : Bool
    }


initModel : Model
initModel =
    Model AddRoles.init initRoles (Table.initialSort "Role") "" NoDialog False

init : (Model, Cmd Msg)
init =
    ( initModel, Cmd.none )


-- UPDATE


type Msg
    = SetQuery String
    | ToggleSelected String
    | ToggleSelectedAll Bool
    | SetTableState Table.State
    | ToggleDialog Dialog
    | AddRoles
    | EditRoles String
    | AddRolesMsg AddRoles.Msg
    | Breakdown

type Dialog
    = AddDialog
    | EditDialog
    | NoDialog

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none )

        ToggleDialog dialog ->
            let
                toggleDialog = if dialog == model.dialogOpened then NoDialog else dialog
            in
                ( { model | dialogOpened = toggleDialog }
                , Cmd.none )

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
                    _ = Debug.log "model: " model
                    ( updatedAddRolesModel, addRolesCmd ) =
                        AddRoles.update subMsg model.addRoles
                in
                    ( { model | addRoles = updatedAddRolesModel }
                    , Cmd.map AddRolesMsg addRolesCmd
                    )

        AddRoles ->
            let
                rs = addIdToRoles (model.roles ++ AddRoles.toRoles(model.addRoles))
            in
                ( { model | roles = rs }
                , Cmd.none
                )

        EditRoles string->
            let
                -- updateRoles = { x | role = string }
                updateRoles = List.map
                     (\x -> if x.selected == True then
                                { x | role = string  }
                            else
                                x
                     )
                     model.roles

            in
                ( { model | roles = updateRoles }
                , Cmd.none
                )

        Breakdown ->
            ( { model | breakdown = not model.breakdown }
            , Cmd.none)
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
    Dashboard.makePanel
        (Just {title = "Skin 2017-01-01", rightItem= Nothing})
        (panelBody model)
        (Just panelFooter)


panelFooter : Html Msg
panelFooter =
    div []
        [ button [ onClick Breakdown ] [ text "BREAKDOWN" ]
        , button [ ] [ text "Export CSV" ]
        , button [ ] [ text "Wrap Skin" ]
        ]


panelBody : Model -> Html Msg
panelBody model =
    div []
        [ viewAddRoles model.dialogOpened model.addRoles
        , viewTableWithSearch model.breakdown model.roles model.tableState model.query        ]

viewAddRoles : Dialog -> AddRoles.Model -> Html Msg
viewAddRoles dialog addRolessModel =
    case dialog of
        AddDialog ->
            div []
                [ Html.map AddRolesMsg (AddRoles.view addRolessModel)
                , button [ onClick AddRoles ] [ text "ADD ROLES" ]
                ]

        EditDialog ->
            viewEditRoles

        _ ->
            div [] []


viewEditRoles : Html Msg
viewEditRoles  =
    div []
        [ text "Edit Role"
        , input [ placeholder "Edit Role", onInput EditRoles ] []
        ]


viewTableWithSearch : Bool -> List Role -> Table.State -> String -> Html Msg
viewTableWithSearch breakdown roles tableState query =
    let
        lowerQuery =
            String.toLower query

        acceptableRole =
            List.filter (String.contains lowerQuery << String.toLower << roleToString) roles

        checkedAll =
            List.all (\x -> x.selected == True) roles

    in
        div []
            [ input [ placeholder "Search by Role", onInput SetQuery ] []
            , button [ onClick (ToggleDialog AddDialog) ] [ text "ADD" ]
            , button [ onClick (ToggleDialog EditDialog) ] [ text "EDIT" ]
            , button [ onClick Breakdown ] [ text "BREAKDOWN" ]
            , input [ type_ "checkbox", onCheck ToggleSelectedAll, checked checkedAll ] []
            , viewTable breakdown tableState acceptableRole
            ]

viewTable : Bool -> Table.State -> List Role -> Html Msg
viewTable bool tableState roles =
    if bool then
        viewTableBreakdown tableState roles
    else
        Table.view config tableState roles


sortBreakdown : List Role -> List Role
sortBreakdown roles =
    List.sortBy .pay
        <| List.sortBy .lunchStart
        <| List.sortBy .lunchLength
        <| List.sortBy .clockIn
        <| List.sortBy .clockOut
        <| roles

compareBreakdown : Role -> Role -> Bool
compareBreakdown x y =
    (x.pay == y.pay) && (x.lunchStart == y.lunchStart) && (x.lunchLength == y.lunchLength) && (x.clockIn == y.clockIn) && (x.clockOut == y.clockOut)


flatListRole : List (List Role) -> List (Role)
flatListRole listRoles=
    case listRoles of
        (x::xs) ->
            let
                role = Maybe.withDefault emptyRole (List.head x)
            in
             [{ role | sum = toString(List.length x) }] ++ flatListRole xs
        [] ->
            []



viewTableBreakdown : Table.State -> List Role -> Html Msg
viewTableBreakdown tableState roles =
    let
        acceptableRole =
            groupWhile (compareBreakdown) (sortBreakdown roles)
        _ = Debug.log "groupWhile: " (List.map (\x -> List.length x) acceptableRole)
        d = Debug.log "groupWhile: " (List.map (\x -> x.first) roles)
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


config : Table.Config Role Msg
config =
  Table.customConfig
    { toId = .id
    , toMsg = SetTableState
    , columns =
        [ checkboxColumn
        , Table.stringColumn "Role" .role
        , Table.stringColumn "First" .first
        , Table.stringColumn "Last" .last
        , Table.stringColumn "Call Start" .callStart
        , Table.stringColumn "Pay" .pay
        , Table.stringColumn "Lunch Start" .lunchStart
        , Table.stringColumn "Lunch length" .lunchLength
        , Table.stringColumn "In" .clockIn
        , Table.stringColumn "Out" .clockOut
        , Table.stringColumn "Call End" .callEnd
        , Table.stringColumn "Email" .email
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
viewCheckbox {selected} =
  Table.HtmlDetails []
    [ input [ type_ "checkbox", checked selected ] []
    ]


toRowAttrs : Role -> List (Attribute Msg)
toRowAttrs role =
  [ onClick (ToggleSelected role.id)
  , style [ ("background", if role.selected then "#CEFAF8" else "white") ]
  ]

viewLink : String -> Html msg
viewLink name =
    li [] [ a [ href ("#" ++ name) ] [ text name ] ]


-- PORTS


-- SUBSCRIPTIONS
