module Client.WardrobePortal.View exposing (..)

import Client.WardrobePortal.Types exposing (..)
import Common.Helpers exposing (unique)
import Common.Navbar exposing (navbar)
import Common.Styles as Styles
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Icon as Icon
import Material.Spinner as Loading
import Material.Options as Options


view : Model -> Html Msg
view model =
    let
        nav =
            navbar model.mdl Mdl Nothing []
    in
        case model.statuses of
            Success wardrobeStatusList ->
                div []
                    [ nav
                    , text "Date filter: "
                    , datePicker wardrobeStatusList
                    , statusList model.mdl model.dateFilter wardrobeStatusList
                    ]

            _ ->
                div []
                    [ nav
                    , div
                        [ style Styles.loadContainer ]
                        [ Loading.spinner [ Loading.active True ] ]
                    ]


datePicker : List WardrobeStatus -> Html Msg
datePicker wardrobeStatusList =
    let
        options : List WardrobeStatus -> List (Html Msg)
        options statuses =
            unique .date statuses
                |> List.map (\val -> option [ value val ] [ text val ])
                |> List.append [ option [ value "None" ] [ text "None" ] ]
    in
        select
            [ onInput SelectDate ]
            (options wardrobeStatusList)


statusList : Material.Model -> DateFilter -> List WardrobeStatus -> Html Msg
statusList mdl filter statusList =
    let
        statuses =
            case filter of
                None ->
                    statusList
                        |> List.sortBy .date

                Filter date ->
                    List.filter (\s -> s.date == date) statusList
    in
        div [] (List.indexedMap (statusItem mdl) statuses)


{-|
TODO: Implement a date picker, instead of showing all dates.
TODO: Categorize extras by role.
-}
statusItem : Material.Model -> Int -> WardrobeStatus -> Html Msg
statusItem mdl idx wardrobeStatus =
    let
        name =
            wardrobeStatus.firstName ++ " " ++ wardrobeStatus.firstName
    in
        Card.view
            [ Options.css "width" "250px"
            , Options.css "height" "130px"
            , Options.css "margin" "8px"
            , Color.background Color.white
            ]
            [ Card.title
                [ Options.css "align-content" "flex-start"
                , Options.css "flex-direction" "row"
                , Options.css "align-items" "flex-start"
                , Options.css "justify-content" "space-between"
                ]
                [ Card.head
                    []
                    [ text wardrobeStatus.date ]
                , input
                    [ style [ ( "display", "none" ) ], id <| wardrobeStatus.id, type_ "file", accept "image/*" ]
                    []
                , div
                    [ onClick (SelectWardrobeStatusPhoto wardrobeStatus.id) ]
                    [ photoOrPlaceholder mdl idx wardrobeStatus.file
                    ]
                ]
            , Card.actions
                [ Card.border
                , Options.css "vertical-align" "center"
                , Options.css "text-align" "right"
                ]
                [ actionButton mdl idx wardrobeStatus ]
            ]


actionButton : Material.Model -> Int -> WardrobeStatus -> Html Msg
actionButton mdl idx status =
    let
        ( buttonText, buttonOptions ) =
            case status.checkStatus of
                CHECKEDOUT ->
                    ( "Check In", [ Button.ripple, Options.onClick (CheckIn status.id) ] )

                Updating ->
                    ( "Wait…", [ Button.disabled ] )

                _ ->
                    ( "Check Out", [ Button.ripple, Options.onClick (CheckOut status.id) ] )
    in
        Button.render Mdl
            [ idx, 1 ]
            mdl
            buttonOptions
            [ text buttonText ]


photoOrPlaceholder : Material.Model -> Int -> Maybe String -> Html Msg
photoOrPlaceholder mdl idx stringMaybe =
    case stringMaybe of
        Just string ->
            img
                [ style [ ( "width", "50px" ) ], src string ]
                []

        Nothing ->
            Button.render
                Mdl
                [ idx, 0 ]
                mdl
                [ Button.ripple, Button.icon ]
                [ Icon.i "add_a_photo" ]
