module Client.PAPortal.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)

import Client.PAPortal.Types exposing (..)
import Client.PAPortal.State exposing (Msg(..), Model)
import Client.PAPortal.HorizontalCalendar exposing (viewCalendar, defaultCalendar)
import Client.PAPortal.Pages.LiveMonitor as LiveMonitor exposing (viewLiveMonitor)
import Client.PAPortal.Pages.SkinManager as Skin
import Client.PAPortal.Pages.Wrap as Wrap exposing (view)
import Client.Generic.Status.Loading exposing (viewLoadingScreen)
import Client.Generic.Dashboard.Dashboard as Dashboard exposing (view)
import Client.Generic.Dashboard.LeftSideMenu exposing (SideMenuTabInput)

viewPAPortal : Model -> Html Msg
viewPAPortal model =
    div []
        [ let
            getTabStyle tabType = (model.currentView == tabType)
          in
            Dashboard.view
              { leftMenuTabs=Just (pageTabs getTabStyle)
              , navbar = { rightItems = Just { avatar = Nothing } }
              , mainPage=viewPAMainPage model
              }
        ]

viewPAMainPage: Model -> Html Msg
viewPAMainPage model =
  div [style [("padding", "4px"), ("overflow-y", "scroll")]]
    [case model.currentView of
      Initializing ->
        viewLoadingScreen
      LiveMonitor ->
        case model.currentSkin of
          Nothing -> viewLoadingScreen
          Just skin ->
            case model.extraInfo of
              Loading -> viewLoadingScreen
              Success extraInfo ->
                div []
                    [ Html.map LiveMsg (viewLiveMonitor model.liveModel extraInfo)
                    ]
      Schedule ->
        div [] [text "Schedul"]
      SkinManager ->
          div []
              [ Html.map SkinMsg (Skin.view model.skinModel) ]
      Wrap ->
        Html.map WrapMsg (Wrap.view model.wrapModel)
    ]
-- skinToExtraInfo : Skin -> List ExtraInfo
-- skinToExtraInfo skin =
--   skin.skinItems
--     |> List.map
--         (\si ->
--          {extraId=si.email
--          , firstName = si.firstName
--          , lastName=si.lastName
--          , role=si.part
--          , pay=si.pay
--          , avatar={url=Nothing}
--         })

pageTabs: (ViewState -> Bool) -> List (SideMenuTabInput Msg)
pageTabs associatedState =
  [ {isSelected=associatedState LiveMonitor
    , onClickMsg=ChangeView LiveMonitor
    , iconName="fa fa-video-camera"
    , text = "Live Monitor"
    }
  , { isSelected=associatedState Schedule
    , onClickMsg=ChangeView Schedule
    , iconName="fa fa-calendar"
    , text ="Schedule"
    }
  , { isSelected=associatedState SkinManager
    , onClickMsg=ChangeView SkinManager
    , iconName="fa fa-users"
    , text ="Skins"
    }
  , { isSelected=associatedState Wrap
    , onClickMsg=ChangeView Wrap
    , iconName="fa fa-briefcase"
    , text ="Wrap"
    }
  ]


baseTabStyle : List ( String, String )
baseTabStyle =
    [ ( "padding", "8px" )
    , ( "font-size", "14px" )
    , ( "font-family", "Roboto-Regular" )
    , ( "font-weight", "300" )
    , ( "color", "#6D717A" )
    , ("width", "125px")
    , ("display", "flex")
    , ("justify-content", "center")
    , ("align-items", "center")
    ]


selectedTabStyle : List ( String, String )
selectedTabStyle =
    List.concat
        [ baseTabStyle
        , [ ( "border-bottom", "2px solid #0043FF" )
          , ( "color", "#0043FF" )
          , ( "font-family", "Roboto-Bold" )
          ]
        ]


viewHeaderInfo : Html msg
viewHeaderInfo =
    div [ style
        [ ( "margin", "8px" )
        , ( "display", "inline-flex" )
        , ( "flex-direction", "column" )
      ]]
      [ span
          [ style
              [ ( "font-family", "Roboto-Regular" )
              , ( "font-size", "12px" )
              , ( "color", "#6D717A" )
              ]
          ]
          [ text "Monday May 25, 2016" ]
      , span
          [ style
              [ ( "font-family", "Roboto-Regular" )
              , ( "font-size", "16px" )
              , ( "margin", "4px 0px 4px 0px" )
              , ( "color", "#282C35" )
              , ( "letter-spacing", "0" )
              ]
          ]
          [ text "RUNABETTERSET Productions" ]
      ]
