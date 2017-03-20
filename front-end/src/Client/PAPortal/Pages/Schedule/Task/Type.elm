module Client.PAPortal.Pages.Schedule.Task.Type exposing (..)

{-| Represents the various types of tasks that can be found in a Schedule

-}


import Html
import Html exposing (Html)

import Html.Attributes as Attr

import Client.PAPortal.Pages.Schedule.Message exposing (..)
import Client.PAPortal.Pages.Schedule.Widget exposing (..)
import Client.PAPortal.Pages.Schedule.Widget as Widget

type Data = Wardrobe | Props | Lunch | Shoot | Wrap    
type alias Type msg = Widget Data msg
    
create : Data -> Type msg
create = Widget.create functions

functions : Widget.Functions Data msg
functions = { default |
              render = render_
            , input = input_
            }
         
render_ : Data -> Html msg
render_ = Html.text << Basics.toString

asOption : Bool -> Data -> Html Message
asOption selected data =
    let attributes = [Attr.selected selected]
        body = [Html.text << toString <| data]
    in Html.option attributes body
         
input_ : Data -> Html Message
input_ data =
    let attributes = []
        body = List.map (\d -> asOption (d == data) d) [Wardrobe, Props, Lunch, Shoot, Wrap]
    in Html.select attributes body


