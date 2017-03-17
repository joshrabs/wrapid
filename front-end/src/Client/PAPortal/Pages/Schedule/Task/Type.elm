module Client.PAPortal.Pages.Schedule.Task.Type exposing (..)

{-| Represents the various types of tasks that can be found in a Schedule

-}


import Html
import Html exposing (Html)

import Html.Attributes as Attr

import Client.PAPortal.Pages.Schedule.Message exposing (..)
import Common.Renderable as Renderable
import Common.Renderable exposing (Renderable)

type Data = Wardrobe | Props | Lunch | Shoot | Wrap    
type alias Type = Renderable Data (Html Message) {}
    
create : Data -> Type
create = Renderable.create render

render : Data -> Html Message
render = Html.text << Basics.toString

asOption : Bool -> Data -> Html Message
asOption selected data =
    let attributes = [Attr.selected selected]
        body = [Html.text << toString <| data]
    in Html.option attributes body
         
input : Data -> Html Message
input data =
    let attributes = []
        body = List.map (\d -> asOption (d == data) d) [Wardrobe, Props, Lunch, Shoot, Wrap]
    in Html.select attributes body

main = input Lunch
