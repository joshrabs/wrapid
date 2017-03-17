module Client.PAPortal.Pages.Schedule.Task.Type exposing (..)

{-| Represents the various types of tasks that can be found in a Schedul

-}


import Html
import Html exposing (Html)

import Common.Renderable as Renderable
import Common.Renderable exposing (Renderable)


type Data = Wardrobe | Props | Lunch | Shoot | Wrap    
type alias Type msg = Renderable Data (Html msg) {}

    
create : Data -> Type msg
create = Renderable.create render

render : Data -> Html msg
render = Html.text << Basics.toString
    
