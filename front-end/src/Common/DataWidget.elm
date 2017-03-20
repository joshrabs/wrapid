module Common.DataWidget exposing (DataWidget, render, input, validate)

import Common.Renderable exposing (..)
import Common.Renderable as Renderable
import Html exposing (Html)

{-| Represents a Data backed Widget that has a form element associated with it.

    The `render` function renders a message independent representation of the data that
    can be viewed anywhere.

    The `input` function renders a message dependent representation of the data. This is
    typically a form input which will trigger a message to update the model it is contained in.

    The `validate` function validates the data and returns Nothing if the data is valid.
    Otherwise, returns a list of error messages
-}
type alias DataWidget data basic input x
    = Renderable data (Html basic) { x |
                                     input : data -> Html input
                                   , validate : data -> Maybe (List String)
                                   }

{-| Renders a DataWidget. -}
render : DataWidget data basic input x -> Html basic
render = Renderable.render
         
{-| Renders a DataWidget as an Input. -}
input : DataWidget data basic input x -> Html input
input record = record.input record.data
               
{-| Validates a DataWidget. -}
validate : DataWidget data basic input x -> Maybe (List String)
validate record = record.validate record.data
