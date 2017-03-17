module Client.PAPortal.Pages.Schedule.Task.Extra exposing (..)

import Html
import Html exposing (Html)

import Html.Attributes as Attr

import Common.Renderable as Renderable
import Common.Renderable exposing (Renderable)


type alias Data = { id : String
                  , img : String
                  , name : String
                  , role : String
                  }
    
type alias Extra msg = Renderable Data (Html msg) {}

create : Data -> Extra msg
create = Renderable.create render

styleId = Html.div [] << List.singleton << Html.text
styleImg img = Html.img [Attr.src img] []
styleName = Html.div [] << List.singleton << Html.text
styleRole = Html.div [] << List.singleton << Html.text

body : Data -> List (Html msg)
body {id, img, name, role} = [ styleId id
                             , styleImg img
                             , styleName name
                             , styleRole role
                             ]
            
render : Data -> Html msg
render data =
    let attributes = []
    in Html.div attributes (body data)

checkBox : String -> Bool -> Html msg
checkBox value selected =
    let attrubutes = []
    in Html.input [ Attr.type_ "checkbox"
                  , Attr.name "extra[]"
                  , Attr.value value
                  , Attr.selected selected
                  ] []

input : Bool -> Data -> Html msg
input selected data =
    let attributes = []
        checkBox_ = checkBox (data.id) selected
    in Html.div attributes (checkBox_::(body data))
