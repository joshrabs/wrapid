module Client.PAPortal.Pages.Schedule.Task.Extra exposing (..)

import Html
import Html exposing (Html)

import Html.Attributes as Attr

import Client.PAPortal.Pages.Schedule.Message exposing (..)
import Client.PAPortal.Pages.Schedule.Widget as Widget
import Client.PAPortal.Pages.Schedule.Widget exposing (..)


type alias Data = { id : String
                  , img : String
                  , name : String
                  , role : String
                  }
    
type alias Extra msg = Widget.Widget Data msg

create : Data -> Extra msg
create = Widget.create functions

functions : Widget.Functions Data msg
functions = { default |
              render = render_
            , input = input_
            }
         
gen : String -> Extra msg
gen name = create { id = "id: " ++ name
                  , img = "img: " ++ name
                  , name = name
                  , role = "role: " ++ name
                  }
         
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
            
render_ : Data -> Html msg
render_ data =
    let attributes = []
    in Html.div attributes (body data)

checkBox : String -> Bool -> Html Message
checkBox value selected =
    let attrubutes = []
    in Html.input [ Attr.type_ "checkbox"
                  , Attr.name "extra[]"
                  , Attr.value value
                  , Attr.selected selected
                  ] []

input_ : Data -> Html Message
input_ data =
    let attributes = []
        checkBox_ = checkBox (data.id) False
    in Html.div attributes (checkBox_::(body data))


