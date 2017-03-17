module Client.PAPortal.Pages.Schedule.Task exposing (..)

import Html
import Html exposing (Html)

import Client.PAPortal.Pages.Schedule.Renderable as Renderable
import Client.PAPortal.Pages.Schedule.Renderable exposing (Renderable)

import Client.PAPortal.Pages.Schedule.Name as Name
import Client.PAPortal.Pages.Schedule.Name exposing (Name)

import Client.PAPortal.Pages.Schedule.Prop as Prop
import Client.PAPortal.Pages.Schedule.Prop exposing (Prop)

import Client.PAPortal.Pages.Schedule.Role as Role
import Client.PAPortal.Pages.Schedule.Role exposing (Role)

import Client.PAPortal.Pages.Schedule.Person as Person
import Client.PAPortal.Pages.Schedule.Person exposing (Person)


type alias Data msg = { prop : Prop msg
                      , role : Role msg
                      , person : Person msg
                      , name : Name msg
                      }

type alias Task msg = Renderable (Data msg) (Html msg) {}

create : Data msg -> Task msg
create = Renderable.create render

render : Data msg -> Html msg
render { prop, role, person, name } =
    let attributes = []
        td x = Html.td [] [x]
        body  = List.map (td << Renderable.doRender)
            [ prop, role, person, name ]
    in Html.tr attributes body


gen n = create { prop = Prop.create ("Prop " ++ n)
               , role = Role.create ("Role " ++ n)
               , person = Person.create ("Person " ++ n)
               , name = Name.create ("Name " ++ n)
               }
        
testTask : Task msg
testTask = create { prop = Prop.create "Prop"
                  , role = Role.create "Role"
                  , person = Person.create "Person"
                  , name = Name.create "Name"
                  }

main = testTask.render testTask.data
