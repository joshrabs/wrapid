module Client.PAPortal.Pages.Schedule.Renderable exposing (..)

import Html
import Html exposing (Html)

type alias Renderable data result x = { x |
                                        data : data
                                      , render : data -> result
                                      }

create : (data -> result) -> data -> Renderable data result {}
create render a = { data = a
                  , render = render
                  }

-- TODO: There is an uncaught compiler error
-- if I name this function render.
doRender : Renderable data result x -> result
doRender {data, render} = render data
