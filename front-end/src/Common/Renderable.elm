module Common.Renderable exposing
    ( Renderable
    , create
    , render)

{-| A Renderable is a record containing a piece of data and a function which
    knows how to render it.
-}

{-| Represents data that can be rendered -}
type alias Renderable data result x = { x |
                                        data : data
                                      , render : data -> result
                                      }

{-| A simple helper function for creating a Renderable record -}
create : (data -> result) -> data -> Renderable data result {}
create render a = { data = a
                  , render = render
                  }

{-| Given a renderable record, renders it. -}
render : Renderable data result x -> result
render record = record.render record.data
