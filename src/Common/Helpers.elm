module Common.Helpers exposing (unique, updateRecord)

import List.Extra as ListExtra


updateRecord : { b | id : a } -> List { b | id : a } -> List { b | id : a }
updateRecord new =
    List.map
        (\old ->
            if new.id == old.id then
                new
            else
                old
        )


unique : (a -> comparable) -> List a -> List comparable
unique func =
    List.map func >> ListExtra.unique
