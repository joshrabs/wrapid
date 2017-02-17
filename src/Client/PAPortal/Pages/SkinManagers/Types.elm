module Client.PAPortal.Pages.SkinManagers.Types exposing (..)

type alias Role =
    { id : String
    , role : String
    , first : String
    , last : String
    , callStart : String
    , pay: String
    , lunchStart : String
    , lunchLength : String
    , clockIn : String
    , clockOut : String
    , callEnd : String
    , email : String
    , sum : String
    , selected : Bool
    }

emptyRole : Role
emptyRole =
    Role "" "" "" "" "" "" "" "" "" "" "" "" "" False

initRoles : List Role
initRoles =
    List.indexedMap (\i x -> { x | id = toString i })
        [ Role "" "Zombie Extra" "Josh" "Weinberg" "8:00 Am" "$ 125/12" "12:00" "1 hr" "" "" "5:00PM" "josh@gmail.com" "" False
        , Role "" "Zombie Super Extra" "Josh" "Weinberg" "8:00 Am" "$ 125/12" "12:00" "1 hr" "" "" "5:00PM" "josh@gmail.com" "" False
        , Role "" "Cop Extra" "Peter" "Geit" "9:00 Am" "$ 125/12" "12:00" "1 hr" "" "" "5:00PM" "joshBig@gmail.com" "" False
        , Role "" "Thief Extra" "Peter" "Geit" "9:00 Am" "$ 125/12" "13:00" "1 hr" "" "" "6:00PM" "joshBIg@gmail.com" "" False
        , Role "" "Thief Extra" "Max" "Marra" "8:30 Am" "$ 150/6" "14:00" "1 hr" "" "" "8:00PM" "joshSmall@gmail.com" "" False
        , Role "" "Zombie Extra" "Josh" "Weinberg" "8:40 Am" "$ 100/12" "13:30" "1.5 hr" "" "" "7:00PM" "joshTall@gmail.com" "" False
        , Role "" "Zombie Extra" "Josh" "Weinberg" "8:15 Am" "$ 50/12" "14:15" "2 hr" "" "" "5:00PM" "peter@gmail.com" "" False
        ]

addIdToRoles : List Role -> List Role
addIdToRoles =
    List.indexedMap (\i x -> { x | id = toString i })


roleToString : Role -> String
roleToString { id
    , role
    , first
    , last
    , callStart
    , pay
    , lunchStart
    , lunchLength
    , clockIn
    , clockOut
    , callEnd
    , email
    , selected
    } =
    role ++ first ++ last ++ callStart ++ pay ++ lunchStart ++ lunchLength ++ clockIn ++ clockOut ++ callEnd ++ email
