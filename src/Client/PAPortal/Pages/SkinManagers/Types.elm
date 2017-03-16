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
        [
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
