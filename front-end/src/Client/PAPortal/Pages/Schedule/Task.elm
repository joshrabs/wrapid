module Client.PAPortal.Pages.Schedule.Task exposing (..)

type alias Prop = String
type alias Role = String
type alias Person = String
type alias TaskNumber = String

type alias Task = { prop : Prop
                  , role : Role
                  , person : Person
                  , taskNumber : TaskNumber
                  }
