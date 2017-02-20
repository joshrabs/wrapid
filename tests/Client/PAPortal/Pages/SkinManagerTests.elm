module Client.PAPortal.Pages.SkinManagerTests exposing (..)

import Client.PAPortal.Pages.SkinManager exposing (acceptableRoles)
import Client.PAPortal.Pages.SkinManagers.Types exposing (Role)
import Test exposing (..)
import Expect
import String

all : Test
all =
    describe "SkinManager"
        [ describe "acceptableRole"
            [ test "Addition" <|
                \() ->
                    Expect.equal (acceptableRoles "Role" [role1]) [role1]
            ]
        ]


role1 = Role "" "Role" "First Name" "" "" "" "" "" "" "" "" "" "" False
