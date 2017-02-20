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
            [ test "Filter by role" <|
                \() ->
                    Expect.equal (acceptableRoles "Role" [role1]) [role1]
            , test "Filter by First with space" <|
                \() ->
                    Expect.equal (acceptableRoles "First Name" [role1]) [role1]
            , test "Filter multiple fields with spaces" <|
                \() ->
                    Expect.equal (acceptableRoles "Role First" [role1]) [role1]
            ]
        ]


role1 = Role "" "Role" "First Name" "" "" "" "" "" "" "" "" "" "" False
