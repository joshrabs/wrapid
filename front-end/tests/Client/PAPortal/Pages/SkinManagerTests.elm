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
            [ test "Filter one field without space" <|
                \() ->
                    Expect.equal (acceptableRoles "Zombie" [ realRole ]) [ realRole ]
            , test "Filter one field with space" <|
                \() ->
                    Expect.equal (acceptableRoles "Zombie Extra" [ realRole ]) [ realRole ]
            , test "Filter multiple fields with spaces" <|
                \() ->
                    Expect.equal (acceptableRoles "Josh Weinberg" [ realRole ]) [ realRole ]
            , test "Filter multiple fields without spaces" <|
                \() ->
                    Expect.equal (acceptableRoles "JoshWeinberg" [ realRole ]) [ realRole ]
            ]
        ]


realRole =
    Role "" "Zombie Extra" "Josh" "Weinberg" "8:00 Am" "$ 125/12" "12:00" "1 hr" "" "" "5:00PM" "josh@gmail.com" "" False
