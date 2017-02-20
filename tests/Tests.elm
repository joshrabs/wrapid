module Tests exposing (..)

import Test exposing (..)
import Client.PAPortal.Pages.SkinManagerTests as SkinManagerTests
import Expect
import Fuzz exposing (list, int, tuple, string)
import String


all : Test
all =
    describe "All Tests"
        [ SkinManagerTests.all ]
