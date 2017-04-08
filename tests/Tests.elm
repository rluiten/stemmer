module Tests exposing (..)

import Test exposing (..)
import StemmerTests
import StemmerTestsFullPorter


all : Test
all =
    describe "Stemmer and StemmerFullPorter tests"
        [ StemmerTests.tests
        , StemmerTestsFullPorter.tests
        ]
