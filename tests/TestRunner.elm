module TestRunner exposing (..)

import String
import ElmTest exposing (..)

import StemmerTests


main =
  runSuite
    ( suite "Element Test Runner Tests"
      [ StemmerTests.tests
      ]
    )
