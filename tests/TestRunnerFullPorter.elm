module TestRunnerFullPorter exposing (..)

import String
import ElmTest exposing (..)


import StemmerTestsFullPorter


main =
  runSuite
    ( suite "Element Test Runner Tests for Full Porter Stemmer tests"
      [ StemmerTestsFullPorter.tests
      ]
    )
