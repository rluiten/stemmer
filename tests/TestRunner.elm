module TestRunner where

import String
import Graphics.Element exposing (Element)

import ElmTest exposing (..)

import StemmerTests


main : Element
main =
    elementRunner
      ( suite "Element Test Runner Tests"
        [ StemmerTests.tests
        ]
      )
