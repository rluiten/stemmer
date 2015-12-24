module TestRunnerFullPorter where

import String
import Graphics.Element exposing (Element)

import ElmTest exposing (..)

import StemmerTestsFullPorter

{-| This test runner runs the full suite of porter stemmer tests which is
well over 20000 so is not fast.
-}

main : Element
main =
    elementRunner
      ( suite "Element Test Runner Tests for Full Porter Stemmer tests"
        [ StemmerTestsFullPorter.tests
        ]
      )
