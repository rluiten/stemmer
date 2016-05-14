module StemmerTestsFullPorter exposing (..)

import ElmTest exposing (..)

import Stemmer
import StemmerFixture

{-| Full set of porter stemmer test as converted from the porter stemmer
website from the voc.txt and output.txt files and put in StemmerFixture.elm.
-}

type alias StemCase = (String, String)


tests : Test
tests =
  suite "Stemmer Tokenizer tests for Full Porter"
    <| List.map testStemmer StemmerFixture.fixture


testStemmer : StemCase -> Test
testStemmer (word, expectedStem) =
  test ("stem " ++ word ++ " to " ++ expectedStem ++ " ")
    <| assertEqual expectedStem (Stemmer.stem word)
