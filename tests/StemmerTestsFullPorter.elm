module StemmerTestsFullPorter exposing (..)

{-
   StemmerFixture one bombs compiling in Elm 0.18
   use alternate string form and split.

   List.map testStemmer StemmerFixture.fixture
-}

import Expect
import Test exposing (..)
import Stemmer
import StemmerFixture2


inputs =
    String.split "," StemmerFixture2.inputString


expects =
    String.split "," StemmerFixture2.expectString


fixture =
    List.map2 (,) inputs expects


{-| Full set of porter stemmer test as converted from the porter stemmer
website from the voc.txt and output.txt files and put in StemmerFixture.elm.
-}
type alias StemCase =
    ( String, String )


tests : Test
tests =
    describe "Stemmer Tokenizer tests for Full Porter" <|
        List.map testStemmer fixture


testStemmer : StemCase -> Test
testStemmer ( word, expectedStem ) =
    test ("stem " ++ word ++ " to " ++ expectedStem ++ " ") <|
        \() -> Expect.equal expectedStem (Stemmer.stem word)
