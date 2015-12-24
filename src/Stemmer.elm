module Stemmer (stem) where

{-| Stemmer is an english language stemmer, this is an Elm
implementation of the PorterStemmer taken from http://tartarus.org/~martin.

Copyright (c) 2016 Robin Luiten

Inspired by Erlang implementation on http://tartarus.org/~martin/PorterStemmer/index.html.

## Usage
@docs stem

## Implementation Details

Step numbers follow general implementation in porter stemmer implementations.

Identifier names were adopted from elrang implementation.

* **drow** stands for reverse of word
* **mets** stands for reverse of stem

-}

import Maybe exposing (withDefault, andThen)
import String

{-| Get the porter stem of a word.

Some examples and what running them produces
```
  Stemmer.stem "fullness" -- produces "full"
  Stemmer.stem "consign" -- produces "consign"
  Stemmer.stem "consigned" -- produces "consign"
  Stemmer.stem "consigning" -- produces "consign"
  Stemmer.stem "consignment" -- produces "consign"
  Stemmer.stem "knot" -- produces "knot"
  Stemmer.stem "knots" -- produces "knot"
```

-}
stem : String -> String
stem word =
    if (String.length word) < 3 then
      word
    else
      allSteps word


allSteps : String -> String
allSteps =
    String.reverse
      << String.fromList
      -- << viewParams "post step5"
      << step5
      -- << viewParams "post step4"
      << step4
      -- << viewParams "post step3"
      << step3
      -- << viewParams "post step2"
      << step2
      -- << viewParams "post step1"
      << step1
      -- << viewParams " pre step1"
      << String.toList
      << String.reverse


-- output trace of parameters, great for getting
-- a good view of stages if enabled in allSteps code
viewParams : String -> List Char -> List Char
viewParams context drow =
    let
      s = String.fromList <| List.reverse drow
      _ = Debug.log("viewParams") (context, s, drow)
    in
      drow


-- gets rid of plurals and -ed or -ing.
step1 : List Char -> List Char
step1 = step1c << step1b << step1a


-- This removes plurals
step1a : List Char -> List Char
step1a drow =
    case drow of
      's' :: 'e' :: 's' :: 's' :: mets ->
        's' :: 's' :: mets
      's' :: 'e' :: 'i' :: mets ->
        'i' :: mets
      's' :: 's' :: _ ->
        drow
      's' :: mets ->
        mets
      _ ->
        drow


step1b : List Char -> List Char
step1b drow =
    case drow of
      'd' :: 'e' :: 'e' :: mets ->
        if (measure mets) > 0 then
          'e' :: 'e' :: mets
        else
          drow

      'd' :: 'e' :: mets ->
        if hasVowel mets then
          step1b2 mets
        else
          drow

      'g' :: 'n' :: 'i' :: mets ->
        if hasVowel mets then
          step1b2 mets
        else
          drow

      _ ->
        drow


step1b2 : List Char -> List Char
step1b2 drow =
    -- let _ = Debug.log("step1b2") (drow)
    -- in
    case drow of
      't' :: 'a' :: mets ->
        'e' :: 't' :: 'a' :: mets
      'l' :: 'b' :: mets ->
        'e' :: 'l' :: 'b' :: mets
      'z' :: 'i' :: mets ->
        'e' :: 'z' :: 'i' :: mets
      h :: drowTail ->
        -- let _ = Debug.log("step1b2 drowTail") (h, drowTail, drow, measure drow, endsWithCVC drow)
        -- in
        if endsWithDoubleCons drow
            && not (h == 'l' || h == 's' || h == 'z') then
          drowTail
        else if (measure drow) == 1 && (endsWithCVC drow) then
          'e' :: drow
        else
          drow
      _ ->
        drow

-- This implements porter stemmer.
-- it appears that lunr.js step 1c departs from the porter stemmer
--  lunr  stem "lay" == "lay"
--  lunr  stem "try" == "tri"
-- but porter stemmer test fixture voc.txt and output.txt state specify.
--  stem "lay" == "lai"
--  stem "try" == "try"
step1c : List Char -> List Char
step1c drow =
    case drow of
      'y' :: mets ->
        if hasVowel mets then
          'i' :: mets
        else
          drow
      _ ->
        drow


-- makes cases below easier to read
toLR = String.toList << String.reverse


step2Rules : List (List Char, List Char)
step2Rules =
    [ (toLR "ational", toLR "ate")
    , (toLR "tional", toLR "tion")
    , (toLR "enci", toLR "ence")
    , (toLR "anci", toLR "ance")
    , (toLR "izer", toLR "ize")
    , (toLR "bli", toLR "ble")
    , (toLR "alli", toLR "al")
    , (toLR "entli", toLR "ent")
    , (toLR "eli", toLR "e")
    , (toLR "ousli", toLR "ous")
    , (toLR "ization", toLR "ize")
    , (toLR "ation", toLR "ate")
    , (toLR "ator", toLR "ate")
    , (toLR "alism", toLR "al")
    , (toLR "iveness", toLR "ive")
    , (toLR "fulness", toLR "ful")
    , (toLR "ousness", toLR "ous")
    , (toLR "aliti", toLR "al")
    , (toLR "iviti", toLR "ive")
    , (toLR "biliti", toLR "ble")
    , (toLR "logi", toLR "log")
    ]


{-
maps double suffices to single ones. so -ization (-ize plus
-ation) maps to -ize etc. note that the string before the suffix must give
m() > 0
-}
step2 : List Char -> List Char
step2 drow =
    replaceStarts 0 step2Rules drow


step3Rules =
    [ (toLR "icate", toLR "ic")
    , (toLR "ative", [])
    , (toLR "alize", toLR "al")
    , (toLR "iciti", toLR "ic")
    , (toLR "ical", toLR "ic")
    , (toLR "ful", [])
    , (toLR "ness", [])
    ]


-- deals with -ic-, -full, -ness etc. similar strategy to previous step
step3 : List Char -> List Char
step3 drow =
    replaceStarts 0 step3Rules drow


step4Rules =
    [ (toLR "al", [])
    , (toLR "ance", [])
    , (toLR "ence", [])
    , (toLR "er", [])
    , (toLR "ic", [])
    , (toLR "able", [])
    , (toLR "ible", [])
    , (toLR "ant", [])
    , (toLR "ement", [])
    , (toLR "ment", [])
    , (toLR "ent", [])
    -- "ion" special case for "sion" "tion" see step4Ion
    , (toLR "ou", [])
    , (toLR "ism", [])
    , (toLR "ate", [])
    , (toLR "iti", [])
    , (toLR "ous", [])
    , (toLR "ive", [])
    , (toLR "ize", [])
    ]


-- takes off -ant, -ence etc., in context <c>vcvc<v>
step4 : List Char -> List Char
step4 drow =
    let
      mThreshold = 1
      ionCase = toLR "ion"
      ionLen = List.length ionCase
      drowStart = List.take ionLen drow
    in
      if (drowStart == ionCase) then -- handle (t)ion (s)ion
        step4Ion mThreshold ionLen drow
      else
        replaceStarts mThreshold step4Rules drow


-- handle (tion) and (sion)
step4Ion : Int -> Int -> List Char -> List Char
step4Ion mThreshold startLen drow =
    let
      drowEnd = List.drop startLen drow
      headMeasureCheck dorowEnd char =
        if (char == 't' || char == 's' )
            && (measure drowEnd) > mThreshold then
          Just drowEnd
        else
          Nothing
    in
      withDefault drow <|
        (List.head drowEnd) `andThen`
          (headMeasureCheck drowEnd)


step5 : List Char -> List Char
step5 = step5b << step5a


step5a : List Char -> List Char
step5a drow =
    case drow of
      'e' :: mets ->
        let
          m = measure mets
          -- _ = Debug.log("step5a") (drow, m, endsWithCVC mets)
        in
          if m > 1 then
            mets
          else if m == 1 && not (endsWithCVC mets) then
            -- let _ = Debug.log("step5a endsWithCVC") (mets, m)
            -- in
            mets
          else
            drow
      _ ->
        drow


step5b : List Char -> List Char
step5b drow =
    case drow of
      'l' :: mets ->
        if (measure mets) > 1
            && endsWithDoubleCons drow then
          mets
        else
          drow
      _ ->
        drow


{- Return result of application of the first rule that matches input pattern
it does not have to actually change the string just match pattern.
-}
replaceStarts : Int -> List (List Char, List Char) -> List Char -> List Char
replaceStarts measureThreshold rules drow =
    case rules of
      [] ->
        drow
      r :: rs ->
        let
          (patternMatched, newDrow) = replaceStart measureThreshold r drow
        in
          if patternMatched then
            newDrow
          else
            replaceStarts measureThreshold rs drow


{-| Apply replacement rule matching start with newStart if measure threshold
is reached. In example the result patternMatched indicates the start pattern
was matched regardless of measureThreshold.

```elm
  (patterMatched, newDrow) = replaceStart measureThreshold rule drow
```

-}
replaceStart : Int -> (List Char, List Char) -> List Char -> (Bool, List Char)
replaceStart measureThreshold (start, newStart) drow =
    let
      startLen = List.length start
      drowStart = List.take startLen drow
    in
      if drowStart == start then
        let
          drowEnd = List.drop startLen drow
          --_ = Debug.log("replaceStart measure") (measure drowEnd, drowEnd)
        in
          -- even if measure threshold not reached we have matched the start
          -- so the result is True for matched prefix
          if (measure drowEnd) > measureThreshold then
            (True, newStart ++ drowEnd)
          else
            (True, drow)
      else
        (False, drow)


isVowel : Char -> Bool
isVowel = isVowelCore False


isVowelWithY : Char -> Bool
isVowelWithY = isVowelCore True


isVowelCore : Bool -> Char -> Bool
isVowelCore includeY c =
    case c of
      'a' -> True
      'e' -> True
      'i' -> True
      'o' -> True
      'u' -> True
      'y' -> if includeY then True else False
      _ -> False


{-| Implements m, the measure of a word or word part.

measures the number of consonant sequences between 0 and j. if c is
a consonant sequence and v a vowel sequence, and <..> indicates arbitrary
presence,

    <c><v>       gives 0
    <c>vc<v>     gives 1
    <c>vcvc<v>   gives 2
    <c>vcvcvc<v> gives 3
     ....

Input word in this implementation is reversed, so correct it is
restored to forward to calculate measure.
-}
measure : List Char -> Int
measure drow =
    let
      word = List.reverse drow
      -- _ = Debug.log("measure forward word") (word)
    in
      case word of
        [] -> 0
        h :: t ->
          case isVowel h of
            True -> foundVowel t 0
            False -> foundLeadingConsonant t


foundLeadingConsonant : List Char -> Int
foundLeadingConsonant word =
    case word of
      [] -> 0
      h :: t ->
        case isVowelWithY h of
          True -> foundVowel t 0
          False -> foundLeadingConsonant t


foundVowel : List Char -> Int -> Int
foundVowel word m =
    case word of
      [] -> m
      h :: t ->
        case isVowel h of
          True -> foundVowel t m
          False -> foundConsonant t (m + 1)


foundConsonant : List Char -> Int -> Int
foundConsonant word m =
    case word of
      [] -> m
      h :: t ->
        case isVowelWithY h of
          True -> foundVowel t m
          False -> foundConsonant t m


-- Implements *S - the stem ends with "s" (and similarly for other letters)
endsWith : Char -> List Char -> Bool
endsWith char drow =
    case drow of
      char :: t -> True
      _ -> False


-- Implements *v* - the stem contains a vowel
hasVowel : List Char -> Bool
hasVowel drow =
    let
      word = List.reverse drow
    in
      case word of
        [] -> False
        h :: t ->
          case isVowel h of
            True -> True
            False -> hasVowel2 t


hasVowel2 : List Char -> Bool
hasVowel2 word =
    case word of
      [] -> False
      h :: t ->
        case isVowelWithY h of
          True -> True
          False -> hasVowel2 t


-- Implements *d - the stem ends with a double consonant.
endsWithDoubleCons : List Char -> Bool
endsWithDoubleCons drow =
    case drow of
      c1 :: c2 :: tail ->
        if not (isVowelWithY c1) && c1 == c2 then
          True
        else
          False
      _ -> False


-- Implements *o - the stem ends cvc, where the second c is not w, x, or y.
endsWithCVC : List Char -> Bool
endsWithCVC drow =
    case drow of
      c2 :: v :: c1 :: t ->
             (not (isVowel c1))
          && (isVowelWithY v)
          && (not ((isVowel c2) || (c2 == 'w') || (c2 == 'x') || (c2 == 'y')))
      _ -> False
