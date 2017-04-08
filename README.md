# Porter Stemmer written in Elm language

Copyright (c) 2016-2017 Robin Luiten

This an implementation of the porter stemmer.

It was created as part of project for Lunr a port of lunr.js to Elm language https://github.com/rluiten/lunr.

On 2015/12/28 Made some changes suggested by smart people thanks Noah, is a lot faster.
Finger in the air tests close to 10 times running the porter stemmer 23531 word tests.

Porter Stemmer web site http://tartarus.org/martin/PorterStemmer/

For quick tests tests/StemmerTests.elm.

For tests derived from porters example vocabulary see tests/StemmerTestsFullPorter.elm.

The full porter tests consist of over 20,000 test cases so take a while to run.

The full porter tests in StemmerTestsFullPorter.elm are generated by running tests/buildStemmerFixtureElm.js with nodejs.
This small script reads in voc.txt and output.txt in local folder and produces elm code.

The following files in the tests folder are duplicates from those on porter stemmer website.

* voc.txt
* output.txt
