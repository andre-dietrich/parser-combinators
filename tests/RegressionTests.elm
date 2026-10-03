module RegressionTests exposing (boundedRepetitionSuite, currentLocationSuite, errorOrderSuite, regexSuite, stackSafetySuite, terminationSuite, unicodeSuite, whitespaceSuite)

import Combine exposing (..)
import Combine.Char as C
import Combine.Num as N
import Dict
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, fuzz2, test)


result : Parser () a -> String -> Result (List String) a
result p s =
    case parse p s of
        Ok ( _, _, v ) ->
            Ok v

        Err ( _, _, ms ) ->
            Err ms


position : Parser () a -> String -> Int
position p s =
    case parse p s of
        Ok ( _, stream, _ ) ->
            stream.position

        Err ( _, stream, _ ) ->
            stream.position


regexSuite : Test
regexSuite =
    describe "regex"
        [ test "multiline mode does not match on a later line" <|
            \() ->
                result (regexWith { caseInsensitive = False, multiline = True } "a+") "b\naaa"
                    |> Expect.equal (Err [ "expected input matching Regexp /^a+/" ])
        , test "multiline mode still matches at the start" <|
            \() ->
                result (regexWith { caseInsensitive = False, multiline = True } "a+") "aa\nb"
                    |> Expect.equal (Ok "aa")
        , test "all alternatives are anchored" <|
            \() ->
                result (regex "foo|bar") "xxbar"
                    |> Expect.equal (Err [ "expected input matching Regexp /^foo|bar/" ])
        , test "alternatives still match at the start" <|
            \() ->
                result (many (regex "foo|bar")) "barfoo"
                    |> Expect.equal (Ok [ "bar", "foo" ])
        , test "anchoring an alternative keeps the submatches" <|
            \() ->
                result (regexSub "(a)|(b)") "b"
                    |> Expect.equal (Ok ( "b", [ Nothing, Just "b" ] ))
        , test "explicit ^ with alternatives" <|
            \() ->
                result (regex "^x|y") "zy"
                    |> Expect.equal (Err [ "expected input matching Regexp /^x|y/" ])
        ]


errorOrderSuite : Test
errorOrderSuite =
    describe "error messages"
        [ test "nested or keeps the order of the alternatives" <|
            \() ->
                result (or (or (string "a") (string "b")) (string "c")) "x"
                    |> Expect.equal (Err [ "expected \"a\"", "expected \"b\"", "expected \"c\"" ])
        , test "or and choice agree" <|
            \() ->
                result (or (or (string "a") (string "b")) (or (string "c") (string "d"))) "x"
                    |> Expect.equal (result (choice [ string "a", string "b", string "c", string "d" ]) "x")
        , test "string does not match a later occurrence" <|
            \() ->
                result (string "abc") (String.repeat 1000 "x" ++ "abc")
                    |> Expect.equal (Err [ "expected \"abc\"" ])
        ]


unicodeSuite : Test
unicodeSuite =
    describe "positions are counted in UTF-16 code units, like String.length"
        [ test "anyChar" <|
            \() -> position C.anyChar "😀x" |> Expect.equal 2
        , test "satisfy agrees with string" <|
            \() -> position (C.anyChar |> keep C.anyChar) "😀x" |> Expect.equal (position (string "😀x") "😀x")
        , test "while" <|
            \() -> position (while (always True)) "a😀b😀" |> Expect.equal 6
        , test "skipWhile" <|
            \() -> position (skipWhile ((/=) 'x')) "😀😀x" |> Expect.equal 4
        , test "skipUntil" <|
            \() -> position (skipUntil (string "x")) "😀x" |> Expect.equal 3
        , test "location after an emoji" <|
            \() ->
                result (C.anyChar |> keep (withColumn succeed)) "😀x"
                    |> Expect.equal (Ok 2)
        , test "while returns the matched emojis" <|
            \() -> result (while ((/=) 'x')) "😀😀x" |> Expect.equal (Ok "😀😀")
        , fuzz Fuzz.string "position equals String.length of the consumed input" <|
            \s ->
                position (many C.anyChar) s |> Expect.equal (String.length s)
        ]


stackSafetySuite : Test
stackSafetySuite =
    let
        n =
            100000
    in
    describe "no stack overflow for long inputs"
        [ test "count" <|
            \() ->
                result (count n C.anyChar |> map List.length) (String.repeat n "a")
                    |> Expect.equal (Ok n)
        , test "chainr" <|
            \() ->
                result (chainr (string "+" |> onsuccess (+)) N.int) ("1" ++ String.repeat n "+1")
                    |> Expect.equal (Ok (n + 1))
        , test "chainr is right associative" <|
            \() ->
                result (chainr (string "-" |> onsuccess (-)) N.int) "10-4-3-2"
                    |> Expect.equal (Ok (10 - (4 - (3 - 2))))
        , test "chainl is left associative" <|
            \() ->
                result (chainl (string "-" |> onsuccess (-)) N.int) "10-4-3-2"
                    |> Expect.equal (Ok (10 - 4 - 3 - 2))
        , test "many" <|
            \() ->
                result (many C.anyChar |> map List.length) (String.repeat n "a")
                    |> Expect.equal (Ok n)
        , test "skipMany" <|
            \() ->
                position (skipMany C.anyChar) (String.repeat n "a")
                    |> Expect.equal n
        , test "sepBy" <|
            \() ->
                result (sepBy (string ",") N.int |> map List.length) ("1" ++ String.repeat n ",1")
                    |> Expect.equal (Ok (n + 1))
        , test "manyTill" <|
            \() ->
                result (manyTill C.anyChar end |> map List.length) (String.repeat n "a")
                    |> Expect.equal (Ok n)
        ]


terminationSuite : Test
terminationSuite =
    describe "combinators terminate on parsers that do not consume input"
        [ test "many" <|
            \() -> result (many (succeed 1)) "abc" |> Expect.equal (Ok [])
        , test "many1" <|
            \() -> result (many1 (succeed 1)) "abc" |> Expect.equal (Ok [ 1 ])
        , test "skipMany" <|
            \() -> position (skipMany (succeed 1)) "abc" |> Expect.equal 0
        , test "upTo" <|
            \() -> result (upTo 5 (succeed 1)) "abc" |> Expect.equal (Ok [])
        , test "sepBy" <|
            \() -> result (sepBy (succeed ()) (succeed 1)) "abc" |> Expect.equal (Ok [ 1 ])
        , test "manyTill" <|
            \() ->
                result (manyTill (succeed 1) (string "x")) "abc"
                    |> Expect.equal (Err [ "manyTill: parser succeeded without consuming input" ])
        , test "chainl" <|
            \() -> result (chainl (succeed (+)) (succeed 1)) "abc" |> Expect.equal (Ok 1)
        , test "chainr" <|
            \() -> result (chainr (succeed (+)) (succeed 1)) "abc" |> Expect.equal (Ok 1)
        , test "trackedLazy stops left recursion" <|
            \() -> result leftRecursive "1+1" |> Expect.equal (Ok 2)
        ]


leftRecursive : Parser s Int
leftRecursive =
    trackedLazy "left" 20 <|
        \() ->
            or
                (leftRecursive |> ignore (string "+") |> map (+) |> andMap N.int)
                N.int


{-| The example from the documentation of `upTo`.
-}
between2And4 : Parser s a -> Parser s (List a)
between2And4 p =
    count 2 p
        |> andThen (\first -> upTo 2 p |> map ((++) first))


boundedRepetitionSuite : Test
boundedRepetitionSuite =
    describe "upTo"
        [ test "upTo stops at its limit" <|
            \() -> outcome (upTo 3 (string "a")) "aaaaa" |> Expect.equal (Ok ( [ "a", "a", "a" ], 3 ))
        , test "between2And4: too few" <|
            \() -> result (between2And4 (string "a")) "a" |> Expect.equal (Err [ "expected \"a\"" ])
        , test "between2And4: minimum" <|
            \() -> outcome (between2And4 (string "a")) "aa" |> Expect.equal (Ok ( [ "a", "a" ], 2 ))
        , test "between2And4: in between" <|
            \() -> outcome (between2And4 (string "a")) "aaab" |> Expect.equal (Ok ( [ "a", "a", "a" ], 3 ))
        , test "between2And4: stops at the maximum" <|
            \() -> outcome (between2And4 (string "a")) "aaaaaaaa" |> Expect.equal (Ok ( [ "a", "a", "a", "a" ], 4 ))
        ]


outcome : Parser () a -> String -> Result Int ( a, Int )
outcome p s =
    case parse p s of
        Ok ( _, stream, v ) ->
            Ok ( v, stream.position )

        Err ( _, stream, _ ) ->
            Err stream.position


whitespaceSuite : Test
whitespaceSuite =
    describe "whitespace matches the same characters as \\s"
        [ test "ascii and unicode spaces" <|
            \() ->
                result whitespace " \t\n\u{000B}\u{000C}\u{000D}\u{00A0}\u{2003}\u{2028}\u{3000}\u{FEFF}x"
                    |> Expect.equal (Ok " \t\n\u{000B}\u{000C}\u{000D}\u{00A0}\u{2003}\u{2028}\u{3000}\u{FEFF}")
        , test "zero width space is not whitespace" <|
            \() -> result whitespace "\u{200B}" |> Expect.equal (Ok "")
        , test "whitespace1 fails without whitespace" <|
            \() -> result whitespace1 "x" |> Expect.equal (Err [ "whitespace" ])
        , test "whitespace1 does not consume on failure" <|
            \() -> position whitespace1 "x" |> Expect.equal 0
        , fuzz Fuzz.string "agrees with the regex" <|
            \s -> result whitespace s |> Expect.equal (result (regex "\\s*") s)
        ]


{-| The implementation before the optimization, as reference.
-}
referenceLocation : InputStream -> ParseLocation
referenceLocation stream =
    let
        find pos currentLine_ lines =
            case lines of
                [] ->
                    ParseLocation "" currentLine_ pos

                line :: rest ->
                    if pos > String.length line then
                        find (pos - String.length line - 1) (currentLine_ + 1) rest

                    else
                        ParseLocation line currentLine_ pos
    in
    find stream.position 0 (String.split "\n" stream.data)


streamAt : String -> Int -> InputStream
streamAt data pos =
    { data = data, input = String.dropLeft pos data, position = pos, lazyTracking = Dict.empty }


currentLocationSuite : Test
currentLocationSuite =
    let
        lineFuzzer =
            Fuzz.oneOf [ Fuzz.string, Fuzz.constant "\n", Fuzz.constant "" ]

        textFuzzer =
            Fuzz.map (String.join "\n") (Fuzz.list lineFuzzer)
    in
    describe "currentLocation"
        [ test "last line without newline" <|
            \() ->
                currentLocation (streamAt "ab\ncd" 4)
                    |> Expect.equal { source = "cd", line = 1, column = 1 }
        , test "end of input" <|
            \() ->
                currentLocation (streamAt "ab\ncd" 5)
                    |> Expect.equal { source = "cd", line = 1, column = 2 }
        , test "empty line" <|
            \() ->
                currentLocation (streamAt "ab\n\ncd" 3)
                    |> Expect.equal { source = "", line = 1, column = 0 }
        , test "beyond the end" <|
            \() ->
                currentLocation (streamAt "ab\ncd" 9)
                    |> Expect.equal (referenceLocation (streamAt "ab\ncd" 9))
        , test "negative position" <|
            \() ->
                currentLocation (streamAt "ab\ncd" -3)
                    |> Expect.equal (referenceLocation (streamAt "ab\ncd" -3))
        , fuzz2 textFuzzer (Fuzz.intRange -5 400) "agrees with the reference implementation" <|
            \data pos ->
                currentLocation (streamAt data pos)
                    |> Expect.equal (referenceLocation (streamAt data pos))
        ]
