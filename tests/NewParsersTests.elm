module NewParsersTests exposing (consumedSuite, stringsSuite, takeUntilSuite, while1Suite)

import Combine exposing (..)
import Combine.Char as C
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, fuzz2, test)


{-| Result and position, so that two parsers can be compared.
-}
outcome : Parser () a -> String -> Result Int ( a, Int )
outcome p s =
    case parse p s of
        Ok ( _, stream, v ) ->
            Ok ( v, stream.position )

        Err ( _, stream, _ ) ->
            Err stream.position


result : Parser () a -> String -> Result (List String) a
result p s =
    case parse p s of
        Ok ( _, _, v ) ->
            Ok v

        Err ( _, _, ms ) ->
            Err ms


{-| Short strings over a small alphabet, so that matches and overlaps are
frequent.
-}
smallString : Fuzzer String
smallString =
    Fuzz.list (Fuzz.oneOf (List.map Fuzz.constant [ 'a', 'b', '*', '/', '😀' ]))
        |> Fuzz.map (List.take 12 >> String.fromList)


stringsSuite : Test
stringsSuite =
    describe "strings"
        [ test "the longest match wins" <|
            \() ->
                result (strings [ "in", "int", "import" ]) "integer"
                    |> Expect.equal (Ok "int")
        , test "the order of the candidates does not matter" <|
            \() ->
                result (strings [ "int", "in" ]) "in x"
                    |> Expect.equal (Ok "in")
        , test "errors list all candidates in their order" <|
            \() ->
                result (strings [ "if", "then" ]) "else"
                    |> Expect.equal (Err [ "expected \"if\"", "expected \"then\"" ])
        , test "no candidates" <|
            \() ->
                result (strings []) "a"
                    |> Expect.equal (Err [])
        , test "the empty string matches only as last resort" <|
            \() ->
                ( result (strings [ "", "a" ]) "ab", result (strings [ "", "a" ]) "b" )
                    |> Expect.equal ( Ok "a", Ok "" )
        , test "end of input" <|
            \() ->
                result (strings [ "a" ]) ""
                    |> Expect.equal (Err [ "expected \"a\"" ])
        , test "emoji position" <|
            \() ->
                outcome (strings [ "😀", "😀😀" ]) "😀😀x"
                    |> Expect.equal (Ok ( "😀😀", 4 ))
        , fuzz2 (Fuzz.list smallString) smallString "behaves like choice over the strings sorted by length" <|
            \candidates input ->
                let
                    sorted =
                        List.sortBy (String.length >> negate) candidates
                in
                outcome (strings candidates) input
                    |> Expect.equal (outcome (choice (List.map string sorted)) input)
        ]


while1Suite : Test
while1Suite =
    describe "while1"
        [ test "matches" <|
            \() -> result (while1 Char.isDigit) "123abc" |> Expect.equal (Ok "123")
        , test "fails without consuming" <|
            \() -> outcome (while1 Char.isDigit) "abc" |> Expect.equal (Err 0)
        , test "error message" <|
            \() -> result (while1 Char.isDigit) "abc" |> Expect.equal (Err [ "could not satisfy predicate" ])
        , test "emoji position" <|
            \() -> outcome (while1 ((/=) 'x')) "😀😀x" |> Expect.equal (Ok ( "😀😀", 4 ))
        , fuzz smallString "behaves like many1 satisfy" <|
            \input ->
                outcome (while1 ((/=) '/')) input
                    |> Expect.equal (outcome (many1 (C.satisfy ((/=) '/')) |> map String.fromList) input)
        ]


takeUntilSuite : Test
takeUntilSuite =
    describe "takeUntil"
        [ test "takes the text in front of the end and consumes the end" <|
            \() ->
                outcome (string "<!--" |> keep (takeUntil "-->")) "<!-- foo -->bar"
                    |> Expect.equal (Ok ( " foo ", 12 ))
        , test "stops at the first occurrence" <|
            \() -> result (takeUntil ",") "a,b,c" |> Expect.equal (Ok "a")
        , test "fails without consuming" <|
            \() -> outcome (takeUntil "*/") "no end" |> Expect.equal (Err 0)
        , test "error message" <|
            \() ->
                result (takeUntil "*/") "no end"
                    |> Expect.equal (Err [ "takeUntil: reached end of input without finding \"*/\"" ])
        , test "regex special characters are taken literally" <|
            \() ->
                List.map (\end_ -> result (takeUntil end_) ("xa" ++ end_ ++ "y"))
                    [ ".", "*/", "a+b", "\\", "]", "$", "^", "(?:", "[a-z]", "/", "|", "{2}" ]
                    |> Expect.equal (List.repeat 12 (Ok "xa"))
        , test "a dot does not match any character" <|
            \() -> result (takeUntil ".") "abc" |> Expect.equal (Err [ "takeUntil: reached end of input without finding \".\"" ])
        , test "empty end" <|
            \() -> outcome (takeUntil "") "abc" |> Expect.equal (Ok ( "", 0 ))
        , test "emoji position" <|
            \() -> outcome (takeUntil "x") "😀x" |> Expect.equal (Ok ( "😀", 3 ))
        , fuzz2 smallString (Fuzz.oneOf (List.map Fuzz.constant [ "*/", "a", "/", "😀", "ab" ])) "behaves like manyTill anyChar on success" <|
            \input end_ ->
                case ( outcome (takeUntil end_) input, outcome (manyTill C.anyChar (string end_) |> map String.fromList) input ) of
                    ( Ok a, Ok b ) ->
                        Expect.equal a b

                    ( Err _, Err _ ) ->
                        Expect.pass

                    ( a, b ) ->
                        Expect.fail ("takeUntil: " ++ Debug.toString a ++ ", manyTill: " ++ Debug.toString b)
        ]


consumedSuite : Test
consumedSuite =
    describe "consumed"
        [ test "identifier" <|
            \() ->
                result (consumed (C.alpha |> ignore (skipWhile Char.isAlphaNum))) "abc123 = 1"
                    |> Expect.equal (Ok "abc123")
        , test "passes errors through" <|
            \() ->
                result (consumed (C.alpha |> ignore (skipWhile Char.isAlphaNum))) "1abc"
                    |> Expect.equal (Err [ "expected an alphabetic character" ])
        , test "nothing consumed" <|
            \() -> result (consumed (succeed 1)) "abc" |> Expect.equal (Ok "")
        , test "emoji" <|
            \() -> outcome (consumed (count 2 C.anyChar)) "😀😀x" |> Expect.equal (Ok ( "😀😀", 4 ))
        , fuzz smallString "equals the concatenated results" <|
            \input ->
                outcome (consumed (many (strings [ "a", "*/", "😀" ]))) input
                    |> Expect.equal (outcome (many (strings [ "a", "*/", "😀" ]) |> map String.concat) input)
        ]
