module Combine exposing
    ( Parser, InputStream, ParseLocation, ParseContext, ParseResult, ParseError, ParseOk
    , parse, runParser
    , primitive, app, lazy, trackedLazy
    , fail, succeed, string, strings, end, whitespace, whitespace1
    , regex, regexSub, regexWith, regexWithSub
    , map, onsuccess, mapError, onerror
    , andThen, andMap, sequence
    , keep, ignore, lookAhead, notFollowedBy, while, while1, consumed, or, choice, optional, maybe, many, many1, manyTill, many1Till, sepBy, sepBy1, sepEndBy, sepEndBy1, skip, skipMany, skipMany1, skipUntil, takeUntil, skipWhile, atLeast, upTo, chainl, chainr, count, between, parens, braces, brackets
    , withState, putState, modifyState, withLocation, withLine, withColumn, withSourceLine, modifyInput, putInput, modifyPosition, putPosition
    , currentLocation, currentSourceLine, currentLine, currentColumn, currentStream
    )

{-| This library provides facilities for parsing structured text data
into concrete Elm values.


## API Reference

  - [Core Types](#core-types)
  - [Running Parsers](#running-parsers)
  - [Constructing Parsers](#constructing-parsers)
  - [Parsers](#parsers)
  - [Combinators](#combinators)
      - [Transforming Parsers](#transforming-parsers)
      - [Chaining Parsers](#chaining-parsers)
      - [Parser Combinators](#parser-combinators)
      - [State Combinators](#state-combinators)


## Core Types

@docs Parser, InputStream, ParseLocation, ParseContext, ParseResult, ParseError, ParseOk


## Running Parsers

@docs parse, runParser


## Constructing Parsers

@docs primitive, app, lazy, trackedLazy


## Parsers

@docs fail, succeed, string, strings, end, whitespace, whitespace1


### Regular Expressions

@docs regex, regexSub, regexWith, regexWithSub


## Combinators


### Transforming Parsers

@docs map, onsuccess, mapError, onerror


### Chaining Parsers

@docs andThen, andMap, sequence


### Parser Combinators

@docs keep, ignore, lookAhead, notFollowedBy, while, while1, consumed, or, choice, optional, maybe, many, many1, manyTill, many1Till, sepBy, sepBy1, sepEndBy, sepEndBy1, skip, skipMany, skipMany1, skipUntil, takeUntil, skipWhile, atLeast, upTo, chainl, chainr, count, between, parens, braces, brackets


### State Combinators

@docs withState, putState, modifyState, withLocation, withLine, withColumn, withSourceLine, modifyInput, putInput, modifyPosition, putPosition


### Miscellaneous

@docs currentLocation, currentSourceLine, currentLine, currentColumn, currentStream

-}

import Dict exposing (Dict)
import Regex
import String


{-| The input stream over which `Parser`s operate.

  - `data` is the initial input provided by the user
  - `input` is the remainder after running a parse
  - `position` is the starting position of `input` in `data` after a parse
  - `lazyTracking` tracks depth per unique lazy parser ID (for trackedLazy)

-}
type alias InputStream =
    { data : String
    , input : String
    , position : Int
    , lazyTracking : Dict String Int
    }


initStream : String -> InputStream
initStream s =
    InputStream s s 0 Dict.empty


{-| A record representing the current parse location in an InputStream.

  - `source` the current line of source code
  - `line` the current line number (starting at 1)
  - `column` the current column (starting at 1)

-}
type alias ParseLocation =
    { source : String
    , line : Int
    , column : Int
    }


{-| A tuple representing the current parser state, the remaining input
stream and the parse result. Don't worry about this type unless
you're writing your own `primitive` parsers.
-}
type alias ParseContext state res =
    ( state, InputStream, ParseResult res )


{-| Running a `Parser` results in one of two states:

  - `Ok res` when the parser has successfully parsed the input
  - `Err messages` when the parser has failed with a list of error messages.

-}
type alias ParseResult res =
    Result (List String) res


{-| A tuple representing a failed parse. It contains the state after
running the parser, the remaining input stream and a list of
error messages.
-}
type alias ParseError state =
    ( state, InputStream, List String )


{-| A tuple representing a successful parse. It contains the state
after running the parser, the remaining input stream and the
result.
-}
type alias ParseOk state res =
    ( state, InputStream, res )


type alias ParseFn state res =
    state -> InputStream -> ParseContext state res


{-| The Parser type.

At their core, `Parser`s wrap functions from some `state` and an
`InputStream` to a tuple representing the new `state`, the
remaining `InputStream` and a `ParseResult res`.

-}
type Parser state res
    = Parser (ParseFn state res)



--| RecursiveParser (L.Lazy (ParseFn state res))


{-| Construct a new primitive Parser.

If you find yourself reaching for this function often consider opening
a [Github issue][issues] with the library to have your custom Parsers
included in the standard distribution.

[issues]: https://github.com/andre-dietrich/parser-combinators/issues

-}
primitive : (state -> InputStream -> ParseContext state res) -> Parser state res
primitive =
    Parser


{-| Unwrap a parser so it can be applied to a state and an input
stream. This function is useful if you want to construct your own
parsers via `primitive`. If you're using this outside of the context
of `primitive` then you might be doing something wrong so try asking
for help on the mailing list.

Here's how you would implement a greedy version of `manyTill` using
`primitive` and `app`:

    manyTill : Parser s a -> Parser s x -> Parser s (List a)
    manyTill p end =
        let
            accumulate acc state stream =
                case app end state stream of
                    ( rstate, rstream, Ok _ ) ->
                        ( rstate, rstream, Ok (List.reverse acc) )

                    _ ->
                        case app p state stream of
                            ( rstate, rstream, Ok res ) ->
                                accumulate (res :: acc) rstate rstream

                            ( estate, estream, Err ms ) ->
                                ( estate, estream, Err ms )
        in
        primitive <| accumulate []

-}
app : Parser state res -> state -> InputStream -> ParseContext state res
app (Parser inner) =
    inner


{-| Parse a string. See `runParser` if your parser needs to manage
some internal state.

    import Combine.Num exposing (int)
    import String

    parseAnInteger : String -> Result String Int
    parseAnInteger input =
      case parse int input of
        Ok (_, stream, result) ->
          Ok result

        Err (_, stream, errors) ->
          Err (String.join " or " errors)

    parseAnInteger "123"
    -- Ok 123

    parseAnInteger "abc"
    -- Err "expected an integer"

-}
parse : Parser () res -> String -> Result (ParseError ()) (ParseOk () res)
parse p =
    runParser p ()


{-| Parse a string while maintaining some internal state.

    import Combine.Num exposing (int)
    import String

    type alias Output =
      { count : Int
      , integers : List Int
      }

    statefulInt : Parse Int Int
    statefulInt =
      -- Parse an int, then increment the state and return
      -- the parsed int. It's important that we try to parse
      -- the int _first_ since modifying the state will
      -- always succeed.
      int |> ignore (modifyState ((+) 1))

    ints : Parse Int (List Int)
    ints =
      sepBy (string " ") statefulInt

    parseIntegers : String -> Result String Output
    parseIntegers input =
      case runParser ints 0 input of
        Ok (state, stream, ints) ->
          Ok { count = state, integers = ints }

        Err (state, stream, errors) ->
          Err (String.join " or " errors)

    parseIntegers ""
    -- Ok { count = 0, integers = [] }

    parseIntegers "1 2 3 45"
    -- Ok { count = 4, integers = [1, 2, 3, 45] }

    parseIntegers "1 a 2"
    -- Ok { count = 1, integers = [1] }

-}
runParser : Parser state res -> state -> String -> Result (ParseError state) (ParseOk state res)
runParser p st s =
    case app p st (initStream s) of
        ( state, stream, Ok res ) ->
            Ok ( state, stream, res )

        ( state, stream, Err ms ) ->
            Err ( state, stream, ms )


{-| Unfortunately this is not a real lazy function anymore, since this
functionality is not accessible anymore by ordinary developers. Use this
function only to avoid "bad-recursion" errors or use the following example
snippet in your code to circumvent this problem:
recursion x =
() -> recursion x

Note that `lazy` does not protect against left recursion: a parser that calls
itself again without consuming input first (`expr = lazy (\() -> expr |> ...)`)
does not hang, but crashes with a stack overflow (`RangeError: Maximum call
stack size exceeded`). Use `trackedLazy` for grammars where this can happen.

Every call evaluates the thunk again, so keep the thunk cheap: refer to
top-level parsers instead of building large combinators inside of it.

-}
lazy : (() -> Parser s a) -> Parser s a
lazy t =
    --    RecursiveParser (L.lazy (\() -> app (t ())))
    Parser <| \state stream -> app (t ()) state stream


{-| A more granular version of lazy that tracks recursion depth per unique parser ID.
This allows different lazy parsers to have independent depth tracking and custom limits.

Use this when you want fine-grained control over infinite loop detection:

    myRecursiveParser : Parser s Expr
    myRecursiveParser =
        trackedLazy "expr-parser" 50 <|
            \() ->
                choice
                    [ string "atom"
                    , myRecursiveParser  -- Each ID has its own counter
                    ]

    parse myRecursiveParser "atom"
    -- Ok "atom"

Parameters:

  - `id`: A unique identifier for this lazy parser (e.g., "json-object", "expression")
  - `maxDepth`: Maximum recursion depth before triggering infinite loop detection
  - `thunk`: The parser to evaluate lazily

-}
trackedLazy : String -> Int -> (() -> Parser s a) -> Parser s a
trackedLazy id maxDepth t =
    Parser <|
        \state stream ->
            let
                currentDepth =
                    Dict.get id stream.lazyTracking |> Maybe.withDefault 0
            in
            if currentDepth >= maxDepth then
                ( state
                , stream
                , Err [ "infinite loop detected: lazy parser '" ++ id ++ "' exceeded depth limit of " ++ String.fromInt maxDepth ]
                )

            else
                let
                    updatedTracking =
                        Dict.insert id (currentDepth + 1) stream.lazyTracking

                    streamWithTracking =
                        { stream | lazyTracking = updatedTracking }
                in
                case app (t ()) state streamWithTracking of
                    ( rstate, rstream, Ok res ) ->
                        -- Always restore depth to what it was before this call
                        let
                            finalTracking =
                                if currentDepth == 0 then
                                    Dict.remove id rstream.lazyTracking

                                else
                                    Dict.insert id currentDepth rstream.lazyTracking
                        in
                        ( rstate, { rstream | lazyTracking = finalTracking }, Ok res )

                    ( estate, estream, Err ms ) ->
                        -- Restore depth on error too
                        let
                            finalTracking =
                                if currentDepth == 0 then
                                    Dict.remove id estream.lazyTracking

                                else
                                    Dict.insert id currentDepth estream.lazyTracking
                        in
                        ( estate, { estream | lazyTracking = finalTracking }, Err ms )



-- State management
-- ----------------


{-| Get the parser's state and pipe it into a parser.

    let
        parser =
            string "a"
                |> keep (withState (\state -> succeed state))
    in
    parse parser "a"
    -- Ok ()

-}
withState : (s -> Parser s a) -> Parser s a
withState f =
    Parser <|
        \state stream ->
            app (f state) state stream


{-| Replace the parser's state.

    let
        parser =
            string "a"
                |> andThen (\_ -> putState 42)
    in
    parse parser "a"
    -- Ok ()

-}
putState : s -> Parser s ()
putState state =
    Parser <|
        \_ stream ->
            ( state, stream, Ok () )


{-| Modify the parser's state.

    let
        parser =
            string "a"
                |> andThen (\_ -> modifyState ((+) 1))
    in
    parse parser "a"
    -- Ok ()

-}
modifyState : (s -> s) -> Parser s ()
modifyState f =
    Parser <|
        \state stream ->
            ( f state, stream, Ok () )


{-| Get the current position in the input stream and pipe it into a parser.

    let
        parser =
            string "a"
                |> keep (withLocation succeed)
    in
    parse parser "a"
    -- Ok { source = "a", line = 0, column = 1 }

-}
withLocation : (ParseLocation -> Parser s a) -> Parser s a
withLocation f =
    Parser <|
        \state stream ->
            app (f <| currentLocation stream) state stream


{-| Get the current line and pipe it into a parser.

    let
        parser =
            string "a\n\n"
                |> keep (withLine succeed)
    in
    parse parser "a\n\n"
    -- Ok 2

-}
withLine : (Int -> Parser s a) -> Parser s a
withLine f =
    Parser <|
        \state stream ->
            app (f <| currentLine stream) state stream


{-| Get the current column and pipe it into a parser.

    let
        parser =
            string "aaa"
                |> keep (withColumn succeed)
    in
    parse parser "aaa"
    -- Ok 3

-}
withColumn : (Int -> Parser s a) -> Parser s a
withColumn f =
    Parser <|
        \state stream ->
            app (f <| currentColumn stream) state stream


{-| Get the current InputStream and pipe it into a parser,
only for debugging purposes ...

    let
        parser =
            string "a"
                |> keep (withSourceLine succeed)
    in
    parse parser "abc"
    -- Ok "bc"

-}
withSourceLine : (String -> Parser s a) -> Parser s a
withSourceLine f =
    Parser <|
        \state stream ->
            app (f <| currentSourceLine stream) state stream


{-| Get the current `(line, column)` in the input stream.
-}
currentLocation : InputStream -> ParseLocation
currentLocation stream =
    let
        -- only the newlines in front of the position are relevant, the current
        -- line is then taken from its start with an anchored regex
        ( line, lastNewline ) =
            String.indexes "\n" (String.left stream.position stream.data)
                |> List.foldl (\i ( n, _ ) -> ( n + 1, i )) ( 0, -1 )
    in
    if stream.position > String.length stream.data then
        ParseLocation "" (line + 1) (stream.position - String.length stream.data - 1)

    else
        ParseLocation
            (restOfLine (String.dropLeft (lastNewline + 1) stream.data))
            line
            (stream.position - lastNewline - 1)


restOfLine : String -> String
restOfLine s =
    case Regex.findAtMost 1 restOfLineRegex s of
        [ match ] ->
            match.match

        _ ->
            ""


restOfLineRegex : Regex.Regex
restOfLineRegex =
    Regex.fromString "^[^\\n]*" |> Maybe.withDefault Regex.never


{-| Get the current source line in the input stream.
-}
currentSourceLine : InputStream -> String
currentSourceLine =
    currentLocation >> .source


{-| Get the current line in the input stream.
-}
currentLine : InputStream -> Int
currentLine =
    currentLocation >> .line


{-| Get the current column in the input stream.
-}
currentColumn : InputStream -> Int
currentColumn =
    currentLocation >> .column


{-| Get the current string stream. That might be useful for applying memorization.
-}
currentStream : InputStream -> String
currentStream =
    .input


{-| Modify the parser's current InputStream input (String).

    parse
        (modifyInput String.toUpper
            |> keep (many (string "A"))
        )
        "aaa"
    -- Ok ["A","A","A"]

-}
modifyInput : (String -> String) -> Parser s ()
modifyInput f =
    Parser <|
        \state stream ->
            ( state, { stream | input = f stream.input }, Ok () )


{-| Replace the remaining input with a new string.

    parse
        (string "a"
            |> ignore (putInput "AAA")
            |> keep (many (string "A"))
        )
        "aaa"
    -- Ok ["A","A","A"]

-}
putInput : String -> Parser s ()
putInput i =
    modifyInput (always i)


{-| Modify the parser's InputStream position (Int).

    let
        parser =
            string "a"
                |> ignore (modifyPosition ((+) 1000))
    in
    parse parser "a"
    -- Ok ((),{ data = "a", input = "", position = 1001 },"a")

-}
modifyPosition : (Int -> Int) -> Parser s ()
modifyPosition f =
    Parser <|
        \state stream ->
            ( state, { stream | position = f stream.position }, Ok () )


{-| Replace the parser position.

    let
        parser =
            string "a"
                |> ignore (putPosition 1000)
    in
    parse parser "a"
    -- Ok ((),{ data = "a", input = "", position = 1000 },"a")

-}
putPosition : Int -> Parser s ()
putPosition i =
    modifyPosition (always i)



-- Transformers
-- ------------


{-| Transform the result of a parser.

    let
      parser =
        string "a"
          |> map String.toUpper
    in
      parse parser "a"
      -- Ok "A"

-}
map : (a -> b) -> Parser s a -> Parser s b
map f p =
    Parser <|
        \state stream ->
            case app p state stream of
                ( rstate, rstream, Ok res ) ->
                    ( rstate, rstream, Ok (f res) )

                ( estate, estream, Err ms ) ->
                    ( estate, estream, Err ms )


{-| Transform the error of a parser.

    let
      parser =
        string "a"
          |> mapError (always ["bad input"])
    in
      parse parser b
      -- Err ["bad input"]

-}
mapError : (List String -> List String) -> Parser s a -> Parser s a
mapError f p =
    Parser <|
        \state stream ->
            case app p state stream of
                ( estate, estream, Err ms ) ->
                    ( estate, estream, Err (f ms) )

                ok ->
                    ok


{-| Sequence two parsers, passing the result of the first parser to a
function that returns the second parser. The value of the second
parser is returned on success.

    import Combine.Num exposing (int)

    choosy : Parser s String
    choosy =
      let
        createParser n =
          if n % 2 == 0 then
            string " is even"
          else
            string " is odd"
      in
        int
          |> andThen createParser

    parse choosy "1 is odd"
    -- Ok " is odd"

    parse choosy "2 is even"
    -- Ok " is even"

    parse choosy "1 is even"
    -- Err ["expected \" is odd\""]

-}
andThen : (a -> Parser s b) -> Parser s a -> Parser s b
andThen f p =
    Parser <|
        \state stream ->
            case app p state stream of
                ( rstate, rstream, Ok res ) ->
                    app (f res) rstate rstream

                ( estate, estream, Err ms ) ->
                    ( estate, estream, Err ms )


{-| Sequence two parsers.

    import Combine.Num exposing (int)

    plus : Parser s String
    plus = string "+"

    sum : Parser s Int
    sum =
      int
        |> map (+)
        |> andMap (plus |> keep int)

    parse sum "1+2"
    -- Ok 3

-}
andMap : Parser s a -> Parser s (a -> b) -> Parser s b
andMap rp lp =
    Parser <|
        \state stream ->
            case app lp state stream of
                ( lstate, lstream, Ok f ) ->
                    case app rp lstate lstream of
                        ( rstate, rstream, Ok x ) ->
                            ( rstate, rstream, Ok (f x) )

                        ( estate, estream, Err ms ) ->
                            ( estate, estream, Err ms )

                ( estate, estream, Err ms ) ->
                    ( estate, estream, Err ms )


{-| Run a list of parsers in sequence, accumulating the results. The
main use case for this parser is when you want to combine a list of
parsers into a single, top-level, parser. For most use cases, you'll
want to use one of the other combinators instead.

    parse (sequence [string "a", string "b"]) "ab"
    -- Ok ["a", "b"]

    parse (sequence [string "a", string "b"]) "ac"
    -- Err ["expected \"b\""]

-}
sequence : List (Parser s a) -> Parser s (List a)
sequence parsers =
    let
        accumulate acc ps state stream =
            case ps of
                [] ->
                    ( state, stream, Ok (List.reverse acc) )

                x :: xs ->
                    case app x state stream of
                        ( rstate, rstream, Ok res ) ->
                            accumulate (res :: acc) xs rstate rstream

                        ( estate, estream, Err ms ) ->
                            ( estate, estream, Err ms )
    in
    Parser <|
        \state stream ->
            accumulate [] parsers state stream



-- Combinators
-- -----------


{-| Fail without consuming any input.

    parse (fail "some error") "hello"
    -- Err ["some error"]

-}
fail : String -> Parser s a
fail m =
    Parser <|
        \state stream ->
            ( state, stream, Err [ m ] )


emptyErr : Parser s a
emptyErr =
    Parser <|
        \state stream ->
            ( state, stream, Err [] )


{-| Return a value without consuming any input.

    parse (succeed 1) "a"
    -- Ok 1

-}
succeed : a -> Parser s a
succeed res =
    Parser <|
        \state stream ->
            ( state, stream, Ok res )


{-| Parse an exact string match.

    parse (string "hello") "hello world"
    -- Ok "hello"

    parse (string "hello") "goodbye"
    -- Err ["expected \"hello\""]

-}
string : String -> Parser s String
string s =
    let
        len =
            String.length s

        error =
            [ "expected \"" ++ s ++ "\"" ]
    in
    Parser <|
        \state stream ->
            -- String.startsWith is implemented as `indexOf(s) === 0`, which
            -- searches the whole remaining input whenever it does not match
            if String.left len stream.input == s then
                ( state
                , { stream | input = String.dropLeft len stream.input, position = stream.position + len }
                , Ok s
                )

            else
                ( state, stream, Err error )


{-| Parse one of several exact strings. If more than one of them matches,
the longest one wins, so a keyword cannot be cut off by a shorter one that
is a prefix of it:

    parse (strings [ "in", "int", "import" ]) "integer"
    -- Ok "int"

    parse (choice [ string "in", string "int" ]) "integer"
    -- Ok "in"

    parse (strings [ "if", "then" ]) "else"
    -- Err ["expected \"if\"", "expected \"then\""]

This is also faster than `choice (List.map string ...)`, since only the
strings that start with the next character of the input are compared.

-}
strings : List String -> Parser s String
strings candidates =
    let
        -- longest first, grouped by their first character
        byFirstChar =
            candidates
                |> List.sortBy (String.length >> negate)
                |> List.foldr
                    (\str dict ->
                        case String.uncons str of
                            Just ( c, _ ) ->
                                Dict.update c (Maybe.withDefault [] >> (::) str >> Just) dict

                            Nothing ->
                                dict
                    )
                    Dict.empty

        -- an empty string always matches, but only as the last resort
        fallback =
            if List.member "" candidates then
                Ok ""

            else
                Err (List.map (\str -> "expected \"" ++ str ++ "\"") candidates)

        firstMatch input list =
            case list of
                str :: rest ->
                    if String.left (String.length str) input == str then
                        Ok str

                    else
                        firstMatch input rest

                [] ->
                    fallback
    in
    Parser <|
        \state stream ->
            let
                found =
                    case String.uncons stream.input of
                        Just ( c, _ ) ->
                            firstMatch stream.input (Dict.get c byFirstChar |> Maybe.withDefault [])

                        Nothing ->
                            fallback
            in
            case found of
                Ok str ->
                    let
                        len =
                            String.length str
                    in
                    ( state
                    , { stream | input = String.dropLeft len stream.input, position = stream.position + len }
                    , Ok str
                    )

                Err ms ->
                    ( state, stream, Err ms )


{-| Parse a Regex match.

Regular expressions must match from the beginning of the input and their
subgroups are ignored. A `^` is added implicitly to the beginning of
every pattern unless one already exists.

    parse (regex "a+") "aaaaab"
    -- Ok "aaaaa"

    parse (regex "a+") "Aaaaab"
    -- Err ["expected input matching Regexp /^a+/"]

Use `regexWith` for more options on allowing case-insensitive or multiline.

Alternatives are anchored as a whole, `regex "if|then"` behaves like
`regex "(?:if|then)"`. Avoid nested quantifiers such as `(a+)+`: on input
that almost matches, JavaScript's backtracking regex engine needs
exponential time, which blocks the program and cannot be interrupted by
the parser.

-}
regex : String -> Parser s String
regex =
    regexer Regex.fromString .match >> Parser


{-| Parse a Regex match.

Same as regex, but returns also submatches as the second parameter in
the result tuple.

    parse (regexSub "(a+)(b+)") "aaaaab"
    -- Ok ("aaaaab",[Just "aaaaa",Just "b"])

    parse (regexSub "(?:a+)(b+)") "aaaaab"
    -- Ok ("aaaaab",[Just "b"])

-}
regexSub : String -> Parser s ( String, List (Maybe String) )
regexSub =
    regexer Regex.fromString
        (\m -> ( m.match, m.submatches ))
        >> Parser


{-| Parse a Regex match.

Since, Regex now also has support for more parameters, this option was
included into this package. Call `regexWith` with two additional parameters:
`caseInsensitive` and `multiline`, which allow you to tweak your expression.
The rest is as follows. Regular expressions must match from the beginning
of the input and their subgroups are ignored. A `^` is added implicitly to
the beginning of every pattern unless one already exists.

    parse
        (regexWith
            { caseInsensitive = True, multiline = False }
            "a+"
        )
        "AaaAAaAab"
    -- Ok "AaaAAaAa"

    parse
        (regexWith
            { caseInsensitive = False, multiline = False }
            "a+"
        )
        "AaaAAaAab"
    -- Err ["expected input matching Regexp /^a+/"]

-}
regexWith : { caseInsensitive : Bool, multiline : Bool } -> String -> Parser s String
regexWith { caseInsensitive, multiline } =
    regexer
        (Regex.fromStringWith
            { caseInsensitive = caseInsensitive
            , multiline = multiline
            }
        )
        .match
        >> Parser


{-| Parse a Regex match.

Similar to `regexWith`, but a tuple is returned, with a list of additional
submatches.
The rest is as follows. Regular expressions must match from the beginning
of the input and their subgroups are ignored. A `^` is added implicitly to
the beginning of every pattern unless one already exists.

    parse
        (regexWithSub
            { caseInsensitive = True, multiline = False }
            "a+"
        )
        "AaaAAaAab"
    -- Ok ("aaaAAaAa", [])

    parse
        (regexWithSub
            { caseInsensitive = False, multiline = False }
            "a+"
        )
        "AaaAAaAab"
    -- Err ["expected input matching Regexp /^a+/"]

-}
regexWithSub : { caseInsensitive : Bool, multiline : Bool } -> String -> Parser s ( String, List (Maybe String) )
regexWithSub { caseInsensitive, multiline } =
    regexer
        (Regex.fromStringWith
            { caseInsensitive = caseInsensitive
            , multiline = multiline
            }
        )
        (\m -> ( m.match, m.submatches ))
        >> Parser


regexer :
    (String -> Maybe Regex.Regex)
    -> (Regex.Match -> res)
    -> String
    -> (state -> InputStream -> ( state, InputStream, ParseResult res ))
regexer input output pat =
    let
        pattern =
            if String.startsWith "^" pat then
                pat

            else
                "^" ++ pat

        -- `^a|b` would only anchor the first alternative, wrapping the pattern
        -- into a non-capturing group anchors all of them (submatches are kept)
        compiledRegex =
            (if String.contains "|" pattern then
                "^(?:" ++ String.dropLeft 1 pattern ++ ")"

             else
                pattern
            )
                |> input
                |> Maybe.withDefault Regex.never

        error =
            [ "expected input matching Regexp /" ++ pattern ++ "/" ]
    in
    \state stream ->
        case Regex.findAtMost 1 compiledRegex stream.input of
            [ match ] ->
                -- in multiline mode `^` also matches at the beginning of every line
                if match.index == 0 then
                    let
                        len =
                            String.length match.match
                    in
                    ( state
                    , { stream | input = String.dropLeft len stream.input, position = stream.position + len }
                    , Ok (output match)
                    )

                else
                    ( state, stream, Err error )

            _ ->
                ( state, stream, Err error )


{-| Consume input while the predicate matches.

    parse (while ((/=) ' ')) "test 123"
    -- Ok "test"

-}
while : (Char -> Bool) -> Parser s String
while pred =
    Parser <|
        \state stream ->
            let
                rest =
                    dropWhile pred stream.input

                -- the length difference also counts emojis (two UTF-16 code
                -- units) correctly, without computing it for every character
                len =
                    String.length stream.input - String.length rest
            in
            ( state
            , { stream | input = rest, position = stream.position + len }
            , Ok (String.left len stream.input)
            )


{-| Like `while`, but at least one character has to match. Much faster than
`many1 (satisfy pred) |> map String.fromList`, since no list is built.

    parse (while1 Char.isDigit) "123abc"
    -- Ok "123"

    parse (while1 Char.isDigit) "abc"
    -- Err ["could not satisfy predicate"]

-}
while1 : (Char -> Bool) -> Parser s String
while1 pred =
    Parser <|
        \state stream ->
            let
                rest =
                    dropWhile pred stream.input

                len =
                    String.length stream.input - String.length rest
            in
            if len == 0 then
                ( state, stream, Err [ "could not satisfy predicate" ] )

            else
                ( state
                , { stream | input = rest, position = stream.position + len }
                , Ok (String.left len stream.input)
                )


{-| Drops the longest prefix whose characters satisfy `pred`.
-}
dropWhile : (Char -> Bool) -> String -> String
dropWhile pred input =
    case String.uncons input of
        Just ( c, rest ) ->
            if pred c then
                dropWhile pred rest

            else
                input

        Nothing ->
            input


{-| Characters outside of the Basic Multilingual Plane (emojis, ...) occupy
two UTF-16 code units, `String.length` and `String.dropLeft` count both.
-}
charWidth : Char -> Int
charWidth c =
    if Char.toCode c > 0xFFFF then
        2

    else
        1


{-| Fail when the input is not empty.

    parse end ""
    -- Ok ()

    parse end "a"
    -- Err ["expected end of input"]

-}
end : Parser s ()
end =
    Parser <|
        \state stream ->
            if stream.input == "" then
                ( state, stream, Ok () )

            else
                ( state, stream, Err [ "expected end of input" ] )


{-| Apply a parser without consuming any input on success.

    parse (lookAhead (string "a") |> keep (string "a")) "a"
    -- Ok "a"

    parse (lookAhead (string "a") |> keep (string "b")) "a"
    -- Err ["expected \"b\""]

    parse (lookAhead (string "a") |> keep (string "b")) "b"
    -- Err ["expected \"a\""]

-}
lookAhead : Parser s a -> Parser s a
lookAhead p =
    Parser <|
        \state stream ->
            case app p state stream of
                ( rstate, _, Ok res ) ->
                    ( rstate, stream, Ok res )

                err ->
                    err


{-| Succeed if the given parser fails, without consuming any input.
Useful for implementing keyword parsers that don't match prefixes.

    import Combine.Char exposing (alphaNum)

    keyword : String -> Parser s String
    keyword kw =
        string kw
            |> ignore (notFollowedBy alphaNum)

    parse (keyword "if") "if x"
    -- Ok "if"

    parse (keyword "if") "iffy"
    -- Err ["unexpected alphanumeric character"]

-}
notFollowedBy : Parser s a -> Parser s ()
notFollowedBy p =
    Parser <|
        \state stream ->
            case app p state stream of
                ( _, _, Ok _ ) ->
                    ( state, stream, Err [ "unexpected input" ] )

                ( _, _, Err _ ) ->
                    ( state, stream, Ok () )


{-| Choose between two parsers.

    parse (or (string "a") (string "b")) "a"
    -- Ok "a"

    parse (or (string "a") (string "b")) "b"
    -- Ok "b"

    parse (or (string "a") (string "b")) "c"
    -- Err ["expected \"a\"", "expected \"b\""]

-}
or : Parser s a -> Parser s a -> Parser s a
or lp rp =
    Parser <|
        \state stream ->
            case app lp state stream of
                ( _, _, Ok _ ) as res ->
                    res

                ( _, _, Err lms ) ->
                    case app rp state stream of
                        ( _, _, Ok _ ) as res ->
                            res

                        ( _, _, Err rms ) ->
                            ( state, stream, Err (lms ++ rms) )


{-| Choose between a list of parsers.

    parse (choice [string "a", string "b"]) "a"
    -- Ok "a"

    parse (choice [string "a", string "b"]) "b"
    -- Ok "b"

-}
choice : List (Parser s a) -> Parser s a
choice xs =
    let
        tryParsers parsers state stream errors =
            case parsers of
                [] ->
                    ( state, stream, Err (List.reverse errors) )

                p :: rest ->
                    case app p state stream of
                        ( _, _, Ok _ ) as res ->
                            res

                        ( _, _, Err ms ) ->
                            tryParsers rest state stream (List.foldl (::) errors ms)
    in
    Parser <| \state stream -> tryParsers xs state stream []


{-| Return a default value when the given parser fails.

    letterA : Parser s String
    letterA = optional "a" (string "a")

    parse letterA "a"
    -- Ok "a"

    parse letterA "b"
    -- Ok "a"

-}
optional : a -> Parser s a -> Parser s a
optional res p =
    Parser <|
        \state stream ->
            case app p state stream of
                ( _, _, Err _ ) ->
                    ( state, stream, Ok res )

                ok ->
                    ok


{-| Wrap the return value into a `Maybe`. Returns `Nothing` on failure.

    parse (maybe (string "a")) "a"
    -- Ok (Just "a")

    parse (maybe (string "a")) "b"
    -- Ok Nothing

-}
maybe : Parser s a -> Parser s (Maybe a)
maybe p =
    Parser <|
        \state stream ->
            case app p state stream of
                ( rstate, rstream, Ok res ) ->
                    ( rstate, rstream, Ok (Just res) )

                _ ->
                    ( state, stream, Ok Nothing )


{-| Apply a parser zero or more times and return a list of the results.

    parse (many (string "a")) "aaab"
    -- Ok ["a", "a", "a"]

    parse (many (string "a")) "bbbb"
    -- Ok []

    parse (many (string "a")) ""
    -- Ok []

-}
many : Parser s a -> Parser s (List a)
many p =
    Parser <|
        \state stream ->
            manyHelp p [] state stream


{-| Applies `p` until it fails or stops consuming input. A parser that
succeeds without consuming would otherwise loop forever.
-}
manyHelp : Parser s a -> List a -> s -> InputStream -> ParseContext s (List a)
manyHelp p acc state stream =
    case app p state stream of
        ( rstate, rstream, Ok res ) ->
            if stream.input == rstream.input then
                ( rstate, rstream, Ok (List.reverse acc) )

            else
                manyHelp p (res :: acc) rstate rstream

        _ ->
            ( state, stream, Ok (List.reverse acc) )


{-| Parse at least one result.

    parse (many1 (string "a")) "a"
    -- Ok ["a"]

    parse (many1 (string "a")) ""
    -- Err ["expected \"a\""]

-}
many1 : Parser s a -> Parser s (List a)
many1 p =
    Parser <|
        \state stream ->
            case app p state stream of
                ( rstate, rstream, Ok res ) ->
                    manyHelp p [ res ] rstate rstream

                ( estate, estream, Err ms ) ->
                    ( estate, estream, Err ms )


{-| Apply the first parser zero or more times until second parser
succeeds. On success, the list of the first parser's results is returned.

    parse
        (string "<!--"
            |> keep
                (manyTill anyChar (string "-->")
                    |> map String.fromList
                )
        )
        "<!--foo bar-->"
    -- Ok "foo bar"

-}
manyTill : Parser s a -> Parser s end -> Parser s (List a)
manyTill p end_ =
    let
        accumulate acc state stream =
            case app end_ state stream of
                ( rstate, rstream, Ok _ ) ->
                    ( rstate, rstream, Ok (List.reverse acc) )

                ( estate, estream, Err ms ) ->
                    case app p state stream of
                        ( rstate, rstream, Ok res ) ->
                            if stream.input == rstream.input then
                                ( estate, estream, Err [ "manyTill: parser succeeded without consuming input" ] )

                            else
                                accumulate (res :: acc) rstate rstream

                        ( _, _, Err _ ) ->
                            if stream.input == "" then
                                ( estate, estream, Err [ "manyTill: reached end of input without finding end parser" ] )

                            else
                                ( estate, estream, Err ms )
    in
    Parser (accumulate [])


{-| Apply the first parser one or more times until second parser
succeeds. On success, the list of the first parser's results is returned.

    parse
        (string "<!--"
            |> keep
                (many1Till anyChar (string "-->")
                    |> map String.fromList
                )
        )
        "<!--foo bar-->"
    -- Ok "foo bar"

-}
many1Till : Parser s a -> Parser s end -> Parser s (List a)
many1Till p =
    manyTill p
        >> andThen
            (\result ->
                case result of
                    [] ->
                        fail "not enough results"

                    _ ->
                        succeed result
            )


{-| Parser zero or more occurrences of one parser separated by another.

    parse (sepBy (string ",") (string "a")) "b"
    -- Ok []

    parse (sepBy (string ",") (string "a")) "a,a,a"
    -- Ok ["a", "a", "a"]

    parse (sepBy (string ",") (string "a")) "a,a,b"
    -- Ok ["a", "a"]

-}
sepBy : Parser s x -> Parser s a -> Parser s (List a)
sepBy sep p =
    or (sepBy1 sep p) (succeed [])


{-| Parse one or more occurrences of one parser separated by another.

    parse (sepBy1 (string ",") (string "a")) ""
    -- Err ["expected \"a\""]

    parse (sepBy1 (string ",") (string "a")) "a"
    -- Ok ["a"]

    parse (sepBy1 (string ",") (string "a")) "a,"
    -- Ok ["a"]

-}
sepBy1 : Parser s x -> Parser s a -> Parser s (List a)
sepBy1 sep p =
    map (::) p |> andMap (many (sep |> keep p))


{-| Parse zero or more occurrences of one parser separated and
optionally ended by another.

    parse (sepEndBy (string ",") (string "a")) "a,a,a,"
    -- Ok ["a", "a", "a"]

-}
sepEndBy : Parser s x -> Parser s a -> Parser s (List a)
sepEndBy sep p =
    or (sepEndBy1 sep p) (succeed [])


{-| Parse one or more occurrences of one parser separated and
optionally ended by another.

    parse (sepEndBy1 (string ",") (string "a")) ""
    -- Err ["expected \"a\""]

    parse (sepEndBy1 (string ",") (string "a")) "a"
    -- Ok ["a"]

    parse (sepEndBy1 (string ",") (string "a")) "a,"
    -- Ok ["a"]

-}
sepEndBy1 : Parser s x -> Parser s a -> Parser s (List a)
sepEndBy1 sep p =
    sepBy1 sep p |> ignore (maybe sep)


{-| Apply a parser and skip its result.

    parse (skip (string "a")) "a"
    -- Ok ()

    parse (skip (string "a")) "b"
    -- Err ["expected \"a\""]

-}
skip : Parser s x -> Parser s ()
skip p =
    p |> onsuccess ()


{-| Apply a parser and skip its result many times.

    parse (skipMany (string "a")) "aaa"
    -- Ok ()

    parse (skipMany (string "a")) ""
    -- Ok ()

-}
skipMany : Parser s x -> Parser s ()
skipMany p =
    let
        accumulate state stream =
            case app p state stream of
                ( rstate, rstream, Ok _ ) ->
                    if stream.input == rstream.input then
                        ( rstate, rstream, Ok () )

                    else
                        accumulate rstate rstream

                _ ->
                    ( state, stream, Ok () )
    in
    Parser accumulate


{-| Apply a parser and skip its result at least once.

    parse (skipMany1 (string "a")) "a"
    -- Ok ()

    parse (skipMany1 (string "a")) ""
    -- Err ["expected \"a\""]

-}
skipMany1 : Parser s x -> Parser s ()
skipMany1 p =
    many1 (skip p) |> onsuccess ()


{-| Skip input until the given parser succeeds, the input matched by this
parser is consumed too. This is similar to `manyTill`, but more efficient as
it doesn't accumulate results. If the end is a fixed string, `takeUntil` is
much faster.

    parse
        (skipUntil (string "-->") |> keep (string "rest"))
        "some text here-->rest"
    -- Ok "rest"

-}
skipUntil : Parser s end -> Parser s ()
skipUntil end_ =
    let
        accumulate state stream =
            case app end_ state stream of
                ( rstate, rstream, Ok _ ) ->
                    ( rstate, rstream, Ok () )

                ( estate, estream, Err _ ) ->
                    case String.uncons stream.input of
                        Just ( c, rest ) ->
                            accumulate state { stream | input = rest, position = stream.position + charWidth c }

                        Nothing ->
                            ( estate, estream, Err [ "skipUntil: reached end of input without finding end parser" ] )
    in
    Parser accumulate


{-| Take everything up to the first occurrence of the given string. The
string itself is consumed too, but it is not part of the result. Fails
without consuming any input, if the string does not occur.

    parse (string "<!--" |> keep (takeUntil "-->")) "<!-- foo -->bar"
    -- Ok " foo "

    parse (takeUntil "*/") "no end"
    -- Err ["takeUntil: reached end of input without finding \"*/\""]

This replaces `manyTill anyChar (string "-->") |> map String.fromList` and is
much faster, the search is done by the browser's native string search.

-}
takeUntil : String -> Parser s String
takeUntil end_ =
    let
        search =
            Regex.fromString (Regex.replace regexSpecialChars (\m -> "\\" ++ m.match) end_)
                |> Maybe.withDefault Regex.never

        error =
            [ "takeUntil: reached end of input without finding \"" ++ end_ ++ "\"" ]
    in
    Parser <|
        \state stream ->
            case Regex.findAtMost 1 search stream.input of
                [ match ] ->
                    let
                        len =
                            match.index + String.length end_
                    in
                    ( state
                    , { stream | input = String.dropLeft len stream.input, position = stream.position + len }
                    , Ok (String.left match.index stream.input)
                    )

                _ ->
                    ( state, stream, Err error )


regexSpecialChars : Regex.Regex
regexSpecialChars =
    Regex.fromString "[.*+?^${}()|[\\]\\\\/]" |> Maybe.withDefault Regex.never


{-| Run a parser and return the part of the input it consumed, instead of its
result. Combined with the `skip...` parsers, this checks the structure of a
token without building lists of characters or strings:

    import Combine.Char exposing (alpha, alphaNum)

    identifier : Parser s String
    identifier =
        consumed (alpha |> ignore (skipWhile Char.isAlphaNum))

    parse identifier "abc123 = 1"
    -- Ok "abc123"

If the parser modifies the input with `modifyInput` or `putInput`, the result
is the prefix of the original input by which the input got shorter.

-}
consumed : Parser s a -> Parser s String
consumed p =
    Parser <|
        \state stream ->
            case app p state stream of
                ( rstate, rstream, Ok _ ) ->
                    ( rstate
                    , rstream
                    , Ok (String.left (String.length stream.input - String.length rstream.input) stream.input)
                    )

                ( estate, estream, Err ms ) ->
                    ( estate, estream, Err ms )


{-| Skip characters while the predicate holds.
More efficient than `skipMany (satisfy pred)` as it doesn't build a list.

    import Combine.Char exposing (space)

    parse
        (skipWhile ((==) ' ') |> keep (string "hello"))
        "    hello"
    -- Ok "hello"

    parse (skipWhile Char.isDigit) "123abc"
    -- Ok ()

-}
skipWhile : (Char -> Bool) -> Parser s ()
skipWhile pred =
    Parser <|
        \state stream ->
            let
                rest =
                    dropWhile pred stream.input
            in
            ( state
            , { stream | input = rest, position = stream.position + String.length stream.input - String.length rest }
            , Ok ()
            )


{-| Parse at least `n` occurrences of a parser.
Complements `upTo` for full bounded repetition control.

    parse (atLeast 2 (string "a")) "aaa"
    -- Ok ["a", "a", "a"]

    parse (atLeast 2 (string "a")) "a"
    -- Err ["expected \"a\""]

    parse (atLeast 0 (string "a")) "b"
    -- Ok []

-}
atLeast : Int -> Parser s a -> Parser s (List a)
atLeast n p =
    count n p
        |> andThen (\initial -> many p |> map (\rest -> initial ++ rest))


{-| Parse at most `n` occurrences of a parser.
Similar to `many`, but with an upper limit.

    parse (upTo 3 (string "a")) "aaaaa"
    -- Ok ["a", "a", "a"]

    parse (upTo 3 (string "a")) "aa"
    -- Ok ["a", "a"]

    parse (upTo 3 (string "a")) "b"
    -- Ok []

Combine with `count` for bounded repetition (`atLeast` does not work here,
it is greedy and would consume all occurrences):

    between2And4 : Parser s a -> Parser s (List a)
    between2And4 p =
        count 2 p
            |> andThen (\first -> upTo 2 p |> map ((++) first))

    parse (between2And4 (string "a")) "aaaaa"
    -- Ok ["a", "a", "a", "a"]

    parse (between2And4 (string "a")) "a"
    -- Err ["expected \"a\""]

-}
upTo : Int -> Parser s a -> Parser s (List a)
upTo n p =
    let
        accumulate remaining acc state stream =
            if remaining <= 0 then
                ( state, stream, Ok (List.reverse acc) )

            else
                case app p state stream of
                    ( rstate, rstream, Ok res ) ->
                        if stream.input == rstream.input then
                            ( rstate, rstream, Ok (List.reverse acc) )

                        else
                            accumulate (remaining - 1) (res :: acc) rstate rstream

                    _ ->
                        ( state, stream, Ok (List.reverse acc) )
    in
    Parser <| \state stream -> accumulate n [] state stream


{-| Parse one or more occurrences of `p` separated by `op`, recursively
apply all functions returned by `op` to the values returned by `p`. See
the `examples/Calc.elm` file for an example.

    let
        addop =
            choice
                [ string "+" |> onsuccess (+)
                , string "-" |> onsuccess (-)
                ]
    in
    parse (chainl addop int) "1+2+3"
    -- Ok 6

    parse (chainl addop int) "1+2+3-X"
    -- Ok 6

-}
chainl : Parser s (a -> a -> a) -> Parser s a -> Parser s a
chainl op p =
    let
        accumulate x state stream =
            case app op state stream of
                ( opstate, opstream, Ok f ) ->
                    if stream.input == opstream.input then
                        ( opstate, opstream, Ok x )

                    else
                        case app p opstate opstream of
                            ( pstate, pstream, Ok y ) ->
                                if opstream.input == pstream.input then
                                    ( pstate, pstream, Ok x )

                                else
                                    accumulate (f x y) pstate pstream

                            ( estate, estream, Err ms ) ->
                                ( estate, estream, Err ms )

                ( _, _, Err _ ) ->
                    ( state, stream, Ok x )
    in
    Parser <|
        \state stream ->
            case app p state stream of
                ( pstate, pstream, Ok x ) ->
                    accumulate x pstate pstream

                ( estate, estream, Err ms ) ->
                    ( estate, estream, Err ms )


{-| Similar to `chainl` but functions of `op` are applied in
right-associative order to the values of `p`. See the
`examples/Python.elm` file for a usage example.

    let
        addop =
            choice
                [ string "+" |> onsuccess (+)
                , string "-" |> onsuccess (-)
                ]
    in

    parse (chainr addop int) "1-2-3"  -- 1 - (2 - 3)
    -- Ok 2

    parse (chainl addop int) "1-2-3"  -- 1 - 2 - 3
    -- Ok (-4)

-}
chainr : Parser s (a -> a -> a) -> Parser s a -> Parser s a
chainr op p =
    let
        -- `pending` holds the operators with their left operand, most recent
        -- first, so the right-associative result is built by a (stack-safe)
        -- left fold instead of non-tail recursion
        finish pending x =
            List.foldl (\( f, l ) r -> f l r) x pending

        accumulate pending x state stream =
            case app op state stream of
                ( opstate, opstream, Ok f ) ->
                    if stream.input == opstream.input then
                        ( opstate, opstream, Ok (finish pending x) )

                    else
                        case app p opstate opstream of
                            ( pstate, pstream, Ok y ) ->
                                if opstream.input == pstream.input then
                                    ( pstate, pstream, Ok (finish pending x) )

                                else
                                    accumulate (( f, x ) :: pending) y pstate pstream

                            ( estate, estream, Err ms ) ->
                                ( estate, estream, Err ms )

                ( _, _, Err _ ) ->
                    ( state, stream, Ok (finish pending x) )
    in
    Parser <|
        \state stream ->
            case app p state stream of
                ( pstate, pstream, Ok x ) ->
                    accumulate [] x pstate pstream

                ( estate, estream, Err ms ) ->
                    ( estate, estream, Err ms )


{-| Parse `n` occurrences of `p`.

    parse (count 3 (string "a")) "aaa"
    -- Ok ["a", "a", "a"]

    parse (count 3 (string "a")) "aa"
    -- Err ["expected \"a\""]

    parse (count 3 (string "a")) "aaaaa"
    -- Ok ["a", "a", "a"]

-}
count : Int -> Parser s a -> Parser s (List a)
count n p =
    let
        -- a loop instead of a chain of n nested `andThen`s, which overflows
        -- the stack for large n
        accumulate x acc state stream =
            if x <= 0 then
                ( state, stream, Ok (List.reverse acc) )

            else
                case app p state stream of
                    ( rstate, rstream, Ok res ) ->
                        accumulate (x - 1) (res :: acc) rstate rstream

                    ( estate, estream, Err ms ) ->
                        ( estate, estream, Err ms )
    in
    Parser <|
        \state stream ->
            accumulate n [] state stream


{-| Parse something between two other parsers.

The parser

    parse
        (between
            (string "(")
            (string ")")
            (string "a")
        )
        "(a)"
    -- Ok "a"

is equivalent to the parser

    string "("
        |> keep (string "a")
        |> ignore (string ")")

-}
between : Parser s l -> Parser s r -> Parser s a -> Parser s a
between lp rp p =
    lp |> keep p |> ignore rp


{-| Parse something between parentheses.

    parse (parens (string "hello")) "(hello)"
    -- Ok "hello"

    parse (parens (string "hello")) "(world)"
    -- Err ["expected \"hello\""]

-}
parens : Parser s a -> Parser s a
parens =
    between (string "(") (string ")")


{-| Parse something between braces `{}`.

    parse (braces (string "hello")) "{hello}"
    -- Ok "hello"

    parse (braces (string "hello")) "{world}"
    -- Err ["expected \"hello\""]

-}
braces : Parser s a -> Parser s a
braces =
    between (string "{") (string "}")


{-| Parse something between square brackets `[]`.

    parse (brackets (string "hello")) "[hello]"
    -- Ok "hello"

    parse (brackets (string "hello")) "[world]"
    -- Err ["expected \"hello\""]

-}
brackets : Parser s a -> Parser s a
brackets =
    between (string "[") (string "]")


{-| Parse zero or more whitespace characters.

    parse
        (whitespace
            |> keep (string "hello")
        )
        "hello"
    -- Ok "hello"

    parse
        (whitespace
            |> keep (string "hello")
        )
        "   hello"
    -- Ok "hello"

-}
whitespace : Parser s String
whitespace =
    Parser <|
        \state stream ->
            -- most of the time there is no whitespace at all, this case is
            -- decided without the regex, longer runs are faster with it
            if startsWithWhitespace stream.input then
                app whitespaceRegex state stream

            else
                ( state, stream, Ok "" )


whitespaceRegex : Parser s String
whitespaceRegex =
    regex "\\s*"


startsWithWhitespace : String -> Bool
startsWithWhitespace input =
    case String.uncons input of
        Just ( c, _ ) ->
            isWhitespace c

        Nothing ->
            False


{-| Parse one or more whitespace characters.

    parse
        (whitespace1
            |> keep (string "hello")
        )
        "hello"
    -- Err ["whitespace"]

    parse
        (whitespace1
            |> keep (string "hello")
        )
        "   hello"
    -- Ok "hello"

-}
whitespace1 : Parser s String
whitespace1 =
    Parser <|
        \state stream ->
            if startsWithWhitespace stream.input then
                app whitespaceRegex state stream

            else
                ( state, stream, Err [ "whitespace" ] )


{-| The same set of characters as `\s` in JavaScript regular expressions.
-}
isWhitespace : Char -> Bool
isWhitespace c =
    let
        code =
            Char.toCode c
    in
    if code < 0xA0 then
        code == 0x20 || (code >= 0x09 && code <= 0x0D)

    else
        (code == 0xA0)
            || (code == 0x1680)
            || (code >= 0x2000 && code <= 0x200A)
            || (code == 0x2028)
            || (code == 0x2029)
            || (code == 0x202F)
            || (code == 0x205F)
            || (code == 0x3000)
            || (code == 0xFEFF)


{-| Variant of `mapError` that replaces the Parser's error with a List
of a single string.

    parse (string "a" |> onerror "gimme an 'a'") "b"
    -- Err ["gimme an 'a'"]

-}
onerror : String -> Parser s a -> Parser s a
onerror m p =
    mapError (always [ m ]) p


{-| Run a parser and return the value on the right on success.

    parse (string "true" |> onsuccess True) "true"
    -- Ok True

    parse (string "true" |> onsuccess True) "false"
    -- Err ["expected \"true\""]

-}
onsuccess : a -> Parser s x -> Parser s a
onsuccess res =
    map (always res)


{-| Join two parsers, keeping only the result of the parser passed as
argument (the one on the right in a pipeline).

    unprefix : Parser s String
    unprefix =
      string ">"
        |> keep (while ((==) ' '))
        |> keep (while ((/=) ' '))

    parse unprefix "> a"
    -- Ok "a"

-}
keep : Parser s a -> Parser s x -> Parser s a
keep p1 p2 =
    Parser <|
        \state stream ->
            case app p2 state stream of
                ( rstate, rstream, Ok _ ) ->
                    app p1 rstate rstream

                ( estate, estream, Err ms ) ->
                    ( estate, estream, Err ms )


{-| Join two parsers, ignoring the result of the parser passed as argument
(the one on the right in a pipeline).

    unsuffix : Parser s String
    unsuffix =
      regex "[a-z]"
        |> ignore (regex "[!?]")

    parse unsuffix "a!"
    -- Ok "a"

-}
ignore : Parser s x -> Parser s a -> Parser s a
ignore p1 p2 =
    Parser <|
        \state stream ->
            case app p2 state stream of
                ( rstate, rstream, Ok res ) ->
                    case app p1 rstate rstream of
                        ( fstate, fstream, Ok _ ) ->
                            ( fstate, fstream, Ok res )

                        ( estate, estream, Err ms ) ->
                            ( estate, estream, Err ms )

                ( estate, estream, Err ms ) ->
                    ( estate, estream, Err ms )
