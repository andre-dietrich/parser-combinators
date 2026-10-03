module Combine.Num exposing (sign, digit, int, float)

{-| This module contains Parsers specific to parsing numbers.


# Parsers

@docs sign, digit, int, float

-}

import Char
import Combine exposing (Parser, app, map, onerror, onsuccess, optional, or, primitive, regex, string)
import Combine.Char
import String


{-| Parse a numeric sign, returning `1` for positive numbers and `-1`
for negative numbers.

    parse sign "+" == Ok 1

    parse sign "-" == Ok -1

    parse sign "a" == Err [ "expected a sign" ]

-}
sign : Parser s Int
sign =
    optional 1
        (or
            (string "+" |> onsuccess 1)
            (string "-" |> onsuccess -1)
        )


{-| Parse a digit.

    parse digit "1" == Ok 1

    parse digit "a" == Err [ "expected a digit" ]

-}
digit : Parser s Int
digit =
    Combine.Char.digit
        -- 48 is the ASCII code for '0'
        |> map (\c -> Char.toCode c - 48)
        |> onerror "expected a digit"


{-| Parse an integer.

    parse int "123" == Ok 123

    parse int "-123" == Ok -123

    parse int "abc" == Err [ "expected an int" ]

-}
int : Parser s Int
int =
    regex "-?(?:0|[1-9]\\d*)"
        |> convert String.toInt
        |> onerror "expected an int"


{-| Parse a float.

    parse float "123.456" == Ok 123.456

    parse float "-123.456" == Ok -123.456

    parse float "abc" == Err [ "expected a float" ]

-}
float : Parser s Float
float =
    regex "-?(?:0|[1-9]\\d*)\\.\\d+"
        |> convert String.toFloat
        |> onerror "expected a float"


{-| Converts the matched string, without `andThen` creating a new parser for
every parsed number.
-}
convert : (String -> Maybe v) -> Parser s String -> Parser s v
convert f p =
    primitive <|
        \state stream ->
            case app p state stream of
                ( rstate, rstream, Ok str ) ->
                    case f str of
                        Just v ->
                            ( rstate, rstream, Ok v )

                        Nothing ->
                            ( rstate, rstream, Err [ "impossible state in Combine.Num.unwrap" ] )

                ( estate, estream, Err ms ) ->
                    ( estate, estream, Err ms )
