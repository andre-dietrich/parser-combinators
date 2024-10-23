module Combine.Char exposing (satisfy, char, anyChar, oneOf, noneOf, space, tab, newline, crlf, eol, lower, upper, digit, octDigit, hexDigit, alpha, alphaNum)

{-| This module contains `Char`-specific Parsers.

Avoid using this module if performance is a concern. You can achieve
everything that you can do with this module by using `Combine.regex`,
`Combine.string` or `Combine.primitive` and, in general, those will be
much faster.


# Parsers

@docs satisfy, char, anyChar, oneOf, noneOf, space, tab, newline, crlf, eol, lower, upper, digit, octDigit, hexDigit, alpha, alphaNum

-}

import Char
import Combine exposing (Parser, onerror, onsuccess, or, primitive, string)
import Flip exposing (flip)
import String


{-| Parse a character matching the predicate.

    parse (satisfy ((==) 'a')) "a" ==
    -- Ok 'a'

    parse (satisfy ((==) 'a')) "b" ==
    -- Err ["could not satisfy predicate"]

-}
satisfy : (Char -> Bool) -> Parser s Char
satisfy pred =
    primitive <|
        \state stream ->
            let
                message =
                    "could not satisfy predicate"
            in
            case String.uncons stream.input of
                Just ( h, rest ) ->
                    if pred h then
                        ( state, { stream | input = rest, position = stream.position + 1 }, Ok h )

                    else
                        ( state, stream, Err [ message ] )

                Nothing ->
                    ( state, stream, Err [ message ] )


{-| Parse an exact character match.

    parse (char 'a') "a" --> Ok 'a'

    parse (char 'a') "b" --> Err ["expected 'a'"]

    -- You can write the expected result on the next line,

    add 41 1
    --> 42

    -- You can write the expected result on the next line,

    add 41 1
    --> 42

-}
char : Char -> Parser s Char
char c =
    satisfy ((==) c) |> onerror ("expected " ++ String.fromChar c)


charList : List Char -> String
charList chars =
    chars
        |> List.map (\c -> "'" ++ String.fromChar c ++ "'")
        |> List.intersperse ", "
        |> String.concat
        |> (\str -> "[" ++ str ++ "]")


{-| Parse any character.

    parse anyChar "a" ==
    -- Ok 'a'

    parse anyChar "" ==
    -- Err ["expected any character"]

-}
anyChar : Parser s Char
anyChar =
    satisfy (always True) |> onerror "expected any character"


{-| Parse a character from the given list.

    parse (oneOf ['a', 'b']) "a" ==
    -- Ok 'a'

    parse (oneOf ['a', 'b']) "c" ==
    -- Err ["expected one of ['a','b']"]

-}
oneOf : List Char -> Parser s Char
oneOf cs =
    satisfy (flip List.member cs)
        |> onerror ("expected one of " ++ charList cs)


{-| Parse a character that is not in the given list.

    parse (noneOf ['a', 'b']) "c" ==
    -- Ok 'c'

    parse (noneOf ['a', 'b']) "a" ==
    -- Err ["expected none of ['a','b']"]

-}
noneOf : List Char -> Parser s Char
noneOf cs =
    satisfy (not << flip List.member cs) |> onerror ("expected none of " ++ charList cs)


{-| Parse a space character.

    parse space " " == Ok ' '

    parse space "a" == Err [ "expected a space" ]

-}
space : Parser s Char
space =
    satisfy ((==) ' ') |> onerror "expected a space"


{-| Parse a `\t` character.

    parse tab "\t" == Ok '\t'

    parse tab "a" == Err [ "expected a tab" ]

-}
tab : Parser s Char
tab =
    satisfy ((==) '\t') |> onerror "expected a tab"


{-| Parse a `\n` character.

    parse newline "\n" == Ok '\n'

    parse newline "a" == Err [ "expected a newline" ]

-}
newline : Parser s Char
newline =
    satisfy ((==) '\n') |> onerror "expected a newline"


{-| Parse a `\r\n` sequence, returning a `\n` character.

    parse crlf "\u{000D}\n" == Ok '\n'

    parse crlf "\n" == Err [ "expected CRLF" ]

    parse crlf "\u{000D}" == Err [ "expected CRLF" ]

-}
crlf : Parser s Char
crlf =
    string "\u{000D}\n" |> onsuccess '\n' |> onerror "expected CRLF"


{-| Parse an end of line character or sequence, returning a `\n` character.

    parse eol "\n" == Ok '\n'

    parse eol "\u{000D}\n" == Ok '\n'

    parse eol "\u{000D}" == Ok '\n'

    parse eol "a" == Err [ "expected an end of line character" ]

-}
eol : Parser s Char
eol =
    or newline crlf


{-| Parse any lowercase character.

    parse lower "a" == Ok 'a'

    parse lower "A" == Err [ "expected a lowercase character" ]

-}
lower : Parser s Char
lower =
    satisfy Char.isLower |> onerror "expected a lowercase character"


{-| Parse any uppercase character.

    parse upper "A" == Ok 'A'

    parse upper "a" == Err [ "expected an uppercase character" ]

-}
upper : Parser s Char
upper =
    satisfy Char.isUpper |> onerror "expected an uppercase character"


{-| Parse any base 10 digit.

    parse digit "0" == Ok '0'

    parse digit "9" == Ok '9'

    parse digit "a" == Err [ "expected a digit" ]

-}
digit : Parser s Char
digit =
    satisfy Char.isDigit |> onerror "expected a digit"


{-| Parse any base 8 digit.

    parse octDigit "0" == Ok '0'

    parse octDigit "7" == Ok '7'

    parse octDigit "8" == Err [ "expected an octal digit" ]

-}
octDigit : Parser s Char
octDigit =
    satisfy Char.isOctDigit |> onerror "expected an octal digit"


{-| Parse any base 16 digit.

    parse hexDigit "0" == Ok '0'

    parse hexDigit "7" == Ok '7'

    parse hexDigit "a" == Ok 'a'

    parse hexDigit "f" == Ok 'f'

    parse hexDigit "g" == Err [ "expected a hexadecimal digit" ]

-}
hexDigit : Parser s Char
hexDigit =
    satisfy Char.isHexDigit |> onerror "expected a hexadecimal digit"


{-| Parse any alphabetic character.

    parse alpha "a" == Ok 'a'

    parse alpha "A" == Ok 'A'

    parse alpha "0" == Err [ "expected an alphabetic character" ]

-}
alpha : Parser s Char
alpha =
    satisfy Char.isAlpha |> onerror "expected an alphabetic character"


{-| Parse any alphanumeric character.

    parse alphaNum "a" == Ok 'a'

    parse alphaNum "A" == Ok 'A'

    parse alphaNum "0" == Ok '0'

    parse alphaNum "-" == Err [ "expected an alphanumeric character" ]

-}
alphaNum : Parser s Char
alphaNum =
    satisfy Char.isAlphaNum |> onerror "expected an alphanumeric character"
