module Combine.Char exposing (satisfy, char, anyChar, peekChar, oneOf, noneOf, space, tab, newline, crlf, eol, lower, upper, digit, octDigit, hexDigit, alpha, alphaNum)

{-| This module contains `Char`-specific Parsers.

Avoid using this module if performance is a concern. You can achieve
everything that you can do with this module by using `Combine.regex`,
`Combine.string` or `Combine.primitive` and, in general, those will be
much faster.


# Parsers

@docs satisfy, char, anyChar, peekChar, oneOf, noneOf, space, tab, newline, crlf, eol, lower, upper, digit, octDigit, hexDigit, alpha, alphaNum

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
satisfy =
    satisfyWith "could not satisfy predicate"


{-| `satisfy` with a custom error message, this saves the extra `onerror`
wrapper for every parsed character.
-}
satisfyWith : String -> (Char -> Bool) -> Parser s Char
satisfyWith message pred =
    let
        error =
            [ message ]
    in
    primitive <|
        \state stream ->
            case String.uncons stream.input of
                Just ( h, rest ) ->
                    if pred h then
                        ( state, { stream | input = rest, position = stream.position + charWidth h }, Ok h )

                    else
                        ( state, stream, Err error )

                Nothing ->
                    ( state, stream, Err error )


{-| Characters outside of the Basic Multilingual Plane occupy two UTF-16 code
units, this keeps `position` consistent with `Combine.string` and `regex`.
-}
charWidth : Char -> Int
charWidth c =
    if Char.toCode c > 0xFFFF then
        2

    else
        1


{-| Parse an exact character match.

    parse (char 'a') "a" --> Ok 'a'

    parse (char 'a') "b" --> Err ["expected a"]

-}
char : Char -> Parser s Char
char c =
    satisfyWith ("expected " ++ String.fromChar c) ((==) c)


charList : List Char -> String
charList chars =
    "["
        ++ String.join ", " (List.map (\c -> "'" ++ String.fromChar c ++ "'") chars)
        ++ "]"


{-| Parse any character.

    parse anyChar "a" ==
    -- Ok 'a'

    parse anyChar "" ==
    -- Err ["expected any character"]

-}
anyChar : Parser s Char
anyChar =
    satisfyWith "expected any character" (always True)


{-| Peek at the next character without consuming any input.
Returns `Nothing` if at end of input.

    parse peekChar "abc" ==
    -- Ok (Just 'a')

    parse (peekChar |> Combine.ignore (char 'a')) "abc" ==
    -- Ok (Just 'a')

    parse peekChar "" ==
    -- Ok Nothing

-}
peekChar : Parser s (Maybe Char)
peekChar =
    primitive <|
        \state stream ->
            case String.uncons stream.input of
                Just ( c, _ ) ->
                    ( state, stream, Ok (Just c) )

                Nothing ->
                    ( state, stream, Ok Nothing )


{-| Parse a character from the given list.

    parse (oneOf ['a', 'b']) "a" ==
    -- Ok 'a'

    parse (oneOf ['a', 'b']) "c" ==
    -- Err ["expected one of ['a','b']"]

-}
oneOf : List Char -> Parser s Char
oneOf cs =
    satisfyWith ("expected one of " ++ charList cs) (flip List.member cs)


{-| Parse a character that is not in the given list.

    parse (noneOf ['a', 'b']) "c" ==
    -- Ok 'c'

    parse (noneOf ['a', 'b']) "a" ==
    -- Err ["expected none of ['a','b']"]

-}
noneOf : List Char -> Parser s Char
noneOf cs =
    satisfyWith ("expected none of " ++ charList cs) (not << flip List.member cs)


{-| Parse a space character.

    parse space " " == Ok ' '

    parse space "a" == Err [ "expected a space" ]

-}
space : Parser s Char
space =
    satisfyWith "expected a space" ((==) ' ')


{-| Parse a `\t` character.

    parse tab "\t" == Ok '\t'

    parse tab "a" == Err [ "expected a tab" ]

-}
tab : Parser s Char
tab =
    satisfyWith "expected a tab" ((==) '\t')


{-| Parse a `\n` character.

    parse newline "\n" == Ok '\n'

    parse newline "a" == Err [ "expected a newline" ]

-}
newline : Parser s Char
newline =
    satisfyWith "expected a newline" ((==) '\n')


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

    parse eol "a" == Err [ "expected a newline", "expected CRLF" ]

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
    satisfyWith "expected a lowercase character" Char.isLower


{-| Parse any uppercase character.

    parse upper "A" == Ok 'A'

    parse upper "a" == Err [ "expected an uppercase character" ]

-}
upper : Parser s Char
upper =
    satisfyWith "expected an uppercase character" Char.isUpper


{-| Parse any base 10 digit.

    parse digit "0" == Ok '0'

    parse digit "9" == Ok '9'

    parse digit "a" == Err [ "expected a digit" ]

-}
digit : Parser s Char
digit =
    satisfyWith "expected a digit" Char.isDigit


{-| Parse any base 8 digit.

    parse octDigit "0" == Ok '0'

    parse octDigit "7" == Ok '7'

    parse octDigit "8" == Err [ "expected an octal digit" ]

-}
octDigit : Parser s Char
octDigit =
    satisfyWith "expected an octal digit" Char.isOctDigit


{-| Parse any base 16 digit.

    parse hexDigit "0" == Ok '0'

    parse hexDigit "7" == Ok '7'

    parse hexDigit "a" == Ok 'a'

    parse hexDigit "f" == Ok 'f'

    parse hexDigit "g" == Err [ "expected a hexadecimal digit" ]

-}
hexDigit : Parser s Char
hexDigit =
    satisfyWith "expected a hexadecimal digit" Char.isHexDigit


{-| Parse any alphabetic character.

    parse alpha "a" == Ok 'a'

    parse alpha "A" == Ok 'A'

    parse alpha "0" == Err [ "expected an alphabetic character" ]

-}
alpha : Parser s Char
alpha =
    satisfyWith "expected an alphabetic character" Char.isAlpha


{-| Parse any alphanumeric character.

    parse alphaNum "a" == Ok 'a'

    parse alphaNum "A" == Ok 'A'

    parse alphaNum "0" == Ok '0'

    parse alphaNum "-" == Err [ "expected an alphanumeric character" ]

-}
alphaNum : Parser s Char
alphaNum =
    satisfyWith "expected an alphanumeric character" Char.isAlphaNum
