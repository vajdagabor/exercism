module Luhn exposing (checksum, clean, ensureLength, toDigits, valid)

import Maybe exposing (andThen)


valid : String -> Bool
valid input =
    case parse input of
        Nothing ->
            False

        Just digits ->
            checksum digits |> modBy 10 |> (==) 0


parse : String -> Maybe (List Int)
parse str =
    str |> clean |> ensureLength |> andThen toDigits


clean : String -> String
clean str =
    String.replace " " "" str


ensureLength : String -> Maybe String
ensureLength str =
    if String.length str > 1 then
        Just str

    else
        Nothing


toDigits : String -> Maybe (List Int)
toDigits str =
    let
        digits =
            str |> String.split "" |> List.filterMap String.toInt
    in
    if String.length str == List.length digits then
        Just digits

    else
        Nothing


checksum : List Int -> Int
checksum digits =
    let
        doubleEven : Int -> Int -> Int
        doubleEven index digit =
            if modBy 2 index == 0 then
                digit

            else
                digit * 2 |> minusNine

        minusNine : Int -> Int
        minusNine num =
            if num > 9 then
                num - 9

            else
                num
    in
    digits
        |> List.reverse
        |> List.indexedMap doubleEven
        |> List.sum
