module Allergies exposing (Allergy(..), isAllergicTo, toList)

import Bitwise


type Allergy
    = Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats


allergies =
    [ ( Eggs, 1 )
    , ( Peanuts, 2 )
    , ( Shellfish, 4 )
    , ( Strawberries, 8 )
    , ( Tomatoes, 16 )
    , ( Chocolate, 32 )
    , ( Pollen, 64 )
    , ( Cats, 128 )
    ]


isAllergicTo : Allergy -> Int -> Bool
isAllergicTo allergy score =
    nameToValue allergy |> isBitOn score


toList : Int -> List Allergy
toList score =
    let
        collect ( name, value ) acc =
            if isBitOn score value then
                name :: acc

            else
                acc
    in
    List.foldl collect [] allergies


isBitOn : Int -> Int -> Bool
isBitOn a b =
    Bitwise.and a b > 0


nameToValue : Allergy -> Int
nameToValue name =
    let
        findByName ( n, v ) acc =
            if n == name then
                v

            else
                acc
    in
    List.foldl findByName 0 allergies
