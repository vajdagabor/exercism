module Isogram exposing (isIsogram)

import Set


isIsogram : String -> Bool
isIsogram sentence =
    let
        letters =
            String.filter Char.isAlpha sentence
    in
    letters
        |> String.toLower
        |> String.split ""
        |> Set.fromList
        |> Set.size
        |> (==) (String.length letters)
