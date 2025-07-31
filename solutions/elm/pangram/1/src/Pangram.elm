module Pangram exposing (isPangram)

import Set


alphas =
    "abcdefghijklmnopqrstuvwxyz"


isPangram : String -> Bool
isPangram sentence =
    sentence
        |> String.toLower
        |> String.toList
        |> Set.fromList
        |> Set.toList
        |> List.filter Char.isAlpha
        |> List.sort
        |> String.fromList
        |> (==) alphas
