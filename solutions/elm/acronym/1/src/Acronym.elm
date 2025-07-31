module Acronym exposing (abbreviate)


abbreviate : String -> String
abbreviate phrase =
    phrase
        |> String.replace "-" " "
        |> String.split " "
        |> List.map (String.left 1)
        |> List.filter (String.isEmpty >> not)
        |> String.concat
        |> String.toUpper
