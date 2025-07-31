module Raindrops exposing (raindrops)


raindrops : Int -> String
raindrops number =
    let
        concatIfDivisible ( num, sound ) str =
            if modBy num number == 0 then str ++ sound else str

        default str =
            if String.isEmpty str then String.fromInt number else str
    in
    [ ( 3, "Pling" ), ( 5, "Plang" ), ( 7, "Plong" ) ]
        |> List.foldl concatIfDivisible ""
        |> default
