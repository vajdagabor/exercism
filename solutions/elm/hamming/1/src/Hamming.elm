module Hamming exposing (distance)


distance : String -> String -> Result String Int
distance left right =
    let
        addIfDifferent ( a, b ) count =
            if a == b then
                count

            else
                Result.map ((+) 1) count
    in
    if String.length left /= String.length right then
        Err "strands must be of equal length"

    else
        List.map2 Tuple.pair (String.toList left) (String.toList right)
            |> List.foldl addIfDifferent (Ok 0)
