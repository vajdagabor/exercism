module SumOfMultiples exposing (sumOfMultiples)

import Set


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit =
    divisors
        |> List.map ((\v -> range v limit v) >> Set.fromList)
        |> List.foldl Set.union Set.empty
        |> Set.foldl (+) 0


range : Int -> Int -> Int -> List Int
range min limit step =
    if min > limit - 1 then
        []

    else
        min :: range (min + step) limit step
