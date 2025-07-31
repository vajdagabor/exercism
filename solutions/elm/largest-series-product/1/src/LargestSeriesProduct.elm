module LargestSeriesProduct exposing (largestProduct)


largestProduct : Int -> String -> Maybe Int
largestProduct span series =
    let
        numbers =
            String.split "" series |> List.filterMap String.toInt

        isValidSeries =
            String.length series == List.length numbers

        chunk : List (List Int) -> List Int -> List (List Int)
        chunk result original =
            if List.length original < span then
                result

            else
                chunk (List.take span original :: result) (List.drop 1 original)
    in
    if isValidSeries && span <= (String.length series) && span > 0 then
        chunk [] numbers
            |> List.map List.product
            |> List.maximum

    else
        Nothing
