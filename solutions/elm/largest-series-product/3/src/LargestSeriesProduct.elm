module LargestSeriesProduct exposing (largestProduct)


largestProduct : Int -> String -> Maybe Int
largestProduct span series =
    let
        numbers =
            String.split "" series |> List.filterMap String.toInt

        isValidSeries =
            String.length series == List.length numbers
    in
    if isValidSeries && span <= String.length series && span > 0 then
        chunk span numbers
            |> List.map List.product
            |> List.maximum

    else
        Nothing


chunk : Int -> List a -> List (List a)
chunk span list =
    if List.length list < span then
        []

    else
        List.take span list :: chunk span (List.drop 1 list)
