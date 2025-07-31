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
chunk size list =
    if List.length list < size then
        []

    else
        List.take size list :: chunk size (List.drop 1 list)
