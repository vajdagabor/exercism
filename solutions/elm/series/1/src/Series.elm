module Series exposing (slices)


slices : Int -> String -> Result String (List (List Int))
slices size input =
    let
        inputLength =
            String.length input

        getSlice : Int -> List Int
        getSlice index =
            String.slice index (size + index) input
                |> String.split ""
                |> List.filterMap String.toInt
    in
    if String.isEmpty input then
        Err "series cannot be empty"

    else if size > inputLength then
        Err "slice length cannot be greater than series length"

    else if size == 0 then
        Err "slice length cannot be zero"

    else if size < 0 then
        Err "slice length cannot be negative"

    else
        List.range 0 (inputLength - size)
            |> List.map getSlice
            |> Ok
