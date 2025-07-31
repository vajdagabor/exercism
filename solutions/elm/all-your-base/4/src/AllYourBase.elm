module AllYourBase exposing (rebase)


rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    if
        (inBase < 2)
            || (outBase < 2)
            || List.any (\d -> d < 0 || d >= inBase) digits
            || List.all ((==) 0) digits
    then
        Nothing

    else
        toDecimal inBase digits |> toDigits outBase |> Just


toDecimal : Int -> List Int -> Int
toDecimal base digits =
    List.foldl (\d sum -> base * sum + d) 0 digits


toDigits : Int -> Int -> List Int
toDigits base decimal =
    let
        nextDigit : Int -> List Int
        nextDigit remainder =
            case remainder of
                0 ->
                    []

                _ ->
                    remainderBy base remainder :: nextDigit (remainder // base)
    in
    nextDigit decimal |> List.reverse
