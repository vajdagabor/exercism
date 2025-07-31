module AllYourBase exposing (rebase)


rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    if List.isEmpty digits then
        Nothing

    else if List.any ((>) 0) digits then
        Nothing

    else if List.all ((==) 0) digits then
        Nothing

    else if List.any ((<=) inBase) digits then
        Nothing

    else if inBase < 2 || outBase < 2 then
        Nothing

    else
        toDecimal inBase digits |> toDigits outBase |> Just


toDecimal : Int -> List Int -> Int
toDecimal base digits =
    List.reverse digits
        |> List.indexedMap (\index digit -> base ^ index * digit)
        |> List.foldl (+) 0


toDigits : Int -> Int -> List Int
toDigits base decimal =
    let
        nextDigit : List Int -> Int -> List Int
        nextDigit digits remainder =
            if remainder == 0 then
                digits

            else
                nextDigit (remainderBy base remainder :: digits) (remainder // base)
    in
    nextDigit [] decimal
