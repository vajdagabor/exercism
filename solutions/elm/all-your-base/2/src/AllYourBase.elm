module AllYourBase exposing (rebase)


rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    toDecimal inBase digits |> Maybe.andThen (toDigits outBase)


toDecimal : Int -> List Int -> Maybe Int
toDecimal base digits =
    case ( parseBase base, parseDigits base digits ) of
        ( Nothing, _ ) ->
            Nothing

        ( _, Nothing ) ->
            Nothing

        ( Just pBase, Just pDigits ) ->
            List.reverse pDigits
                |> List.indexedMap (\index digit -> pBase ^ index * digit)
                |> List.sum
                |> Just


toDigits : Int -> Int -> Maybe (List Int)
toDigits base decimal =
    Maybe.map
        (\validBase ->
            let
                nextDigit : List Int -> Int -> List Int
                nextDigit digits remainder =
                    if remainder == 0 then
                        digits

                    else
                        nextDigit (remainderBy validBase remainder :: digits) (remainder // base)
            in
            nextDigit [] decimal
        )
        (parseBase base)


parseBase : Int -> Maybe Int
parseBase base =
    if base < 2 then
        Nothing

    else
        Just base


parseDigits : Int -> List Int -> Maybe (List Int)
parseDigits base digits =
    let
        isEmpty =
            List.isEmpty digits

        hasNegatives =
            List.any ((>) 0) digits

        zeroesOnly =
            List.all ((==) 0) digits

        tooLargeNumbers =
            List.any ((<=) base) digits

        allChecks =
            [ isEmpty, hasNegatives, zeroesOnly, tooLargeNumbers ]

        isInvalid =
            List.member True allChecks
    in
    if isInvalid then
        Nothing

    else
        Just (trimZeros digits)


trimZeros : List Int -> List Int
trimZeros digits =
    case digits of
        [] ->
            []

        head :: tail ->
            if head == 0 then
                trimZeros tail

            else
                digits
