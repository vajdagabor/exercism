module TwelveDays exposing (recite)


recite : Int -> Int -> List String
recite start stop =
    List.range start stop |> List.map day


day : Int -> String
day num =
    if num >= 1 && num <= 12 then
        "On the "
            ++ ordinal num
            ++ " day of Christmas my true love gave to me: "
            ++ allGiftsOnDay num
            ++ "."

    else
        ""


allGiftsOnDay : Int -> String
allGiftsOnDay num =
    let
        prefix =
            if num == 1 then
                "a"

            else
                "and a"

        gifts =
            [ prefix ++ " Partridge in a Pear Tree"
            , "two Turtle Doves"
            , "three French Hens"
            , "four Calling Birds"
            , "five Gold Rings"
            , "six Geese-a-Laying"
            , "seven Swans-a-Swimming"
            , "eight Maids-a-Milking"
            , "nine Ladies Dancing"
            , "ten Lords-a-Leaping"
            , "eleven Pipers Piping"
            , "twelve Drummers Drumming"
            ]
    in
    gifts
        |> List.take num
        |> List.reverse
        |> String.join ", "


ordinal : Int -> String
ordinal num =
    case num of
        1  -> "first"
        2  -> "second"
        3  -> "third"
        4  -> "fourth"
        5  -> "fifth"
        6  -> "sixth"
        7  -> "seventh"
        8  -> "eighth"
        9  -> "ninth"
        10 -> "tenth"
        11 -> "eleventh"
        12 -> "twelfth"
        _  -> "nth"
