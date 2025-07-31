module TwelveDays exposing (recite)

import Array


recite : Int -> Int -> List String
recite start stop =
    List.range start stop |> List.map day


ordinal : Int -> String
ordinal num =
    let
        ordinals =
            Array.fromList
                [ "zeroth"
                , "first"
                , "second"
                , "third"
                , "fourth"
                , "fifth"
                , "sixth"
                , "seventh"
                , "eighth"
                , "ninth"
                , "tenth"
                , "eleventh"
                , "twelfth"
                ]
    in
    Array.get num ordinals |> Maybe.withDefault "nth"


day : Int -> String
day num =
    if num >= 1 && num <= 12 then
        "On the "
            ++ ordinal num
            ++ " day of Christmas my true love gave to me: "
            ++ allPresentsOnDay num
            ++ "."

    else
        ""


allPresentsOnDay : Int -> String
allPresentsOnDay num =
    let
        prefix =
            if num == 1 then
                "a"

            else
                "and a"

        presents =
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
    presents
        |> List.take num
        |> List.reverse
        |> String.join ", "
