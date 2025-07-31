module Raindrops exposing (raindrops)


raindrops : Int -> String
raindrops number =
    [ modBy 3 number, modBy 5 number, modBy 7 number ]
        |> List.indexedMap mapNumber
        |> String.concat
        |> valueIfEmpty (String.fromInt number)


dropSound : Int -> String
dropSound index =
    case index of
        0 ->
            "Pling"

        1 ->
            "Plang"

        2 ->
            "Plong"

        _ ->
            ""


mapNumber : Int -> Int -> String
mapNumber index number =
    if number == 0 then
        dropSound index

    else
        ""


valueIfEmpty : String -> String -> String
valueIfEmpty value str =
    if str == "" then
        value

    else
        str
