module Raindrops exposing (raindrops)


raindrops : Int -> String
raindrops number =
    let
        drops =
            [ ( 3, "Pling" ), ( 5, "Plang" ), ( 7, "Plong" ) ]
                |> List.filter (\( factor, _ ) -> modBy factor number == 0)
                |> List.map Tuple.second
                |> String.concat
    in
    case drops of
        "" ->
            String.fromInt number

        _ ->
            drops
