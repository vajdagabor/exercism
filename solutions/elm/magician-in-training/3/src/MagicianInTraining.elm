module MagicianInTraining exposing (..)

import Array exposing (..)


getCard : Int -> Array Int -> Maybe Int
getCard =
    get


setCard : Int -> Int -> Array Int -> Array Int
setCard =
    set


addCard : Int -> Array Int -> Array Int
addCard =
    push


removeCard : Int -> Array Int -> Array Int
removeCard index deck =
    let
        before =
            slice 0 index deck

        after =
            slice (index + 1) (length deck) deck
    in
    append before after


evenCardCount : Array Int -> Int
evenCardCount deck =
    let
        isEven n =
            modBy 2 n == 0
    in
    deck
        |> filter isEven
        |> length
