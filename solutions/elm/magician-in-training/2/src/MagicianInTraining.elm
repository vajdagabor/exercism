module MagicianInTraining exposing (..)

import Array exposing (Array)


getCard : Int -> Array Int -> Maybe Int
getCard =
    Array.get


setCard : Int -> Int -> Array Int -> Array Int
setCard =
    Array.set


addCard : Int -> Array Int -> Array Int
addCard =
    Array.push


removeCard : Int -> Array Int -> Array Int
removeCard index deck =
    let
        before =
            Array.slice 0 index deck

        after =
            Array.slice (index + 1) (Array.length deck) deck
    in
    Array.append before after


evenCardCount : Array Int -> Int
evenCardCount =
    let
        countIfEven : Int -> Int -> Int
        countIfEven num sum =
            1 - remainderBy 2 num + sum
    in
    Array.foldl countIfEven 0
