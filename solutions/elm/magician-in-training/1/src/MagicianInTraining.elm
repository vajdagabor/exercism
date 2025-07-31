module MagicianInTraining exposing (..)

import Array exposing (Array)


getCard : Int -> Array Int -> Maybe Int
getCard index deck =
    Array.get index deck


setCard : Int -> Int -> Array Int -> Array Int
setCard index newCard deck =
    Array.set index newCard deck


addCard : Int -> Array Int -> Array Int
addCard newCard deck =
    Array.push newCard deck


removeCard : Int -> Array Int -> Array Int
removeCard index deck =
    let
        before = Array.slice 0 index deck
        after = Array.slice (index + 1) (Array.length deck) deck
    in
        Array.append before after

evenCardCount : Array Int -> Int
evenCardCount deck =
    Array.foldl countIfEven 0 deck

countIfEven : Int -> Int -> Int
countIfEven num sum =
    if remainderBy 2 num == 0 then sum + 1 else sum
