module GottaSnatchEmAll exposing (..)

import Set exposing (Set)


type alias Card =
    String


newCollection : Card -> Set Card
newCollection =
    Set.singleton


addCard : Card -> Set Card -> ( Bool, Set Card )
addCard card collection =
    ( Set.member card collection, Set.insert card collection )


tradeCard : Card -> Card -> Set Card -> ( Bool, Set Card )
tradeCard =
    toTuple isTradePossible tradeCardAnyway


tradeCardAnyway : Card -> Card -> Set Card -> Set Card
tradeCardAnyway yourCard theirCard collection =
    (Set.remove yourCard >> Set.insert theirCard) collection


isTradePossible : Card -> Card -> Set Card -> Bool
isTradePossible yourCard theirCard collection =
    Set.member yourCard collection && not (Set.member theirCard collection)


removeDuplicates : List Card -> List Card
removeDuplicates cards =
    (Set.fromList >> Set.toList >> List.sort) cards


extraCards : Set Card -> Set Card -> Int
extraCards yourCollection theirCollection =
    Set.diff yourCollection theirCollection |> Set.size


boringCards : List (Set Card) -> List Card
boringCards collections =
    let
        ( first, rest ) =
            splitCollectionList collections
    in
    List.foldl Set.intersect first rest |> Set.toList |> List.sort


totalCards : List (Set Card) -> Int
totalCards collections =
    let
        ( first, rest ) =
            splitCollectionList collections
    in
    List.foldl Set.union first rest |> Set.size


splitShinyCards : Set Card -> ( List Card, List Card )
splitShinyCards collection =
    Set.partition isShiny collection
        |> Tuple.mapBoth (Set.toList >> List.sort) (Set.toList >> List.sort)


isShiny : Card -> Bool
isShiny card =
    String.startsWith "Shiny" card



{- Call two functions with the same three parameters,
   and return the result as a tuple
-}


toTuple : (x -> y -> z -> a) -> (x -> y -> z -> b) -> x -> y -> z -> ( a, b )
toTuple fn1 fn2 x y z =
    ( fn1 x y z, fn2 x y z )


splitCollectionList : List (Set a) -> ( Set a, List (Set a) )
splitCollectionList list =
    case list of
        [] ->
            ( Set.empty, [] )

        x :: xs ->
            ( x, xs )
