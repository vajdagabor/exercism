module TisburyTreasureHunt exposing (..)


type alias TreasureLocation =
    ( Int, Char )


type alias PlaceLocation =
    ( Char, Int )


type alias Treasure =
    ( String, TreasureLocation )


type alias Place =
    ( String, PlaceLocation )


placeLocationToTreasureLocation : PlaceLocation -> TreasureLocation
placeLocationToTreasureLocation ( letter, num ) =
    ( num, letter )


treasureLocationMatchesPlaceLocation : PlaceLocation -> TreasureLocation -> Bool
treasureLocationMatchesPlaceLocation placeLocation treasureLocation =
    placeLocationToTreasureLocation placeLocation == treasureLocation


countPlaceTreasures : ( String, PlaceLocation ) -> List Treasure -> Int
countPlaceTreasures ( _, placeLocation ) treasures =
    let
        treasureLocation =
            placeLocationToTreasureLocation placeLocation

        matchesLocation ( _, tLocation ) =
            treasureLocation == tLocation
    in
    List.filter matchesLocation treasures |> List.length


specialCaseSwapPossible : Treasure -> Place -> Treasure -> Bool
specialCaseSwapPossible ( foundTreasure, _ ) ( place, _ ) ( desiredTreasure, _ ) =
    case ( foundTreasure, place, desiredTreasure ) of
        ( "Brass Spyglass", "Abandoned Lighthouse", _ ) ->
            True

        ( "Amethyst Octopus", "Stormy Breakwater", desired ) ->
            desired == "Crystal Crab" || desired == "Glass Starfish"

        ( "Vintage Pirate Hat", "Harbor Managers Office", desired ) ->
            desired == "Model Ship in Large Bottle" || desired == "Antique Glass Fishnet Float"

        _ ->
            False
