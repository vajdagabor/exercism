module TreasureFactory exposing (TreasureChest, getTreasure, makeChest, makeTreasureChest, secureChest, uniqueTreasures)


type TreasureChest treasure
    = TreasureChest String treasure


getTreasure : String -> TreasureChest a -> Maybe a
getTreasure passwordAttempt (TreasureChest password treasure) =
    if passwordAttempt == password then
        Just treasure

    else
        Nothing


type Chest treasure conditions
    = Chest String treasure


makeChest : String -> treasure -> Chest treasure {}
makeChest password treasure =
    Chest password treasure


secureChest : Chest treasure conditions -> Maybe (Chest treasure { conditions | securePassword : () })
secureChest (Chest password treasure) =
    if isStrongPassword password then
        Just (Chest password treasure)

    else
        Nothing


uniqueTreasures : List (Chest treasure conditions) -> List (Chest treasure { conditions | uniqueTreasure : () })
uniqueTreasures chests =
    List.filterMap
        (\((Chest pw treasure) as chest) ->
            if getTreasureCount chest chests == 1 then
                Just (Chest pw treasure)

            else
                Nothing
        )
        chests


getTreasureCount : Chest treasure conditions -> List (Chest treasure conditions) -> Int
getTreasureCount (Chest _ treasure) list =
    List.filter (\(Chest _ currentTreasure) -> currentTreasure == treasure) list
        |> List.length


makeTreasureChest : Chest treasure { conditions | securePassword : (), uniqueTreasure : () } -> TreasureChest treasure
makeTreasureChest (Chest password treasure) =
    TreasureChest password treasure


isStrongPassword : String -> Bool
isStrongPassword password =
    String.length password >= 8
