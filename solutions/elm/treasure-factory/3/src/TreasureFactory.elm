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
    let
        treasures =
            List.map (\(Chest _ treasure) -> treasure) chests
    in
    List.filterMap
        (\(Chest pw treasure) ->
            if isUnique treasure treasures then
                Just (Chest pw treasure)

            else
                Nothing
        )
        chests


isUnique : a -> List a -> Bool
isUnique item items =
    List.filter (\currentItem -> currentItem == item) items
        |> List.length
        |> (==) 1


makeTreasureChest : Chest treasure { conditions | securePassword : (), uniqueTreasure : () } -> TreasureChest treasure
makeTreasureChest (Chest password treasure) =
    TreasureChest password treasure


isStrongPassword : String -> Bool
isStrongPassword password =
    String.length password >= 8
