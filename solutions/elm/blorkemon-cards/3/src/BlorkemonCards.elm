module BlorkemonCards exposing
    ( Card
    , compareShinyPower
    , expectedWinner
    , isMorePowerful
    , maxPower
    , sortByCoolness
    , sortByMonsterName
    )


type alias Card =
    { monster : String, power : Int, shiny : Bool }


isMorePowerful : Card -> Card -> Bool
isMorePowerful card1 card2 =
    card1.power > card2.power


maxPower : Card -> Card -> Int
maxPower card1 card2 =
    max card1.power card2.power


sortByMonsterName : List Card -> List Card
sortByMonsterName cards =
    List.sortBy .monster cards


sortByCoolness : List Card -> List Card
sortByCoolness cards =
    let
        order { shiny, power } =
            ( negate (boolToInt shiny), negate power )
    in
    List.sortBy order cards


compareShinyPower : Card -> Card -> Order
compareShinyPower card1 card2 =
    compare ( card1.power, boolToInt card1.shiny ) ( card2.power, boolToInt card2.shiny )


expectedWinner : Card -> Card -> String
expectedWinner card1 card2 =
    case compareShinyPower card1 card2 of
        GT ->
            card1.monster

        LT ->
            card2.monster

        EQ ->
            "too close to call"


boolToInt : Bool -> Int
boolToInt b =
    if b == False then
        0

    else
        1
