module TreasureChest exposing (..)


type TreasureChest a
    = TreasureChest String a


getTreasure : String -> TreasureChest a -> Maybe a
getTreasure passwordAttempt (TreasureChest password content) =
    if passwordAttempt == password then
        Just content

    else
        Nothing

multiplyTreasure : (a -> List a) -> TreasureChest a -> TreasureChest (List a)
multiplyTreasure multiplier (TreasureChest password content) =
    TreasureChest password (multiplier content)
