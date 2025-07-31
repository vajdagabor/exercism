module SecureTreasureChest exposing
    ( Password
    , SecureTreasureChest
    , createPassword
    , createTreasure
    , getTreasure
    )


type Password
    = Password String


type SecureTreasureChest treasure
    = SecureTreasureChest Password treasure


createPassword : String -> Maybe Password
createPassword passwordCandidate =
    if isPasswordValid passwordCandidate then
        Just (Password passwordCandidate)
    else
        Nothing

isPasswordValid : String -> Bool
isPasswordValid password =
    String.length password >= 8

createTreasure : treasure -> Password -> SecureTreasureChest treasure
createTreasure treasure password =
    SecureTreasureChest password treasure

getTreasure : String -> SecureTreasureChest treasure -> Maybe treasure
getTreasure passwordAttempt (SecureTreasureChest password treasure) =
    if Password passwordAttempt == password then
        Just treasure
    else
        Nothing
