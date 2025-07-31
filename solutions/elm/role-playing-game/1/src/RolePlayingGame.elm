module RolePlayingGame exposing (Player, castSpell, introduce, revive)


type alias Player =
    { name : Maybe String
    , level : Int
    , health : Int
    , mana : Maybe Int
    }


introduce : Player -> String
introduce { name } =
    Maybe.withDefault "Mighty Magician" name


revive : Player -> Maybe Player
revive player =
    let
        freshMana =
            if player.level >= 10 then
                Just 100

            else
                Nothing
    in
    case player.health of
        0 ->
            Just
                { player | health = 100, mana = freshMana }

        _ ->
            Nothing


castSpell : Int -> Player -> ( Player, Int )
castSpell manaCost player =
    case player.mana of
        Just mana ->
            if mana >= manaCost then
                ( { player | mana = Just (mana - manaCost) }, manaCost * 2 )

            else
                ( player, 0 )

        Nothing ->
            ( { player | health = max (player.health - manaCost) 0 }, 0 )
