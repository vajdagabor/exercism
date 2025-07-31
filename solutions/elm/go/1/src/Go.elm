module Go exposing (..)

import GoSupport exposing (..)


applyRules : Game -> Rule -> NonValidatingRule -> Rule -> Rule -> Game
applyRules game oneStonePerPointRule captureRule libertyRule koRule =
    case ( koRule game, libertyRule game, oneStonePerPointRule game ) of
        ( Err msg, _, _ ) ->
            { game | error = msg }

        ( _, Err msg, _ ) ->
            { game | error = msg }

        ( _, _, Err msg ) ->
            { game | error = msg }

        _ ->
            changePlayer (captureRule game)
