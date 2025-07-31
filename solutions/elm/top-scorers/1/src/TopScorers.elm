module TopScorers exposing (..)

import Dict exposing (Dict)
import TopScorersSupport exposing (PlayerName)


updateGoalCountForPlayer : PlayerName -> Dict PlayerName Int -> Dict PlayerName Int
updateGoalCountForPlayer playerName playerGoalCounts =
    let
        valueOrDefault : Maybe Int -> Maybe Int
        valueOrDefault maybevalue =
            Just (Maybe.withDefault 0 maybevalue + 1)
    in
    Dict.update playerName valueOrDefault playerGoalCounts


aggregateScorers : List PlayerName -> Dict PlayerName Int
aggregateScorers playerNames =
    List.foldl updateGoalCountForPlayer Dict.empty playerNames


removeInsignificantPlayers : Int -> Dict PlayerName Int -> Dict PlayerName Int
removeInsignificantPlayers goalThreshold playerGoalCounts =
    Dict.filter (\_ count -> count >= goalThreshold) playerGoalCounts


resetPlayerGoalCount : PlayerName -> Dict PlayerName Int -> Dict PlayerName Int
resetPlayerGoalCount playerName playerGoalCounts =
    Dict.update playerName (\_ -> Just 0) playerGoalCounts


formatPlayer : PlayerName -> Dict PlayerName Int -> String
formatPlayer playerName playerGoalCounts =
    let
        count =
            Dict.get playerName playerGoalCounts |> Maybe.withDefault 0
    in
    goalCountToString playerName count


formatPlayers : Dict PlayerName Int -> String
formatPlayers players =
    Dict.toList players
        |> List.map (\( name, count ) -> goalCountToString name count)
        |> String.join ", "


combineGames : Dict PlayerName Int -> Dict PlayerName Int -> Dict PlayerName Int
combineGames game1 game2 =
    Dict.merge
        (\name count merged -> Dict.insert name count merged)
        (\name count1 count2 merged -> Dict.insert name (count1 + count2) merged)
        (\name count merged -> Dict.insert name count merged)
        game1
        game2
        Dict.empty


goalCountToString : PlayerName -> Int -> String
goalCountToString playerName count =
    playerName ++ ": " ++ String.fromInt count
