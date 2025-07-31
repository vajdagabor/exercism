module RobotSimulator exposing
    ( Bearing(..)
    , Robot
    , advance
    , defaultRobot
    , simulate
    , turnLeft
    , turnRight
    )


type Bearing
    = North
    | East
    | South
    | West


type alias Robot =
    { currentBearing : Bearing
    , coordinates : { x : Int, y : Int }
    }


defaultRobot : Robot
defaultRobot =
    { currentBearing = North
    , coordinates = { x = 0, y = 0 }
    }


turnRight : Robot -> Robot
turnRight ({ currentBearing } as robot) =
    let
        newBearing =
            case currentBearing of
                North -> East
                East -> South
                South -> West
                West -> North
    in
    { robot | currentBearing = newBearing }


turnLeft : Robot -> Robot
turnLeft ({ currentBearing } as robot) =
    let
        newBearing =
            case currentBearing of
                North -> West
                West -> South
                South -> East
                East -> North
    in
    { robot | currentBearing = newBearing }


advance : Robot -> Robot
advance ({ currentBearing, coordinates } as robot) =
    let
        { x, y } =
            coordinates

        newCoords =
            case currentBearing of
                North -> { coordinates | y = y + 1 }
                East -> { coordinates | x = x + 1 }
                South -> { coordinates | y = y - 1 }
                West -> { coordinates | x = x - 1 }
    in
    { robot | coordinates = newCoords }


simulate : String -> Robot -> Robot
simulate directions robot =
    directions
        |> String.toList
        |> List.foldl processCommand robot


processCommand : Char -> Robot -> Robot
processCommand command robot =
    case command of
        'L' -> turnLeft robot
        'R' -> turnRight robot
        'A' -> advance robot
        _ -> robot
