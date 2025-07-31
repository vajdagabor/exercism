module MazeMaker exposing (..)

import Random exposing (Generator)


type Maze
    = DeadEnd
    | Room Treasure
    | Branch (List Maze)


type Treasure
    = Gold
    | Diamond
    | Friendship


deadend : Generator Maze
deadend =
    Random.constant DeadEnd


treasure : Generator Treasure
treasure =
    Random.uniform Gold [ Diamond, Friendship ]


room : Generator Maze
room =
    Random.map (\t -> Room t) treasure


branch : Generator Maze -> Generator Maze
branch mazeGenerator =
    Random.int 2 4
        |> Random.andThen (\n -> Random.list n mazeGenerator)
        |> Random.map Branch


maze : Generator Maze
maze =
    Random.andThen identity <|
        Random.weighted
            ( 60, deadend )
            [ ( 15, room )
            , ( 25, Random.lazy (\_ -> branch maze) )
            ]


mazeOfDepth : Int -> Generator Maze
mazeOfDepth depth =
    if depth < 1 then
        Random.uniform deadend [ room ] |> Random.andThen identity

    else
        Random.lazy (\_ -> branch (mazeOfDepth (depth - 1)))
