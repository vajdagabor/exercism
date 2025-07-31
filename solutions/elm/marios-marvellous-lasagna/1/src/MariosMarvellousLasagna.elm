module MariosMarvellousLasagna exposing (remainingTimeInMinutes)


remainingTimeInMinutes : Int -> Int -> Int
remainingTimeInMinutes numOfLayers minutesElapsed =
    let
        expectedMinutesInOven =
            40

        preparationTimeInMinutes layers =
            layers * 2
    in
    expectedMinutesInOven + preparationTimeInMinutes numOfLayers - minutesElapsed
