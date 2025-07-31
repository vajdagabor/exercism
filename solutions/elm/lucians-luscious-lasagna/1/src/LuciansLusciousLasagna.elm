module LuciansLusciousLasagna exposing (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)

expectedMinutesInOven = 40

preparationTimeInMinutes : Int -> Int
preparationTimeInMinutes layers =
    let
        layerPrepTime = 2
    in
    layerPrepTime * layers

elapsedTimeInMinutes : Int -> Int -> Int
elapsedTimeInMinutes layers timeElapsed = (preparationTimeInMinutes layers) + timeElapsed
