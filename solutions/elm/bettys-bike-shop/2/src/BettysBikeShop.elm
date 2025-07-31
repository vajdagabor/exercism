module BettysBikeShop exposing (penceToPounds, poundsToString)


penceToPounds : Int -> Float
penceToPounds =
    toFloat >> (*) 0.01


poundsToString : Float -> String
poundsToString =
    String.fromFloat >> (++) "£"
