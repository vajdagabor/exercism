module Leap exposing (isLeapYear)


isLeapYear : Int -> Bool
isLeapYear year =
    if modBy 100 year == 0 then
        modBy 400 year == 0

    else
        modBy 4 year == 0
