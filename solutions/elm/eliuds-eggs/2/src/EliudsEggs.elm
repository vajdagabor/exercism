module EliudsEggs exposing (eggCount)


eggCount : Int -> Int
eggCount n =
    bitCount n 0

bitCount : Int -> Int -> Int
bitCount n count =
    let
        isOneBitOn = remainderBy 2 n == 1
        nextCount = if isOneBitOn then count + 1 else count
    in
        if n > 0 then bitCount (n // 2) nextCount else count
