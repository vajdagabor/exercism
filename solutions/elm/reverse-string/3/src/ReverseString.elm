module ReverseString exposing (reverse)

import String exposing (cons, foldl)


reverse : String -> String
reverse =
    foldl cons ""
