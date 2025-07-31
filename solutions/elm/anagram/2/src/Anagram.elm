module Anagram exposing (detect)

import String exposing (fromList, toList, toLower)


detect : String -> List String -> List String
detect word =
    List.filter
        (\cand ->
            (lowerSort cand == lowerSort word) && (toLower cand /= toLower word)
        )


lowerSort : String -> String
lowerSort =
    toLower >> toList >> List.sort >> fromList
