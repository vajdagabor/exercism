module Anagram exposing (detect)

import String exposing (fromList, toList, toLower)


detect : String -> List String -> List String
detect word =
    List.filter
        (\cand ->
            (sortLower cand == sortLower word) && (toLower cand /= toLower word)
        )


sortLower : String -> String
sortLower =
    toLower >> toList >> List.sort >> fromList
