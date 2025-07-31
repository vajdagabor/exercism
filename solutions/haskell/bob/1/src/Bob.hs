module Bob (responseFor) where

import Data.Char (isSpace, isAlpha, isUpper)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor xs
  | isSilence xs = "Fine. Be that way!"
  | isYell xs && isQuestion xs = "Calm down, I know what I'm doing!"
  | isQuestion xs = "Sure."
  | isYell xs = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    isSilence = all isSpace
    isYell s = any isAlpha s && (all isUpper . filter isAlpha $ s)
    isQuestion = (== '?') . last . dropWhileEnd isSpace
