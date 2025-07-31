module Bob (responseFor) where

import Data.Char (isSpace, isAlpha, isLower)

responseFor :: String -> String
responseFor s
  | isSilence = "Fine. Be that way!"
  | otherwise = case (isYell, isQuestion) of
      (True, True) -> "Calm down, I know what I'm doing!"
      (True, False) -> "Whoa, chill out!"
      (False, True) -> "Sure."
      (False, False) -> "Whatever."
  where
    isSilence = all isSpace s
    isYell = any isAlpha s && not (any isLower s)
    isQuestion = (== '?') . last . filter (not . isSpace) $ s
