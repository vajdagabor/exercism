module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = all (`elem` lowcaseText) ['a'..'z']
  where
    lowcaseText = map toLower text
