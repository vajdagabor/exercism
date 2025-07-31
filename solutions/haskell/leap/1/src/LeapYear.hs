module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool

isLeapYear y
  | y `rem` 400 == 0 = True
  | y `rem` 100 == 0 = False
  | y `rem` 4 == 0 = True
  | otherwise = False
