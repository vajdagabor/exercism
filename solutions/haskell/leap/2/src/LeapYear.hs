module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool

isLeapYear y
  | divisibleBy 400 = True
  | divisibleBy 100 = False
  | divisibleBy 4 = True
  | otherwise = False
  where
    divisibleBy x = y `rem` x == 0
