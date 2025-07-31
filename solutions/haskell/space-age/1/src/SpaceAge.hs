module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = case planet of
  Mercury -> ey seconds / 0.2408467
  Venus -> ey seconds / 0.61519726
  Earth -> ey seconds
  Mars -> ey seconds / 1.8808158
  Jupiter -> ey seconds / 11.862615
  Saturn -> ey seconds / 29.447498
  Uranus -> ey seconds / 84.016846
  Neptune -> ey seconds / 164.79132
  where
    ey s = s / 31557600
