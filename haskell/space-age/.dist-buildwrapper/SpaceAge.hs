module SpaceAge (Planet(..), ageOn) where

data Planet = Earth | Mercury | Venus | Mars |
  Jupiter | Saturn | Uranus | Neptune deriving (Eq, Show)

ageOn :: Planet -> Double -> Double
ageOn planet age =
  age / orbitalPeriod planet / earthYear

orbitalPeriod :: Planet -> Double
orbitalPeriod planet = case planet of
  Earth -> 1
  Mercury -> 0.2408467
  Venus -> 0.61519726
  Mars -> 1.8808158
  Jupiter -> 11.862615
  Uranus -> 84.016846
  Saturn -> 29.447498
  Neptune -> 164.79132

earthYear = 31557600
  