module Allergies (Allergen(..), isAllergicTo, allergies) where

data Allergen =
  Eggs | Peanuts | Shellfish | Strawberries |
  Tomatoes | Chocolate | Pollen | Cats
    deriving (Enum, Eq, Show)

type Score = Int
type Allergens = [Allergen]

allergens :: Allergens
allergens = [Eggs .. Cats]

allergies :: Score -> Allergens
allergies score =
  map toAllergen allergensAndBinOne
  where
    binScore = reverse $ decToBin score
    allergensAndBinOne = filter onlyBinOne allergensAndBin
    allergensAndBin = zip allergens binScore
    toAllergen (a,_) = a

    onlyBinOne (_,1) = True
    onlyBinOne _ = False

decToBin :: Int -> [Int]
decToBin x = reverse $ decToBin' x
  where
    decToBin' 0 = []
    decToBin' y = let (a,b) = quotRem y 2 in b:decToBin' a

isAllergicTo :: Allergen -> Score -> Bool
isAllergicTo allergen =
  elem allergen . allergies
