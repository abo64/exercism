module FoodChain (song) where

import Data.List (intercalate)

type Verse = String

song :: String
song = verses $ length lexicon

verses :: Int -> String
verses howMany =
  intercalate verseSeparator verseLines  ++ verseSeparator
  where
    verseLines :: [Verse]
    verseLines = map verse [0..howMany-1] ++ [lastVerse]
    verseSeparator = "\n\n"

verse :: Int -> Verse
verse n =
  intro ++ animal n ++ ".\n" ++
  (optionalLine . exclamation) n ++
  swallow n ++ outro
  where
    optionalLine line
      | (not . null) line = line ++ "\n"
      | otherwise = ""

swallow :: Int -> Verse
swallow n
  | n > 0 =
    "She swallowed the " ++ animal n ++ " to catch the " ++
    animal (n-1) ++ description (n-1) ++ ".\n" ++
    swallow (n-1)
  | otherwise = ""

intro :: String
intro = "I know an old lady who swallowed a "

outro :: String
outro = "I don't know why she swallowed the fly. Perhaps she'll die."

lastVerse :: String
lastVerse = intro ++ "horse.\nShe's dead, of course!"

type Animal = String
type Exclamation = String
type Description = String
type LexiconLookup a = Int -> a

animal :: LexiconLookup Animal
animal whichOne = a
  where (a, _, _) = lexicon !! whichOne

exclamation :: LexiconLookup Exclamation
exclamation whichOne = e
  where (_, e, _) = lexicon !! whichOne

description :: LexiconLookup Description
description whichOne = d
  where (_, _, d) = lexicon !! whichOne

lexicon ::[(Animal,Exclamation,Description)]
lexicon = [
  ("fly", "", ""),
  ("spider", "It wriggled and jiggled and tickled inside her.", " that wriggled and jiggled and tickled inside her"),
  ("bird", "How absurd to swallow a bird!", ""),
  ("cat", "Imagine that, to swallow a cat!", ""),
  ("dog", "What a hog, to swallow a dog!", ""),
  ("goat", "Just opened her throat and swallowed a goat!", ""),
  ("cow", "I don't know how she swallowed a cow!", "")]
