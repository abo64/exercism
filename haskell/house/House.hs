module House ( rhyme ) where

import Data.List (intercalate)

type Sentence = String
type Noun = String
type Verb = String
type Embeddings = Int

rhyme :: String
rhyme = sentences $ length lexicon

sentences :: Embeddings -> Sentence
sentences howMany =
  intercalate "\n\n" sentenceList ++ "\n\n"
  where
    sentenceList = map sentence [0..howMany-1]

sentence :: Embeddings -> Sentence
sentence = ("This is" ++) . flip nounPhrase ""

nounPhrase :: Embeddings -> Sentence -> Sentence
nounPhrase embeddings sentenceSoFar =
  relativeClause embeddings $ sentenceSoFar ++ np
  where
    np = " the " ++ noun embeddings

relativeClause :: Embeddings -> Sentence -> Sentence
relativeClause embeddings sentenceSoFar
  | terminate = sentenceContinued
  | otherwise = nounPhrase (embeddings - 1) sentenceContinued
  where
    sentenceContinued = sentenceSoFar ++ clause
    clause = separator ++ "that " ++ verb embeddings
    separator = if terminate then " " else "\n"
    terminate = embeddings == 0

noun :: Embeddings -> Noun
noun =  fst . (lexicon !!)

verb :: Embeddings -> Verb
verb =  snd . (lexicon !!)

lexicon :: [(Noun, Verb)]
lexicon = reverse [
 ("horse and the hound and the horn", "belonged to"),
 ("farmer sowing his corn", "kept"),
 ("rooster that crowed in the morn", "woke"),
 ("priest all shaven and shorn", "married"),
 ("man all tattered and torn", "kissed"),
 ("maiden all forlorn", "milked"),
 ("cow with the crumpled horn", "tossed"),
 ("dog", "worried"),
 ("cat", "killed"),
 ("rat", "ate"),
 ("malt", "lay in"),
 ("house", "Jack built.")]