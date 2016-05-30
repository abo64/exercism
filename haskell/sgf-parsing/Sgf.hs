module Sgf (parseSgf) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tree
import Data.Text (Text, pack, unpack, singleton)
import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((*>), (<$>))
import Data.Maybe (fromMaybe)

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
--
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe SgfTree
parseSgf text = either (const Nothing) Just parseResult
  where parseResult = parse sgfGameTree "sgfGameTree" (unpack text)

sgfGameTree :: Parser SgfTree
sgfGameTree = do
  _ <- char '('
  (rootNode:subNodes) <- many1 sgfNode
  maybeSubTrees <- optionMaybe $ many1 sgfGameTree
  _ <- char ')'
  return $ toSgfTree subNodes maybeSubTrees rootNode

toSgfTree :: [SgfNode] -> Maybe [SgfTree] -> SgfNode -> SgfTree
toSgfTree subNodes maybeSubTrees node =
  Node node $ subNodeForest ++ subTreeForest
  where
    subNodeForest = toSgfTree [] Nothing <$> subNodes
    subTreeForest = fromMaybe [] maybeSubTrees

sgfNode :: Parser SgfNode
sgfNode = do
  maybeProp <- char ';' *> optionMaybe sgfProperty
  let property = fromMaybe Map.empty maybeProp
  return property

sgfProperty :: Parser SgfNode
sgfProperty = do
  propIdent <- singleton <$> upper
  propValues <- many1 sgfPropValue
  return $ Map.singleton propIdent propValues

sgfPropValue :: Parser Text
sgfPropValue = pack <$> between (char '[') (char ']') value
--  char '[' *> (pack <$> value) <* char ']'
  where
    value = concat <$> many1 valuePart
    valuePart =
      quotedClosingBracket
      <|> quotedNewline
      <|> quotedBackslash
      <|> backslash
      <|> whitespace
      <|> propIdent
    quotedClosingBracket = toString $ quoted ']'
    quotedBackslash = toString $ quoted '\\'
    quotedNewline = ignore $ quoted '\n'
    whitespace = replace " " space
    backslash = ignore $ char '\\'
    propIdent = toString $ noneOf "]"

    quoted c = try $ const c <$> string ['\\', c]
    ignore = replace ""
    replace s = fmap $ const s
    toString = fmap (: [])
