module Days.Day22.Parser (
    parseInput,
    Infection(..),
    V2
) where

import Text.Parsec.Text (Parser)
import Text.Parsec (char, many1, sepBy1, string, newline, parse)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List (transpose, foldl')
import Data.Monoid (Sum(Sum))
import Data.Map (Map, fromList, empty, union)

data Infection = Old | New | Clean | Flagged | Weakened
    deriving (Show, Eq)

type Column = Sum Int
type Row = Sum Int
type V2 = (Column, Row)

cellParser :: Parser Infection
cellParser = (char '#' $> Old) <|> (char '.' $> Clean)

rowParser :: Parser [(Column, Infection)]
rowParser = (\row -> let bound = (length row - 1) `div` 2 in zip (Sum <$> [-bound..bound]) row) <$> many1 cellParser

rowsParser :: Parser [(Row, [(Column, Infection)])]
rowsParser = (\rows -> let bound = (length rows - 1) `div` 2 in zip (Sum <$> reverse [-bound..bound]) rows) <$> sepBy1 rowParser newline

rowsToMap rows = fromList $ do
    (y, cols) <- rows
    (x, cell) <- cols
    return ((x, y), cell)

inputParser :: Parser (Map V2 Infection)
inputParser = rowsToMap <$> rowsParser

parseInput = either (error . show) id . parse inputParser "input.txt"
