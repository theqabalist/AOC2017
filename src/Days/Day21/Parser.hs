module Days.Day21.Parser (
    parseInput
) where

import Text.Parsec.Text (Parser)
import Text.Parsec (char, many1, sepBy1, string, newline, parse)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Matrix (Matrix, fromLists, toList)
import Data.List (transpose, foldl')
import Data.Map (Map, fromList, empty, union)

cellParser :: Parser Int
cellParser = (char '#' $> 1) <|> (char '.' $> 0)

rowParser :: Parser [Int]
rowParser = many1 cellParser

flipVert = reverse
flipHoriz = fmap reverse
rot90 = fmap reverse . transpose
rot180 = rot90 . rot90
rot270 = reverse . transpose

transformations a = do
    flip <- [id, flipVert, flipHoriz]
    rot <- [id, rot90, rot180, rot270]
    return $ flip $ rot a

inputMatrixParser :: Parser [Matrix Int]
inputMatrixParser = fmap fromLists . transformations <$> sepBy1 rowParser (char '/')

outputMatrixParser :: Parser (Matrix Int)
outputMatrixParser = fromLists <$> sepBy1 rowParser (char '/')

instance (Ord a) => Ord (Matrix a) where
    compare m1 m2 = compare (toList m1) (toList m2)

intoMap :: [Matrix Int] -> Matrix Int -> Map (Matrix Int) (Matrix Int)
intoMap is os = fromList $ zip is (repeat os)

lineParser :: Parser (Map (Matrix Int) (Matrix Int))
lineParser = intoMap <$> inputMatrixParser <*> (string " => " *> outputMatrixParser)

inputParser :: Parser (Map (Matrix Int) (Matrix Int))
inputParser = foldl' union empty <$> sepBy1 lineParser newline

parseInput = either (error . show) id . parse inputParser "input.txt"
