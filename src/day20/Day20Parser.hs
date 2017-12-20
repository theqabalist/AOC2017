module Day20Parser (
    parseLine,
    V3,
    Particle(P)
) where

import Data.Monoid (Sum(Sum), (<>))
import Text.Parsec.Text (Parser)
import Text.Parsec (string, option, char, many1, digit, parse)

type V3 = (Sum Int, Sum Int, Sum Int)
data Particle = P V3 V3 V3
    deriving (Show, Eq)

number :: Parser Int
number = read <$> ((<>) <$> option "" (string "-") <*> many1 digit)

v3 :: String -> Parser V3
v3 pref = (,,) <$> (Sum <$> (string pref *> number)) <*> (Sum <$> (string "," *> number)) <*> (Sum <$> (string "," *> number <* string ">"))

lineParser :: Parser Particle
lineParser = P <$> v3 "p=<" <*> (string ", " *> v3 "v=<") <*> (string ", " *> v3 "a=<")

parseLine = either (error . show) id . parse lineParser ""
