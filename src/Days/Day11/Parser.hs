{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Days.Day11.Parser (
    parseInput,
    Direction(..)
) where

import Lib hiding (parse)
import Text.Parsec.Text (Parser)
import Text.Parsec (try, sepBy, parse)
import Text.Parsec.Char (string)
import Control.Applicative ((<|>))
import Data.Functor (($>))

newtype Direction = Dir {getDir::(Int, Int)}

dir :: Parser Direction
dir = try (string "nw" $> Dir (1, -1)) <|>
      try (string "se" $> Dir (-1, 1)) <|>
      try (string "ne" $> Dir (-1, 0)) <|>
      try (string "sw" $> Dir (1, 0)) <|>
      string "n" $> Dir (0, -1) <|>
      string "s" $> Dir (0, 1)

input :: Parser [Direction]
input = dir `sepBy` string ","

parseInput = unwrap . parse input ""
