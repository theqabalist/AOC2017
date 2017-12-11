{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Day11Parser (
    parseInput,
    Direction
) where

import Lib hiding (parse)
import Text.Parsec.Text (Parser)
import Text.Parsec (try, sepBy, parse)
import Text.Parsec.Char (string)
import Control.Applicative ((<|>))
import Data.Functor (($>))

type Direction = (Int, Int)

dir :: Parser Direction
dir = try (string "nw" $> (1, -1)) <|>
      try (string "se" $> (-1, 1)) <|>
      try (string "ne" $> (-1, 0)) <|>
      try (string "sw" $> (1, 0)) <|>
      string "n" $> (0, -1) <|>
      string "s" $> (0, 1)

input :: Parser [Direction]
input = dir `sepBy` string ","

parseInput = unwrap . parse input ""
