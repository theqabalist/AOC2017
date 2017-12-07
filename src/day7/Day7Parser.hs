{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Day7Parser (
    parseLine
) where

import Lib hiding (parse)
import Data.Functor (($>))
import Text.Parsec (many1, Parsec, sepBy1, parse)
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (digit, char, letter, spaces, string)
import Data.Text (Text, pack)
import Data.Text.Read (decimal)
import Control.Applicative ((<|>))

number :: Parser Int
number = read <$> many1 digit

weight :: Parser Int
weight = char '(' *> number <* char ')'

name :: Parser Text
name = pack <$> many1 letter

right :: Parser ()
right = string "->" $> ()

comma :: Parser ()
comma = char ',' $> ()

lineParser :: Parsec Text () (Text, Int, [Text])
lineParser = (,,) <$> (name <* spaces) <*> (weight <* spaces) <*> ((right *> spaces *> sepBy1 name (comma *> spaces)) <|> pure [])

parseLine :: Text -> (Text, Int, [Text])
parseLine = unwrap . parse lineParser ""
