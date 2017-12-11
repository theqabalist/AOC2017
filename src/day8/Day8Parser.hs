{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Day8Parser (
    parseLine,
    Op,
    ConditionalF
) where

import Lib hiding (parse)
import Data.Functor (($>))
import Text.Parsec (many1, Parsec, parse, option, try)
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (digit, char, letter, spaces, string)
import Data.Text (Text, pack)
import Data.Text.Read (decimal)
import Control.Applicative ((<|>))

register :: Parser Text
register = pack <$> many1 letter

type Op = Int -> Int -> Int
instance Show Op where
    show = const "<(Int -> Int -> Int)>"

op :: Parser (Int -> Int -> Int)
op = string "inc" $> (+) <|>
     string "dec" $> (-)

number = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

type ConditionalF = Int -> Int -> Bool

instance Show ConditionalF where
    show = const "<(Int -> Int -> Bool)"

condition :: Parser ConditionalF
condition = try (string "<=") $> (<=) <|>
            try (string ">=") $> (>=) <|>
            string "!=" $> (/=) <|>
            string "==" $> (==) <|>
            string "<" $> (<) <|>
            string ">" $> (>)

guard :: Parser (Text, ConditionalF, Int)
guard = (,,) <$> (string "if" *> spaces *> register) <*> (spaces *> condition) <*> (spaces *> number)

instruction :: Parser (Text, Op, Int)
instruction = (,,) <$> register <*> (spaces *> op) <*> (spaces *> number)

guardedInstruction :: Parser ((Text, Op, Int), (Text, ConditionalF, Int))
guardedInstruction = (,) <$> instruction <*> (spaces *> guard)

parseLine :: Text -> ((Text, Op, Int), (Text, ConditionalF, Int))
parseLine = unwrap . parse guardedInstruction ""
