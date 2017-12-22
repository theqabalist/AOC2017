{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Days.Day2 (partOne, partTwo) where

import Lib
import Prelude hiding (lines)
import Data.Text (Text, splitOn, lines)
import Data.Text.Read (decimal)
import Data.Traversable (sequence, traverse)
import Data.List (find, sort)

type Parsed = [[Int]]

instance Parseable Parsed where
    parse = unwrap . traverse (traverse (fmap fst . decimal) . splitOn "\t") . lines

partOne :: Parsed -> Int
partOne = sum . fmap (\row -> let maximum = foldr max 0 row
                                in maximum - foldr min maximum row)

divisibleDivision :: [Int] -> Int
divisibleDivision (v:vs) = let other = find ((==0) . (`mod` v)) vs
                            in maybe (divisibleDivision vs) (`div` v) other
divisibleDivision [] = -1

partTwo :: Parsed -> Int
partTwo = sum . fmap (divisibleDivision . sort)
