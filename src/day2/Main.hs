{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Main where

import Lib
import Prelude hiding (lines)
import Data.Text (Text, splitOn, lines)
import Data.Text.Read (decimal)
import Data.Traversable (sequence, traverse)

type Parsed = [[Int]]

instance Parseable Parsed where
    parse = unwrap . sequence . fmap (traverse (fmap fst . decimal) . splitOn "\t") . lines

partOne :: Parsed -> Int
partOne = sum . fmap (\row -> let maximum = foldr max 0 row
                                in maximum - foldr min maximum row)

divisibleDivision :: [Int] -> Int
divisibleDivision vs = head $ do
    x <- vs
    y <- vs
    if x `mod` y == 0 && x /= y
      then return $ x `div` y
      else []

partTwo :: Parsed -> Int
partTwo = sum . fmap divisibleDivision

main :: IO ()
main = forkInteract partOne partTwo
