{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Days.Day11 (partOne, partTwo) where

import Lib
import Days.Day11.Parser (parseInput, Direction(..))
import Data.List (foldl', maximum)

type Parsed = [Direction]

instance Parseable Parsed where
    parse = parseInput

distance :: (Int, Int) -> Int
distance (a, b) = (abs a + abs (a + b) + abs b) `div` 2

distanceForPath :: [Direction] -> Int
distanceForPath = distance . foldl' (\(x, y) (a, b) -> (x + a, y + b)) (0, 0) . fmap getDir

partOne :: Parsed -> Int
partOne = distanceForPath

subPaths :: [Direction] -> [[Direction]]
subPaths input = let size = length input
                     subLens = [0..(size - 1)]
                 in fmap (`take` input) subLens

partTwo :: Parsed -> Int
partTwo = maximum . fmap distanceForPath . subPaths
