{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Main where

import Lib
import Prelude hiding (lines)
import Data.Text (Text, splitOn, lines, strip)
import Data.Text.Read (decimal)
import Data.Traversable (sequence, traverse)
import Data.List (find, sort)
import Data.Bifunctor (bimap)
import Data.Map.Strict (Map, empty, findWithDefault, insert)

type Coord = (Int, Int)

neighboorhoodSum :: Coord -> Map Coord Int -> Int
neighboorhoodSum location graph = if theSum == 0 then 1 else theSum
  where
    theSum = sum $ do
      x <- [-1..1]
      y <- [-1..1]
      return $ findWithDefault 0 ((fst location) + x, (snd location) + y) graph

type Parsed = Int

instance Parseable Parsed where
    parse = unwrap . fmap fst . decimal . strip

spiral :: [(Int, Int)]
spiral = do
    idx <- ([1..] :: [Double])
    let armSize = ceiling $ idx / 2
    let sign = if armSize `mod` 2 == 1 then 1 else -1
    let dir = if (floor idx) `mod` 2 == 1 then
                (sign, 0)
              else
                (0, sign)
    take armSize (repeat dir)

partOne :: Parsed -> Int
partOne input = (\(x, y) -> x + y) $ bimap abs abs $ foldr (\(xsum, ysum) (x, y) -> (xsum + x, ysum + y)) (0, 0) $ take (input - 1) spiral

spiralAbs :: [(Int, Int)]
spiralAbs = fmap head $ iterate (\(position:delta:rest) -> (fst position + fst delta, snd position + snd delta):rest) $ [(0, 0)] ++ spiral

summedSpiral :: [(Coord, Int)]
summedSpiral = fmap (\(graph, coord, _) -> (coord, findWithDefault 0 coord graph)) $ iterate (\(graph, c, (curr:rest)) -> (insert curr (neighboorhoodSum curr graph) graph, curr, rest)) ((insert (0, 0) 1 empty) :: Map Coord Int, (0, 0), spiralAbs)

partTwo :: Parsed -> Int
partTwo input = snd . head $ dropWhile (\(coord, v) -> v <= input) summedSpiral

main :: IO ()
main = forkInteract partOne partTwo
