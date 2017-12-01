{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Lib
import Data.Text (strip, chunksOf, Text, unpack, pack)
import Data.Text.Read (decimal)
import Data.Traversable (sequence, traverse)
import Data.Foldable
import Data.Either

pairUp :: Int -> [Int] -> [(Int, Int)]
pairUp distance vs = let indexed = zip [0..] vs :: [(Int, Int)]
    in foldl' (\a (i, v) -> a ++ [(v, vs !! ((i + distance) `mod` length vs))]) [] indexed

type PairUp = ([Int] -> [(Int, Int)])

type Parsed = Either String [Int]

instance Parseable Parsed where
    parse = traverse (fmap fst . decimal) . chunksOf 1

solution :: PairUp -> Parsed -> Int
solution pu = unwrap . fmap (sum . fmap fst . filter (uncurry (==)) . pu)

main :: IO ()
main = forkInteract (solution (pairUp 1)) (solution (\vs -> pairUp (length vs `div` 2) vs))
