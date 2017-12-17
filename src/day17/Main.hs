{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Main where

import Lib (Parseable(parse), forkInteract)
import Data.Text (unpack)
import Data.List (splitAt)
import Debug.Trace (traceShowId)

instance Parseable Int where
    parse = read . unpack

solve :: Int -> Int -> [Int] -> Int -> Int
solve 2018 i l _ = l !! i
solve n i l rots = solve (n + 1) (pos + 1) (firstPart ++ [n] ++ lastPart) rots
    where (firstPart, lastPart) = splitAt pos l
          pos = (i + rots) `mod` length l

partOne = solve 1 0 [0]

solve2 :: Int -> Int -> Int -> Int -> Int -> Int
solve2 50000000 i c1 l _ = c1
solve2 n i c1 l rots = solve2 (n + 1) (pos + 1) (if pos == 0 then n else c1) (l + 1) rots
    where pos = (i + rots) `mod` l

partTwo = solve2 1 0 0 1

main = forkInteract partOne partTwo
