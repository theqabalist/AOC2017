{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Main where

import Lib
import Prelude hiding (lines)
import Data.Text (splitOn, lines)
import Data.Text.Read (decimal)
import Data.Foldable (foldl')
import Data.List (dropWhile)
import Debug.Trace (traceShowId, traceShow)
import Control.Applicative ((<*>))

type Firewall = [(Int, Int)]

readText = fst . unwrap . decimal

instance Parseable Firewall where
    parse = fmap ((\[x, y] -> (x, y)) . fmap readText . splitOn ": ") . lines

costWithDelay :: Int -> Firewall -> Int
costWithDelay delay = foldl' (\cost (location, range) -> if (location + delay) `mod` ((range - 1) * 2) == 0 then cost + location * range else cost) 0

partOne :: Firewall -> Int
partOne = foldl' (\cost (location, range) -> if location `mod` ((range - 1) * 2) == 0 then cost + location * range else cost) 0

isCaught :: Int -> Firewall -> Bool
isCaught delay = foldl' (\caught (location, range) -> caught || (location + delay) `mod` ((range - 1) * 2) == 0) False

partTwo :: Firewall -> Int
partTwo = fst . head . dropWhile snd . zip [0..] . (fmap isCaught [0..] <*>) . pure

main = forkInteract partOne partTwo
