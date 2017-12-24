{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverloadedStrings #-}
module Days.Day24 (partOne, partTwo) where

import Lib (Parseable(parse))
import Prelude hiding (lines)
import Data.Text (splitOn, lines, unpack)
import Data.Monoid ((<>))
import Data.Foldable (maximum, maximumBy)
import Debug.Trace (traceShowId)

data Port = Port { sideA :: Int, sideB :: Int }
    deriving (Show, Eq)

instance Parseable [Port] where
    parse = fmap ((\[x, y] -> Port {sideA=x, sideB=y}) . fmap (read . unpack) . splitOn "/") . lines

connects :: Int -> Port -> Bool
connects x Port{sideA=a, sideB=b} = x == a || x == b

exposes :: Int -> Port -> Int
exposes x Port{sideA=a, sideB=b} = if x == a then b else a

strength :: Port -> Int
strength Port{sideA=a, sideB=b} = a + b

chainStrength :: [Port] -> Int
chainStrength = sum . fmap strength

findMaxChain :: Int -> [Port] -> Int -> [Port] -> Int
findMaxChain max chain exposed pool | not (any (connects exposed) pool) = max
                                    | otherwise = let candidates = filter (connects exposed) pool
                                                  in maximum (max:fmap (\c -> findMaxChain (chainStrength (chain <> [c])) (chain <> [c]) (exposes exposed c) (filter (/= c) pool)) candidates)

partOne :: [Port] -> Int
partOne = findMaxChain (-1) [] 0

maxLongest (str1, len1) (str2, len2) = if len1 == len2 then compare str1 str2 else compare len1 len2

findLongestChain :: (Int, Int) -> [Port] -> Int -> [Port] -> (Int, Int)
findLongestChain m@(str, len) chain exposed pool | not (any (connects exposed) pool) = m
                                                 | otherwise = let candidates = filter (connects exposed) pool
                                                               in maximumBy maxLongest (fmap (\c -> let newChain = chain <> [c]
                                                                                                    in findLongestChain (chainStrength newChain, length newChain) newChain (exposes exposed c) (filter (/= c) pool)) candidates)

partTwo :: [Port] -> Int
partTwo = fst . findLongestChain (-1, -1) [] 0
