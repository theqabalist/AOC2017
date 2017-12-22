{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Days.Day15 (partOne, partTwo) where

import Prelude hiding (lines, words)
import Lib (Parseable(parse), forkInteract, binWord)
import Data.Text (lines, words, unpack)
import Numeric (showIntAtBase)
import Data.Bits ((.&.))

type Seed = Int
type Factor = Int

instance Parseable [(Seed, Factor)] where
    parse = (`zip` [16807, 48271]) . fmap (read . unpack . last . words) . lines

judge :: Seed -> Int
judge = (.&. 0xFFFF)

divisibleBy :: Int -> Int -> Bool
divisibleBy n x = x `mod` n == 0

type GenStream = [(Seed, Factor)]

initStream :: (Seed, Factor) -> [(Seed, Factor)]
initStream = iterate (\(s, f) -> ((s * f) `mod` 2147483647, f))

doesMatch :: ((Seed, Factor), (Seed, Factor)) -> Bool
doesMatch (genA, genB) = judge (fst genA) == judge (fst genB)

solution :: ((Seed, Factor) -> Bool) -> ((Seed, Factor) -> Bool) -> Int -> [(Seed, Factor)] -> Int
solution fA fB iters [genA, genB] = let streamA = filter fA $ initStream genA
                                        streamB = filter fB $ initStream genB
                                    in length . filter id . take iters . fmap doesMatch $ zip streamA streamB

partOne = (solution (const True) (const True) 40000000)
partTwo = (solution (divisibleBy 4 . fst) (divisibleBy 8 . fst) 5000000)
