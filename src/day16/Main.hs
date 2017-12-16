{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Main where

import Day16Parser (Instruction(..), parseInput)
import Lib (forkInteract, Parseable(parse), unwrap)
import Data.Vector.Unboxed (Vector, fromList, toList)
import Data.Foldable (foldl')

instance Parseable [Instruction Char] where
    parse = unwrap . parseInput

applyInstruction :: Instruction a -> Vector a -> Vector a
applyInstruction (Rotate _ f) = f
applyInstruction (SwapPos _ _ f) = f
applyInstruction (SwapVal _ _ f) = f

partOne :: [Instruction Char] -> Vector Char
partOne = foldl' (flip applyInstruction) (fromList "abcdefghijklmnop")

partTwo :: [Instruction Char] -> Vector Char
partTwo input = danceStream !! interestPoint
    where run x = foldl' (flip applyInstruction) x input
          danceStream = iterate run (fromList "abcdefghijklmnop")
          cycleSize = (+1) . length . takeWhile (/="abcdefghijklmnop") . tail . fmap toList $ danceStream
          interestPoint = 1000000000 `mod` cycleSize
main = forkInteract partOne partTwo
