{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Lib
import Prelude hiding (lines)
import Data.Vector (fromList, Vector, (//), (!))
import Data.Text (lines, Text, pack)
import Data.Text.Read (decimal, signed)

type Parsed = Vector Int
instance Parseable Parsed where
    parse = fromList . unwrap . traverse (fmap fst . signed decimal) . lines

travel :: Int -> Int -> Vector Int -> Int
travel runs idx v = if idx < 0 || idx >= length v then
                        runs
                    else
                        travel (runs + 1) (idx + value) new
                          where
                              value = v ! idx
                              new = v // [(idx, value + 1)]

travel2 :: Int -> Int -> Vector Int -> Int
travel2 runs idx v = if idx < 0 || idx >= length v then
                       runs
                     else
                       travel2 (runs + 1) (idx + value) new
                         where
                           value = v ! idx
                           new = v // [(idx, if value >= 3 then value - 1 else value + 1)]

partOne :: Parsed -> Int
partOne = travel 0 0

partTwo :: Parsed -> Int
partTwo = travel2 0 0

main :: IO ()
main = forkInteract partOne partTwo
