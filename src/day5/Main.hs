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

travel :: (Int -> Int) -> Int -> Int -> Vector Int -> Int
travel valAdj runs idx v = if idx < 0 || idx >= length v then
                       runs
                     else
                       travel valAdj (runs + 1) (idx + value) new
                         where
                           value = v ! idx
                           new = v // [(idx, valAdj value)]

partOne :: Parsed -> Int
partOne = travel (+1) 0 0

partTwo :: Parsed -> Int
partTwo = travel (\v -> if v >= 3 then v - 1 else v + 1) 0 0

main :: IO ()
main = forkInteract partOne partTwo
