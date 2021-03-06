{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Days.Day5 (partOne, partTwo) where

import Lib
import Prelude hiding (lines, read, length)
import Data.Vector.Unboxed (fromList, Vector, (//), (!), thaw, length)
import Data.Vector.Unboxed.Mutable (modify, MVector, read)
import Data.Text (lines, Text, pack)
import Data.Text.Read (decimal, signed)
import Data.STRef (newSTRef, modifySTRef')
import Control.Monad.ST (runST)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Debug.Trace (traceShowId)

type Parsed = Vector Int
instance Parseable Parsed where
    parse = fromList . unwrap . traverse (fmap fst . signed decimal) . lines

-- mutable ds (second attempt)
travel' :: (Int -> Int) -> Vector Int -> Int
travel' valAdj v = runST $ do
  let size = length v
  vec <- thaw v
  let loop runs idx | idx < 0 || idx >= size = return runs
                    | otherwise = do
                        value <- read vec idx
                        modify vec valAdj idx
                        loop (runs + 1) (idx + value)
  loop 0 0

-- immutable ds (first attempt)
travel'' :: (Int -> Int) -> Int -> Int -> Vector Int -> Int
travel'' valAdj runs idx v = if idx < 0 || idx >= length v then
                             runs
                           else
                             travel'' valAdj (runs + 1) (idx + value) new
                               where
                                 value = v ! idx
                                 new = v // [(idx, valAdj value)]

travel :: (Int -> Int) -> Vector Int -> Int
travel valAdj = travel'' valAdj 0 0

partOne :: Parsed -> Int
partOne = travel' (+1)

partTwo :: Parsed -> Int
partTwo = travel' (\v -> if v >= 3 then v - 1 else v + 1)
