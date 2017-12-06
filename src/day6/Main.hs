{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Main where

import Lib
import Prelude hiding (read)
import Data.Vector (Vector, fromList, foldl', elemIndex, thaw, freeze)
import Data.Vector.Mutable (read, write, modify)
import Data.Text (splitOn)
import Data.Text.Read (decimal)
import Data.Maybe (fromMaybe)
import Control.Monad.ST (runST)
import Data.Set (Set)
import qualified Data.Set as S

type Parsed = Vector Int
instance Parseable Parsed where
    parse = fromList . unwrap . traverse (fmap fst . decimal) . splitOn "\t"

maxIndex :: Vector Int -> Int
maxIndex v = fromMaybe (-1) $ elemIndex (foldl' max 0 v) v

redistribute :: Vector Int -> Vector Int
redistribute v = runST $ do
    let idx = maxIndex v
    let size = length v
    vec <- thaw v
    remaining <- read vec idx
    write vec idx 0
    let go rem idx | rem == 0 = freeze vec
                   | otherwise = do
                        modify vec (+1) idx
                        go (rem - 1) ((idx + 1) `mod` size)
    go remaining ((idx + 1) `mod` size)

intermediate :: Parsed -> (Set (Vector Int), Vector Int)
intermediate = accum S.empty
  where accum s v | S.member v s = (s, v)
                  | otherwise = accum (S.insert v s) (redistribute v)

partOne :: Parsed -> Int
partOne = S.size . fst . intermediate

partTwo :: Parsed -> Int
partTwo input = S.size $ fst $ intermediate (snd $ intermediate input)

main :: IO ()
main = forkInteract partOne partTwo
