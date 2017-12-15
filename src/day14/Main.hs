{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Main where

import Lib (knotHash, Parseable(parse), forkInteract, unwrap)
import Data.Text (unpack, Text, pack, unlines)
import Data.Bits ((.&.), shift)
import Numeric (readHex, showIntAtBase)
import Data.Monoid ((<>))
import Data.Foldable (foldl')
import Data.Map (fromList, Map, insert, empty, foldWithKey, toList)
import Control.Monad.State (State, evalState, get, put)
import Debug.Trace (traceShowId)

type Key = String
instance Parseable Key where
    parse = unpack

countOnes :: Int -> Int -> Int
countOnes total remaining | remaining == 0 = total
                          | otherwise = countOnes (total + remaining .&. 1) (shift remaining (-1))

onesInHash :: Key -> Int
onesInHash = sum . fmap (countOnes 0 . fst . head . readHex . return) . knotHash

partOne :: Key -> Int
partOne = sum . fmap onesInHash . (`fmap` [0..127]) . (\key x -> key ++ "-" ++ show x)

mapBin :: Int -> Char
mapBin 0 = '0'
mapBin 1 = '1'

binToFill :: Char -> Labeled
binToFill '0' = Unlabel Empty
binToFill '1' = Unlabel Filled

data Fill = Filled | Empty
    deriving (Show)

data Labeled = Label Int | Unlabel Fill
    deriving (Show)

keyToRow :: Key -> Map Int Labeled
keyToRow = fromList . zip [0..] . fmap binToFill . foldl' (<>) "" . fmap ((\x -> showIntAtBase 2 mapBin x "") . fst . head . readHex . return) . knotHash

rowsToMap :: [Map Int Labeled] -> Map (Int, Int) Labeled
rowsToMap = foldl' (\m (rowIdx, row) -> foldWithKey (\columnIdx v m2 -> insert (columnIdx, rowIdx) v m2) m row) empty . zip [0..]

labelAll :: Int -> (Int, Int) -> Labeled -> Map (Int, Int) Labeled -> State Int (Map (Int, Int) Labeled)
labelAll group coord (Unlabel Empty) map = return $ insert coord (Unlabel Empty) map
labelAll group coord (Label _) map = return map
labelAll group coord (Unlabel Filled) map = do
    let newMap = insert coord (Label (traceShowId group)) map
    put (group + 1)
    return newMap

labelWithState :: (Int, Int) -> Labeled -> State Int (Map (Int, Int) Labeled) -> State Int (Map (Int, Int) Labeled)
labelWithState coord value ms = do
    map <- ms
    curGroup <- get
    labelAll curGroup coord value map

labelMap :: Map (Int, Int) Labeled -> Map (Int, Int) Labeled
labelMap = (`evalState` 0) . foldWithKey labelWithState (return empty)

partTwo :: Key -> Map (Int, Int) Labeled
partTwo = labelMap . rowsToMap . fmap keyToRow . (`fmap` [0..127]) . (\key x -> key ++ "-" ++ show x)

main = forkInteract partOne partTwo
