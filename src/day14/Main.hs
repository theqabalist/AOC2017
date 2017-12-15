{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Main where

import Prelude hiding (lookup)
import Lib (knotHash, Parseable(parse), forkInteract, unwrap, binWord)
import Data.Text (unpack, Text, pack, unlines)
import Data.Bits ((.&.), shift)
import Numeric (readHex)
import Data.Monoid ((<>))
import Data.Foldable (foldl')
import Data.Map (fromList, Map, insert, keys, empty, size, lookup, foldWithKey, toList, (!))
import Control.Monad (foldM)
import Control.Monad.State (State, execState, get, put)

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

binToFill :: Char -> Labeled
binToFill '0' = Unlabel Empty
binToFill '1' = Unlabel Filled

data Fill = Filled | Empty
    deriving (Show, Eq)

data Labeled = Label Int | Unlabel Fill
    deriving (Show, Eq)

keyToRow :: Key -> Map Int Labeled
keyToRow = fromList . zip [0..] . fmap binToFill . foldl' (<>) "" . fmap (binWord 4 . fst . head . readHex . return) . knotHash

rowsToMap :: [Map Int Labeled] -> Map (Int, Int) Labeled
rowsToMap = foldl' (\m (rowIdx, row) -> foldWithKey (\columnIdx v m2 -> insert (columnIdx, rowIdx) v m2) m row) empty . zip [0..]

ensureKey :: (Show v) => v -> Maybe a -> a
ensureKey v (Just v2) = v2
ensureKey v Nothing = error $ "Couldn't find key " ++ show v ++ " in map."

unsafeLookup :: (Show k, Ord k) => k -> Map k a -> a
unsafeLookup k m = ensureKey k $ lookup k m

neighbors :: (Int, Int) -> Map (Int, Int) Labeled -> [(Int, Int)]
neighbors (x, y) m = ensureFilled $ ensureBounds [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    where ensureBounds = filter (\(x, y) -> x >= 0 && y >= 0 && x < 128 && y < 128)
          ensureFilled = filter (\coord -> unsafeLookup coord m == Unlabel Filled)

fillCoord :: Int -> Map (Int, Int) Labeled -> (Int, Int) -> Map (Int, Int) Labeled
fillCoord label m coord = foldl' (fillCoord label) (insert coord (Label label) m) (neighbors coord m)

labelAll :: Int -> (Int, Int) -> Labeled -> Map (Int, Int) Labeled -> State Int (Map (Int, Int) Labeled)
labelAll group coord v@(Unlabel Empty) m = return m
labelAll group coord v@(Label _) m = return m
labelAll group coord (Unlabel Filled) m = do
    let newMap = fillCoord group m coord
    put (group + 1)
    return newMap

labelWithState :: Map (Int, Int) Labeled -> (Int, Int) -> State Int (Map (Int, Int) Labeled)
labelWithState m coord = do
    curGroup <- get
    labelAll curGroup coord (m ! coord) m

labelMap :: Map (Int, Int) Labeled -> Int
labelMap m = (`execState` 0) $ foldM labelWithState m (keys m)

partTwo :: Key -> Int
partTwo = labelMap . rowsToMap . fmap keyToRow . (`fmap` [0..127]) . (\key x -> key ++ "-" ++ show x)

main = forkInteract partOne partTwo
