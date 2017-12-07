{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Main where

import Lib
import Prelude hiding (lines, lookup)
import Data.Text (Text, lines, pack)
import Day7Parser (parseLine)
import Data.Map (Map, empty, insert, lookup, notMember, fromList)
import Data.List (foldl', partition, sortBy, groupBy)
import Control.Monad.Reader (Reader, runReader)
import Control.Applicative ((<|>))
import Data.Ord (comparing)

type Parsed = [(Text, Int, [Text])]

instance Parseable Parsed where
    parse = fmap parseLine . lines

reverseIndex :: Parsed -> Map Text Text
reverseIndex = foldl' (\m (name, _, children) -> foldl' (\m child -> insert child name m) m children) empty

findRoot :: Map Text Text -> Text -> Maybe Text
findRoot m t | notMember t m = Just t
             | otherwise = lookup t m >>= findRoot m

unsafeMaybe :: Maybe a -> a
unsafeMaybe (Just v) = v
unsafeMaybe Nothing = error "Too unsafe."

partOne :: Parsed -> Maybe Text
partOne ls = findRoot (reverseIndex ls) ((\(seed, _, _) -> seed) $ head ls)

weightMap :: Parsed -> Map Text Int
weightMap = foldl' (\m (name, weight, _) -> insert name weight m) empty

type Output = Either (Text, Int, Int) (Text, Int)

-- something :: Map Text Int -> Parsed -> Output
-- something weights = filter (\(_, _, children) -> not . null $ children) . fmap (\(name, weight, children) -> (name, weight, unsafeMaybe $ traverse (`lookup` weights) children))

unsafeLookup v m = unsafeMaybe $ lookup v m

findOutlier :: [(Text, Int)] -> [[(Text, Int)]]
findOutlier vs = sortBy (comparing length) $ groupBy (\x y -> snd x == snd y) $ sortBy (comparing snd) vs

findBroken :: Map Text (Int, [Text]) -> Text -> Either (Text, Int, Int) (Text, Int)
findBroken treeDir node = let (weight, children) = unsafeLookup node treeDir in
    if null children then
        Right (node, weight)
    else do
        weights <- traverse (findBroken treeDir) children
        let correct = all (== (snd $ head weights)) (snd <$> tail weights)
        if correct then
            return (node, weight + sum (fmap snd weights))
        else let outlier = findOutlier weights
                 errantNode = head $ head outlier
                 conformNodeWeight = snd $ head $ outlier !! 1
                 actualWeight = fst $ unsafeLookup (fst errantNode) treeDir
                 balanceDiff = conformNodeWeight - snd errantNode
                 adjustedWeight = actualWeight + balanceDiff
             in Left (fst errantNode, actualWeight, adjustedWeight)

partTwo :: Parsed -> Output
partTwo input = findBroken (fromList $ fmap (\(name, weight, children) -> (name, (weight, children))) input) (unsafeMaybe (partOne input))

main = forkInteract partOne partTwo
