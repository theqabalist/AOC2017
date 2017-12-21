{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Main where

import Prelude hiding (sum)
import Lib (forkInteract, Parseable(parse))
import Day21Parser (parseInput)
import Data.Matrix (Matrix, fromLists, nrows, submatrix, (<|>), (<->), toList, forceMatrix)
import Data.Map (Map, (!))
import Data.Map.Strict (union)
import Data.List.Split (chunksOf)
import Data.List (foldl')
import Debug.Trace (traceShowId)
import Data.Foldable (sum)

type KB = (Map (Matrix Int) (Matrix Int))

instance Parseable KB where
    parse = parseInput

starting = fromLists [[0, 1, 0],
                      [0, 0, 1],
                      [1, 1, 1]]

foldAgainstHead f (x:xs) = foldl' f x xs

subMatrixList :: Int -> Matrix Int -> [Matrix Int]
subMatrixList n m = do
    x <- subIndices
    y <- subIndices
    return $ forceMatrix $ submatrix (x + 1) (x + n) (y + 1) (y + n) m
    where subIndices = takeWhile (< size) $ filter ((== 0) . (`mod` n)) [0..]
          size = nrows m

expand1' kb n m = foldAgainstHead (<->) $ fmap (foldAgainstHead (<|>)) $ chunksOf (size `div` n) $ (kb !) <$> subMatrixList n m
    where size = nrows m

expand1 kb m = if nrows m `mod` 2 == 0
    then expand1' kb 2 m
    else expand1' kb 3 m

expandN' :: Int -> Int -> KB -> Matrix Int -> Int
expandN' depth 0 kb m = sum m
expandN' depth n kb m | (depth - n) `mod` 3 == 0 = sum (expandN' depth (n - 1) kb . expand1 kb <$> subMatrixList 3 m)
                      | otherwise = expandN' depth (n - 1) kb (expand1 kb m)

expandN depth = expandN' depth depth

partOne :: KB -> Int
partOne kb = expandN 5 kb starting

partTwo :: KB -> Int
partTwo kb = expandN 18 kb starting

main = forkInteract partOne partTwo
