{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Days.Day12 (partOne, partTwo) where

import Lib
import Prelude hiding (lines, lookup)
import Data.List (maximum)
import Data.Text (splitOn, lines)
import Data.Text.Read (decimal)
import Control.Monad (join)
import Data.Set (empty, insert, member, Set, size)
import Data.Foldable (foldl')
import Data.Map (fromList, Map, lookup, keys, delete)
import qualified Data.Map as M
import Data.Maybe (maybeToList)

type Adjacency = Map Int [Int]

readText = fst . unwrap . decimal

instance Parseable Adjacency where
    parse = fromList . fmap ((\(prog:rest) -> (readText prog, fmap readText . join $ fmap (splitOn ", ") rest)) . splitOn " <-> ") . lines

countUp :: Set Int -> [Int] -> Adjacency -> Set Int
countUp visited fringe directory = let newFringe = filter (not . (`member` visited)) $ fringe >>= (join . maybeToList . (`lookup` directory))
                                   in if null newFringe
                                        then
                                            visited
                                        else
                                            countUp (foldl' (flip insert) visited newFringe) newFringe directory

partOne :: Adjacency -> Int
partOne = size . countUp empty [0]

countGroups :: Int -> Adjacency -> Int
countGroups groups directory = if M.null directory
                                then groups
                                else let sample = head $ keys directory
                                         group = countUp empty [sample] directory
                                         newDir = foldl' (flip delete) directory group
                                     in countGroups (groups + 1) newDir

partTwo :: Adjacency -> Int
partTwo = countGroups 0

main = forkInteract partOne partTwo
