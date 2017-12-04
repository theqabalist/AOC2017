{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Lib
import Prelude hiding (words, lines)
import Data.Text (words, lines, Text, unpack)
import Data.Set (fromList, member)
import Control.Applicative ((<$>))
import Data.List (sort)

type Parsed = [[Text]]
instance Parseable Parsed where
    parse = fmap words . lines

hasRepeat :: [Text] -> Bool
hasRepeat [_] = False
hasRepeat (v:vs) = member v (fromList vs) || hasRepeat vs

hasAnagram :: [Text] -> Bool
hasAnagram [_] = False
hasAnagram (v:vs) = member ((sort . unpack) v) (fromList ((sort . unpack) <$> vs)) || hasAnagram vs

partOne :: Parsed -> Int
partOne = length . filter not . fmap hasRepeat

partTwo :: Parsed -> Int
partTwo = length . filter not . fmap hasAnagram

main :: IO ()
main = forkInteract partOne partTwo
