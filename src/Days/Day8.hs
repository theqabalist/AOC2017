{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Days.Day8 (partOne, partTwo) where

import Lib
import Prelude hiding (lines, lookup)
import Data.Text (Text, lines, pack)
import Data.Maybe (fromMaybe)
import Days.Day8.Parser (parseLine, Op, ConditionalF)
import Data.Map (Map, empty, insert, lookup, notMember, fromList, toList)
import Data.List (foldl', partition, sortBy, groupBy, maximum)
import Control.Monad.Reader (Reader, runReader)
import Control.Applicative ((<|>))
import Data.Ord (comparing)
import Control.Monad (join)

type FullInstruction = ((Text, Op, Int), (Text, ConditionalF, Int))
type Parsed = [FullInstruction]
type Output = Int
instance Parseable Parsed where
    parse = fmap parseLine . lines

compileInstruction :: FullInstruction -> Map Text Int -> Map Text Int
compileInstruction ((r, op, v), (gr, cond, gv)) m = let access = fromMaybe 0 (lookup gr m)
                                                        current = fromMaybe 0 (lookup r m)
                                                    in if cond access gv then
                                                        insert r (op current v) m
                                                    else m

partOne :: Parsed -> Output
partOne = maximum . fmap snd . toList . foldl' (flip ($)) empty . fmap compileInstruction

partTwo :: Parsed -> Output
partTwo = maximum . fmap snd . join . fmap toList . foldl' (\(x:xs) f -> f x:x:xs) [empty] . fmap compileInstruction
