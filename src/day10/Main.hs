{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Main where

import Lib
import Prelude hiding (splitOn, length)
import qualified Prelude as P
import Data.Text (splitOn, unpack)
import Data.Text.Read (decimal)
import Data.Vector.Unboxed (Vector, fromList, (!), length, thaw, freeze, toList)
import Data.Vector.Unboxed.Mutable (modify, write)
import Control.Monad (foldM_, join)
import Control.Monad.ST (runST)
import Data.Char (ord)
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Data.Bits (xor)
import Numeric (showHex)

type Parsed = [Int]
instance Parseable Parsed where
    parse = fmap (fst . unwrap . decimal) . splitOn ","

pancakeFlip :: Int -> Int -> Vector Int -> Vector Int
pancakeFlip p l v = runST $ do
    let size = length v
    let flipped = zip [0..] $ fmap (v !) $ reverse $ fmap (`mod` size) [p..(p + l - 1)]
    vec <- thaw v
    foldM_ (\_ (offset, value) -> write vec ((p + offset) `mod` size) value) () flipped
    freeze vec

runFlips :: Int -> Int -> [Int] -> Vector Int -> Vector Int
runFlips position skips ls v | null ls = v
                             | otherwise = let len = head ls
                                           in runFlips ((position + skips + len) `mod` length v) (skips + 1) (tail ls) (pancakeFlip position len v)

partOne :: Parsed -> Int
partOne input = let jumbled = runFlips 0 0 input (fromList [0..255])
                in (jumbled ! 0) * (jumbled ! 1)

newtype Parsed2 = Parsed2 {getParsed :: [Int]}
instance Parseable Parsed2 where
    parse = Parsed2 . fmap ord . unpack

partTwo :: Parsed2 -> String
partTwo input = let instructions = join . replicate 64 . flip (++) [17, 31, 73, 47, 23] . getParsed $ input
                    jumbled = toList $ runFlips 0 0 instructions (fromList [0..255])
                    chunked = chunksOf 16 jumbled
                    reduced = fmap (\(head:rest) -> foldl' xor head rest) chunked
                    converted = fmap (zeroPad 2 . (`showHex` "")) reduced
                in foldl' (++) "" converted

main = forkInteract partOne partTwo
