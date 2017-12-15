{-# LANGUAGE OverloadedStrings #-}

module Lib (
    aperture,
    Parseable(parse),
    forkInteract,
    unwrap,
    knotHash
) where

import Data.List (tails)
import Data.Text (pack, unpack, Text, strip, concat)
import Data.Text.IO (interact)
import Prelude hiding (concat, interact, length)
import qualified Prelude as P
import Control.Monad (join, foldM_)
import Data.Char (ord)
import Data.Vector.Unboxed (length, Vector, fromList, toList, (!), thaw, freeze)
import Data.Vector.Unboxed.Mutable (write)
import Control.Monad.ST (runST)
import Data.List.Split (chunksOf)
import Data.Foldable (foldl')
import Data.Bits (xor)
import Numeric (showHex)

aperture' :: Int -> [a] -> [[a]]
aperture' n = map (take n) . tails

takeLengthOf :: [a] -> [b] -> [b]
takeLengthOf = zipWith (flip const)

aperture :: Int -> [a] -> [[a]]
aperture n xs = takeLengthOf (drop (n-1) xs) (aperture' n xs)

class Parseable a where
  parse :: Text -> a

instance Parseable Text where
  parse = id

forkInteract :: (Parseable a, Show b, Parseable c, Show d) => (a -> b) -> (c -> d) -> IO ()
forkInteract f1 f2 = interact $ (\input -> concat ["part 1: ", pack . show $ f1 $ parse input, "\n\npart 2: ", pack . show $ f2 $ parse input, "\n"]) . strip

unwrap :: Show a => Either a b -> b
unwrap (Right x) = x
unwrap (Left x) = error (show x)

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

zeroPad :: String -> String
zeroPad s | null s = "00"
          | P.length s == 1 = "0" ++ s
          | otherwise = s

knotHash :: String -> String
knotHash input = let instructions = join . replicate 64 . flip (++) [17, 31, 73, 47, 23] . fmap ord $ input
                     jumbled = toList $ runFlips 0 0 instructions (fromList [0..255])
                     chunked = chunksOf 16 jumbled
                     reduced = fmap (\(head:rest) -> foldl' xor head rest) chunked
                     converted = fmap (zeroPad . (`showHex` "")) reduced
                 in foldl' (++) "" converted
