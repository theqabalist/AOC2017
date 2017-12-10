{-# LANGUAGE OverloadedStrings #-}

module Lib (
    aperture,
    Parseable(parse),
    forkInteract,
    unwrap
) where

import Data.List (tails)
import Data.Text (pack, unpack, Text, strip, concat)
import Data.Text.IO (interact)
import Prelude hiding (concat, interact)

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
