{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Lib
import Text.Regex (subRegex, mkRegex, matchRegex)
import Data.Text (Text, pack, unpack)
import Data.List (foldl')

removeCanceled :: String -> String
removeCanceled x = subRegex (mkRegex "!.") x ""

removeGarbage :: String -> String
removeGarbage x = subRegex (mkRegex "<[^>]*>") x ""

removeComma :: String -> String
removeComma = filter (/= ',')

processBrace :: [Int] -> Char -> [Int]
processBrace (x:y:rest) '{' = (length rest + 2):x:y:rest
processBrace [x] '{' = [1, x]
processBrace (x:y:rest) '}' = (x + y):rest

countScore :: String -> Int
countScore = head . foldl' processBrace [0]

partOne :: Text -> Int
partOne = countScore . removeComma . removeGarbage . removeCanceled . unpack

processChar :: (Bool, Int) -> Char -> (Bool, Int)
processChar (False, v) '<' = (True, v)
processChar (True, v) '>' = (False, v)
processChar (True, v) _ = (True, v + 1)
processChar (False, v) _ = (False, v)

countGarbage :: String -> Int
countGarbage = snd . foldl' processChar (False, 0)

partTwo :: Text -> Int
partTwo = countGarbage . removeCanceled . unpack

main = forkInteract partOne partTwo
