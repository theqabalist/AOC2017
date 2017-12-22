module Main where

import Prelude hiding (readFile, putStr)
import System.Environment (getArgs)
import Days
import Data.Text.IO (readFile, putStr)
import Data.Text (unpack)
import Data.Monoid ((<>))

main = do
    args <- getArgs
    let day = read $ head args
    let program = days !! (day - 1)
    input <- readFile $ "inputs/" <> show day <> ".txt"
    putStr $ program input
