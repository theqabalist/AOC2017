module Days.Day25 (partOne, partTwo) where

import Days.Day25.Parser (parseInput, Rule(..), Machine(..), Preamble(..), Move(..))
import Lib (Parseable(parse))
import Data.Map (Map, empty, insert, fromList, elems, (!), findWithDefault)
import Data.Foldable (sum)

instance Parseable Machine where
    parse = parseInput

type Tape = (Int, Map Int Int)
emptyTape = (0, empty)

adjust :: (Int, Move) -> Tape -> Tape
adjust (v, MLeft) (i, m) = (i - 1, insert i v m)
adjust (v, MRight) (i, m) = (i + 1, insert i v m)

readTape :: Tape -> Int
readTape (i, m) = findWithDefault 0 i m

runMachine :: Int -> Char -> Map Char (Int -> (Int, Move, Char)) -> Tape -> Int
runMachine 0 _ _ (_, t) = sum t
runMachine n s rules t = let rule = rules ! s
                             (write, move, state) = rule $ readTape t
                         in runMachine (n - 1) state rules (adjust (write, move) t)

partOne :: Machine -> Int
partOne (Machine (Pre (start, times), rules)) = runMachine times start (fromList (fmap runRule rules)) emptyTape

partTwo :: Machine -> Machine
partTwo = id
