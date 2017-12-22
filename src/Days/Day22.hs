{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Days.Day22 (partOne, partTwo) where

import Prelude hiding (lookup)
import Lib (forkInteract, Parseable(parse))
import Days.Day22.Parser (parseInput, V2, Infection(..))
import Data.Map (Map, lookup, insert)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Debug.Trace (traceShowId)

type Grid = Map V2 Infection

instance Parseable Grid
    where parse = parseInput

bear Clean (0, 1) = (-1, 0)
bear Clean (-1, 0) = (0, -1)
bear Clean (0, -1) = (1, 0)
bear Clean (1, 0) = (0, 1)
bear Weakened d = d
bear Flagged (x, y) = (-x, -y)
bear _ (0, 1) = (1, 0)
bear _ (1, 0) = (0, -1)
bear _ (0, -1) = (-1, 0)
bear _ (-1, 0) = (0, 1)

data Virus = V { location :: V2, direction :: V2 }
    deriving (Show)
defaultVirus = V { location = (0, 0), direction = (0, 1) }

move inf V {location=loc, direction=dir} = V {location=loc <> newDir, direction = newDir}
    where newDir = bear inf dir

recolor1 loc Clean grid = insert loc New grid
recolor1 loc _ grid = insert loc Clean grid

solve' rec tp 0 infs v grid = infs
solve' rec tp n infs v@V {location=loc} grid = let cell = fromMaybe Clean $ lookup loc grid
                                                   newV = move cell v
                                                   newGrid = rec loc cell grid
                                               in solve' rec tp (n - 1) (infs + if cell == tp then 1 else 0) newV newGrid

solve1 iter = solve' recolor1 Clean iter 0 defaultVirus

partOne = solve1 10000

recolor2 loc Clean = insert loc Weakened
recolor2 loc Weakened = insert loc New
recolor2 loc New = insert loc Flagged
recolor2 loc Flagged = insert loc Clean
recolor2 loc Old = insert loc Flagged

partTwo = solve' recolor2 Weakened 10000000 0 defaultVirus

main = forkInteract partOne partTwo
