{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Days.Day20 (partOne, partTwo) where

import Prelude hiding (lines)
import Lib (Parseable(parse), forkInteract)
import Days.Day20.Parser (parseLine, Particle(P))
import Data.Text (lines)
import Data.Monoid (Sum, (<>))
import Control.Applicative (liftA3)
import Data.Foldable (foldl')
import Data.Either (either)
import Data.List (sort)

newtype Ixed a = Ixed { getIxed :: (Int, a) }
    deriving (Show, Eq)

instance Functor Ixed where
    fmap f (Ixed (i, p)) = Ixed (i, f p)

instance Parseable [Ixed Particle] where
    parse = fmap Ixed . zip [0..] . fmap parseLine . lines

mhdV3 (x, y, z) = abs x + abs y + abs z

instance Ord Particle where
    compare (P p1 v1 a1) (P p2 v2 a2) = compare (mhdV3 a1) (mhdV3 a2)

instance (Ord a) => Ord (Ixed a) where
    compare (Ixed (_, p1)) (Ixed (_, p2)) = compare p1 p2

partOne :: [Ixed Particle] -> Int
partOne = fst . getIxed . minimum

tick :: Particle -> Particle
tick (P p v a) = P p' v' a
    where v' = v <> a
          p' = p <> v'

samePosition :: Ixed Particle -> Ixed Particle -> Bool
samePosition (Ixed (i1, P p1 _ _)) (Ixed (i2, P p2 _ _)) = p1 == p2 && i1 /= i2

removeCollisions :: [Ixed Particle] -> Ixed Particle -> [Ixed Particle]
removeCollisions ps ixp = let collides = not . null $ filter (samePosition ixp) ps
                          in if collides then
                              filter (not . samePosition ixp) $ filter (not . (==) ixp) ps
                          else ps

removeAllCollisions :: [Ixed Particle] -> [Ixed Particle]
removeAllCollisions ps = foldl' removeCollisions ps ps

runSim :: Int -> [Ixed Particle] -> Int
runSim 0 ps = length ps
runSim n ps = let safePs = removeAllCollisions ps
                  newPs = fmap (fmap tick) safePs
                  tickDown = if safePs == ps then -1 else 0
              in runSim (n + tickDown) newPs

partTwo :: [Ixed Particle] -> Int
partTwo = runSim 1000

main = forkInteract partOne partTwo
