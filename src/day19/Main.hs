{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Main where

import Data.Map (Map, insert, empty, (!))
import Data.Foldable (foldl')
import Lib (Parseable(parse), forkInteract')
import Data.Text (unpack)
import Control.Arrow ((***))
import Control.Monad.Writer (Writer, tell, writer, execWriter)
import Data.Char (isAlpha)
import Data.Monoid (Sum, getSum)
import Control.Monad ((>=>))

type Coord = (Int, Int)

instance Parseable (Map Coord Char) where
    parse = foldl' (\m (y, l) -> foldl' (\m (x, c) -> insert (x, y) c m) m l) empty . zip [0..] . fmap (zip [0..]) . lines . unpack

data Packet = Packet {
    position :: Coord,
    bearing :: Coord
}

addPoints :: Coord -> Coord -> Coord
addPoints pos = (+ fst pos) *** (+ snd pos)

move :: Packet -> Packet
move p@Packet {position=pos, bearing=bear} = Packet {position=addPoints pos bear, bearing=bear}

switch :: Map Coord Char -> Packet -> Packet
switch m Packet {position=pos, bearing=(1, 0)} = Packet {position=pos, bearing=if m ! addPoints pos (0, 1) == '|' then (0, 1) else (0, -1)}
switch m Packet {position=pos, bearing=(-1, 0)} = Packet {position=pos, bearing=if m ! addPoints pos (0, 1) == '|' then (0, 1) else (0, -1)}
switch m Packet {position=pos, bearing=(0, 1)} = Packet {position=pos, bearing=if m ! addPoints pos (1, 0) == '-' then (1, 0) else (-1, 0)}
switch m Packet {position=pos, bearing=(0, -1)} = Packet {position=pos, bearing=if m ! addPoints pos (1, 0) == '-' then (1, 0) else (-1, 0)}

packetStart = Packet {position=(63, -1), bearing=(0, 1)}

halted :: Map Coord Char -> Packet -> Bool
halted m Packet {position=pos, bearing=bear} = m ! addPoints pos bear == ' '

step :: Map Coord Char -> Packet -> Writer (Sum Int, String) Packet
step m p = do
    let newPacket = move p
    let c = m ! position newPacket
    let switchPacket = if c == '+' then switch m newPacket else newPacket
    writer (switchPacket, if isAlpha c then (1, [c]) else (1, ""))

travel :: Packet -> Map Coord Char -> Writer (Sum Int, String) Packet
travel p m | halted m p = return p
           | otherwise = step m p >>= (`travel` m)

solution :: Packet -> Map Coord Char -> (Sum Int, String)
solution p = execWriter . travel p

partOne :: Map Coord Char -> String
partOne = snd . solution packetStart

partTwo :: Map Coord Char -> Int
partTwo = getSum . fst . solution packetStart

main = forkInteract' partOne partTwo
