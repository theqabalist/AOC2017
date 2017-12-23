module Days.Day23 (partOne, partTwo) where

import Days.Day23.Parser (parseInput, Instruction(..), VM(..), runInstruction, defaultVM, RegInt(..))
import Lib (Parseable(parse))
import Data.Map ((!), insert)
import Control.Lens.Lens (lens)
import Control.Lens (set, view)
import Debug.Trace (traceShowId)

newtype Program = Program {getInstructions::[Instruction]}
    deriving (Show)

instance Parseable Program where
    parse = Program . parseInput

isMul Mul{} = True
isMul _ = False

pcOutOfRange is pc = pc >= length is || pc < 0

countMulsUntilHalt :: Int -> VM -> [Instruction] -> Int
countMulsUntilHalt muls vm@VM {getPC=pc} is | pcOutOfRange is pc = muls
                                            | otherwise = let ins = is !! pc
                                                              compiled = runInstruction ins
                                                          in countMulsUntilHalt (if isMul ins then muls + 1 else muls) (compiled vm) is

partOne :: Program -> Int
partOne = countMulsUntilHalt 0 defaultVM . getInstructions

runToHalt :: VM -> [Instruction] -> VM
runToHalt vm@VM {getPC=pc} is | pcOutOfRange is pc = vm
                              | otherwise = runToHalt (runInstruction (is !! pc) (traceShowId vm)) is

register r = lens ((! r) . getRegisters) (\(vm@VM{getRegisters=rs}) v -> vm {getRegisters=insert r v rs})

notPrime x = any ((== 0) . (x `mod`)) $ takeWhile ((<= x) . (^ 2)) [2..]

findNotPrimes from to step = filter notPrime $ takeWhile (<= to) $ iterate (+step) from

getValFromInstruction :: Instruction -> Int
getValFromInstruction (Set _ (RIInt v) _) = v
getValFromInstruction (Sub _ (RIInt v) _) = v
getValFromInstruction _ = error "Program cannot be optimized."

partTwo :: Program -> Int
partTwo p = let is = getInstructions p
                b = getValFromInstruction (head is) * 100 + 100000
                step = negate $ getValFromInstruction (is !! 30)
            in length $ findNotPrimes b (b + (step * 1000)) step
