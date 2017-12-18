{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Main where

import Lib (Parseable(parse), forkInteract)
import Day18Parser (parseInput, Instruction(..), runInstruction, defaultVM, VM(..))
import Data.Maybe (fromJust, fromMaybe)
import Data.Vector (Vector, (!?), fromList)
import Debug.Trace (traceShowId)
import Data.Map (insert)
import Control.Lens (view, Lens')
import Control.Lens.Tuple (_1, _2)

instance Parseable (Vector Instruction) where
    parse = fromList . parseInput

onRcv :: Instruction -> Bool
onRcv (Rcv _ _) = True
onRcv _ = False

runInstructionsUntilRcv :: VM -> Vector Instruction -> Int
runInstructionsUntilRcv vm is | onRcv $ fromJust (is !? getPC vm) = head $ getSnd vm
runInstructionsUntilRcv vm is = let ins = runInstruction $ fromJust (is !? getPC vm)
                                in runInstructionsUntilRcv (ins vm) is

partOne :: Vector Instruction -> Int
partOne = runInstructionsUntilRcv defaultVM

tx :: (VM, VM) -> (VM, VM)
tx (vm1, vm2) = let tx1 = getSnd vm1
                    tx2 = getSnd vm2
                    rx1 = getRcv vm1
                    rx2 = getRcv vm2
                in (vm1 {getRcv=tx2 ++ rx1, getSnd=[]}, vm2 {getRcv=tx1 ++ rx2, getSnd=[]})

pcOutOfRange :: VM -> Vector Instruction -> Bool
pcOutOfRange vm is = pc < 0 || pc >= length is
    where pc = getPC vm

runProgram :: VM -> Vector Instruction -> VM
runProgram vm is = let pc = getPC vm
                       ins = fromMaybe (Nul id) $ is !? pc
                   in runInstruction ins vm

runDouble :: Bool -> Int -> (VM, VM) -> Vector Instruction -> Int
runDouble True txs _ _ = txs
runDouble False txs (vm1, vm2) is = let newTxs = txs + length (getSnd vm2)
                                        (txvm1, txvm2) = tx (vm1, vm2)
                                        newVM1 = runProgram txvm1 is
                                        newVM2 = runProgram txvm2 is
                                        halted = (pcOutOfRange newVM1 is && pcOutOfRange newVM2 is) ||
                                                 (pcOutOfRange newVM1 is && getPC newVM2 == getPC vm2) ||
                                                 (getPC newVM1 == getPC vm1 && pcOutOfRange vm2 is) ||
                                                 (getPC newVM1 == getPC vm1 && getPC newVM2 == getPC vm2)
                                    in runDouble halted newTxs (newVM1, newVM2) is

partTwo :: Vector Instruction -> Int
partTwo = runDouble False 0 (defaultVM, setP 1 defaultVM)

setP :: Int -> VM -> VM
setP i vm = vm {getRegisters=insert 'p' i (getRegisters vm)}

main = forkInteract partOne partTwo 
