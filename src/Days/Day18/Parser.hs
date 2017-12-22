{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Days.Day18.Parser (
    parseInput,
    Instruction(..),
    runInstruction,
    defaultVM,
    VM(..),
) where

import Lib (unwrap)
import Prelude hiding (snd, lines)
import Data.Map (Map, fromList, empty, insert, (!))
import Data.Text (Text, lines)
import Text.Parsec.Text (Parser)
import Text.Parsec (option, many1, try, parse, sepBy1)
import Text.Parsec.Char (string, letter, char, digit)
import Control.Applicative ((<|>))

type Register = Char
data RegInt = RIReg Register | RIInt Int
    deriving (Show)

type Compiled = VM -> VM

instance Show Compiled where
    show = const "<Native>"

data Instruction = Snd RegInt Compiled |
                   Set Register RegInt Compiled |
                   Add Register RegInt Compiled |
                   Mul Register RegInt Compiled |
                   Mod Register RegInt Compiled |
                   Rcv Register Compiled |
                   Jgz RegInt RegInt Compiled |
                   Nul Compiled
    deriving (Show)

data VM = VM {getRegisters :: Map Register Int, getSnd :: [Int], getRcv :: [Int], getPC :: Int}
    deriving (Show, Eq)
defaultVM = VM {
    getRegisters=fromList $ zip "abcdefghijklmnopqrstuvwxyz" (repeat 0),
    getSnd=[],
    getRcv=[],
    getPC=0
}

snd :: RegInt -> Compiled
snd (RIReg r) vm = vm {getSnd=getRegisters vm ! r:getSnd vm, getPC=getPC vm + 1}
snd (RIInt i) vm = vm {getSnd=i:getSnd vm, getPC=getPC vm + 1}

set :: Register -> RegInt -> Compiled
set c (RIReg r) vm = let regs = getRegisters vm
                     in vm {getRegisters=insert c (regs ! r) regs, getPC=getPC vm + 1}
set c (RIInt i) vm = vm {getRegisters=insert c i (getRegisters vm), getPC=getPC vm + 1}

math :: (Int -> Int -> Int) -> Register -> RegInt -> Compiled
math op c (RIReg r) vm = let regs = getRegisters vm
                         in vm {getRegisters=insert c ((regs ! c) `op` (regs ! r)) regs, getPC=getPC vm + 1}
math op c (RIInt i) vm = let regs = getRegisters vm
                         in vm {getRegisters=insert c ((regs ! c) `op` i) regs, getPC=getPC vm + 1}

add = math (+)
mul = math (*)
mod' = math mod

rcv :: Register -> Compiled
rcv c vm | null (getRcv vm) = vm
         | otherwise = let recv = getRcv vm
                           pc = getPC vm
                       in vm {getRegisters=insert c (last recv) (getRegisters vm), getRcv=init recv, getPC=pc + 1}

jgz :: RegInt -> RegInt -> Compiled
jgz (RIReg r1) (RIReg r2) vm = let regs = getRegisters vm
                               in if regs ! r1 > 0
                                   then vm {getPC=getPC vm + (regs ! r2)}
                                   else vm {getPC=getPC vm + 1}
jgz (RIReg r) (RIInt i) vm = let regs = getRegisters vm
                             in if regs ! r > 0
                                 then vm {getPC=getPC vm + i}
                                 else vm {getPC=getPC vm + 1}
jgz (RIInt i) (RIReg r) vm = let regs = getRegisters vm
                             in if i > 0
                                 then vm {getPC=getPC vm + (regs ! r)}
                                 else vm {getPC=getPC vm + 1}
jgz (RIInt i1) (RIInt i2) vm = if i1 > 0
                                 then vm {getPC=getPC vm + i2}
                                 else vm {getPC=getPC vm + 1}

regIntParser :: Parser RegInt
regIntParser = (RIReg <$> letter) <|>
               (RIInt . read <$> ((++) <$> option "" (return <$> char '-') <*> many1 digit))

sndParser :: Parser Instruction
sndParser = do
    string "snd"
    char ' '
    r <- regIntParser
    return $ Snd r (snd r)

op2Parser name cons op = do
    string name
    char ' '
    r1 <- letter
    char ' '
    r2 <- regIntParser
    return $ cons r1 r2 (op r1 r2)

setParser = op2Parser "set" Set set
addParser = op2Parser "add" Add add
mulParser = op2Parser "mul" Mul mul
modParser = op2Parser "mod" Mod mod'

rcvParser :: Parser Instruction
rcvParser = do
    string "rcv"
    char ' '
    r <- letter
    return $ Rcv r (rcv r)

jgzParser = do
    string "jgz"
    char ' '
    r1 <- regIntParser
    char ' '
    r2 <- regIntParser
    return $ Jgz r1 r2 (jgz r1 r2)

instructionParser = try sndParser <|>
                     try setParser <|>
                     try addParser <|>
                     try mulParser <|>
                     try modParser <|>
                     try rcvParser <|>
                     jgzParser

parseInput :: Text -> [Instruction]
parseInput = unwrap . parse (sepBy1 instructionParser (char '\n')) ""

runInstruction :: Instruction -> Compiled
runInstruction (Snd _ ins) = ins
runInstruction (Set _ _ ins) = ins
runInstruction (Add _ _ ins) = ins
runInstruction (Mul _ _ ins) = ins
runInstruction (Mod _ _ ins) = ins
runInstruction (Rcv _ ins) = ins
runInstruction (Jgz _ _ ins) = ins
