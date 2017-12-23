{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Days.Day23.Parser (
    parseInput,
    Instruction(..),
    runInstruction,
    defaultVM,
    VM(..),
    RegInt(..)
) where

import Lib (unwrap)
import Data.Map (Map, fromList, empty, insert, (!))
import Data.Text (Text, lines)
import Text.Parsec.Text (Parser)
import Text.Parsec (option, many1, try, parse, sepBy1)
import Text.Parsec.Char (string, letter, char, digit, newline)
import Control.Applicative ((<|>))

type Register = Char
data RegInt = RIReg Register | RIInt Int
    deriving (Show)

type Compiled = VM -> VM

instance Show Compiled where
    show = const "<Native>"

data Instruction = Set Register RegInt Compiled |
                   Sub Register RegInt Compiled |
                   Mul Register RegInt Compiled |
                   Jnz RegInt RegInt Compiled |
                   Nul Compiled
    deriving (Show)

data VM = VM {getRegisters :: Map Register Int, getPC :: Int}
    deriving (Show, Eq)
defaultVM = VM {
    getRegisters=fromList $ zip "abcdefgh" (repeat 0),
    getPC=0
}

set :: Register -> RegInt -> Compiled
set c (RIReg r) vm = let regs = getRegisters vm
                     in vm {getRegisters=insert c (regs ! r) regs, getPC=getPC vm + 1}
set c (RIInt i) vm = vm {getRegisters=insert c i (getRegisters vm), getPC=getPC vm + 1}

math :: (Int -> Int -> Int) -> Register -> RegInt -> Compiled
math op c (RIReg r) vm = let regs = getRegisters vm
                         in vm {getRegisters=insert c ((regs ! c) `op` (regs ! r)) regs, getPC=getPC vm + 1}
math op c (RIInt i) vm = let regs = getRegisters vm
                         in vm {getRegisters=insert c ((regs ! c) `op` i) regs, getPC=getPC vm + 1}

sub = math (-)
mul = math (*)

jnz :: RegInt -> RegInt -> Compiled
jnz (RIReg r1) (RIReg r2) vm = let regs = getRegisters vm
                               in if regs ! r1 /= 0
                                   then vm {getPC=getPC vm + (regs ! r2)}
                                   else vm {getPC=getPC vm + 1}
jnz (RIReg r) (RIInt i) vm = let regs = getRegisters vm
                             in if regs ! r /= 0
                                 then vm {getPC=getPC vm + i}
                                 else vm {getPC=getPC vm + 1}
jnz (RIInt i) (RIReg r) vm = let regs = getRegisters vm
                             in if i /= 0
                                 then vm {getPC=getPC vm + (regs ! r)}
                                 else vm {getPC=getPC vm + 1}
jnz (RIInt i1) (RIInt i2) vm = if i1 /= 0
                                 then vm {getPC=getPC vm + i2}
                                 else vm {getPC=getPC vm + 1}

regIntParser :: Parser RegInt
regIntParser = (RIReg <$> letter) <|>
               (RIInt . read <$> ((++) <$> option "" (return <$> char '-') <*> many1 digit))

op2Parser name cons op = do
    string name
    char ' '
    r1 <- letter
    char ' '
    r2 <- regIntParser
    return $ cons r1 r2 (op r1 r2)

setParser = op2Parser "set" Set set
subParser = op2Parser "sub" Sub sub
mulParser = op2Parser "mul" Mul mul

jnzParser = do
    string "jnz"
    char ' '
    r1 <- regIntParser
    char ' '
    r2 <- regIntParser
    return $ Jnz r1 r2 (jnz r1 r2)

instructionParser = try setParser <|>
                     try subParser <|>
                     try mulParser <|>
                     jnzParser

parseInput :: Text -> [Instruction]
parseInput = unwrap . parse (sepBy1 instructionParser newline) "23.txt"

runInstruction :: Instruction -> Compiled
runInstruction (Set _ _ ins) = ins
runInstruction (Sub _ _ ins) = ins
runInstruction (Mul _ _ ins) = ins
runInstruction (Jnz _ _ ins) = ins
