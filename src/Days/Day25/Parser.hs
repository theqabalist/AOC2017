module Days.Day25.Parser (
    parseInput,
    Rule(..),
    Machine(..),
    Preamble(..),
    Move(..)
) where

import Text.Parsec.Text (Parser)
import Text.Parsec (string, newline, letter, many1, digit, spaces, try, parse, sepBy1, option)
import Data.Text (unpack)
import Data.Monoid ((<>))
import Data.Functor (($>))
import Data.Char (digitToInt)
import Control.Applicative ((<|>))

initStateParser :: Parser Char
initStateParser = string "Begin in state " *> letter <* string "." <* newline

diagnosticParser :: Parser Int
diagnosticParser = string "Perform a diagnostic checksum after " *> (read <$> many1 digit) <* string " steps." <* newline

newtype Preamble = Pre {runPreamble :: (Char, Int)}
    deriving (Show)

preambleParser :: Parser Preamble
preambleParser = Pre <$> ((,) <$> initStateParser <*> diagnosticParser)

data Move = MLeft | MRight
    deriving (Show)

newtype Rule = Rule {runRule :: (Char, Int -> (Int, Move, Char))}

instance Show Rule where
    show (Rule (c, _)) = "<Rule " <> [c] <> ">"

newtype Branch = Branch (Int, (Int, Move, Char))

ruleParser :: Parser Rule
ruleParser = Rule <$> ((,) <$> (string "In state " *> letter <* string ":" <* newline) <*> compositeBranchParser)

compositeBranch :: Branch -> Branch -> Int -> (Int, Move, Char)
compositeBranch (Branch (0, a)) _ 0 = a
compositeBranch _ (Branch (1, b)) 1 = b

moveParser :: Parser Move
moveParser = try (string "right" $> MRight) <|>
             string "left" $> MLeft

branchParser :: Int -> Parser Branch
branchParser b = Branch <$> ((,) <$> (curVal $> b) <*> transition)
    where curVal = spaces *> string ("If the current value is " <> show b <> ":")
          transition = (,,) <$> (spaces *> string "- Write the value " *> (digitToInt <$> digit) <* string "." <* newline)
                            <*> (spaces *> string "- Move one slot to the " *> moveParser <* string "." <* newline)
                            <*> (spaces *> string "- Continue with state " *> letter <* string "." <* option '\0' (try newline))

compositeBranchParser :: Parser (Int -> (Int, Move, Char))
compositeBranchParser = compositeBranch <$> branchParser 0 <*> branchParser 1

newtype Machine = Machine (Preamble, [Rule])
    deriving (Show)

inputParser :: Parser Machine
inputParser = Machine <$> ((,) <$> (preambleParser <* newline) <*> sepBy1 ruleParser newline)

parseInput = either (error . show) id . parse inputParser "25.txt"
