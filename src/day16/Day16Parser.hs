module Day16Parser (
    parseInput,
    Instruction(..)
) where

import Prelude hiding (read, length)
import qualified Prelude as P

import Data.Vector.Unboxed (Unbox, Vector, modify, elemIndex, toList, freeze, fromList)
import Data.Vector.Unboxed.Mutable (read, write, swap, clone, length)
import Text.Parsec.Text (Parser)
import Text.Parsec (many1, try, parse, sepBy1)
import Text.Parsec.Char (anyChar, char, digit)
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Data.Foldable (foldl')
import Control.Monad (foldM_)

data Instruction a = Rotate Int (Vector a -> Vector a)      |
                     SwapPos Int Int (Vector a -> Vector a) |
                     SwapVal a a (Vector a -> Vector a)

instance (Show a) => Show (Instruction a) where
    show (Rotate v _) = "<Rotate " ++ show v ++ ">"
    show (SwapPos x y _) = "<SwapPos " ++ show (x, y) ++ ">"
    show (SwapVal a b _) = "<SwapVal " ++ show (a, b) ++ ">"

rotateR1 :: (Unbox a) => Vector a -> Vector a
rotateR1 = modify (\v -> do
    v2 <- clone v
    let end = length v - 1
    foldM_ (\_ i -> do
        val <- read v2 i
        write v (i + 1) val) () [0..end - 1]
    endVal <- read v2 end
    write v 0 endVal)

rotateR :: (Unbox a) => Int -> Vector a -> Vector a
rotateR 0 v = v
rotateR n v = rotateR (n - 1) (rotateR1 v)

swapPos :: (Unbox a) => Int -> Int -> Vector a -> Vector a
swapPos x y = modify (\v -> swap v x y)

swapVal :: (Unbox a, Eq a) => a -> a -> Vector a -> Vector a
swapVal a b v = let x = fromJust $ elemIndex a v
                    y = fromJust $ elemIndex b v
                in swapPos x y v

instructionParser :: Parser [Instruction Char]
instructionParser = sepBy1 (try posParser <|>
                            try valParser <|>
                            rotParser) (char ',')

posParser :: Parser (Instruction Char)
posParser = do
    x <- P.read <$> (char 'x' *> many1 digit)
    y <- P.read <$> (char '/' *> many1 digit)
    return $ SwapPos x y (swapPos x y)

valParser :: Parser (Instruction Char)
valParser = do
    a <- char 'p' *> anyChar
    b <- char '/' *> anyChar
    return $ SwapVal a b (swapVal a b)

rotParser :: Parser (Instruction Char)
rotParser = do
    n <- P.read <$> (char 's' *> many1 digit)
    return $ Rotate n (rotateR n)

parseInput = parse instructionParser ""
