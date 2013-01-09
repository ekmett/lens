{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  BrainfuckFinal
-- Copyright   :  (C) 2012 Edward Kmett, nand`
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  TH, Rank2, NoMonomorphismRestriction
--
-- A simple interpreter for the esoteric programming language "Brainfuck"
-- written using lenses and zippers.
--
-- This version of the interpreter is 'finally encoded' without going through
-- an AST.
-----------------------------------------------------------------------------
module Main where

import Prelude hiding (Either(..))

import Control.Lens
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word (Word8)

import System.Environment (getArgs)
import System.IO

-- | Brainfuck is defined to have a memory of 30000 cells.
memoryCellNum :: Int
memoryCellNum = 30000

-- Low level syntax form

-- Higher level semantic graph

-- * State/Writer-based interpreter

type Cell   = Word8
type Input  = [Cell]
type Output = [Cell]
type Memory = Top :>> [Cell] :>> Cell -- list zipper

data MachineState = MachineState
  { _input  :: [Cell]
  , _memory :: Memory }

makeLenses ''MachineState

type Program = StateT MachineState (Writer Output) ()

compile :: String -> Program
compile = fst . bracket []

branch :: Program -> Program -> Program
branch z n = do
  c <- use (memory.focus)
  if c == 0 then z else n

bracket :: [Program] -> String -> (Program, [Program])
bracket [] ""      = (return () , [])
bracket _  ""      = error "Mismatched opening bracket"
bracket [] (']':_) = error "Mismatched closing bracket"

-- Match a closing bracket: Pop a forward continuation, push backwards
bracket (c:cs) (']': xs) = (branch n c, n:bs) where
  (n, bs) = bracket cs xs

-- Match an opening bracket: Pop a backwards continuation, push forwards
bracket cs ('[': xs) = (branch b n, bs) where
  (n, b:bs) = bracket (n:cs) xs

-- Match any other symbol in the trivial way
bracket cs (x:xs) = over _1 (f x >>) (bracket cs xs) where
  f '+' = memory.focus += 1
  f '-' = memory.focus -= 1
  f '>' = memory %= wrapRight
  f '<' = memory %= wrapLeft
  f ',' = do
    memory.focus <~ uses input head
    input %= tail
  f '.' = do
    x <- use (memory.focus)
    tell [x]
  f _   = return ()

-- | Initial memory configuration
initial :: Input -> MachineState
initial i = MachineState i (zipper (replicate memoryCellNum 0) & fromWithin traverse)

interpret :: Input -> Program -> Output
interpret i = execWriter . flip execStateT (initial i)

-- | Zipper helpers
wrapRight, wrapLeft :: (a :>> b) -> (a :>> b)
wrapRight = liftM2 fromMaybe leftmost rightward
wrapLeft  = liftM2 fromMaybe rightmost leftward

-- Main program action to actually run this stuff

main :: IO ()
main = do
  as <- getArgs
  case as of
    -- STDIN is program
    [ ] -> do
      hSetBuffering stdin  NoBuffering
      hSetBuffering stdout NoBuffering
      getContents >>= eval noInput

    -- STDIN is input
    [f] -> join $ eval <$> getInput <*> readFile f

    -- Malformed command line
    _ -> putStrLn "Usage: brainfuck [program]"

eval :: Input -> String -> IO ()
eval i = mapM_ putByte . interpret i . compile
  where putByte = BS.putStr . BS.pack . return

-- | EOF is represented as 0
getInput :: IO Input
getInput = f <$> BS.getContents
  where f s = BS.unpack s ++ repeat 0

noInput :: Input
noInput = repeat 0
