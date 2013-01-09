{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Brainfuck
-- Copyright   :  (C) 2012 Edward Kmett, nand`
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  TH, Rank2, NoMonomorphismRestriction
--
-- A simple interpreter for the esoteric programming language "Brainfuck"
-- written using lenses and zippers.
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

-- Missing Fix-point stuff

newtype Fix f = Fix (f (Fix f))

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f (Fix m) = f $ cata f <$> m

-- Low level syntax form

data Instr = Plus | Minus | Right | Left | Comma | Dot | Open | Close
type Code = [Instr]

parse :: String -> Code
parse = mapMaybe (`lookup` symbols)
  where symbols = [ ('+', Plus ), ('-', Minus), ('<', Left), ('>', Right)
                  , (',', Comma), ('.', Dot  ), ('[', Open), (']', Close) ]

-- Higher level semantic graph

data Brainfuck n
  = Succ n | Pred n  -- Increment or decrement the current value
  | Next n | Prev n  -- Shift memory left or right
  | Read n | Write n -- Input or output the current value
  | Halt             -- End execution

  -- Branching semantic, used for both sides of loops
  | Branch { zero :: n, nonzero :: n }

  deriving Functor

type Program = Fix Brainfuck

compile :: Code -> Program
compile = fst . bracket []

bracket :: [Program] -> Code -> (Program, [Program])
bracket [] []        = (Fix Halt, [])
bracket _  []        = error "Mismatched opening bracket"
bracket [] (Close:_) = error "Mismatched closing bracket"

-- Match a closing bracket: Pop a forward continuation, push backwards
bracket (c:cs) (Close : xs) = (Fix (Branch n c), n:bs)
  where (n, bs) = bracket cs xs

-- Match an opening bracket: Pop a backwards continuation, push forwards
bracket cs (Open : xs) = (Fix (Branch b n), bs)
  where (n, b:bs) = bracket (n:cs) xs

-- Match any other symbol in the trivial way
bracket cs (x:xs) = over _1 (Fix . f x) (bracket cs xs)
  where
    f Plus  = Succ; f Minus = Pred
    f Right = Next; f Left  = Prev
    f Comma = Read; f Dot   = Write

-- * State/Writer-based interpreter

type Cell   = Word8
type Input  = [Cell]
type Output = [Cell]
type Memory = Top :>> [Cell] :>> Cell -- list zipper

data MachineState = MachineState
  { _input  :: [Cell]
  , _memory :: Memory }
makeLenses ''MachineState

type Interpreter = StateT MachineState (Writer Output) ()

-- | Initial memory configuration
initial :: Input -> MachineState
initial i = MachineState i (zipper (replicate memoryCellNum 0) & fromWithin traverse)

interpret :: Input -> Program -> Output
interpret i = execWriter . flip execStateT (initial i) . run

-- | Evaluation function
run :: Program -> Interpreter
run = cata $ \case
    Halt   -> return ()
    Succ n -> memory.focus += 1   >> n
    Pred n -> memory.focus -= 1   >> n
    Next n -> memory %= wrapRight >> n
    Prev n -> memory %= wrapLeft  >> n

    Read n -> do
      memory.focus <~ uses input head
      input %= tail
      n

    Write n -> do
      x <- use (memory.focus)
      tell [x]
      n

    Branch z n -> do
      c <- use (memory.focus)
      if c == 0 then z else n

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
eval i = mapM_ putByte . interpret i . compile . parse
  where putByte = BS.putStr . BS.pack . return

-- | EOF is represented as 0
getInput :: IO Input
getInput = f <$> BS.getContents
  where f s = BS.unpack s ++ repeat 0

noInput :: Input
noInput = repeat 0
