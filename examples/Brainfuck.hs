{-# LANGUAGE TypeOperators #-}
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
import Control.Monad.Free
import Control.Monad.RWS

import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromMaybe)
import qualified Data.Stream.Infinite as S
import Data.Word (Word8)

import System.Environment (getArgs)
import System.IO


-- Low level syntax form

data Instr = Plus | Minus | Right | Left | Comma | Dot | Open | Close
type Code = [Instr]

parse :: String -> Code
parse = concatMap (maybe [] return . (`lookup` symbols))
  where symbols = [ ('+', Plus ), ('-', Minus), ('<', Left), ('>', Right)
                  , (',', Comma), ('.', Dot  ), ('[', Open), (']', Close) ]

-- Higher level semantic graph

data Brainfuck n
  = Succ n | Pred n  -- Increment or decrement the current value
  | Next n | Prev n  -- Shift memory left or right
  | Read n | Write n -- Input or output the current value

  -- Branching semantic, used for both sides of loops
  | Branch { zero :: n, nonzero :: n }

type Program = Free Brainfuck ()

compile :: Code -> Program
compile = fst . bracket []

bracket :: [Program] -> Code -> (Program, [Program])
bracket [] []        = (Pure (), [])
bracket _  []        = error "Mismatched opening bracket"
bracket [] (Close:_) = error "Mismatched closing bracket"

-- Match a closing bracket: Pop a forward continuation, push backwards
bracket (c:cs) (Close : xs) = (Free (Branch n c), n:bs)
  where (n, bs) = bracket cs xs

-- Match an opening bracket: Pop a backwards continuation, push forwards
bracket cs (Open : xs) = (Free (Branch b n), bs)
  where (n, b:bs) = bracket (n:cs) xs

-- Match any other symbol in the trivial way
bracket cs (x:xs) = over _1 (Free . f x) (bracket cs xs)
  where
    f Plus  = Succ; f Minus = Pred
    f Right = Next; f Left  = Prev
    f Comma = Read; f Dot   = Write

-- * RWS-based interpreter

type Cell   = Word8
type Input  = S.Stream Cell
type Output = [Cell]
type Memory = Top :> [Cell] :> Cell -- list zipper

type Interpreter = RWS Input Output Memory ()

-- | Initial memory configuration
initial :: Memory
initial = zipper (replicate 30000 0) & fromWithin traverse

interpret :: Input -> Program -> Output
interpret i p = snd $ execRWS (run p) i initial

-- | Evaluation function
run :: Program -> Interpreter
run (Pure _) = return ()
run (Free f) = case f of
  Succ n -> focus += 1       >> run n
  Pred n -> focus -= 1       >> run n
  Next n -> modify wrapRight >> run n
  Prev n -> modify wrapLeft  >> run n

  Read n -> do
    focus <~ asks S.head
    local S.tail $ run n

  Write n -> do
    tell . return =<< use focus
    run n

  Branch z n -> do
    c <- use focus
    run $ case c of 0 -> z; _ -> n

-- | Zipper helpers
wrapRight, wrapLeft :: (a :> b) -> (a :> b)
wrapRight = liftM2 fromMaybe leftmost  right
wrapLeft  = liftM2 fromMaybe rightmost left

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
  where f s = S.fromList (BS.unpack s ++ repeat 0)

noInput :: Input
noInput = S.repeat 0
