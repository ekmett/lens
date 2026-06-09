{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- Benchmark for issue #183/#590: combinators used *under-applied*.
--
-- Each binding below fixes a combinator's leading arguments and is marked
-- NOINLINE, so the partially-applied form (`set _1`, `over both (+1)`, ...)
-- survives as a real binding. With the combinator at full arity it cannot
-- inline into that binding and each application pays for the newtype/decompose
-- path; with the trailing arguments moved right of the `=` the binding
-- saturates it, the combinator inlines, and worker/wrapper fuses the hot loop.
-- See NOTE: [Inlining and arity] in Control.Lens.Fold.
--
-- Coverage mirrors the change set: set/set', partsOf, mapAccumLOf, both/both1,
-- and auf. (mapAccumLOf is exercised both directly and via scanl1Of.)
module Main (main) where

import Control.Lens
import Criterion.Main
import Data.Monoid (Sum (..))

-- `set _1` held under-applied at the definition site.
setFst :: Int -> (Int, Int) -> (Int, Int)
setFst = set _1
{-# NOINLINE setFst #-}

set'Snd :: Int -> (Int, Int) -> (Int, Int)
set'Snd = set' _2
{-# NOINLINE set'Snd #-}

-- `partsOf traverse` held under-applied (arity 3 -> 1).
reverseParts :: [Int] -> [Int]
reverseParts = over (partsOf traverse) reverse
{-# NOINLINE reverseParts #-}

-- scanl1Of drives mapAccumLOf under-applied (arity 4 -> 3).
scanSum :: [Int] -> [Int]
scanSum = scanl1Of traverse (+)
{-# NOINLINE scanSum #-}

-- mapAccumLOf held under-applied directly (arity 4 -> 3).
runningSum :: [Int] -> (Int, [Int])
runningSum = mapAccumLOf traverse (\acc x -> let acc' = acc + x in (acc', acc')) 0
{-# NOINLINE runningSum #-}

-- `both`/`both1` used first-class as optic values in a hot consumer.
bumpBoth :: (Int, Int) -> (Int, Int)
bumpBoth = over both (+ 1)
{-# NOINLINE bumpBoth #-}

bumpBoth1 :: (Int, Int) -> (Int, Int)
bumpBoth1 = over both1 (+ 1)
{-# NOINLINE bumpBoth1 #-}

-- `auf` with the iso and consumer fixed, held under-applied (arity 3 -> 1).
aufBoth :: (String -> Int) -> (String, String) -> Int
aufBoth = auf (_Wrapping Sum) (foldMapOf both)
{-# NOINLINE aufBoth #-}

-- Hammer the under-applied (Int,Int)-setter in a tight loop.
loopSet :: (Int -> (Int, Int) -> (Int, Int)) -> Int -> Int
loopSet f = \n ->
  let go !i !p@(a, b)
        | i >= n    = a + b
        | otherwise = go (i + 1) (f i p)
  in go 0 (0, 0)
{-# INLINE loopSet #-}

-- As loopSet, but for an endomorphism that takes no index (both/both1).
loopEndo :: ((Int, Int) -> (Int, Int)) -> Int -> Int
loopEndo f = \n ->
  let go !i !p@(a, b)
        | i >= n    = a + b
        | otherwise = go (i + 1) (f p)
  in go 0 (0, 0)
{-# INLINE loopEndo #-}

-- Strictly sum `work i` over [0..n); `work` applies the under-applied
-- combinator to an i-dependent input, so each iteration forces a fresh
-- evaluation (no sharing) without going through `map`/`replicate`.
loopSum :: (Int -> Int) -> Int -> Int
loopSum work = \n ->
  let go !i !acc
        | i >= n    = acc
        | otherwise = go (i + 1) (acc + work i)
  in go 0 0
{-# INLINE loopSum #-}

xs :: [Int]
xs = [1 .. 100]
{-# NOINLINE xs #-}

main :: IO ()
main = defaultMain
  [ bgroup "set-underapplied"
    [ bench "set _1"  $ whnf (loopSet setFst)  1000000
    , bench "set' _2" $ whnf (loopSet set'Snd) 1000000
    ]
  , bgroup "traversal-underapplied"
    [ bench "partsOf traverse"     $ nf (\v -> sum (concatMap reverseParts (replicate 1000 v))) xs
    , bench "scanl1Of traverse"    $ nf (\v -> sum (concatMap scanSum     (replicate 1000 v))) xs
    , bench "mapAccumLOf traverse" $ whnf (loopSum (\i -> fst (runningSum [i .. i + 99]))) 10000
    ]
  , bgroup "bitraversal-underapplied"
    [ bench "over both"  $ whnf (loopEndo bumpBoth)  1000000
    , bench "over both1" $ whnf (loopEndo bumpBoth1) 1000000
    ]
  , bgroup "iso-underapplied"
    [ bench "auf _Wrapping Sum" $ whnf (loopSum (\i -> aufBoth length (show i, "world"))) 100000
    ]
  ]
