{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 #-}
-- Benchmark for ReifiedFoo newtypes be replaced by the monomorphic AFoo aliases.
-- Both can be stored in a container; this measures whether storing an AFoo
-- (then recovering an optic via clone*, or using a dedicated A* consumer) costs
-- the same as storing a ReifiedFoo and unwrapping. Each stored optic is NOINLINE
-- so it survives as an opaque value -- the cost you actually pay when it is pulled
-- out of a container -- rather than being constant-folded into the loop.
module Main (main) where

import Control.Lens
import Criterion.Main
import Data.Monoid (Endo)

-- Tight accumulating loop; `f i` applies a stored optic to a value built from i.
loop :: (Int -> Int) -> Int -> Int
loop f = \n ->
  let go !i !acc
        | i >= n    = acc
        | otherwise = go (i + 1) (acc + f i)
  in go 0 0
{-# INLINE loop #-}

-- Stored optics, opaque at a monomorphic type ------------------------------

rLens :: ReifiedLens' (Int, Int) Int
rLens = Lens _2
{-# NOINLINE rLens #-}
aLens :: ALens' (Int, Int) Int
aLens = _2
{-# NOINLINE aLens #-}

rSet :: ReifiedSetter' (Int, Int) Int
rSet = Setter _1
{-# NOINLINE rSet #-}
aSet :: ASetter' (Int, Int) Int
aSet = _1
{-# NOINLINE aSet #-}

rIso :: ReifiedIso' Int Int
rIso = Iso (iso (+ 1) (subtract 1))
{-# NOINLINE rIso #-}
aIso :: AnIso' Int Int
aIso = iso (+ 1) (subtract 1)
{-# NOINLINE aIso #-}

rPrism :: ReifiedPrism' (Maybe Int) Int
rPrism = Prism _Just
{-# NOINLINE rPrism #-}
aPrism :: APrism' (Maybe Int) Int
aPrism = _Just
{-# NOINLINE aPrism #-}

rTrav :: ReifiedTraversal' (Int, Int) Int
rTrav = Traversal both
{-# NOINLINE rTrav #-}
aTrav :: ATraversal' (Int, Int) Int
aTrav = both
{-# NOINLINE aTrav #-}

rFold :: ReifiedFold (Int, Int) Int
rFold = Fold both
{-# NOINLINE rFold #-}
gFold :: Getting (Endo (Endo Int)) (Int, Int) Int   -- result monoid baked in
gFold = both
{-# NOINLINE gFold #-}

rGet :: ReifiedGetter (Int, Int) Int
rGet = Getter _2
{-# NOINLINE rGet #-}
gGet :: Getting Int (Int, Int) Int
gGet = _2
{-# NOINLINE gGet #-}

rRev :: ReifiedReview Int Int
rRev = Review (unto (+ 1))
{-# NOINLINE rRev #-}
aRev :: AReview Int Int
aRev = unto (+ 1)
{-# NOINLINE aRev #-}

iters :: Int
iters = 1000000

main :: IO ()
main = defaultMain
  [ bgroup "lens"
    [ bench "Reified (runLens)"  $ whnf (loop (\i -> view (runLens rLens) (i, i + 1))) iters
    , bench "ALens  (cloneLens)" $ whnf (loop (\i -> view (cloneLens aLens) (i, i + 1))) iters
    , bench "ALens  (^#)"        $ whnf (loop (\i -> (i, i + 1) ^# aLens)) iters
    ]
  , bgroup "setter"
    [ bench "Reified (runSetter)"   $ whnf (loop (\i -> fst (over (runSetter rSet) (+ 1) (i, i + 1)))) iters
    , bench "ASetter (cloneSetter)" $ whnf (loop (\i -> fst (over (cloneSetter aSet) (+ 1) (i, i + 1)))) iters
    , bench "ASetter (over)"        $ whnf (loop (\i -> fst (over aSet (+ 1) (i, i + 1)))) iters
    ]
  , bgroup "iso"
    [ bench "Reified (runIso)"  $ whnf (loop (\i -> view (runIso rIso) i)) iters
    , bench "AnIso  (cloneIso)" $ whnf (loop (\i -> view (cloneIso aIso) i)) iters
    , bench "AnIso  (withIso)"  $ whnf (loop (\i -> withIso aIso (\f _ -> f i))) iters
    ]
  , bgroup "prism"
    [ bench "Reified (runPrism)"   $ whnf (loop (\i -> maybe 0 id (preview (runPrism rPrism) (Just i)))) iters
    , bench "APrism  (clonePrism)" $ whnf (loop (\i -> maybe 0 id (preview (clonePrism aPrism) (Just i)))) iters
    , bench "APrism  (withPrism)"  $ whnf (loop (\i -> withPrism aPrism (\_ m -> either (const 0) id (m (Just i))))) iters
    ]
  , bgroup "traversal"
    [ bench "Reified (runTraversal)"      $ whnf (loop (\i -> sumOf (runTraversal rTrav) (i, i + 1))) iters
    , bench "ATraversal (cloneTraversal)" $ whnf (loop (\i -> sumOf (cloneTraversal aTrav) (i, i + 1))) iters
    ]
  , bgroup "fold"
    [ bench "Reified (runFold)" $ whnf (loop (\i -> sumOf (runFold rFold) (i, i + 1))) iters
    , bench "Getting (sumOf)"   $ whnf (loop (\i -> sumOf gFold (i, i + 1))) iters
    ]
  , bgroup "getter"
    [ bench "Reified (runGetter)" $ whnf (loop (\i -> view (runGetter rGet) (i, i + 1))) iters
    , bench "Getting (view)"      $ whnf (loop (\i -> view gGet (i, i + 1))) iters
    ]
  , bgroup "review"
    [ bench "Reified (runReview)" $ whnf (loop (\i -> review (runReview rRev) i)) iters
    , bench "AReview (review)"    $ whnf (loop (\i -> review aRev i)) iters
    ]
  ]
