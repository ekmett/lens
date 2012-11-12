module Main where

import Control.Lens
import Control.Lens.Internal
import Control.Exception

import Criterion.Main
import Criterion.Config

overA :: Setting s t a b -> (a -> b) -> s -> t
overA l f = runMutator . l (Mutator . f)
{-# INLINE overA #-}

mappedA :: Setting [a] [b] a b
mappedA f = Mutator . map (runMutator . f)
{-# INLINE mappedA #-}

overB :: Setting s t a b -> (a -> b) -> s -> t
overB = over
{-# INLINE overB #-}

mappedB :: Setting [a] [b] a b
mappedB = mapped
{-# INLINE mappedB #-}


mapA :: (a -> b) -> [a] -> [b]
mapA f l = overA mappedA f l
{-# INLINE mapA #-}

mapB :: (a -> b) -> [a] -> [b]
mapB f l = overB mappedB f l
{-# INLINE mapB #-}

main :: IO ()
main = do
    let l = replicate 1000 "hi"
    evaluate (length l)
    defaultMainWith config (return ())
        [ bench "mapA" $ nf (mapA length) l
        , bench "mapB" $ nf (mapB length) l
        ]
  where
    config = defaultConfig { cfgSamples = ljust 1000 }
