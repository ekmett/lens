{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Lens
import Control.Lens.Internal
import Control.Exception

import Criterion.Main
import Criterion.Types (Config(..))

import Data.Functor.Identity (Identity(..))

import GHC.Exts

overS :: ASetter s t a b -> (a -> b) -> s -> t
overS l f = runIdentity . l (Identity . f)
{-# INLINE overS #-}

mappedS :: ASetter [a] [b] a b
mappedS f = Identity . map (runIdentity . f)
{-# INLINE mappedS #-}

overU :: ASetter s t a b -> (a -> b) -> s -> t
overU = over
{-# INLINE overU #-}

mappedU :: ASetter [a] [b] a b
mappedU = mapped
{-# INLINE mappedU #-}


-- Need to eta-expand for full inlining in the NOINLINE cases?
-- Doesn't seem to make a difference, though.

mapSN :: (a -> b) -> [a] -> [b]
mapSN f l = overS mappedS f l
{-# NOINLINE mapSN #-}

mapSI :: (a -> b) -> [a] -> [b]
mapSI f = overS mappedS f
{-# INLINE mapSI #-}

mapUN :: (a -> b) -> [a] -> [b]
mapUN f l = overU mappedU f l
{-# NOINLINE mapUN #-}

mapUI :: (a -> b) -> [a] -> [b]
mapUI f = overU mappedU f
{-# INLINE mapUI #-}

main :: IO ()
main = do
    let n = 1000
        l = replicate n "hi"; f = length
        --l = replicate n ();   f = (\ _ -> ())
        --l = replicate n ();   f = (\ !_ -> ()) -- strange results
        --l = replicate n ();   f = lazy (\_ -> ())
    defaultMainWith config
        [ bench "map   safe noinline" $ nf (mapSN f) l
        , bench "map   safe   inline" $ nf (mapSI f) l
        , bench "map unsafe noinline" $ nf (mapUN f) l
        , bench "map unsafe   inline" $ nf (mapUI f) l
        ]
  where
    config = defaultConfig { resamples = 1000 }
