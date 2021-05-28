{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Data.Vector.Generic.Lens
import Data.ByteString.Lens

import Control.Lens
import Criterion.Main
import Criterion.Types

main :: IO ()
main = defaultMainWith config
  [
    bgroup "vector"
    [ bgroup "map"
      [ bench "native"     $ nf (V.map (+100)) v
      , bench "itraversed" $ nf (over itraversed (+100)) v
      ]
    , bgroup "imap"
      [ bench "native"     $ nf (V.imap           (\i x -> x + i +100)) v
      , bench "imap"       $ nf (imap             (\i x -> x + i +100)) v
      , bench "itraversed" $ nf (iover itraversed (\i x -> x + i +100)) v
      ]
    ]
  , bgroup "unboxed-vector"
    [ bgroup "map"
      [ bench "native"     $ nf (U.map (+100)) u
      , bench "itraversed" $ nf (over each (+100)) u
      ]
    , bgroup "imap"
      [ bench "native"     $ nf (U.imap (\i x -> x + i +100)) u
      , bench "itraversed" $ nf (iover vectorTraverse (\i x -> x + i) :: U.Vector Int -> U.Vector Int) u
      ]
    ]
  , bgroup "sequence"
    [ bgroup "map"
      [ bench "native" $ nf (fmap            (+100)) s
      , bench "each"   $ nf (over each       (+100)) s
      ]
    , bgroup "imap"
      [ bench "native" $ nf (S.mapWithIndex    (\i x -> x + i +100)) s
      , bench "imap"   $ nf (imap              (\i x -> x + i +100)) s
      ]
    ]
  , bgroup "bytestring"
    [ bgroup "map"
      [ bench "native" $ nf (BS.map     (+100)) b
      , bench "each"   $ nf (over each  (+100)) b
      ]
    , bgroup "imap"
      [
        bench "bytes" $ nf (iover bytes (\i x -> x + fromIntegral i +100)) b
      ]
    ]
  , bgroup "list"
    [ bgroup "map"
      [ bench "native" $ nf (map       (+100)) l
      , bench "each"   $ nf (over each (+100)) l
      ]
    , bgroup "imap"
      [ bench "imap" $ nf (imap (\i x -> x + i +100)) l
      ]
    ]
  , bgroup "map"
    [ bgroup "map"
      [ bench "native"     $ nf (fmap            (+100)) m
      , bench "each"       $ nf (over each       (+100)) m
      , bench "itraversed" $ nf (over itraversed (+100)) m
      ]
    , bgroup "imap"
      [ bench "native" $ nf (M.mapWithKey (\i x -> x + i +100)) m
      , bench "each"   $ nf (imap         (\i x -> x + i +100)) m
      ]
    ]
  , bgroup "hash map"
    [ bgroup "map"
      [ bench "native" $ nf (HM.map    (+100)) h
      , bench "each"   $ nf (over each (+100)) h
      ]
    , bgroup "imap"
      [ bench "native" $ nf (HM.mapWithKey (\i x -> x + i +100)) h
      , bench "imap"   $ nf (imap          (\i x -> x + i +100)) h
      ]
    ]
  ]
  where
    config = defaultConfig { timeLimit = 1 }
    l  = [0..10000] :: [Int]
    xl = [0..100000] :: [Int]
    b  = BS.pack $ map fromIntegral xl
    h  = HM.fromList $ zip l l
    m  = M.fromList $ zip l l
    s  = S.fromList l
    u  = U.fromList xl
    v  = V.fromList l
