{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.HashMap.Lazy as HM
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
    [ bgroup "toList"
      [ bench "native" $ nf V.toList v
      , bench "each"   $ nf (toListOf each) v
      ]
    , bgroup "itoList"
      [ bench "native"     $ nf (V.toList . V.indexed) v
      , bench "itraversed" $ nf (itoListOf itraversed) v
      ]
    ]
  , bgroup "unboxed-vector"
    [ bgroup "toList"
      [ bench "native" $ nf U.toList u
      , bench "each"   $ nf (toListOf each) u
      ]
    , bgroup "itoList"
      [ bench "native"     $ nf (U.toList . U.indexed) u
      , bench "vTraverse" $ nf (itoListOf vectorTraverse) u
      ]
    ]
  , bgroup "sequence"
    [ bgroup "toList"
      [ bench "native" $ nf F.toList s
      , bench "each"   $ nf (toListOf each) s
      ]
    , bgroup "itoList"
      [ bench "native"     $ nf (F.toList . S.mapWithIndex (,)) s
      , bench "itraversed" $ nf (itoListOf itraversed) s
      ]
    ]
  , bgroup "bytestring"
    [ bgroup "toList"
      [ bench "native" $ nf BS.unpack b
      , bench "bytes"  $ nf (toListOf bytes) b
      , bench "each"   $ nf (toListOf each) b
      ]
    , bgroup "itoList"
      [ bench "native" $ nf (zip [(0::Int)..] . BS.unpack) b
      , bench "bytes"  $ nf (itoListOf bytes) b
      ]
    ]
  , bgroup "list"
    [ bgroup "toList"
      [ bench "native" $ nf F.toList l
      , bench "each"   $ nf (toListOf each) l
      ]
    , bgroup "itoList"
      [ bench "native"     $ nf (zip [(0::Int)..]) l
      , bench "itraversed" $ nf (itoListOf itraversed) l
      ]
    ]
  , bgroup "map"
    [ bgroup "toList"
      [ bench "native" $ nf F.toList m
      , bench "each"   $ nf itoList m
      ]
    , bgroup "itoList"
      [ bench "native"     $ nf (zip [(0::Int)..] . F.toList) m
      , bench "itraversed" $ nf (itoListOf itraversed) m
      ]
    ]
  , bgroup "hash map"
    [ bgroup "toList"
      [ bench "native" $ nf HM.keys h
      , bench "each"   $ nf (toListOf each) h
      ]
    , bgroup "itoList"
      [ bench "native"     $ nf HM.toList h
      , bench "itoList"    $ nf itoList h
      , bench "itraversed" $ nf (itoListOf itraversed) h
      ]
    , bgroup "sum"
      [ bench "native" $ nf (sum . id . F.toList) h
      , bench "each"   $ nf (sumOf each) h
      ]
    ]
  ]
  where
    config = defaultConfig { timeLimit = 1 }
    l = [0..10000] :: [Int]
    b = BS.pack $ map fromIntegral l
    h = HM.fromList $ zip l l
    m = M.fromList $ zip l l
    s = S.fromList l
    u = U.fromList l
    v = V.fromList l

