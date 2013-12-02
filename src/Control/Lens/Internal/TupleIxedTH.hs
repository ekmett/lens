{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.TupleIxedTH
-- Copyright   :  (C) 2012-13 Eric Mertens
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.TupleIxedTH (makeAllTupleIxed) where

import Control.Applicative
import Data.Traversable (traverse)
import Language.Haskell.TH

import Control.Lens.Internal.TH

-- This module needs to be used by Control.Lens.At which defines these names
indexN, ixValueN, ixedN, ixN :: Name
indexN   = mkName "Index"
ixValueN = mkName "IxValue"
ixedN    = mkName "Ixed"
ixN      = mkName "ix"

-- While GHC supports tuples up to 62, it can't cope with the resulting
-- large definitions. 9-tuples should be enough for anyone.

-- This generates all of the Ixed instances for tuples up to 9.
makeAllTupleIxed :: DecsQ
makeAllTupleIxed = fmap concat (traverse makeTupleIxed [2..9])

-- type instance Index (a,..) = Int
-- type instance IxValue (a,..) = a
-- instance (a~b,a~c...) => Ixed (a,b,c..) where
--   ix i f (a,b,c..) = fmap (\x -> (a,b,c...x..)) (f z)
--   ix _ f x         = pure x
makeTupleIxed :: Int -> DecsQ
makeTupleIxed n = sequence [tupleIndex n, tupleIxValue n, tupleIxed n]

-- type instance Index (a,..) = Int
tupleIndex :: Int -> DecQ
tupleIndex n = tySynInstD' indexN [fullTupleT n] [t|Int|]

-- type instance IxValue (a,..) = a
tupleIxValue :: Int -> DecQ
tupleIxValue n = tySynInstD' ixValueN [fullTupleT n] (head tupleVarTypes)

-- (a,..)
fullTupleT :: Int -> TypeQ
fullTupleT n = toTupleT (take n tupleVarTypes)

-- instance (a~b,a~c...) => Ixed (a,b,c..) where
--   ix i f (a,b,c..) = fmap (\x -> (a,b,c...x..)) (f z)
--   ix _ f x         = pure x
tupleIxed :: Int -> DecQ
tupleIxed n = instanceD (cxt eqs) (conT ixedN `appT` fullTupleT n) [funD ixN clauses]
  where
  ty0:tyN = take n tupleVarTypes
  eqs     = [ty0 `equalP` ty | ty <- tyN]
  clauses = map nClause [0..n-1] ++ [otherClause]

  -- ix i f (a,..) = fmap (\x->(a,..x..)) (f z)
  nClause i = do
    let iP = litP (integerL (fromIntegral i))
    f  <- newName "f"
    let fP = varP f
        fE = varE f
    xs <- mapM newName (take n nameSource)
    let xsP = map varP xs
        xsE = map varE xs
        xE  = varE (xs !! i)
    clause [iP, fP, toTupleP xsP]
           (normalB [| fmap (\x -> $(toTupleE (replaceAt i [|x|] xsE))) ($fE $xE) |])
           []

  -- ix _ _ x = pure x
  otherClause = do
    x <- newName "x"
    clause [wildP, wildP, varP x] (normalB [|pure $(varE x)|]) []

tupleVarTypes :: [TypeQ]
tupleVarTypes = map (varT . mkName) nameSource

-- [a,b,c,d...a1,b1,b2...a2,b2,c3...]
nameSource :: [String]
nameSource = [ a:n | n <- "" : map show [1 :: Int ..]
                   , a <- ['a'..'z']
                   ]

-- While this could be easily implemented as 'set . ix'
-- this local definition removes any circular dependency
-- issues.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt 0 x (_:ys) = x : ys
replaceAt i x (y:ys) = y : replaceAt (i-1) x ys
replaceAt _ _ []     = error "replaceAt: index too large"
