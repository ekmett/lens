{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Sequence.Lens
  ( viewL, viewR
  , sliced, slicedTo, slicedFrom
  , seqOf
  ) where

import Control.Applicative
import Control.Lens
import Data.Monoid
import Data.Sequence as Seq
import Prelude

-- $setup
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g

-- * Sequence isomorphisms

-- | A 'Seq' is isomorphic to a 'ViewL'
--
-- @'viewl' m ≡ m '^.' 'viewL'@
--
-- >>> Seq.fromList [a,b,c] ^. viewL
-- a :< fromList [b,c]
--
-- >>> Seq.empty ^. viewL
-- EmptyL
--
-- >>> EmptyL ^. from viewL
-- fromList []
--
-- >>> review viewL $ a Seq.:< fromList [b,c]
-- fromList [a,b,c]
viewL :: Iso (Seq a) (Seq b) (ViewL a) (ViewL b)
viewL = iso viewl $ \ xs -> case xs of
  EmptyL ->  mempty
  a Seq.:< as -> a Seq.<| as
{-# INLINE viewL #-}

-- | A 'Seq' is isomorphic to a 'ViewR'
--
-- @'viewr' m ≡ m '^.' 'viewR'@
--
-- >>> Seq.fromList [a,b,c] ^. viewR
-- fromList [a,b] :> c
--
-- >>> Seq.empty ^. viewR
-- EmptyR
--
-- >>> EmptyR ^. from viewR
-- fromList []
--
-- >>> review viewR $ fromList [a,b] Seq.:> c
-- fromList [a,b,c]
viewR :: Iso (Seq a) (Seq b) (ViewR a) (ViewR b)
viewR = iso viewr $ \xs -> case xs of
  EmptyR  -> mempty
  as Seq.:> a -> as Seq.|> a
{-# INLINE viewR #-}

-- | Traverse the first @n@ elements of a 'Seq'
--
-- >>> fromList [a,b,c,d,e] ^.. slicedTo 2
-- [a,b]
--
-- >>> fromList [a,b,c,d,e] & slicedTo 2 %~ f
-- fromList [f a,f b,c,d,e]
--
-- >>> fromList [a,b,c,d,e] & slicedTo 10 .~ x
-- fromList [x,x,x,x,x]
slicedTo :: Int -> IndexedTraversal' Int (Seq a) a
slicedTo n f m = case Seq.splitAt n m of
  (l,r) -> (>< r) <$> itraverse (indexed f) l
{-# INLINE slicedTo #-}

-- | Traverse all but the first @n@ elements of a 'Seq'
--
-- >>> fromList [a,b,c,d,e] ^.. slicedFrom 2
-- [c,d,e]
--
-- >>> fromList [a,b,c,d,e] & slicedFrom 2 %~ f
-- fromList [a,b,f c,f d,f e]
--
-- >>> fromList [a,b,c,d,e] & slicedFrom 10 .~ x
-- fromList [a,b,c,d,e]
slicedFrom :: Int -> IndexedTraversal' Int (Seq a) a
slicedFrom n f m = case Seq.splitAt n m of
  (l,r) -> (l ><) <$> itraverse (indexed f . (+n)) r
{-# INLINE slicedFrom #-}

-- | Traverse all the elements numbered from @i@ to @j@ of a 'Seq'
--
-- >>> fromList [a,b,c,d,e] & sliced 1 3 %~ f
-- fromList [a,f b,f c,d,e]

-- >>> fromList [a,b,c,d,e] ^.. sliced 1 3
-- [f b,f c]
--
-- >>> fromList [a,b,c,d,e] & sliced 1 3 .~ x
-- fromList [a,x,x,b,e]
sliced :: Int -> Int -> IndexedTraversal' Int (Seq a) a
sliced i j f s = case Seq.splitAt i s of
  (l,mr) -> case Seq.splitAt (j-i) mr of
     (m, r) -> itraverse (indexed f . (+i)) m <&> \n -> l >< n >< r
{-# INLINE sliced #-}

-- | Construct a 'Seq' from a 'Getter', 'Control.Lens.Fold.Fold', 'Control.Lens.Traversal.Traversal', 'Control.Lens.Lens.Lens' or 'Control.Lens.Iso.Iso'.
--
-- >>> seqOf folded ["hello","world"]
-- fromList ["hello","world"]
--
-- >>> seqOf (folded._2) [("hello",1),("world",2),("!!!",3)]
-- fromList [1,2,3]
--
-- @
-- 'seqOf' :: 'Getter' s a     -> s -> 'Seq' a
-- 'seqOf' :: 'Fold' s a       -> s -> 'Seq' a
-- 'seqOf' :: 'Iso'' s a       -> s -> 'Seq' a
-- 'seqOf' :: 'Lens'' s a      -> s -> 'Seq' a
-- 'seqOf' :: 'Traversal'' s a -> s -> 'Seq' a
-- @
seqOf :: Getting (Seq a) s a -> s -> Seq a
seqOf l = views l Seq.singleton
{-# INLINE seqOf #-}
