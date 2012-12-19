{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Sequence.Lens
  ( viewL, viewR
  , _head, _tail
  , _last, _init
  , sliced, slicedTo, slicedFrom
  ) where

import Control.Applicative
import Control.Lens
import Data.Monoid
import Data.Sequence as Seq

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
-- >>> from viewL ^$ a :< fromList [b,c]
-- fromList [a,b,c]
--
viewL :: Iso (Seq a) (Seq b) (ViewL a) (ViewL b)
viewL = iso viewl $ \ xs -> case xs of
  EmptyL ->  mempty
  a :< as -> a <| as
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
-- >>> from viewR ^$ fromList [a,b] :> c
-- fromList [a,b,c]
viewR :: Iso (Seq a) (Seq b) (ViewR a) (ViewR b)
viewR = iso viewr $ \xs -> case xs of
  EmptyR  -> mempty
  as :> a -> as |> a
{-# INLINE viewR #-}

-- * Traversals

-- | Traverse the head of a 'Seq'
--
-- >>> fromList [a,b,c,d] & _head %~ f
-- fromList [f a,b,c,d]
--
-- >>> fromList [] ^? _head
-- Nothing
--
-- >>> fromList [a,b,c,d] ^? _head
-- Just a
_head :: IndexedTraversal' Int (Seq a) a
_head = indexed $ \f m -> case viewl m of
  a :< as -> (<| as) <$> f (0::Int) a
  EmptyL  -> pure m
{-# INLINE _head #-}

-- | Traverse the tail of a 'Seq'
--
-- >>> fromList [a,b] & _tail .~ fromList [c,d,e]
-- fromList [a,c,d,e]
--
-- >>> fromList [a,b,c] ^? _tail
-- Just (fromList [b,c])
--
-- >>> fromList [] ^? _tail
-- Nothing
_tail :: Traversal' (Seq a) (Seq a)
_tail f m = case viewl m of
  a :< as -> (a <|) <$> f as
  EmptyL  -> pure m
{-# INLINE _tail #-}

-- | Traverse the last element of a 'Seq'
--
-- >>> fromList [a,b,c,d] & _last %~ f
-- fromList [a,b,c,f d]
--
-- >>> fromList [a,b,c,d] ^? _last
-- Just d
--
-- >>> fromList [] ^? _last
-- Nothing
_last :: IndexedTraversal' Int (Seq a) a
_last = indexed $ \f m ->  case viewr m of
  as :> a -> (as |>) <$> f (Seq.length as) a
  EmptyR  -> pure m
{-# INLINE _last #-}

-- | Traverse all but the last element of a 'Seq'
--
-- >>> fromList [1,2,3] ^? _init
-- Just (fromList [1,2])
--
-- >>> fromList [a,b,c,d] & _init.traverse %~ f
-- fromList [f a,f b,f c,d]
--
-- >>> fromList [] & _init .~ fromList [a,b,c]
-- fromList []
_init :: Traversal' (Seq a) (Seq a)
_init f m = case viewr m of
  as :> a -> (|> a) <$> f as
  EmptyR  -> pure m
{-# INLINE _init #-}

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
slicedTo n = indexed $ \f m -> case Seq.splitAt n m of
  (l,r) -> (>< r) <$> itraverse f l
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
slicedFrom n = indexed $ \ f m -> case Seq.splitAt n m of
  (l,r) -> (l ><) <$> itraverse (f . (+n)) r
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
sliced i j = indexed $ \ f s -> case Seq.splitAt i s of
  (l,mr) -> case Seq.splitAt (j-i) mr of
     (m, r) -> itraverse (f . (+i)) m <&> \n -> l >< n >< r
{-# INLINE sliced #-}
