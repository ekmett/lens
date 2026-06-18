{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Data.Plated
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- The 'Plated' class on its own, with no dependency on @lens@, so that other
-- libraries can provide 'Plated' instances for their types without pulling in
-- (or writing orphan instances against) the whole of @lens@.
--
-- This package depends only on @base@ and @containers@ (a GHC boot library). The
-- instances that would require heavier dependencies (@free@, @comonad@) live in
-- the companion @plated-instances@ package; import @Data.Plated.Instances@ to
-- bring those instances into scope (it re-exports nothing else). The rich set of
-- combinators that operate on 'Plated' values (@rewrite@, @transform@,
-- @universe@, @cosmos@, @holes@, @para@, ...) lives in @Control.Lens.Plated@ in
-- the @lens@ package, which re-exports this class unchanged.
-------------------------------------------------------------------------------
module Data.Plated
  (
  -- * Plated
    Plated(..)
  -- * Type
  , Traversal'
  -- * Data
  , uniplate
  -- * Generics
  , gplate
  , gplate1
  , GPlated
  , GPlated1
  ) where

import Data.Data (Data, gfoldl)
import Data.Tree (Tree(..))
import Data.Typeable (Typeable, eqT)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Generics

-- | A @'Traversal'' s a@ — the same type @lens@ uses, written out here so that
-- this package need not depend on @lens@. Because type synonyms are transparent,
-- it is literally the same type as @lens@'s @Control.Lens.Type.Traversal'@, so a
-- 'plate' defined here unifies with every @lens@ combinator unchanged.
--
-- Read it aloud: given an applicative effect @f@ and a way to turn each focused
-- @a@ into an @f a@, turn a whole @s@ into an @f s@.
type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

-- | A 'Plated' type is one where we know how to extract its immediate
-- self-similar children.
--
-- For example, a little expression language (no @lens@ import required):
--
-- @
-- import Data.Plated
--
-- data Expr = Val Int | Neg Expr | Add Expr Expr
--
-- instance 'Plated' Expr where
--   'plate' f (Neg e)   = Neg '<$>' f e
--   'plate' f (Add a b) = Add '<$>' f a '<*>' f b
--   'plate' _ x         = 'pure' x
-- @
--
-- If your type derives 'Data', you can omit the body entirely and let the
-- default fire:
--
-- @
-- data Expr = Val Int | Neg Expr | Add Expr Expr deriving 'Data'
--
-- instance 'Plated' Expr   -- 'plate' defaults to 'uniplate'
-- @
--
-- or, with a 'Generic' instance, use @'plate' = 'gplate'@.
--
-- Note the distinction between the hand-written and 'Data'-derived
-- definitions: the former treats only the direct @Expr@ children as
-- descendants, while 'uniplate' will also reach @Expr@ values nested inside
-- other types (for example, inside a list).
--
-- When in doubt, pick a @Traversal@ and just use the various @...Of@
-- combinators rather than pollute 'Plated' with orphan instances!
class Plated a where
  -- | 'Traversal' of the immediate children of this structure.
  --
  -- If your type has a 'Data' instance, 'plate' defaults to 'uniplate', so you
  -- can leave the instance body empty.
  plate :: Traversal' a a
  default plate :: Data a => Traversal' a a
  plate = uniplate

instance Plated [a] where
  plate f (x:xs) = (x:) <$> f xs
  plate _ [] = pure []

instance Plated (Tree a) where
  plate f (Node a as) = Node a <$> traverse f as

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

-- | A portable, @base@-only implementation of @uniplate@: a 'Traversal'' over
-- the immediate self-similar children of a 'Data' value.
--
-- This finds the /shallowest/ values of the same type reachable from the input,
-- descending through values of other types along the way. For example, given
-- @data T = T [T]@, @'uniplate'@ on a @T@ reaches the @T@s held inside the list,
-- not the list itself.
--
-- @lens@'s @Data.Data.Lens.uniplate@ computes the same traversal but adds a
-- 'Typeable' oracle that prunes descent into types that cannot contain an @a@;
-- it is faster on deeply nested non-matching structure, but otherwise behaves
-- identically. Prefer that one (or a hand-written 'plate') where performance
-- matters.
uniplate :: Data a => Traversal' a a
uniplate f = gtraverse (descend f)
{-# INLINE uniplate #-}

-- | Visit the shallowest @a@-typed values inside any 'Data' value, descending
-- through values whose type is not @a@.
descend :: forall a c f. (Applicative f, Typeable a, Data c) => (a -> f a) -> c -> f c
descend f c = case eqT :: Maybe (c :~: a) of
  Just Refl -> f c
  Nothing   -> gtraverse (descend f) c
{-# INLINE descend #-}

-- | An 'Applicative' analogue of @gmapM@: rebuild a 'Data' value, applying an
-- effectful function to each immediate child.
gtraverse :: forall a f. (Applicative f, Data a) => (forall d. Data d => d -> f d) -> a -> f a
gtraverse f = gfoldl (\rest d -> rest <*> f d) pure
{-# INLINE gtraverse #-}

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

-- | Implement 'plate' operation for a type using its 'Generic' instance.
--
-- Note: the behavior may be different than with 'uniplate' in some special
-- cases. 'gplate' doesn't look through other types in a group of mutually
-- recursive types. See @Control.Lens.Plated@ in the @lens@ package for worked
-- examples.
gplate :: (Generic a, GPlated a (Rep a)) => Traversal' a a
gplate f x = GHC.Generics.to <$> gplate' f (GHC.Generics.from x)
{-# INLINE gplate #-}

class GPlated a g where
  gplate' :: Traversal' (g p) a

instance GPlated a f => GPlated a (M1 i c f) where
  gplate' f (M1 x) = M1 <$> gplate' f x
  {-# INLINE gplate' #-}

instance (GPlated a f, GPlated a g) => GPlated a (f :+: g) where
  gplate' f (L1 x) = L1 <$> gplate' f x
  gplate' f (R1 x) = R1 <$> gplate' f x
  {-# INLINE gplate' #-}

instance (GPlated a f, GPlated a g) => GPlated a (f :*: g) where
  gplate' f (x :*: y) = (:*:) <$> gplate' f x <*> gplate' f y
  {-# INLINE gplate' #-}

instance {-# OVERLAPPING #-} GPlated a (K1 i a) where
  gplate' f (K1 x) = K1 <$> f x
  {-# INLINE gplate' #-}

instance GPlated a (K1 i b) where
  gplate' _ = pure
  {-# INLINE gplate' #-}

instance GPlated a U1 where
  gplate' _ = pure
  {-# INLINE gplate' #-}

instance GPlated a V1 where
  gplate' _ v = v `seq` error "GPlated/V1"
  {-# INLINE gplate' #-}

instance GPlated a (URec b) where
  gplate' _ = pure
  {-# INLINE gplate' #-}

-- | Implement 'plate' operation for a type using its 'Generic1' instance.
gplate1 :: (Generic1 f, GPlated1 f (Rep1 f)) => Traversal' (f a) (f a)
gplate1 f x = GHC.Generics.to1 <$> gplate1' f (GHC.Generics.from1 x)
{-# INLINE gplate1 #-}

class GPlated1 f g where
  gplate1' :: Traversal' (g a) (f a)

-- | recursive match
instance GPlated1 f g => GPlated1 f (M1 i c g) where
  gplate1' f (M1 x) = M1 <$> gplate1' f x
  {-# INLINE gplate1' #-}

-- | recursive match
instance (GPlated1 f g, GPlated1 f h) => GPlated1 f (g :+: h) where
  gplate1' f (L1 x) = L1 <$> gplate1' f x
  gplate1' f (R1 x) = R1 <$> gplate1' f x
  {-# INLINE gplate1' #-}

-- | recursive match
instance (GPlated1 f g, GPlated1 f h) => GPlated1 f (g :*: h) where
  gplate1' f (x :*: y) = (:*:) <$> gplate1' f x <*> gplate1' f y
  {-# INLINE gplate1' #-}

-- | ignored
instance GPlated1 f (K1 i a) where
  gplate1' _ = pure
  {-# INLINE gplate1' #-}

-- | ignored
instance GPlated1 f Par1 where
  gplate1' _ = pure
  {-# INLINE gplate1' #-}

-- | ignored
instance GPlated1 f U1 where
  gplate1' _ = pure
  {-# INLINE gplate1' #-}

-- | ignored
instance GPlated1 f V1 where
  gplate1' _ = pure
  {-# INLINE gplate1' #-}

-- | match
instance {-# OVERLAPPING #-} GPlated1 f (Rec1 f) where
  gplate1' f (Rec1 x) = Rec1 <$> f x
  {-# INLINE gplate1' #-}

-- | ignored
instance GPlated1 f (Rec1 g) where
  gplate1' _ = pure
  {-# INLINE gplate1' #-}

-- | recursive match under outer 'Traversable' instance
instance (Traversable t, GPlated1 f g) => GPlated1 f (t :.: g) where
  gplate1' f (Comp1 x) = Comp1 <$> traverse (gplate1' f) x
  {-# INLINE gplate1' #-}

-- | ignored
instance GPlated1 f (URec a) where
  gplate1' _ = pure
  {-# INLINE gplate1' #-}
