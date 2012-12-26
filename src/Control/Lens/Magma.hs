{-# LANGUAGE Rank2Types #-}
module Control.Lens.Magma
  ( Magma(..)
  , size
  , magmaIns
  , magmaOuts
  , magma
  , nullLeft
  , nullRight
  ) where

import Control.Applicative
import Control.Lens.Fold
import Control.Lens.Internal
import Control.Lens.Lens
import Control.Lens.Traversal
import Data.Monoid
import Data.Foldable

type Size = Int -- computed lazily

------------------------------------------------------------------------------
-- Magma
------------------------------------------------------------------------------

data Magma a
  = Ap Size Bool Bool (Magma a) (Magma a) -- size, left-to-right null check, right-to-left null check, left, right
  | Leaf a
  | Pure
  deriving Show

size :: Magma a -> Int
size (Ap s _ _ _ _) = s
size Leaf{}       = 1
size Pure         = 0
{-# INLINE size #-}

nullLeft :: Magma a -> Bool
nullLeft (Ap _ ltr _ _ _) = ltr
nullLeft (Leaf _)         = False
nullLeft Pure             = True
{-# INLINE nullLeft #-}

nullRight :: Magma a -> Bool
nullRight (Ap _ _ rtl _ _) = rtl
nullRight (Leaf _)         = False
nullRight Pure             = True
{-# INLINE nullRight #-}

instance Functor Magma where
  fmap f (Ap m ltr rtl l r) = Ap m ltr rtl (fmap f l) (fmap f r)
  fmap f (Leaf a)           = Leaf (f a)
  fmap _ Pure               = Pure
  {-# INLINE fmap #-}

instance Foldable Magma where
  foldMap f (Ap _ _ _ l r) = foldMap f l `mappend` foldMap f r
  foldMap f (Leaf a)       = f a
  foldMap _ Pure           = mempty
  {-# INLINE foldMap #-}

instance Traversable Magma where
  traverse f (Ap m ltr rtl l r) = Ap m ltr rtl <$> traverse f l <*> traverse f r
  traverse f (Leaf a)           = Leaf <$> f a
  traverse _ Pure               = pure Pure
  {-# INLINE traverse #-}

-- | An illegal 'Monoid'
instance Monoid (Magma a) where
  mempty = Pure
  {-# INLINE mempty #-}

  l `mappend` r = Ap (size l + size r) (nullLeft l && nullLeft r) (nullRight r && nullRight l) l r
  {-# INLINE mappend #-}

-- | Attempt to compress a 'Traversable'
magmaIns :: Bazaar (->) a b t -> Magma a
magmaIns = foldMapOf (flip runBazaar) Leaf
{-# INLINE magmaIns #-}

------------------------------------------------------------------------------
-- Putting it back in the tree
------------------------------------------------------------------------------

newtype Flow e a = Flow { runFlow :: Magma e -> a }

instance Functor (Flow e) where
  fmap f (Flow g) = Flow (f . g)
  {-# INLINE fmap #-}

-- | This is an illegal 'Applicative'.
instance Applicative (Flow e) where
  pure a = Flow (const a)
  {-# INLINE pure #-}
  Flow mf <*> Flow ma = Flow $ \ s -> case s of
    Ap _ _ _ l r -> mf l (ma r)
    _            -> mf s (ma s)
  {-# INLINE (<*>) #-}

magmaOuts :: Bazaar (->) a b t -> Magma b -> t
magmaOuts bz = runFlow go where
  go = runBazaar bz $ \_ -> Flow $ \ t -> case t of
    Leaf x -> x
    _      -> error "magmaOuts: wrong shape"
{-# INLINE magmaOuts #-}

-- | This is only a valid 'Lens' if you don't change the shape of the 'Magma'
magma :: ATraversal s t a b -> Lens s t (Magma a) (Magma b)
magma l f s = magmaOuts bz <$> f (magmaIns bz) where
  bz = l sell s
{-# INLINE magma #-}
