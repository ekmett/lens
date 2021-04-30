{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Indexed
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal implementation details for 'Indexed' lens-likes
----------------------------------------------------------------------------
module Control.Lens.Internal.Indexed
  (
  -- * An Indexed Profunctor
    Indexed(..)
  -- * Classes
  , Conjoined(..)
  , Indexable(..)
  -- * Indexing
  , Indexing(..)
  , indexing
  -- * 64-bit Indexing
  , Indexing64(..)
  , indexing64
  -- * Converting to Folds
  , withIndex
  , asIndex
  ) where

import Prelude ()

import Control.Arrow as Arrow
import qualified Control.Category as C
import Control.Comonad
import Control.Lens.Internal.Prelude
import Control.Lens.Internal.Instances ()
import Control.Monad.Fix
import Data.Coerce
import Data.Distributive
import Data.Functor.Bind
import Data.Int
import Data.Profunctor.Closed
import Data.Profunctor.Rep

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Numeric.Lens
-- >>> import Data.Semigroup (Semigroup (..))
--
------------------------------------------------------------------------------
-- Conjoined
------------------------------------------------------------------------------

-- | This is a 'Profunctor' that is both 'Corepresentable' by @f@ and 'Representable' by @g@ such
-- that @f@ is left adjoint to @g@. From this you can derive a lot of structure due
-- to the preservation of limits and colimits.
class
  ( Choice p, Corepresentable p, Comonad (Corep p), Traversable (Corep p)
  , Strong p, Representable p, Monad (Rep p), MonadFix (Rep p), Distributive (Rep p)
  , Costrong p, ArrowLoop p, ArrowApply p, ArrowChoice p, Closed p
  ) => Conjoined p where

  -- | 'Conjoined' is strong enough to let us distribute every 'Conjoined'
  -- 'Profunctor' over every Haskell 'Functor'. This is effectively a
  -- generalization of 'fmap'.
  distrib :: Functor f => p a b -> p (f a) (f b)
  distrib = tabulate . collect . sieve
  {-# INLINE distrib #-}

  -- | This permits us to make a decision at an outermost point about whether or not we use an index.
  --
  -- Ideally any use of this function should be done in such a way so that you compute the same answer,
  -- but this cannot be enforced at the type level.
  conjoined :: ((p ~ (->)) => q (a -> b) r) -> q (p a b) r -> q (p a b) r
  conjoined _ r = r
  {-# INLINE conjoined #-}

instance Conjoined (->) where
  distrib = fmap
  {-# INLINE distrib #-}
  conjoined l _ = l
  {-# INLINE conjoined #-}

----------------------------------------------------------------------------
-- Indexable
----------------------------------------------------------------------------

-- | This class permits overloading of function application for things that
-- also admit a notion of a key or index.
class Conjoined p => Indexable i p where
  -- | Build a function from an 'indexed' function.
  indexed :: p a b -> i -> a -> b

instance Indexable i (->) where
  indexed = const
  {-# INLINE indexed #-}

-----------------------------------------------------------------------------
-- Indexed Internals
-----------------------------------------------------------------------------

-- | A function with access to a index. This constructor may be useful when you need to store
-- an 'Indexable' in a container to avoid @ImpredicativeTypes@.
--
-- @index :: Indexed i a b -> i -> a -> b@
newtype Indexed i a b = Indexed { runIndexed :: i -> a -> b }

instance Functor (Indexed i a) where
  fmap g (Indexed f) = Indexed $ \i a -> g (f i a)
  {-# INLINE fmap #-}

instance Apply (Indexed i a) where
  Indexed f <.> Indexed g = Indexed $ \i a -> f i a (g i a)
  {-# INLINE (<.>) #-}

instance Applicative (Indexed i a) where
  pure b = Indexed $ \_ _ -> b
  {-# INLINE pure #-}
  Indexed f <*> Indexed g = Indexed $ \i a -> f i a (g i a)
  {-# INLINE (<*>) #-}

instance Bind (Indexed i a) where
  Indexed f >>- k = Indexed $ \i a -> runIndexed (k (f i a)) i a
  {-# INLINE (>>-) #-}

instance Monad (Indexed i a) where
  return = pure
  {-# INLINE return #-}
  Indexed f >>= k = Indexed $ \i a -> runIndexed (k (f i a)) i a
  {-# INLINE (>>=) #-}

instance MonadFix (Indexed i a) where
  mfix f = Indexed $ \ i a -> let o = runIndexed (f o) i a in o
  {-# INLINE mfix #-}

instance Profunctor (Indexed i) where
  dimap ab cd ibc = Indexed $ \i -> cd . runIndexed ibc i . ab
  {-# INLINE dimap #-}
  lmap ab ibc = Indexed $ \i -> runIndexed ibc i . ab
  {-# INLINE lmap #-}
  rmap bc iab = Indexed $ \i -> bc . runIndexed iab i
  {-# INLINE rmap #-}
  (.#) ibc _ = coerce ibc
  {-# INLINE (.#) #-}
  (#.) _ = coerce
  {-# INLINE (#.) #-}

instance Closed (Indexed i) where
  closed (Indexed iab) = Indexed $ \i xa x -> iab i (xa x)

instance Costrong (Indexed i) where
  unfirst (Indexed iadbd) = Indexed $ \i a -> let
      (b, d) = iadbd i (a, d)
    in b

instance Sieve (Indexed i) ((->) i) where
  sieve = flip . runIndexed
  {-# INLINE sieve #-}

instance Representable (Indexed i) where
  type Rep (Indexed i) = (->) i
  tabulate = Indexed . flip
  {-# INLINE tabulate #-}

instance Cosieve (Indexed i) ((,) i) where
  cosieve = uncurry . runIndexed
  {-# INLINE cosieve #-}

instance Corepresentable (Indexed i) where
  type Corep (Indexed i) = (,) i
  cotabulate = Indexed . curry
  {-# INLINE cotabulate #-}

instance Choice (Indexed i) where
  right' = right
  {-# INLINE right' #-}

instance Strong (Indexed i) where
  second' = second
  {-# INLINE second' #-}

instance C.Category (Indexed i) where
  id = Indexed (const id)
  {-# INLINE id #-}
  Indexed f . Indexed g = Indexed $ \i -> f i . g i
  {-# INLINE (.) #-}

instance Arrow (Indexed i) where
  arr f = Indexed (\_ -> f)
  {-# INLINE arr #-}
  first f = Indexed (Arrow.first . runIndexed f)
  {-# INLINE first #-}
  second f = Indexed (Arrow.second . runIndexed f)
  {-# INLINE second #-}
  Indexed f *** Indexed g = Indexed $ \i -> f i *** g i
  {-# INLINE (***) #-}
  Indexed f &&& Indexed g = Indexed $ \i -> f i &&& g i
  {-# INLINE (&&&) #-}

instance ArrowChoice (Indexed i) where
  left f = Indexed (left . runIndexed f)
  {-# INLINE left #-}
  right f = Indexed (right . runIndexed f)
  {-# INLINE right #-}
  Indexed f +++ Indexed g = Indexed $ \i -> f i +++ g i
  {-# INLINE (+++)  #-}
  Indexed f ||| Indexed g = Indexed $ \i -> f i ||| g i
  {-# INLINE (|||) #-}

instance ArrowApply (Indexed i) where
  app = Indexed $ \ i (f, b) -> runIndexed f i b
  {-# INLINE app #-}

instance ArrowLoop (Indexed i) where
  loop (Indexed f) = Indexed $ \i b -> let (c,d) = f i (b, d) in c
  {-# INLINE loop #-}

instance Conjoined (Indexed i) where
  distrib (Indexed iab) = Indexed $ \i fa -> iab i <$> fa
  {-# INLINE distrib #-}

instance i ~ j => Indexable i (Indexed j) where
  indexed = runIndexed
  {-# INLINE indexed #-}

------------------------------------------------------------------------------
-- Indexing
------------------------------------------------------------------------------

-- | 'Applicative' composition of @'Control.Monad.Trans.State.Lazy.State' 'Int'@ with a 'Functor', used
-- by 'Control.Lens.Indexed.indexed'.
newtype Indexing f a = Indexing { runIndexing :: Int -> (Int, f a) }

instance Functor f => Functor (Indexing f) where
  fmap f (Indexing m) = Indexing $ \i -> case m i of
    (j, x) -> (j, fmap f x)
  {-# INLINE fmap #-}

instance Apply f => Apply (Indexing f) where
  Indexing mf <.> Indexing ma = Indexing $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <.> fa)
  {-# INLINE (<.>) #-}

instance Applicative f => Applicative (Indexing f) where
  pure x = Indexing $ \i -> (i, pure x)
  {-# INLINE pure #-}
  Indexing mf <*> Indexing ma = Indexing $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <*> fa)
  {-# INLINE (<*>) #-}

instance Contravariant f => Contravariant (Indexing f) where
  contramap f (Indexing m) = Indexing $ \i -> case m i of
    (j, ff) -> (j, contramap f ff)
  {-# INLINE contramap #-}

instance Semigroup (f a) => Semigroup (Indexing f a) where
    Indexing mx <> Indexing my = Indexing $ \i -> case mx i of
      (j, x) -> case my j of
         ~(k, y) -> (k, x <> y)
    {-# INLINE (<>) #-}

-- |
--
-- >>> "cat" ^@.. (folded <> folded)
-- [(0,'c'),(1,'a'),(2,'t'),(0,'c'),(1,'a'),(2,'t')]
--
-- >>> "cat" ^@.. indexing (folded <> folded)
-- [(0,'c'),(1,'a'),(2,'t'),(3,'c'),(4,'a'),(5,'t')]
instance Monoid (f a) => Monoid (Indexing f a) where
    mempty = Indexing $ \i -> (i, mempty)
    {-# INLINE mempty #-}

#if !(MIN_VERSION_base(4,11,0))
    mappend (Indexing mx) (Indexing my) = Indexing $ \i -> case mx i of
      (j, x) -> case my j of
         ~(k, y) -> (k, mappend x y)
    {-# INLINE mappend #-}
#endif

-- | Transform a 'Control.Lens.Traversal.Traversal' into an 'Control.Lens.Traversal.IndexedTraversal' or
-- a 'Control.Lens.Fold.Fold' into an 'Control.Lens.Fold.IndexedFold', etc.
--
-- @
-- 'indexing' :: 'Control.Lens.Type.Traversal' s t a b -> 'Control.Lens.Type.IndexedTraversal' 'Int' s t a b
-- 'indexing' :: 'Control.Lens.Type.Prism' s t a b     -> 'Control.Lens.Type.IndexedTraversal' 'Int' s t a b
-- 'indexing' :: 'Control.Lens.Type.Lens' s t a b      -> 'Control.Lens.Type.IndexedLens' 'Int'  s t a b
-- 'indexing' :: 'Control.Lens.Type.Iso' s t a b       -> 'Control.Lens.Type.IndexedLens' 'Int' s t a b
-- 'indexing' :: 'Control.Lens.Type.Fold' s a          -> 'Control.Lens.Type.IndexedFold' 'Int' s a
-- 'indexing' :: 'Control.Lens.Type.Getter' s a        -> 'Control.Lens.Type.IndexedGetter' 'Int' s a
-- @
--
-- @'indexing' :: 'Indexable' 'Int' p => 'Control.Lens.Type.LensLike' ('Indexing' f) s t a b -> 'Control.Lens.Type.Over' p f s t a b@
indexing :: Indexable Int p => ((a -> Indexing f b) -> s -> Indexing f t) -> p a (f b) -> s -> f t
indexing l iafb s = snd $ runIndexing (l (\a -> Indexing (\i -> i `seq` (i + 1, indexed iafb i a))) s) 0
{-# INLINE indexing #-}

------------------------------------------------------------------------------
-- Indexing64
------------------------------------------------------------------------------

-- | 'Applicative' composition of @'Control.Monad.Trans.State.Lazy.State' 'Int64'@ with a 'Functor', used
-- by 'Control.Lens.Indexed.indexed64'.
newtype Indexing64 f a = Indexing64 { runIndexing64 :: Int64 -> (Int64, f a) }

instance Functor f => Functor (Indexing64 f) where
  fmap f (Indexing64 m) = Indexing64 $ \i -> case m i of
    (j, x) -> (j, fmap f x)
  {-# INLINE fmap #-}

instance Apply f => Apply (Indexing64 f) where
  Indexing64 mf <.> Indexing64 ma = Indexing64 $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <.> fa)
  {-# INLINE (<.>) #-}

instance Applicative f => Applicative (Indexing64 f) where
  pure x = Indexing64 $ \i -> (i, pure x)
  {-# INLINE pure #-}
  Indexing64 mf <*> Indexing64 ma = Indexing64 $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <*> fa)
  {-# INLINE (<*>) #-}

instance Contravariant f => Contravariant (Indexing64 f) where
  contramap f (Indexing64 m) = Indexing64 $ \i -> case m i of
    (j, ff) -> (j, contramap f ff)
  {-# INLINE contramap #-}

-- | Transform a 'Control.Lens.Traversal.Traversal' into an 'Control.Lens.Traversal.IndexedTraversal' or
-- a 'Control.Lens.Fold.Fold' into an 'Control.Lens.Fold.IndexedFold', etc.
--
-- This combinator is like 'indexing' except that it handles large traversals and folds gracefully.
--
-- @
-- 'indexing64' :: 'Control.Lens.Type.Traversal' s t a b -> 'Control.Lens.Type.IndexedTraversal' 'Int64' s t a b
-- 'indexing64' :: 'Control.Lens.Type.Prism' s t a b     -> 'Control.Lens.Type.IndexedTraversal' 'Int64' s t a b
-- 'indexing64' :: 'Control.Lens.Type.Lens' s t a b      -> 'Control.Lens.Type.IndexedLens' 'Int64' s t a b
-- 'indexing64' :: 'Control.Lens.Type.Iso' s t a b       -> 'Control.Lens.Type.IndexedLens' 'Int64' s t a b
-- 'indexing64' :: 'Control.Lens.Type.Fold' s a          -> 'Control.Lens.Type.IndexedFold' 'Int64' s a
-- 'indexing64' :: 'Control.Lens.Type.Getter' s a        -> 'Control.Lens.Type.IndexedGetter' 'Int64' s a
-- @
--
-- @'indexing64' :: 'Indexable' 'Int64' p => 'Control.Lens.Type.LensLike' ('Indexing64' f) s t a b -> 'Control.Lens.Type.Over' p f s t a b@
indexing64 :: Indexable Int64 p => ((a -> Indexing64 f b) -> s -> Indexing64 f t) -> p a (f b) -> s -> f t
indexing64 l iafb s = snd $ runIndexing64 (l (\a -> Indexing64 (\i -> i `seq` (i + 1, indexed iafb i a))) s) 0
{-# INLINE indexing64 #-}

-------------------------------------------------------------------------------
-- Converting to Folds
-------------------------------------------------------------------------------

-- | Fold a container with indices returning both the indices and the values.
--
-- The result is only valid to compose in a 'Traversal', if you don't edit the
-- index as edits to the index have no effect.
--
-- >>> [10, 20, 30] ^.. ifolded . withIndex
-- [(0,10),(1,20),(2,30)]
--
-- >>> [10, 20, 30] ^.. ifolded . withIndex . alongside negated (re _Show)
-- [(0,"10"),(-1,"20"),(-2,"30")]
--
withIndex :: (Indexable i p, Functor f) => p (i, s) (f (j, t)) -> Indexed i s (f t)
withIndex f = Indexed $ \i a -> snd <$> indexed f i (i, a)
{-# INLINE withIndex #-}

-- | When composed with an 'IndexedFold' or 'IndexedTraversal' this yields an
-- ('Indexed') 'Fold' of the indices.
asIndex :: (Indexable i p, Contravariant f, Functor f) => p i (f i) -> Indexed i s (f s)
asIndex f = Indexed $ \i _ -> phantom (indexed f i i)
{-# INLINE asIndex #-}
