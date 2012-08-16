{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- These are some of the explicit Functor instances that leak into the
-- type signatures of Control.Lens. You shouldn't need to import this
-- module directly, unless you are coming up with a whole new kind of
-- \"Family\" and need to add instances.
--
----------------------------------------------------------------------------
module Control.Lens.Internal
  (
  -- * Implementation details
    IndexedStore(..)
  , Focusing(..)
  , FocusingWith(..)
  , FocusingPlus(..)
  , FocusingOn(..)
  , FocusingErr(..)
  , Err(..)
  , Traversed(..)
  , Sequenced(..)
  , AppliedState(..)
  , Min(..)
  , getMin
  , Max(..)
  , getMax
  , ElementOf(..)
  , ElementOfResult(..)
  , Kleene(..), kleene
  ) where


import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Error.Class
import Prelude hiding ((.),id)
import Data.Monoid

-----------------------------------------------------------------------------
-- Functors
-----------------------------------------------------------------------------

-- | Used by 'Control.Lens.Type.Zoom' to 'Control.Lens.Type.zoom' into 'Control.Monad.State.StateT'
newtype Focusing m c a = Focusing { unfocusing :: m (c, a) }

instance Monad m => Functor (Focusing m c) where
  fmap f (Focusing m) = Focusing $ do
     (c, a) <- m
     return (c, f a)

instance (Monad m, Monoid c) => Applicative (Focusing m c) where
  pure a = Focusing (return (mempty, a))
  Focusing mf <*> Focusing ma = Focusing $ do
    (c, f) <- mf
    (d, a) <- ma
    return (mappend c d, f a)

-- | Used by 'Control.Lens.Type.Zoom' to 'Control.Lens.Type.zoom' into 'Control.Monad.RWS.RWST'
newtype FocusingWith w m c a = FocusingWith { unfocusingWith :: m (c, a, w) }

instance Monad m => Functor (FocusingWith w m c) where
  fmap f (FocusingWith m) = FocusingWith $ do
     (c, a, w) <- m
     return (c, f a, w)

instance (Monad m, Monoid c, Monoid w) => Applicative (FocusingWith w m c) where
  pure a = FocusingWith (return (mempty, a, mempty))
  FocusingWith mf <*> FocusingWith ma = FocusingWith $ do
    (c, f, w) <- mf
    (d, a, w') <- ma
    return (mappend c d, f a, mappend w w')

-- | Used by 'Control.Lens.Type.Zoom' to 'Control.Lens.Type.zoom' into 'Control.Monad.Writer.WriterT'.
newtype FocusingPlus w k c a = FocusingPlus { unfocusingPlus :: k (c, w) a }

instance Functor (k (c, w)) => Functor (FocusingPlus w k c) where
  fmap f (FocusingPlus as) = FocusingPlus (fmap f as)

instance (Monoid w, Applicative (k (c, w))) => Applicative (FocusingPlus w k c) where
  pure = FocusingPlus . pure
  FocusingPlus kf <*> FocusingPlus ka = FocusingPlus (kf <*> ka)

-- | Used by 'Control.Lens.Type.Zoom' to 'Control.Lens.Type.zoom' into 'Control.Monad.Trans.Maybe.MaybeT' or 'Control.Monad.Trans.List.ListT'
newtype FocusingOn f k c a = FocusingOn { unfocusingOn :: k (f c) a }

instance Functor (k (f c)) => Functor (FocusingOn f k c) where
  fmap f (FocusingOn as) = FocusingOn (fmap f as)

instance Applicative (k (f c)) => Applicative (FocusingOn f k c) where
  pure = FocusingOn . pure
  FocusingOn kf <*> FocusingOn ka = FocusingOn (kf <*> ka)

-- | Make a monoid out of 'Either' using 'Error'.
newtype Err e a = Err { getErr :: Either e a }

instance (Error e, Monoid a) => Monoid (Err e a) where
  mempty = Err (Left noMsg)
  Err (Left e) `mappend` _ = Err (Left e)
  _ `mappend` Err (Left e) = Err (Left e)
  Err (Right a) `mappend` Err (Right b) = Err (Right (mappend a b))

-- | Used by 'Control.Lens.Type.Zoom' to 'Control.Lens.Type.zoom' into 'Control.Monad.Error.ErrorT'
newtype FocusingErr e k c a = FocusingErr { unfocusingErr :: k (Err e c) a }

instance Functor (k (Err e c)) => Functor (FocusingErr e k c) where
  fmap f (FocusingErr as) = FocusingErr (fmap f as)

instance (Error e, Applicative (k (Err e c))) => Applicative (FocusingErr e k c) where
  pure = FocusingErr . pure
  FocusingErr kf <*> FocusingErr ka = FocusingErr (kf <*> ka)

-- | The indexed store can be used to characterize a 'Control.Lens.Type.Lens'
-- and is used by 'Control.Lens.Type.clone'

data IndexedStore c d a = IndexedStore (d -> a) c

instance Functor (IndexedStore c d) where
  fmap f (IndexedStore g c) = IndexedStore (f . g) c

-- | Applicative composition of @'Control.Monad.Trans.State.Lazy.State' 'Int'@ with a 'Functor', used
-- by 'Control.Lens.Traversal.elementOf', 'Control.Lens.Traversal.elementsOf', 'Control.Lens.Traversal.traverseElement', 'Control.Lens.Traversal.traverseElementsOf'

newtype AppliedState f a = AppliedState { runAppliedState :: Int -> (f a, Int) }

instance Functor f => Functor (AppliedState f) where
  fmap f (AppliedState m) = AppliedState $ \i -> case m i of
    (fa, j) -> (fmap f fa, j)

instance Applicative f => Applicative (AppliedState f) where
  pure a = AppliedState (\i -> (pure a, i))
  AppliedState mf <*> AppliedState ma = AppliedState $ \i -> case mf i of
    (ff, j) -> case ma j of
       (fa, k) -> (ff <*> fa, k)

-- | Used internally by 'Control.Lens.Traversal.traverseOf_' and the like.

newtype Traversed f = Traversed { getTraversed :: f () }

instance Applicative f => Monoid (Traversed f) where
  mempty = Traversed (pure ())
  Traversed ma `mappend` Traversed mb = Traversed (ma *> mb)

-- | Used internally by 'Control.Lens.Traversal.mapM_' and the like.
newtype Sequenced m = Sequenced { getSequenced :: m () }

instance Monad m => Monoid (Sequenced m) where
  mempty = Sequenced (return ())
  Sequenced ma `mappend` Sequenced mb = Sequenced (ma >> mb)

-- | Used for 'Control.Lens.Fold.minimumOf'
data Min a = NoMin | Min a

instance Ord a => Monoid (Min a) where
  mempty = NoMin
  mappend NoMin m = m
  mappend m NoMin = m
  mappend (Min a) (Min b) = Min (min a b)

-- | Obtain the minimum.
getMin :: Min a -> Maybe a
getMin NoMin   = Nothing
getMin (Min a) = Just a

-- | Used for 'Control.Lens.Fold.maximumOf'
data Max a = NoMax | Max a

instance Ord a => Monoid (Max a) where
  mempty = NoMax
  mappend NoMax m = m
  mappend m NoMax = m
  mappend (Max a) (Max b) = Max (max a b)

-- | Obtain the maximum
getMax :: Max a -> Maybe a
getMax NoMax   = Nothing
getMax (Max a) = Just a

-- | The result of trying to find the /n/th 'Control.Lens.Traversal.element' of a 'Control.Lens.Traversal.Traversal'.
data ElementOfResult f a
  = Searching {-# UNPACK #-} !Int a
  | Found {-# UNPACK #-} !Int (f a)
  | NotFound String

instance Functor f => Functor (ElementOfResult f) where
  fmap f (Searching i a) = Searching i (f a)
  fmap f (Found i as) = Found i (fmap f as)
  fmap _ (NotFound e) = NotFound e

-- | Used to find the /n/th 'Control.Lens.Traversal.element' of a 'Control.Lens.Traversal.Traversal'.
newtype ElementOf f a = ElementOf { getElementOf :: Int -> ElementOfResult f a }

instance Functor f => Functor (ElementOf f) where
  fmap f (ElementOf m) = ElementOf $ \i -> case m i of
    Searching j a -> Searching j (f a)
    Found j as    -> Found j (fmap f as)
    NotFound e    -> NotFound e

instance Functor f => Applicative (ElementOf f) where
  pure a = ElementOf $ \i -> Searching i a
  ElementOf mf <*> ElementOf ma = ElementOf $ \i -> case mf i of
    Found j ff -> case ma j of
      Found _ _     -> NotFound "multiple results"
      Searching k a -> Found k (fmap ($a) ff)
      NotFound e    -> NotFound e
    Searching j f -> case ma j of
      Found k as    -> Found k (fmap f as)
      Searching k a -> Searching k (f a)
      NotFound e    -> NotFound e
    NotFound e -> NotFound e


-- | The "Indexed Kleene Store comonad", aka the 'indexed cartesian store comonad' or an indexed 'FunList'.
--
-- This is used to characterize a 'Control.Lens.Traversal.Traversal'.
--
-- <http://twanvl.nl/blog/haskell/non-regular1>

data Kleene c d a
  = Done a
  | More (Kleene c d (d -> a)) c

instance Functor (Kleene c d) where
  fmap f (Done a) = Done (f a)
  fmap f (More k b) = More (fmap (f .) k)  b

instance Applicative (Kleene c d) where
  pure = Done
  Done f   <*> m = fmap f m
  More k c <*> m = More (flip <$> k <*> m) c

kleene :: Applicative f => (c -> f d) -> Kleene c d b -> f b
kleene _ (Done b) = pure b
kleene f (More k c) = f c <**> kleene f k
