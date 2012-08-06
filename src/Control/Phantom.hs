{-# LANGUAGE Rank2Types #-}
module Control.Phantom 
  where

import Control.Applicative
import Control.Applicative.Backwards
import Data.Functor.Identity

newtype Mutator a = Mutator { runMutator :: a }

instance Functor Mutator where
  fmap f (Mutator a) = Mutator (f a)

instance Applicative Mutator where
  pure = Mutator
  Mutator f <*> Mutator a = Mutator (f a)

{-
type IndexedTraversal a b c d = forall k f. (Indexed i k, Applicative f)           => k (c -> f d) (a -> f b)
type Iso a b c d              = forall k f. (Isomorphic k, Functor f)              => k (c -> f d) (a -> f b)
type Lens a b c d             = forall f. Functor f                                => (c -> f d)        -> a -> f b
type Traversal a b c d        = forall f. Applicative f                            => (c -> f d)        -> a -> f b
type Setter a b c d           =                                           (c -> Identity d) -> a -> Identity b

type IndexedFold a b c d      = forall k f. (Indexed i k, Applicative m, Monoid r) => k (c -> Compose m (Const r) d) (a -> Compose m (Const r) b)
type Fold a c                 = forall m b d r. (Applicative m, Monoid r) => (c -> Compose m (Const r) d)  -> a -> Compose m (Const r) b
type Action m a c             = forall m b d r.(c -> Compose m (Const r) d)  -> a -> Compose m (Const r) b
-}

infixr 4 .~
(.~) :: ((c -> Mutator d) -> a -> Mutator b) -> d -> a -> b
l .~ d = runMutator . l (Mutator . const d)

type Getter a c = forall f b d. Gettable f => (c -> f d) -> a -> f b
to :: (a -> c) -> Getter a c
to f g = coerce . g . f

type Action m a c = forall f b d. (Applicative m, Trivial f) => ((c -> Compose m f d) -> a -> Compose m f b

-- (^!) :: Applicative m => a -> ((c -> Compose m (Accessor c) d) -> a -> Compose m (Accessor c) b) -> m c
(^!) :: Applicative m => a -> Action m a c -> m c
a ^! l = active (l (from active . pure) a)

class Active m r | 
  lift m = 

class Gettable f => Trivial f r | f -> r, r -> f where
  konst :: Isomorphic k => k r (f a)

instance Trivial Accessor where
  konst = isomorphic Accessor getAccessor

instance Trivial f => Trivial (Backwards f) where
  konst = isomorphic Backwards forwards . konst

compose :: Isomorphic k => k (f (g a)) (Compose f g a)
compose = isomorphic Compose getCompose

fmap' :: (Isomorphic k, Functor f) => k a b -> k (f a) (f b)
fmap' = isomap fmap fmap

active :: (Isomorphic k, Functor f, Trivial g) => k (Compose f g a) (f r)
active = fmap' konst . from compose

