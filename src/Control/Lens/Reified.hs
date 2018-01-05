{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Reified
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
------------------------------------------------------------------------------
module Control.Lens.Reified where

import Control.Applicative
import Control.Arrow
import qualified Control.Category as Cat
import Control.Comonad
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Internal.Indexed
import Control.Lens.Traversal (ignored)
import Control.Lens.Type
import Control.Monad
import Control.Monad.Reader.Class
import Data.Distributive
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Functor.Bind
import Data.Functor.Extend
import Data.Functor.Identity
import Data.Functor.Plus
import Data.Profunctor.Closed
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif

-- $setup
-- >>> import Control.Lens

------------------------------------------------------------------------------
-- Lens
------------------------------------------------------------------------------

-- | Reify a 'Lens' so it can be stored safely in a container.
newtype ReifiedLens s t a b = Lens { runLens :: Lens s t a b }

-- | @
-- type 'ReifiedLens'' = 'Simple' 'ReifiedLens'
-- @
type ReifiedLens' s a = ReifiedLens s s a a

------------------------------------------------------------------------------
-- IndexedLens
------------------------------------------------------------------------------

-- | Reify an 'IndexedLens' so it can be stored safely in a container.
newtype ReifiedIndexedLens i s t a b = IndexedLens { runIndexedLens :: IndexedLens i s t a b }

-- | @
-- type 'ReifiedIndexedLens'' i = 'Simple' ('ReifiedIndexedLens' i)
-- @
type ReifiedIndexedLens' i s a = ReifiedIndexedLens i s s a a

------------------------------------------------------------------------------
-- IndexedTraversal
------------------------------------------------------------------------------

-- | Reify an 'IndexedTraversal' so it can be stored safely in a container.
newtype ReifiedIndexedTraversal i s t a b = IndexedTraversal { runIndexedTraversal :: IndexedTraversal i s t a b }

-- | @
-- type 'ReifiedIndexedTraversal'' i = 'Simple' ('ReifiedIndexedTraversal' i)
-- @
type ReifiedIndexedTraversal' i s a = ReifiedIndexedTraversal i s s a a

------------------------------------------------------------------------------
-- Traversal
------------------------------------------------------------------------------

-- | A form of 'Traversal' that can be stored monomorphically in a container.
newtype ReifiedTraversal s t a b = Traversal { runTraversal :: Traversal s t a b }

-- | @
-- type 'ReifiedTraversal'' = 'Simple' 'ReifiedTraversal'
-- @
type ReifiedTraversal' s a = ReifiedTraversal s s a a

------------------------------------------------------------------------------
-- Getter
------------------------------------------------------------------------------

-- | Reify a 'Getter' so it can be stored safely in a container.
--
-- This can also be useful when combining getters in novel ways, as
-- 'ReifiedGetter' is isomorphic to '(->)' and provides similar instances.
--
-- >>> ("hello","world","!!!")^.runGetter ((,) <$> Getter _2 <*> Getter (_1.to length))
-- ("world",5)
newtype ReifiedGetter s a = Getter { runGetter :: Getter s a }

instance Distributive (ReifiedGetter s) where
  distribute as = Getter $ to $ \s -> fmap (\(Getter l) -> view l s) as

instance Functor (ReifiedGetter s) where
  fmap f l = Getter (runGetter l.to f)
  {-# INLINE fmap #-}

instance Semigroup s => Extend (ReifiedGetter s) where
  duplicated (Getter l) = Getter $ to $ \m -> Getter $ to $ \n -> view l (m <> n)
  {-# INLINE duplicated #-}

instance Monoid s => Comonad (ReifiedGetter s) where
  extract (Getter l) = view l mempty
  {-# INLINE extract #-}
  duplicate (Getter l) = Getter $ to $ \m -> Getter $ to $ \n -> view l (mappend m n)
  {-# INLINE duplicate #-}

instance Monoid s => ComonadApply (ReifiedGetter s) where
  Getter mf <@> Getter ma = Getter $ to $ \s -> view mf s (view ma s)
  {-# INLINE (<@>) #-}
  m <@ _ = m
  {-# INLINE (<@) #-}
  _ @> m = m
  {-# INLINE (@>) #-}

instance Apply (ReifiedGetter s) where
  Getter mf <.> Getter ma = Getter $ to $ \s -> view mf s (view ma s)
  {-# INLINE (<.>) #-}
  m <. _ = m
  {-# INLINE (<.) #-}
  _ .> m = m
  {-# INLINE (.>) #-}

instance Applicative (ReifiedGetter s) where
  pure a = Getter $ to $ \_ -> a
  {-# INLINE pure #-}
  Getter mf <*> Getter ma = Getter $ to $ \s -> view mf s (view ma s)
  {-# INLINE (<*>) #-}
  m <* _ = m
  {-# INLINE (<*) #-}
  _ *> m = m
  {-# INLINE (*>) #-}

instance Bind (ReifiedGetter s) where
  Getter ma >>- f = Getter $ to $ \s -> view (runGetter (f (view ma s))) s
  {-# INLINE (>>-) #-}

instance Monad (ReifiedGetter s) where
  return = pure
  {-# INLINE return #-}
  Getter ma >>= f = Getter $ to $ \s -> view (runGetter (f (view ma s))) s
  {-# INLINE (>>=) #-}

instance MonadReader s (ReifiedGetter s) where
  ask = Getter id
  {-# INLINE ask #-}
  local f m = Getter (to f . runGetter m)
  {-# INLINE local #-}

instance Profunctor ReifiedGetter where
  dimap f g l = Getter $ to f.runGetter l.to g
  {-# INLINE dimap #-}
  lmap g l    = Getter $ to g.runGetter l
  {-# INLINE lmap #-}
  rmap f l    = Getter $ runGetter l.to f
  {-# INLINE rmap #-}

instance Closed ReifiedGetter where
  closed l = Getter $ to $ \f -> view (runGetter l) . f

instance Cosieve ReifiedGetter Identity where
  cosieve (Getter l) = view l . runIdentity

instance Corepresentable ReifiedGetter where
  type Corep ReifiedGetter = Identity
  cotabulate f = Getter $ to (f . Identity)

instance Sieve ReifiedGetter Identity where
  sieve (Getter l) = Identity . view l

instance Representable ReifiedGetter where
  type Rep ReifiedGetter = Identity
  tabulate f = Getter $ to (runIdentity . f)

instance Costrong ReifiedGetter where
  unfirst l = Getter $ to $ unfirst $ view (runGetter l)

instance Conjoined ReifiedGetter

instance Strong ReifiedGetter where
  first' l = Getter $ \f (s,c) ->
    phantom $ runGetter l (dimap (flip (,) c) phantom f) s
  {-# INLINE first' #-}
  second' l = Getter $ \f (c,s) ->
    phantom $ runGetter l (dimap ((,) c) phantom f) s
  {-# INLINE second' #-}

instance Choice ReifiedGetter where
  left' l = Getter $ to $ left' $ view $ runGetter l
  {-# INLINE left' #-}
  right' l = Getter $ to $ right' $ view $ runGetter l
  {-# INLINE right' #-}

instance Cat.Category ReifiedGetter where
  id = Getter id
  l . r = Getter (runGetter r.runGetter l)
  {-# INLINE (.) #-}

instance Arrow ReifiedGetter where
  arr f = Getter (to f)
  {-# INLINE arr #-}
  first l = Getter $ to $ first $ view $ runGetter l
  {-# INLINE first #-}
  second l = Getter $ to $ second $ view $ runGetter l
  {-# INLINE second #-}
  Getter l *** Getter r = Getter $ to $ view l *** view r
  {-# INLINE (***) #-}
  Getter l &&& Getter r = Getter $ to $ view l &&& view r
  {-# INLINE (&&&) #-}

instance ArrowApply ReifiedGetter where
  app = Getter $ to $ \(Getter bc, b) -> view bc b
  {-# INLINE app #-}

instance ArrowChoice ReifiedGetter where
  left l = Getter $ to $ left $ view $ runGetter l
  {-# INLINE left #-}
  right l = Getter $ to $ right $ view $ runGetter l
  {-# INLINE right #-}
  Getter l +++ Getter r = Getter $ to $ view l +++ view r
  {-# INLINE (+++) #-}
  Getter l ||| Getter r = Getter $ to $ view l ||| view r
  {-# INLINE (|||) #-}

instance ArrowLoop ReifiedGetter where
  loop l = Getter $ to $ loop $ view $ runGetter l
  {-# INLINE loop #-}

------------------------------------------------------------------------------
-- IndexedGetter
------------------------------------------------------------------------------

-- | Reify an 'IndexedGetter' so it can be stored safely in a container.
newtype ReifiedIndexedGetter i s a = IndexedGetter { runIndexedGetter :: IndexedGetter i s a }

instance Profunctor (ReifiedIndexedGetter i) where
  dimap f g l = IndexedGetter (to f . runIndexedGetter l . to g)
  {-# INLINE dimap #-}

instance Sieve (ReifiedIndexedGetter i) ((,) i) where
  sieve = iview . runIndexedGetter
  {-# INLINE sieve #-}

instance Representable (ReifiedIndexedGetter i) where
  type Rep (ReifiedIndexedGetter i) = (,) i
  tabulate f = IndexedGetter $ ito f
  {-# INLINE tabulate #-}

instance Strong (ReifiedIndexedGetter i) where
  first' l = IndexedGetter $ \f (s,c) ->
    phantom $ runIndexedGetter l (dimap (flip (,) c) phantom f) s
  {-# INLINE first' #-}
  second' l = IndexedGetter $ \f (c,s) ->
    phantom $ runIndexedGetter l (dimap ((,) c) phantom f) s
  {-# INLINE second' #-}

instance Functor (ReifiedIndexedGetter i s) where
  fmap f l = IndexedGetter (runIndexedGetter l.to f)
  {-# INLINE fmap #-}

instance Semigroup i => Apply (ReifiedIndexedGetter i s) where
  IndexedGetter mf <.> IndexedGetter ma = IndexedGetter $ \k s ->
    case iview mf s of
      (i, f) -> case iview ma s of
        (j, a) -> phantom $ indexed k (i <> j) (f a)
  {-# INLINE (<.>) #-}


------------------------------------------------------------------------------
-- Fold
------------------------------------------------------------------------------

-- | Reify a 'Fold' so it can be stored safely in a container.
--
-- This can also be useful for creatively combining folds as
-- @'ReifiedFold' s@ is isomorphic to @ReaderT s []@ and provides similar
-- instances.
--
-- >>> ("hello","world")^..runFold ((,) <$> Fold _2 <*> Fold both)
-- [("world","hello"),("world","world")]
newtype ReifiedFold s a = Fold { runFold :: Fold s a }

instance Profunctor ReifiedFold where
  dimap f g l = Fold (to f . runFold l . to g)
  {-# INLINE dimap #-}
  rmap g l = Fold (runFold l . to g)
  {-# INLINE rmap #-}
  lmap f l = Fold (to f . runFold l)
  {-# INLINE lmap #-}

instance Sieve ReifiedFold [] where
  sieve = toListOf . runFold

instance Representable ReifiedFold where
  type Rep ReifiedFold = []
  tabulate f = Fold (folding f)

instance Strong ReifiedFold where
  first' l = Fold $ \f (s,c) ->
    phantom $ runFold l (dimap (flip (,) c) phantom f) s
  {-# INLINE first' #-}
  second' l = Fold $ \f (c,s) ->
    phantom $ runFold l (dimap ((,) c) phantom f) s
  {-# INLINE second' #-}

instance Choice ReifiedFold where
  left' (Fold l) = Fold $ folding $ \esc -> case esc of
    Left s -> Left <$> toListOf l s
    Right c -> [Right c]
  {-# INLINE left' #-}
  right' (Fold l) = Fold $ folding $ \ecs -> case ecs of
    Left c -> [Left c]
    Right s -> Right <$> toListOf l s
  {-# INLINE right' #-}

instance Cat.Category ReifiedFold where
  id = Fold id
  l . r = Fold (runFold r . runFold l)
  {-# INLINE (.) #-}

instance Arrow ReifiedFold where
  arr f = Fold (to f)
  {-# INLINE arr #-}
  first = first'
  {-# INLINE first #-}
  second = second'
  {-# INLINE second #-}
  Fold l *** Fold r = Fold $ folding $ \(x,y) -> (,) <$> toListOf l x <*> toListOf r y
  {-# INLINE (***) #-}
  Fold l &&& Fold r = Fold $ folding $ \x -> (,) <$> toListOf l x <*> toListOf r x
  {-# INLINE (&&&) #-}

instance ArrowChoice ReifiedFold where
  left = left'
  {-# INLINE left #-}
  right = right'
  {-# INLINE right #-}

instance ArrowApply ReifiedFold where
  app = Fold $ folding $ \(Fold bc, b) -> toListOf bc b
  {-# INLINE app #-}

instance Functor (ReifiedFold s) where
  fmap f l = Fold (runFold l.to f)
  {-# INLINE fmap #-}

instance Apply (ReifiedFold s) where
  Fold mf <.> Fold ma = Fold $ folding $ \s -> toListOf mf s <.> toListOf ma s
  {-# INLINE (<.>) #-}
  Fold mf <. Fold ma = Fold $ folding $ \s -> toListOf mf s <. toListOf ma s
  {-# INLINE (<.) #-}
  Fold mf .> Fold ma = Fold $ folding $ \s -> toListOf mf s .> toListOf ma s
  {-# INLINE (.>) #-}

instance Applicative (ReifiedFold s) where
  pure a = Fold $ folding $ \_ -> [a]
  {-# INLINE pure #-}
  Fold mf <*> Fold ma = Fold $ folding $ \s -> toListOf mf s <*> toListOf ma s
  {-# INLINE (<*>) #-}
  Fold mf <* Fold ma = Fold $ folding $ \s -> toListOf mf s <* toListOf ma s
  {-# INLINE (<*) #-}
  Fold mf *> Fold ma = Fold $ folding $ \s -> toListOf mf s *> toListOf ma s
  {-# INLINE (*>) #-}

instance Alternative (ReifiedFold s) where
  empty = Fold ignored
  {-# INLINE empty #-}
  Fold ma <|> Fold mb = Fold $ folding (\s -> toListOf ma s ++ toListOf mb s)
  {-# INLINE (<|>) #-}

instance Bind (ReifiedFold s) where
  Fold ma >>- f = Fold $ folding $ \s -> toListOf ma s >>- \a -> toListOf (runFold (f a)) s
  {-# INLINE (>>-) #-}

instance Monad (ReifiedFold s) where
  return = pure
  {-# INLINE return #-}
  Fold ma >>= f = Fold $ folding $ \s -> toListOf ma s >>= \a -> toListOf (runFold (f a)) s
  {-# INLINE (>>=) #-}

instance MonadPlus (ReifiedFold s) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

instance MonadReader s (ReifiedFold s) where
  ask = Fold id
  {-# INLINE ask #-}
  local f m = Fold (to f . runFold m)
  {-# INLINE local #-}

instance Semigroup (ReifiedFold s a) where
  (<>) = (<|>)
  {-# INLINE (<>) #-}

instance Monoid (ReifiedFold s a) where
  mempty = Fold ignored
  {-# INLINE mempty #-}
  mappend = (<|>)
  {-# INLINE mappend #-}

instance Alt (ReifiedFold s) where
  (<!>) = (<|>)
  {-# INLINE (<!>) #-}

instance Plus (ReifiedFold s) where
  zero = Fold ignored
  {-# INLINE zero #-}

------------------------------------------------------------------------------
-- IndexedFold
------------------------------------------------------------------------------

newtype ReifiedIndexedFold i s a = IndexedFold { runIndexedFold :: IndexedFold i s a }

instance Semigroup (ReifiedIndexedFold i s a) where
  (<>) = (<!>)
  {-# INLINE (<>) #-}

instance Monoid (ReifiedIndexedFold i s a) where
  mempty = IndexedFold ignored
  {-# INLINE mempty #-}
  mappend = (<!>)
  {-# INLINE mappend #-}

instance Alt (ReifiedIndexedFold i s) where
  IndexedFold ma <!> IndexedFold mb = IndexedFold $
    ifolding $ \s -> itoListOf ma s ++ itoListOf mb s
  {-# INLINE (<!>) #-}

instance Plus (ReifiedIndexedFold i s) where
  zero = IndexedFold ignored
  {-# INLINE zero #-}

instance Functor (ReifiedIndexedFold i s) where
  fmap f l = IndexedFold (runIndexedFold l . to f)
  {-# INLINE fmap #-}

instance Profunctor (ReifiedIndexedFold i) where
  dimap f g l = IndexedFold (to f . runIndexedFold l . to g)
  {-# INLINE dimap #-}
  lmap f l = IndexedFold (to f . runIndexedFold l)
  {-# INLINE lmap #-}
  rmap g l = IndexedFold (runIndexedFold l . to g)
  {-# INLINE rmap #-}

instance Sieve (ReifiedIndexedFold i) (Compose [] ((,) i)) where
  sieve (IndexedFold l) = Compose . itoListOf l
  {-# INLINE sieve #-}

instance Representable (ReifiedIndexedFold i) where
  type Rep (ReifiedIndexedFold i) = Compose [] ((,) i)
  tabulate k = IndexedFold $ \f -> phantom . traverse_ (phantom . uncurry (indexed f)) . getCompose . k
  {-# INLINE tabulate #-}

instance Strong (ReifiedIndexedFold i) where
  first' l  = IndexedFold $ \f (s,c) ->
    phantom $ runIndexedFold l (dimap (flip (,) c) phantom f) s
  {-# INLINE first' #-}
  second' l = IndexedFold $ \f (c,s) ->
    phantom $ runIndexedFold l (dimap ((,) c) phantom f) s
  {-# INLINE second' #-}


------------------------------------------------------------------------------
-- Setter
------------------------------------------------------------------------------

-- | Reify a 'Setter' so it can be stored safely in a container.
newtype ReifiedSetter s t a b = Setter { runSetter :: Setter s t a b }

-- | @
-- type 'ReifiedSetter'' = 'Simple' 'ReifiedSetter'
-- @
type ReifiedSetter' s a = ReifiedSetter s s a a

------------------------------------------------------------------------------
-- IndexedSetter
------------------------------------------------------------------------------

-- | Reify an 'IndexedSetter' so it can be stored safely in a container.
newtype ReifiedIndexedSetter i s t a b =
  IndexedSetter { runIndexedSetter :: IndexedSetter i s t a b }

-- | @
-- type 'ReifiedIndexedSetter'' i = 'Simple' ('ReifiedIndexedSetter' i)
-- @
type ReifiedIndexedSetter' i s a = ReifiedIndexedSetter i s s a a

------------------------------------------------------------------------------
-- Iso
------------------------------------------------------------------------------

-- | Reify an 'Iso' so it can be stored safely in a container.
newtype ReifiedIso s t a b = Iso { runIso :: Iso s t a b }

-- | @
-- type 'ReifiedIso'' = 'Simple' 'ReifiedIso'
-- @
type ReifiedIso' s a = ReifiedIso s s a a

------------------------------------------------------------------------------
-- Prism
------------------------------------------------------------------------------

-- | Reify a 'Prism' so it can be stored safely in a container.
newtype ReifiedPrism s t a b = Prism { runPrism :: Prism s t a b }

-- | @
-- type 'ReifiedPrism'' = 'Simple' 'ReifiedPrism'
-- @
type ReifiedPrism' s a = ReifiedPrism s s a a
