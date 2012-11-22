{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
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
  -- * Internal Types
    May(..)
  , Folding(..)
  , Effect(..)
  , EffectRWS(..)
  , Accessor(..)
  , Err(..)
  , Traversed(..)
  , Sequenced(..)
  , Focusing(..)
  , FocusingWith(..)
  , FocusingPlus(..)
  , FocusingOn(..)
  , FocusingMay(..)
  , FocusingErr(..)
  , Mutator(..)
  , Bazaar(..), bazaar, duplicateBazaar, sell
  , Context(..)
  , Max(..), getMax
  , Min(..), getMin
  , ElementOfResult(..), ElementOf(..)
  , Indexing(..)
  , Level(..)
  , levelWidth
  , leftLevel, left1Level, leftmostLevel
  , rightmostLevel, rightLevel, right1Level
  , focusLevel
  , rezipLevel
  -- * Hidden implementations
  , BazaarT
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Category
import Control.Comonad
import Control.Comonad.Store.Class
import Control.Lens.Isomorphic
import Control.Lens.Internal.BazaarT
import Control.Lens.Classes
import Control.Lens.Unsafe
import Control.Monad
import Prelude hiding ((.),id)
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Identity
import Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Unsafe.Coerce

-----------------------------------------------------------------------------
-- Functors
-----------------------------------------------------------------------------

-- | Used by 'Control.Lens.Type.Zoom' to 'Control.Lens.Type.zoom' into 'Control.Monad.State.StateT'
newtype Focusing m s a = Focusing { unfocusing :: m (s, a) }

instance Monad m => Functor (Focusing m s) where
  fmap f (Focusing m) = Focusing $ do
     (s, a) <- m
     return (s, f a)

instance (Monad m, Monoid s) => Applicative (Focusing m s) where
  pure a = Focusing (return (mempty, a))
  Focusing mf <*> Focusing ma = Focusing $ do
    (s, f) <- mf
    (s', a) <- ma
    return (mappend s s', f a)

-- | Used by 'Control.Lens.Type.Zoom' to 'Control.Lens.Type.zoom' into 'Control.Monad.RWS.RWST'
newtype FocusingWith w m s a = FocusingWith { unfocusingWith :: m (s, a, w) }

instance Monad m => Functor (FocusingWith w m s) where
  fmap f (FocusingWith m) = FocusingWith $ do
     (s, a, w) <- m
     return (s, f a, w)

instance (Monad m, Monoid s, Monoid w) => Applicative (FocusingWith w m s) where
  pure a = FocusingWith (return (mempty, a, mempty))
  FocusingWith mf <*> FocusingWith ma = FocusingWith $ do
    (s, f, w) <- mf
    (s', a, w') <- ma
    return (mappend s s', f a, mappend w w')

-- | Used by 'Control.Lens.Type.Zoom' to 'Control.Lens.Type.zoom' into 'Control.Monad.Writer.WriterT'.
newtype FocusingPlus w k s a = FocusingPlus { unfocusingPlus :: k (s, w) a }

instance Functor (k (s, w)) => Functor (FocusingPlus w k s) where
  fmap f (FocusingPlus as) = FocusingPlus (fmap f as)

instance (Monoid w, Applicative (k (s, w))) => Applicative (FocusingPlus w k s) where
  pure = FocusingPlus . pure
  FocusingPlus kf <*> FocusingPlus ka = FocusingPlus (kf <*> ka)

-- | Used by 'Control.Lens.Type.Zoom' to 'Control.Lens.Type.zoom' into 'Control.Monad.Trans.Maybe.MaybeT' or 'Control.Monad.Trans.List.ListT'
newtype FocusingOn f k s a = FocusingOn { unfocusingOn :: k (f s) a }

instance Functor (k (f s)) => Functor (FocusingOn f k s) where
  fmap f (FocusingOn as) = FocusingOn (fmap f as)

instance Applicative (k (f s)) => Applicative (FocusingOn f k s) where
  pure = FocusingOn . pure
  FocusingOn kf <*> FocusingOn ka = FocusingOn (kf <*> ka)

-- | Make a monoid out of 'Maybe' for error handling
newtype May a = May { getMay :: Maybe a }

instance Monoid a => Monoid (May a) where
  mempty = May (Just mempty)
  May Nothing `mappend` _ = May Nothing
  _ `mappend` May Nothing = May Nothing
  May (Just a) `mappend` May (Just b) = May (Just (mappend a b))

-- | Used by 'Control.Lens.Type.Zoom' to 'Control.Lens.Type.zoom' into 'Control.Monad.Error.ErrorT'
newtype FocusingMay k s a = FocusingMay { unfocusingMay :: k (May s) a }

instance Functor (k (May s)) => Functor (FocusingMay k s) where
  fmap f (FocusingMay as) = FocusingMay (fmap f as)

instance Applicative (k (May s)) => Applicative (FocusingMay k s) where
  pure = FocusingMay . pure
  FocusingMay kf <*> FocusingMay ka = FocusingMay (kf <*> ka)

-- | Make a monoid out of 'Either' for error handling
newtype Err e a = Err { getErr :: Either e a }

instance Monoid a => Monoid (Err e a) where
  mempty = Err (Right mempty)
  Err (Left e) `mappend` _ = Err (Left e)
  _ `mappend` Err (Left e) = Err (Left e)
  Err (Right a) `mappend` Err (Right b) = Err (Right (mappend a b))

-- | Used by 'Control.Lens.Type.Zoom' to 'Control.Lens.Type.zoom' into 'Control.Monad.Error.ErrorT'
newtype FocusingErr e k s a = FocusingErr { unfocusingErr :: k (Err e s) a }

instance Functor (k (Err e s)) => Functor (FocusingErr e k s) where
  fmap f (FocusingErr as) = FocusingErr (fmap f as)

instance Applicative (k (Err e s)) => Applicative (FocusingErr e k s) where
  pure = FocusingErr . pure
  FocusingErr kf <*> FocusingErr ka = FocusingErr (kf <*> ka)

-- | Applicative composition of @'Control.Monad.Trans.State.Lazy.State' 'Int'@ with a 'Functor', used
-- by 'Control.Lens.Indexed.indexed'
newtype Indexing f a = Indexing { runIndexing :: Int -> (f a, Int) }

instance Functor f => Functor (Indexing f) where
  fmap f (Indexing m) = Indexing $ \i -> case m i of
    (x, j) -> (fmap f x, j)

instance Applicative f => Applicative (Indexing f) where
  pure x = Indexing (\i -> (pure x, i))
  Indexing mf <*> Indexing ma = Indexing $ \i -> case mf i of
    (ff, j) -> case ma j of
       ~(fa, k) -> (ff <*> fa, k)

instance Trustworthy f => Trustworthy (Indexing f)

instance Gettable f => Gettable (Indexing f) where
  coerce (Indexing m) = Indexing $ \i -> case m i of
    (ff, j) -> (coerce ff, j)

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

-- | The indexed store can be used to characterize a 'Control.Lens.Type.Lens'
-- and is used by 'Control.Lens.Type.clone'
--
-- @'Context' a b t@ is isomorphic to
-- @newtype Context a b t = Context { runContext :: forall f. Functor f => (a -> f b) -> f t }@,
-- and to @exists s. (s, 'Control.Lens.Type.Lens' s t a b)@.
--
-- A 'Context' is like a 'Control.Lens.Type.Lens' that has already been applied to a some structure.
data Context a b t = Context (b -> t) a

instance Functor (Context a b) where
  fmap f (Context g t) = Context (f . g) t

instance (a ~ b) => Comonad (Context a b) where
  extract   (Context f a) = f a
  duplicate (Context f a) = Context (Context f) a
  extend g  (Context f a) = Context (g . Context f) a

instance (a ~ b) => ComonadStore a (Context a b) where
  pos (Context _ a) = a
  peek b (Context g _) = g b
  peeks f (Context g a) = g (f a)
  seek a (Context g _) = Context g a
  seeks f (Context g a) = Context g (f a)
  experiment f (Context g a) = g <$> f a

-- | This is used to characterize a 'Control.Lens.Traversal.Traversal'.
--
-- a.k.a. indexed Cartesian store comonad, indexed Kleene store comonad, or an indexed 'FunList'.
--
-- <http://twanvl.nl/blog/haskell/non-regular1>
--
-- @'Bazaar' a b t@ is isomorphic to @data Bazaar a b t = Buy t | Trade (Bazaar a b (b -> t)) a@,
-- and to @exists s. (s, 'Control.Lens.Traversal.Traversal' s t a b)@.
--
-- A 'Bazaar' is like a 'Control.Lens.Traversal.Traversal' that has already been applied to some structure.
--
-- Where a @'Context' a b t@ holds an @a@ and a function from @b@ to
-- @t@, a @'Bazaar' a b t@ holds N @a@s and a function from N
-- @b@s to @t@.
--
-- Mnemonically, a 'Bazaar' holds many stores and you can easily add more.
--
-- This is a final encoding of 'Bazaar'.
newtype Bazaar a b t = Bazaar { runBazaar :: forall f. Applicative f => (a -> f b) -> f t }

instance Functor (Bazaar a b) where
  fmap f (Bazaar k) = Bazaar (fmap f . k)

instance Applicative (Bazaar a b) where
  pure a = Bazaar (\_ -> pure a)
  {-# INLINE pure #-}
  Bazaar mf <*> Bazaar ma = Bazaar (\k -> mf k <*> ma k)
  {-# INLINE (<*>) #-}

instance (a ~ b) => Comonad (Bazaar a b) where
  extract (Bazaar m) = runIdentity (m Identity)
  {-# INLINE extract #-}
  duplicate = duplicateBazaar
  {-# INLINE duplicate #-}

-- | Given an action to run for each matched pair, traverse a bazaar.
--
-- @'bazaar' :: 'Control.Lens.Traversal.Traversal' ('Bazaar' a b t) t a b@
bazaar :: Applicative f => (a -> f b) -> Bazaar a b t -> f t
bazaar afb (Bazaar m) = m afb
{-# INLINE bazaar #-}

-- | 'Bazaar' is an indexed 'Comonad'.
duplicateBazaar :: Bazaar a c t -> Bazaar a b (Bazaar b c t)
duplicateBazaar (Bazaar m) = getCompose (m (Compose . fmap sell . sell))
{-# INLINE duplicateBazaar #-}
-- duplicateBazaar' (Bazaar m) = Bazaar (\g -> getCompose (m (Compose . fmap sell . g)))

-- | A trivial 'Bazaar'.
sell :: a -> Bazaar a b b
sell i = Bazaar (\k -> k i)
{-# INLINE sell #-}

instance (a ~ b) => ComonadApply (Bazaar a b) where
  (<@>) = (<*>)

-- | Wrap a monadic effect with a phantom type argument.
newtype Effect m r a = Effect { getEffect :: m r }

instance Functor (Effect m r) where
  fmap _ (Effect m) = Effect m

instance (Monad m, Monoid r) => Monoid (Effect m r a) where
  mempty = Effect (return mempty)
  Effect ma `mappend` Effect mb = Effect (liftM2 mappend ma mb)

instance (Monad m, Monoid r) => Applicative (Effect m r) where
  pure _ = Effect (return mempty)
  Effect ma <*> Effect mb = Effect (liftM2 mappend ma mb)

instance Trustworthy (Effect m r)

instance Gettable (Effect m r) where
  coerce (Effect m) = Effect m

instance Monad m => Effective m r (Effect m r) where
  effective = isomorphic Effect getEffect
  {-# INLINE effective #-}

-- | Wrap a monadic effect with a phantom type argument. Used when magnifying RWST.
newtype EffectRWS w st m s a = EffectRWS { getEffectRWS :: st -> m (s,st,w) }

instance Functor (EffectRWS w st m s) where
  fmap _ (EffectRWS m) = EffectRWS m

instance Trustworthy (EffectRWS w st m s)

instance Gettable (EffectRWS w st m s) where
  coerce (EffectRWS m) = EffectRWS m

-- Effective EffectRWS

instance (Monoid s, Monoid w, Monad m) => Applicative (EffectRWS w st m s) where
  pure _ = EffectRWS $ \st -> return (mempty, st, mempty)
  EffectRWS m <*> EffectRWS n = EffectRWS $ \st -> m st >>= \ (s,t,w) -> n t >>= \ (s',u,w') -> return (mappend s s', u, mappend w w')

{-
-- | Wrap a monadic effect with a phantom type argument. Used when magnifying StateT.
newtype EffectS st k s a = EffectS { runEffect :: st -> k (s, st) a }

instance Functor (k (s, st)) => Functor (EffectS st m s) where
  fmap f (EffectS m) = EffectS (fmap f . m)

instance (Monoid s, Monad m) => Applicative (EffectS st m s) where
  pure _ = EffectS $ \st -> return (mempty, st)
  EffectS m <*> EffectS n = EffectS $ \st -> m st >>= \ (s,t) -> n st >>= \ (s', u) -> return (mappend s s', u)
-}

-------------------------------------------------------------------------------
-- Accessors
-------------------------------------------------------------------------------

--instance Gettable (EffectS st m s) where
--  coerce (EffectS m) = EffectS m

-- | Used instead of 'Const' to report
--
-- @No instance of ('Control.Lens.Setter.Settable' 'Accessor')@
--
-- when the user attempts to misuse a 'Control.Lens.Setter.Setter' as a
-- 'Control.Lens.Getter.Getter', rather than a monolithic unification error.
newtype Accessor r a = Accessor { runAccessor :: r }

instance Functor (Accessor r) where
  fmap _ (Accessor m) = Accessor m

instance Monoid r => Applicative (Accessor r) where
  pure _ = Accessor mempty
  Accessor a <*> Accessor b = Accessor (mappend a b)

instance Trustworthy (Accessor r)

instance Gettable (Accessor r) where
  coerce (Accessor m) = Accessor m

instance Effective Identity r (Accessor r) where
  effective = isomorphic (Accessor . runIdentity) (Identity . runAccessor)
  {-# INLINE effective #-}

-- | A 'Monoid' for a 'Gettable' 'Applicative'.
newtype Folding f a = Folding { getFolding :: f a }

instance (Gettable f, Applicative f) => Monoid (Folding f a) where
  mempty = Folding noEffect
  {-# INLINE mempty #-}
  Folding fr `mappend` Folding fs = Folding (fr *> fs)
  {-# INLINE mappend #-}

-----------------------------------------------------------------------------
-- Mutators
-----------------------------------------------------------------------------

-- | 'Mutator' is just a renamed 'Identity' functor to give better error
-- messages when someone attempts to use a getter as a setter.
--
-- Most user code will never need to see this type.
newtype Mutator a = Mutator { runMutator :: a }

instance Functor Mutator where
  fmap f (Mutator a) = Mutator (f a)

instance Applicative Mutator where
  pure = Mutator
  Mutator f <*> Mutator a = Mutator (f a)

instance Settable Mutator where
  untainted = runMutator
  untainted# = unsafeCoerce
  {-# INLINE untainted #-}
  tainted# = unsafeCoerce
  {-# INLINE tainted# #-}

-----------------------------------------------------------------------------
-- Level
-----------------------------------------------------------------------------

-- | A basic non-empty list zipper
--
-- All combinators assume the invariant that the length stored matches the number
-- of elements in list of items to the left, and the list of items to the left is
-- stored reversed.
data Level a = Level {-# UNPACK #-} !Int [a] a [a]

-- | How many entries are there in this level?
levelWidth :: Level a -> Int
levelWidth (Level n _ _ rs) = n + 1 + length rs
{-# INLINE levelWidth #-}

-- | Pull the non-emtpy list zipper left one entry
leftLevel :: Level a -> Maybe (Level a)
leftLevel (Level _ []     _ _ ) = Nothing
leftLevel (Level n (l:ls) a rs) = Just (Level (n - 1) ls l (a:rs))
{-# INLINE leftLevel #-}

-- | Pull the non-empty list zipper left one entry, stopping at the first entry.
left1Level :: Level a -> Level a
left1Level z = fromMaybe z (leftLevel z)
{-# INLINE left1Level #-}

-- | Pull the non-empty list zipper all the way to the left.
leftmostLevel :: Level a -> Level a
leftmostLevel (Level _ ls m rs) = case Prelude.reverse ls ++ m : rs of
  (c:cs) -> Level 0 [] c cs
  _ -> error "the impossible happened"
{-# INLINE leftmostLevel #-}

-- | Pul the non-empty list zipper all the way to the right.
-- /NB:/, when given an infinite list this may not terminate.
rightmostLevel :: Level a -> Level a
rightmostLevel (Level _ ls m rs) = go 0 [] (Prelude.head xs) (Prelude.tail xs) where
  xs = Prelude.reverse ls ++ m : rs
  go n zs y []     = Level n zs y []
  go n zs y (w:ws) = (go $! n + 1) (y:zs) w ws
{-# INLINE rightmostLevel #-}

-- | Pull the non-empty list zipper right one entry.
rightLevel :: Level a -> Maybe (Level a)
rightLevel (Level _ _  _ []    ) = Nothing
rightLevel (Level n ls a (r:rs)) = Just (Level (n + 1) (a:ls) r rs)
{-# INLINE rightLevel #-}

-- | Pull the non-empty list zipper right one entry, stopping at the last entry.
right1Level :: Level a -> Level a
right1Level z = fromMaybe z (rightLevel z)
{-# INLINE right1Level #-}

-- | This is a 'Lens' targeting the value that we would 'extract' from the non-empty list zipper.
--
-- @'view' 'focusLevel' ≡ 'extract'@
--
-- @'focusLevel' :: 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' ('Level' a) a@
focusLevel :: Functor f => (a -> f a) -> Level a -> f (Level a)
focusLevel f (Level n ls a rs) = (\b -> Level n ls b rs) <$> f a
{-# INLINE focusLevel #-}

instance Functor Level where
  fmap f (Level n ls a rs) = Level n (f <$> ls) (f a) (f <$> rs)

instance Foldable Level where
  foldMap f (Level _ ls a rs) = foldMap f (Prelude.reverse ls) <> f a <> foldMap f rs

instance Traversable Level where
  traverse f (Level n ls a rs) = Level n <$> forwards (traverse (Backwards . f) ls) <*> f a <*> traverse f rs

-- | Zip a non-empty list zipper back up, and return the result.
rezipLevel :: Level a -> NonEmpty a
rezipLevel (Level _ ls a rs) = NonEmpty.fromList (Prelude.reverse ls ++ a : rs)
{-# INLINE rezipLevel #-}

instance Comonad Level where
  extract (Level _ _ a _) = a
  extend f w@(Level n ls m rs) = Level n (gol (n - 1) (m:rs) ls) (f w) (gor (n + 1) (m:ls) rs) where
    gol k zs (y:ys) = f (Level k ys y zs) : (gol $! k - 1) (y:zs) ys
    gol _ _ [] = []
    gor k ys (z:zs) = f (Level k ys z zs) : (gor $! k + 1) (z:ys) zs
    gor _ _ [] = []

instance ComonadStore Int Level where
  pos (Level n _ _ _) = n
  peek n (Level m ls a rs) = case compare n m of
    LT -> ls Prelude.!! (m - n)
    EQ -> a
    GT -> rs Prelude.!! (n - m)

-- | The result of searching for a particular element in a Traversal.
data ElementOfResult f a = Searching Int a (Maybe (f a))

instance Functor f => Functor (ElementOfResult f) where
  fmap f (Searching i a as) = Searching i (f a) (fmap f <$> as)

-- | Searches for a particular element in a Traversal.
newtype ElementOf f a = ElementOf { getElementOf :: Int -> ElementOfResult f a }

instance Functor f => Functor (ElementOf f) where
  fmap f (ElementOf m) = ElementOf (fmap f . m)

instance Functor f => Applicative (ElementOf f) where
  pure a = ElementOf $ \i -> Searching i a Nothing
  ElementOf mf <*> ElementOf ma = ElementOf $ \i -> case mf i of
    Searching j f mff -> case ma j of
      ~(Searching k a maa) -> Searching k (f a) $ fmap ($ a) <$> mff
                                              <|> fmap f <$> maa

instance Trustworthy f => Trustworthy (ElementOf f)

instance Gettable f => Gettable (ElementOf f) where
  coerce (ElementOf m) = ElementOf $ \i -> case m i of
    Searching j _ mas -> Searching j (error "coerced while searching") (coerce <$> mas)
