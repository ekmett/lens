{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Bazaar
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Bazaar
  ( Bizarre(..)
  , Bazaar(..), Bazaar'
  , BazaarT(..), BazaarT'
  , Bizarre1(..)
  , Bazaar1(..), Bazaar1'
  , BazaarT1(..), BazaarT1'
  ) where

import Prelude ()

import Control.Arrow as Arrow
import qualified Control.Category as C
import Control.Comonad
import Control.Lens.Internal.Prelude
import Control.Lens.Internal.Context
import Control.Lens.Internal.Indexed
import Data.Functor.Apply
import Data.Profunctor.Rep

------------------------------------------------------------------------------
-- Bizarre
------------------------------------------------------------------------------

-- | This class is used to run the various 'Bazaar' variants used in this
-- library.
class Profunctor p => Bizarre p w | w -> p where
  bazaar :: Applicative f => p a (f b) -> w a b t -> f t

------------------------------------------------------------------------------
-- Bazaar
------------------------------------------------------------------------------

-- | This is used to characterize a 'Control.Lens.Traversal.Traversal'.
--
-- a.k.a. indexed Cartesian store comonad, indexed Kleene store comonad, or an indexed 'FunList'.
--
-- <http://twanvl.nl/blog/haskell/non-regular1>
--
-- A 'Bazaar' is like a 'Control.Lens.Traversal.Traversal' that has already been applied to some structure.
--
-- Where a @'Context' a b t@ holds an @a@ and a function from @b@ to
-- @t@, a @'Bazaar' a b t@ holds @N@ @a@s and a function from @N@
-- @b@s to @t@, (where @N@ might be infinite).
--
-- Mnemonically, a 'Bazaar' holds many stores and you can easily add more.
--
-- This is a final encoding of 'Bazaar'.
newtype Bazaar p a b t = Bazaar { runBazaar :: forall f. Applicative f => p a (f b) -> f t }
-- type role Bazaar representatonal nominal nominal nominal

-- | This alias is helpful when it comes to reducing repetition in type signatures.
--
-- @
-- type 'Bazaar'' p a t = 'Bazaar' p a a t
-- @
type Bazaar' p a = Bazaar p a a

instance IndexedFunctor (Bazaar p) where
  ifmap f (Bazaar k) = Bazaar (fmap f . k)
  {-# INLINE ifmap #-}

instance Conjoined p => IndexedComonad (Bazaar p) where
  iextract (Bazaar m) = runIdentity $ m (arr Identity)
  {-# INLINE iextract #-}
  iduplicate (Bazaar m) = getCompose $ m (Compose #. distrib sell C.. sell)
  {-# INLINE iduplicate #-}

instance Corepresentable p => Sellable p (Bazaar p) where
  sell = cotabulate $ \ w -> Bazaar $ tabulate $ \k -> pure (cosieve k w)
  {-# INLINE sell #-}

instance Profunctor p => Bizarre p (Bazaar p) where
  bazaar g (Bazaar f) = f g
  {-# INLINE bazaar #-}

instance Functor (Bazaar p a b) where
  fmap = ifmap
  {-# INLINE fmap #-}
  x <$ Bazaar k = Bazaar ( (x <$) . k )
  {-# INLINE (<$) #-}

instance Apply (Bazaar p a b) where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}
  (.>) = (*>)
  {-# INLINE (.>) #-}
  (<.) = (<*)
  {-# INLINE (<.) #-}

instance Applicative (Bazaar p a b) where
  pure a = Bazaar $ \_ -> pure a
  {-# INLINE pure #-}
  Bazaar mf <*> Bazaar ma = Bazaar $ \ pafb -> mf pafb <*> ma pafb
  {-# INLINE (<*>) #-}
#if MIN_VERSION_base(4,10,0)
  liftA2 f (Bazaar mx) (Bazaar my) = Bazaar $ \pafb -> liftA2 f (mx pafb) (my pafb)
  {-# INLINE liftA2 #-}
#endif
  Bazaar mx *> Bazaar my = Bazaar $ \pafb -> mx pafb *> my pafb
  {-# INLINE (*>) #-}
  Bazaar mx <* Bazaar my = Bazaar $ \pafb -> mx pafb <* my pafb
  {-# INLINE (<*) #-}

instance (a ~ b, Conjoined p) => Comonad (Bazaar p a b) where
  extract = iextract
  {-# INLINE extract #-}
  duplicate = iduplicate
  {-# INLINE duplicate #-}

instance (a ~ b, Conjoined p) => ComonadApply (Bazaar p a b) where
  (<@>) = (<*>)
  {-# INLINE (<@>) #-}
  (@>) = (*>)
  {-# INLINE (@>) #-}
  (<@) = (<*)
  {-# INLINE (<@) #-}

------------------------------------------------------------------------------
-- BazaarT
------------------------------------------------------------------------------

-- | 'BazaarT' is like 'Bazaar', except that it provides a questionable 'Contravariant' instance
-- To protect this instance it relies on the soundness of another 'Contravariant' type, and usage conventions.
--
-- For example. This lets us write a suitably polymorphic and lazy 'Control.Lens.Traversal.taking', but there
-- must be a better way!
newtype BazaarT p (g :: * -> *) a b t = BazaarT { runBazaarT :: forall f. Applicative f => p a (f b) -> f t }
type role BazaarT representational nominal nominal nominal nominal

-- | This alias is helpful when it comes to reducing repetition in type signatures.
--
-- @
-- type 'BazaarT'' p g a t = 'BazaarT' p g a a t
-- @
type BazaarT' p g a = BazaarT p g a a

instance IndexedFunctor (BazaarT p g) where
  ifmap f (BazaarT k) = BazaarT (fmap f . k)
  {-# INLINE ifmap #-}

instance Conjoined p => IndexedComonad (BazaarT p g) where
  iextract (BazaarT m) = runIdentity $ m (arr Identity)
  {-# INLINE iextract #-}
  iduplicate (BazaarT m) = getCompose $ m (Compose #. distrib sell C.. sell)
  {-# INLINE iduplicate #-}

instance Corepresentable p => Sellable p (BazaarT p g) where
  sell = cotabulate $ \ w -> BazaarT (`cosieve` w)
  {-# INLINE sell #-}

instance Profunctor p => Bizarre p (BazaarT p g) where
  bazaar g (BazaarT f) = f g
  {-# INLINE bazaar #-}

instance Functor (BazaarT p g a b) where
  fmap = ifmap
  {-# INLINE fmap #-}
  x <$ BazaarT k = BazaarT ( (x <$) . k )
  {-# INLINE (<$) #-}

instance Apply (BazaarT p g a b) where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}
  (.>) = (*>)
  {-# INLINE (.>) #-}
  (<.) = (<*)
  {-# INLINE (<.) #-}

instance Applicative (BazaarT p g a b) where
  pure a = BazaarT $ tabulate $ \_ -> pure (pure a)
  {-# INLINE pure #-}
  BazaarT mf <*> BazaarT ma = BazaarT $ \ pafb -> mf pafb <*> ma pafb
  {-# INLINE (<*>) #-}
#if MIN_VERSION_base(4,10,0)
  liftA2 f (BazaarT mx) (BazaarT my) = BazaarT $ \pafb -> liftA2 f (mx pafb) (my pafb)
  {-# INLINE liftA2 #-}
#endif
  BazaarT mf *> BazaarT ma = BazaarT $ \ pafb -> mf pafb *> ma pafb
  {-# INLINE (*>) #-}
  BazaarT mf <* BazaarT ma = BazaarT $ \ pafb -> mf pafb <* ma pafb
  {-# INLINE (<*) #-}

instance (a ~ b, Conjoined p) => Comonad (BazaarT p g a b) where
  extract = iextract
  {-# INLINE extract #-}
  duplicate = iduplicate
  {-# INLINE duplicate #-}

instance (a ~ b, Conjoined p) => ComonadApply (BazaarT p g a b) where
  (<@>) = (<*>)
  {-# INLINE (<@>) #-}
  (@>) = (*>)
  {-# INLINE (@>) #-}
  (<@) = (<*)
  {-# INLINE (<@) #-}

instance (Profunctor p, Contravariant g) => Contravariant (BazaarT p g a b) where
  contramap _ = (<$) (error "contramap: BazaarT")
  {-# INLINE contramap #-}

instance Contravariant g => Semigroup (BazaarT p g a b t) where
  BazaarT a <> BazaarT b = BazaarT $ \f -> a f <* b f
  {-# INLINE (<>) #-}

instance Contravariant g => Monoid (BazaarT p g a b t) where
  mempty = BazaarT $ \_ -> pure (error "mempty: BazaarT")
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  BazaarT a `mappend` BazaarT b = BazaarT $ \f -> a f <* b f
  {-# INLINE mappend #-}
#endif


------------------------------------------------------------------------------
-- Bizarre1
------------------------------------------------------------------------------

class Profunctor p => Bizarre1 p w | w -> p where
  bazaar1 :: Apply f => p a (f b) -> w a b t -> f t

------------------------------------------------------------------------------
-- Bazaar1
------------------------------------------------------------------------------

-- | This is used to characterize a 'Control.Lens.Traversal.Traversal'.
--
-- a.k.a. indexed Cartesian store comonad, indexed Kleene store comonad, or an indexed 'FunList'.
--
-- <http://twanvl.nl/blog/haskell/non-regular1>
--
-- A 'Bazaar1' is like a 'Control.Lens.Traversal.Traversal' that has already been applied to some structure.
--
-- Where a @'Context' a b t@ holds an @a@ and a function from @b@ to
-- @t@, a @'Bazaar1' a b t@ holds @N@ @a@s and a function from @N@
-- @b@s to @t@, (where @N@ might be infinite).
--
-- Mnemonically, a 'Bazaar1' holds many stores and you can easily add more.
--
-- This is a final encoding of 'Bazaar1'.
newtype Bazaar1 p a b t = Bazaar1 { runBazaar1 :: forall f. Apply f => p a (f b) -> f t }
-- type role Bazaar1 representatonal nominal nominal nominal

-- | This alias is helpful when it comes to reducing repetition in type signatures.
--
-- @
-- type 'Bazaar1'' p a t = 'Bazaar1' p a a t
-- @
type Bazaar1' p a = Bazaar1 p a a

instance IndexedFunctor (Bazaar1 p) where
  ifmap f (Bazaar1 k) = Bazaar1 (fmap f . k)
  {-# INLINE ifmap #-}

instance Conjoined p => IndexedComonad (Bazaar1 p) where
  iextract (Bazaar1 m) = runIdentity $ m (arr Identity)
  {-# INLINE iextract #-}
  iduplicate (Bazaar1 m) = getCompose $ m (Compose #. distrib sell C.. sell)
  {-# INLINE iduplicate #-}

instance Corepresentable p => Sellable p (Bazaar1 p) where
  sell = cotabulate $ \ w -> Bazaar1 $ tabulate $ \k -> pure (cosieve k w)
  {-# INLINE sell #-}

instance Profunctor p => Bizarre1 p (Bazaar1 p) where
  bazaar1 g (Bazaar1 f) = f g
  {-# INLINE bazaar1 #-}

instance Functor (Bazaar1 p a b) where
  fmap = ifmap
  {-# INLINE fmap #-}
  x <$ Bazaar1 k = Bazaar1 ((x <$) . k)
  {-# INLINE (<$) #-}

instance Apply (Bazaar1 p a b) where
  Bazaar1 mf <.> Bazaar1 ma = Bazaar1 $ \ pafb -> mf pafb <.> ma pafb
  {-# INLINE (<.>) #-}
  Bazaar1 mf .> Bazaar1 ma = Bazaar1 $ \ pafb -> mf pafb .> ma pafb
  {-# INLINE (.>) #-}
  Bazaar1 mf <. Bazaar1 ma = Bazaar1 $ \ pafb -> mf pafb <. ma pafb
  {-# INLINE (<.) #-}

instance (a ~ b, Conjoined p) => Comonad (Bazaar1 p a b) where
  extract = iextract
  {-# INLINE extract #-}
  duplicate = iduplicate
  {-# INLINE duplicate #-}

instance (a ~ b, Conjoined p) => ComonadApply (Bazaar1 p a b) where
  (<@>) = (<.>)
  {-# INLINE (<@>) #-}
  (@>) = (.>)
  {-# INLINE (@>) #-}
  (<@) = (<.)
  {-# INLINE (<@) #-}

------------------------------------------------------------------------------
-- BazaarT1
------------------------------------------------------------------------------

-- | 'BazaarT1' is like 'Bazaar1', except that it provides a questionable 'Contravariant' instance
-- To protect this instance it relies on the soundness of another 'Contravariant' type, and usage conventions.
--
-- For example. This lets us write a suitably polymorphic and lazy 'Control.Lens.Traversal.taking', but there
-- must be a better way!
newtype BazaarT1 p (g :: * -> *) a b t = BazaarT1 { runBazaarT1 :: forall f. Apply f => p a (f b) -> f t }
type role BazaarT1 representational nominal nominal nominal nominal

-- | This alias is helpful when it comes to reducing repetition in type signatures.
--
-- @
-- type 'BazaarT1'' p g a t = 'BazaarT1' p g a a t
-- @
type BazaarT1' p g a = BazaarT1 p g a a

instance IndexedFunctor (BazaarT1 p g) where
  ifmap f (BazaarT1 k) = BazaarT1 (fmap f . k)
  {-# INLINE ifmap #-}

instance Conjoined p => IndexedComonad (BazaarT1 p g) where
  iextract (BazaarT1 m) = runIdentity $ m (arr Identity)
  {-# INLINE iextract #-}
  iduplicate (BazaarT1 m) = getCompose $ m (Compose #. distrib sell C.. sell)
  {-# INLINE iduplicate #-}

instance Corepresentable p => Sellable p (BazaarT1 p g) where
  sell = cotabulate $ \ w -> BazaarT1 (`cosieve` w)
  {-# INLINE sell #-}

instance Profunctor p => Bizarre1 p (BazaarT1 p g) where
  bazaar1 g (BazaarT1 f) = f g
  {-# INLINE bazaar1 #-}

instance Functor (BazaarT1 p g a b) where
  fmap = ifmap
  {-# INLINE fmap #-}
  x <$ BazaarT1 k = BazaarT1 ((x <$) . k)
  {-# INLINE (<$) #-}

instance Apply (BazaarT1 p g a b) where
  BazaarT1 mf <.> BazaarT1 ma = BazaarT1 $ \ pafb -> mf pafb <.> ma pafb
  {-# INLINE (<.>) #-}
  BazaarT1 mf .> BazaarT1 ma = BazaarT1 $ \ pafb -> mf pafb .> ma pafb
  {-# INLINE (.>) #-}
  BazaarT1 mf <. BazaarT1 ma = BazaarT1 $ \ pafb -> mf pafb <. ma pafb
  {-# INLINE (<.) #-}

instance (a ~ b, Conjoined p) => Comonad (BazaarT1 p g a b) where
  extract = iextract
  {-# INLINE extract #-}
  duplicate = iduplicate
  {-# INLINE duplicate #-}

instance (a ~ b, Conjoined p) => ComonadApply (BazaarT1 p g a b) where
  (<@>) = (<.>)
  {-# INLINE (<@>) #-}
  (@>) = (.>)
  {-# INLINE (@>) #-}
  (<@) = (<.)
  {-# INLINE (<@) #-}

instance (Profunctor p, Contravariant g) => Contravariant (BazaarT1 p g a b) where
  contramap _ = (<$) (error "contramap: BazaarT1")
  {-# INLINE contramap #-}

instance Contravariant g => Semigroup (BazaarT1 p g a b t) where
  BazaarT1 a <> BazaarT1 b = BazaarT1 $ \f -> a f <. b f
  {-# INLINE (<>) #-}
