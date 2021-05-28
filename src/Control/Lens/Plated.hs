{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-} -- template-haskell
#endif

#include "lens-common.h"

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Plated
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- The name \"plate\" stems originally from \"boilerplate\", which was the term
-- used by the \"Scrap Your Boilerplate\" papers, and later inherited by Neil
-- Mitchell's \"Uniplate\".
--
-- <https://www.cs.york.ac.uk/fp/darcs/uniplate/uniplate.htm>
--
-- The combinators in here are designed to be compatible with and subsume the
-- @uniplate@ API with the notion of a 'Traversal' replacing
-- a 'Data.Data.Lens.uniplate' or 'Data.Data.Lens.biplate'.
--
-- By implementing these combinators in terms of 'plate' instead of
-- 'Data.Data.Lens.uniplate' additional type safety is gained, as the user is
-- no longer responsible for maintaining invariants such as the number of
-- children they received.
--
-- Note: The @Biplate@ is /deliberately/ excluded from the API here, with the
-- intention that you replace them with either explicit traversals, or by using the
-- @On@ variants of the combinators below with 'Data.Data.Lens.biplate' from
-- @Data.Data.Lens@. As a design, it forced the user into too many situations where
-- they had to choose between correctness and ease of use, and it was brittle in the
-- face of competing imports.
--
-- The sensible use of these combinators makes some simple assumptions.  Notably, any
-- of the @On@ combinators are expecting a 'Traversal', 'Setter' or 'Fold'
-- to play the role of the 'Data.Data.Lens.biplate' combinator, and so when the
-- types of the contents and the container match, they should be the 'id' 'Traversal',
-- 'Setter' or 'Fold'.
--
-- It is often beneficial to use the combinators in this module with the combinators
-- from @Data.Data.Lens@ or @GHC.Generics.Lens@ to make it easier to automatically
-- derive definitions for 'plate', or to derive custom traversals.
-------------------------------------------------------------------------------
module Control.Lens.Plated
  (
  -- * Uniplate
    Plated(..)

  -- * Uniplate Combinators
  , children
  , rewrite, rewriteOf, rewriteOn, rewriteOnOf
  , rewriteM, rewriteMOf, rewriteMOn, rewriteMOnOf
  , universe, universeOf, universeOn, universeOnOf
  , cosmos, cosmosOf, cosmosOn, cosmosOnOf
  , transform, transformOf, transformOn, transformOnOf
  , transformM, transformMOf, transformMOn, transformMOnOf
  , contexts, contextsOf, contextsOn, contextsOnOf
  , holes, holesOn, holesOnOf
  , para, paraOf
  , (...), deep

  -- * Compos
  -- $compos
  , composOpFold

  -- * Parts
  , parts

  -- * Generics
  , gplate
  , gplate1
  , GPlated
  , GPlated1
  )
  where

import Prelude ()

import Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as CoTrans
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Indexed
import Control.Lens.Internal.Context
import Control.Lens.Internal.Prelude
import Control.Lens.Type
import Control.Lens.Setter
import Control.Lens.Traversal
import Control.Monad.Free as Monad
import Control.Monad.Free.Church as Church
import Control.Monad.Trans.Free as Trans
import qualified Language.Haskell.TH as TH
import Data.Data
import Data.Data.Lens
import Data.Tree
import GHC.Generics

-- $setup
-- >>> :set -XDeriveGeneric -XDeriveDataTypeable
-- >>> import Control.Applicative
-- >>> import Data.Data (Data)
-- >>> import GHC.Generics (Generic)
-- >>> import Control.Lens

-- | A 'Plated' type is one where we know how to extract its immediate self-similar children.
--
-- /Example 1/:
--
-- @
-- import Control.Applicative
-- import Control.Lens
-- import Control.Lens.Plated
-- import Data.Data
-- import Data.Data.Lens ('Data.Data.Lens.uniplate')
-- @
--
-- @
-- data Expr
--   = Val 'Int'
--   | Neg Expr
--   | Add Expr Expr
--   deriving ('Eq','Ord','Show','Read','Data')
-- @
--
-- @
-- instance 'Plated' Expr where
--   'plate' f (Neg e) = Neg '<$>' f e
--   'plate' f (Add a b) = Add '<$>' f a '<*>' f b
--   'plate' _ a = 'pure' a
-- @
--
-- /or/
--
-- @
-- instance 'Plated' Expr where
--   'plate' = 'Data.Data.Lens.uniplate'
-- @
--
-- /Example 2/:
--
-- @
-- import Control.Applicative
-- import Control.Lens
-- import Control.Lens.Plated
-- import Data.Data
-- import Data.Data.Lens ('Data.Data.Lens.uniplate')
-- @
--
-- @
-- data Tree a
--   = Bin (Tree a) (Tree a)
--   | Tip a
--   deriving ('Eq','Ord','Show','Read','Data')
-- @
--
-- @
-- instance 'Plated' (Tree a) where
--   'plate' f (Bin l r) = Bin '<$>' f l '<*>' f r
--   'plate' _ t = 'pure' t
-- @
--
-- /or/
--
-- @
-- instance 'Data' a => 'Plated' (Tree a) where
--   'plate' = 'uniplate'
-- @
--
-- Note the big distinction between these two implementations.
--
-- The former will only treat children directly in this tree as descendents,
-- the latter will treat trees contained in the values under the tips also
-- as descendants!
--
-- When in doubt, pick a 'Traversal' and just use the various @...Of@ combinators
-- rather than pollute 'Plated' with orphan instances!
--
-- If you want to find something unplated and non-recursive with 'Data.Data.Lens.biplate'
-- use the @...OnOf@ variant with 'ignored', though those usecases are much better served
-- in most cases by using the existing 'Lens' combinators! e.g.
--
-- @
-- 'toListOf' 'biplate' ≡ 'universeOnOf' 'biplate' 'ignored'
-- @
--
-- This same ability to explicitly pass the 'Traversal' in question is why there is no
-- analogue to uniplate's @Biplate@.
--
-- Moreover, since we can allow custom traversals, we implement reasonable defaults for
-- polymorphic data types, that only 'Control.Traversable.traverse' into themselves, and /not/ their
-- polymorphic arguments.

class Plated a where
  -- | 'Traversal' of the immediate children of this structure.
  --
  -- If you're using GHC 7.2 or newer and your type has a 'Data' instance,
  -- 'plate' will default to 'uniplate' and you can choose to not override
  -- it with your own definition.
  plate :: Traversal' a a
  default plate :: Data a => Traversal' a a
  plate = uniplate

instance Plated [a] where
  plate f (x:xs) = (x:) <$> f xs
  plate _ [] = pure []

instance Traversable f => Plated (Monad.Free f a) where
  plate f (Monad.Free as) = Monad.Free <$> traverse f as
  plate _ x         = pure x

instance (Traversable f, Traversable m) => Plated (Trans.FreeT f m a) where
  plate f (Trans.FreeT xs) = Trans.FreeT <$> traverse (traverse f) xs

instance Traversable f => Plated (Church.F f a) where
  plate f = fmap Church.toF . plate (fmap Church.fromF . f . Church.toF) . Church.fromF

-- -- This one can't work
--
-- instance (Traversable f, Traversable m) => Plated (ChurchT.FT f m a) where
--   plate f = fmap ChurchT.toFT . plate (fmap ChurchT.fromFT . f . ChurchT.toFT) . ChurchT.fromFT

instance (Traversable f, Traversable w) => Plated (CoTrans.CofreeT f w a) where
  plate f (CoTrans.CofreeT xs) = CoTrans.CofreeT <$> traverse (traverse f) xs

instance Traversable f => Plated (Cofree f a) where
  plate f (a :< as) = (:<) a <$> traverse f as

instance Plated (Tree a) where
  plate f (Node a as) = Node a <$> traverse f as

{- Default uniplate instances -}
instance Plated TH.Exp
instance Plated TH.Dec
instance Plated TH.Con
instance Plated TH.Type
instance Plated TH.Stmt
instance Plated TH.Pat


infixr 9 ...
-- | Compose through a plate
(...) :: (Applicative f, Plated c) => LensLike f s t c c -> Over p f c c a b -> Over p f s t a b
l ... m = l . plate . m
{-# INLINE (...) #-}


-- | Try to apply a traversal to all transitive descendants of a 'Plated' container, but
-- do not recurse through matching descendants.
--
-- @
-- 'deep' :: 'Plated' s => 'Fold' s a                 -> 'Fold' s a
-- 'deep' :: 'Plated' s => 'IndexedFold' s a          -> 'IndexedFold' s a
-- 'deep' :: 'Plated' s => 'Traversal' s s a b        -> 'Traversal' s s a b
-- 'deep' :: 'Plated' s => 'IndexedTraversal' s s a b -> 'IndexedTraversal' s s a b
-- @
deep :: (Conjoined p, Applicative f, Plated s) => Traversing p f s s a b -> Over p f s s a b
deep = deepOf plate

-------------------------------------------------------------------------------
-- Children
-------------------------------------------------------------------------------

-- | Extract the immediate descendants of a 'Plated' container.
--
-- @
-- 'children' ≡ 'toListOf' 'plate'
-- @
children :: Plated a => a -> [a]
children = toListOf plate
{-# INLINE children #-}

-------------------------------------------------------------------------------
-- Rewriting
-------------------------------------------------------------------------------

-- | Rewrite by applying a rule everywhere you can. Ensures that the rule cannot
-- be applied anywhere in the result:
--
-- @
-- propRewrite r x = 'all' ('Data.Just.isNothing' '.' r) ('universe' ('rewrite' r x))
-- @
--
-- Usually 'transform' is more appropriate, but 'rewrite' can give better
-- compositionality. Given two single transformations @f@ and @g@, you can
-- construct @\\a -> f a '<|>' g a@ which performs both rewrites until a fixed point.
rewrite :: Plated a => (a -> Maybe a) -> a -> a
rewrite = rewriteOf plate
{-# INLINE rewrite #-}

-- | Rewrite by applying a rule everywhere you can. Ensures that the rule cannot
-- be applied anywhere in the result:
--
-- @
-- propRewriteOf l r x = 'all' ('Data.Just.isNothing' '.' r) ('universeOf' l ('rewriteOf' l r x))
-- @
--
-- Usually 'transformOf' is more appropriate, but 'rewriteOf' can give better
-- compositionality. Given two single transformations @f@ and @g@, you can
-- construct @\\a -> f a '<|>' g a@ which performs both rewrites until a fixed point.
--
-- @
-- 'rewriteOf' :: 'Control.Lens.Iso.Iso'' a a       -> (a -> 'Maybe' a) -> a -> a
-- 'rewriteOf' :: 'Lens'' a a      -> (a -> 'Maybe' a) -> a -> a
-- 'rewriteOf' :: 'Traversal'' a a -> (a -> 'Maybe' a) -> a -> a
-- 'rewriteOf' :: 'Setter'' a a    -> (a -> 'Maybe' a) -> a -> a
-- @
rewriteOf :: ASetter a b a b -> (b -> Maybe a) -> a -> b
rewriteOf l f = go where
  go = transformOf l (\x -> maybe x go (f x))
{-# INLINE rewriteOf #-}

-- | Rewrite recursively over part of a larger structure.
--
-- @
-- 'rewriteOn' :: 'Plated' a => 'Control.Lens.Iso.Iso'' s a       -> (a -> 'Maybe' a) -> s -> s
-- 'rewriteOn' :: 'Plated' a => 'Lens'' s a      -> (a -> 'Maybe' a) -> s -> s
-- 'rewriteOn' :: 'Plated' a => 'Traversal'' s a -> (a -> 'Maybe' a) -> s -> s
-- 'rewriteOn' :: 'Plated' a => 'ASetter'' s a   -> (a -> 'Maybe' a) -> s -> s
-- @
rewriteOn :: Plated a => ASetter s t a a -> (a -> Maybe a) -> s -> t
rewriteOn b = over b . rewrite
{-# INLINE rewriteOn #-}

-- | Rewrite recursively over part of a larger structure using a specified 'Setter'.
--
-- @
-- 'rewriteOnOf' :: 'Control.Lens.Iso.Iso'' s a       -> 'Control.Lens.Iso.Iso'' a a       -> (a -> 'Maybe' a) -> s -> s
-- 'rewriteOnOf' :: 'Lens'' s a      -> 'Lens'' a a      -> (a -> 'Maybe' a) -> s -> s
-- 'rewriteOnOf' :: 'Traversal'' s a -> 'Traversal'' a a -> (a -> 'Maybe' a) -> s -> s
-- 'rewriteOnOf' :: 'Setter'' s a    -> 'Setter'' a a    -> (a -> 'Maybe' a) -> s -> s
-- @
rewriteOnOf :: ASetter s t a b -> ASetter a b a b -> (b -> Maybe a) -> s -> t
rewriteOnOf b l = over b . rewriteOf l
{-# INLINE rewriteOnOf #-}

-- | Rewrite by applying a monadic rule everywhere you can. Ensures that the rule cannot
-- be applied anywhere in the result.
rewriteM :: (Monad m, Plated a) => (a -> m (Maybe a)) -> a -> m a
rewriteM = rewriteMOf plate
{-# INLINE rewriteM #-}

-- | Rewrite by applying a monadic rule everywhere you recursing with a user-specified 'Traversal'.
-- Ensures that the rule cannot be applied anywhere in the result.
rewriteMOf :: Monad m => LensLike (WrappedMonad m) a b a b -> (b -> m (Maybe a)) -> a -> m b
rewriteMOf l f = go where
  go = transformMOf l (\x -> f x >>= maybe (return x) go)
{-# INLINE rewriteMOf #-}

-- | Rewrite by applying a monadic rule everywhere inside of a structure located by a user-specified 'Traversal'.
-- Ensures that the rule cannot be applied anywhere in the result.
rewriteMOn :: (Monad m, Plated a) => LensLike (WrappedMonad m) s t a a -> (a -> m (Maybe a)) -> s -> m t
rewriteMOn b = mapMOf b . rewriteM
{-# INLINE rewriteMOn #-}

-- | Rewrite by applying a monadic rule everywhere inside of a structure located by a user-specified 'Traversal',
-- using a user-specified 'Traversal' for recursion. Ensures that the rule cannot be applied anywhere in the result.
rewriteMOnOf :: Monad m => LensLike (WrappedMonad m) s t a b -> LensLike (WrappedMonad m) a b a b -> (b -> m (Maybe a)) -> s -> m t
rewriteMOnOf b l = mapMOf b . rewriteMOf l
{-# INLINE rewriteMOnOf #-}

-------------------------------------------------------------------------------
-- Universe
-------------------------------------------------------------------------------

-- | Retrieve all of the transitive descendants of a 'Plated' container, including itself.
universe :: Plated a => a -> [a]
universe = universeOf plate
{-# INLINE universe #-}

-- | Given a 'Fold' that knows how to locate immediate children, retrieve all of the transitive descendants of a node, including itself.
--
-- @
-- 'universeOf' :: 'Fold' a a -> a -> [a]
-- @
universeOf :: Getting [a] a a -> a -> [a]
universeOf l = go where
  go a = a : foldMapOf l go a
{-# INLINE universeOf #-}

-- | Given a 'Fold' that knows how to find 'Plated' parts of a container retrieve them and all of their descendants, recursively.
universeOn ::  Plated a => Getting [a] s a -> s -> [a]
universeOn b = universeOnOf b plate
{-# INLINE universeOn #-}

-- | Given a 'Fold' that knows how to locate immediate children, retrieve all of the transitive descendants of a node, including itself that lie
-- in a region indicated by another 'Fold'.
--
-- @
-- 'toListOf' l ≡ 'universeOnOf' l 'ignored'
-- @
universeOnOf :: Getting [a] s a -> Getting [a] a a -> s -> [a]
universeOnOf b = foldMapOf b . universeOf
{-# INLINE universeOnOf #-}

-- | Fold over all transitive descendants of a 'Plated' container, including itself.
cosmos :: Plated a => Fold a a
cosmos = cosmosOf plate
{-# INLINE cosmos #-}

-- | Given a 'Fold' that knows how to locate immediate children, fold all of the transitive descendants of a node, including itself.
--
-- @
-- 'cosmosOf' :: 'Fold' a a -> 'Fold' a a
-- @
cosmosOf :: (Applicative f, Contravariant f) => LensLike' f a a -> LensLike' f a a
-- The 'Contravariant' constraint isn't required for the implementation. Since any 'Traversal' produced with 'cosmosOf' is more likely than
-- not to be broken, the additional constraint serves to restrict 'cosmosOf' to 'Fold's.
cosmosOf d f s = f s *> d (cosmosOf d f) s
{-# INLINE cosmosOf #-}

-- | Given a 'Fold' that knows how to find 'Plated' parts of a container fold them and all of their descendants, recursively.
--
-- @
-- 'cosmosOn' :: 'Plated' a => 'Fold' s a -> 'Fold' s a
-- @
cosmosOn :: (Applicative f, Contravariant f, Plated a) => LensLike' f s a -> LensLike' f s a
cosmosOn d = cosmosOnOf d plate
{-# INLINE cosmosOn #-}

-- | Given a 'Fold' that knows how to locate immediate children, fold all of the transitive descendants of a node, including itself that lie
-- in a region indicated by another 'Fold'.
--
-- @
-- 'cosmosOnOf' :: 'Fold' s a -> 'Fold' a a -> 'Fold' s a
-- @
cosmosOnOf :: (Applicative f, Contravariant f) => LensLike' f s a -> LensLike' f a a -> LensLike' f s a
cosmosOnOf d p = d . cosmosOf p
{-# INLINE cosmosOnOf #-}

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

-- | Transform every element in the tree, in a bottom-up manner.
--
-- For example, replacing negative literals with literals:
--
-- @
-- negLits = 'transform' $ \\x -> case x of
--   Neg (Lit i) -> Lit ('negate' i)
--   _           -> x
-- @
transform :: Plated a => (a -> a) -> a -> a
transform = transformOf plate
{-# INLINE transform #-}

-- | Transform every element in the tree in a bottom-up manner over a region indicated by a 'Setter'.
--
-- @
-- 'transformOn' :: 'Plated' a => 'Traversal'' s a -> (a -> a) -> s -> s
-- 'transformOn' :: 'Plated' a => 'Setter'' s a    -> (a -> a) -> s -> s
-- @
transformOn :: Plated a => ASetter s t a a -> (a -> a) -> s -> t
transformOn b = over b . transform
{-# INLINE transformOn #-}

-- | Transform every element by recursively applying a given 'Setter' in a bottom-up manner.
--
-- @
-- 'transformOf' :: 'Traversal'' a a -> (a -> a) -> a -> a
-- 'transformOf' :: 'Setter'' a a    -> (a -> a) -> a -> a
-- @
transformOf :: ASetter a b a b -> (b -> b) -> a -> b
transformOf l f = go where
  go = f . over l go
{-# INLINE transformOf #-}

-- | Transform every element in a region indicated by a 'Setter' by recursively applying another 'Setter'
-- in a bottom-up manner.
--
-- @
-- 'transformOnOf' :: 'Setter'' s a -> 'Traversal'' a a -> (a -> a) -> s -> s
-- 'transformOnOf' :: 'Setter'' s a -> 'Setter'' a a    -> (a -> a) -> s -> s
-- @
transformOnOf :: ASetter s t a b -> ASetter a b a b -> (b -> b) -> s -> t
transformOnOf b l = over b . transformOf l
{-# INLINE transformOnOf #-}

-- | Transform every element in the tree, in a bottom-up manner, monadically.
transformM :: (Monad m, Plated a) => (a -> m a) -> a -> m a
transformM = transformMOf plate
{-# INLINE transformM #-}

-- | Transform every element in the tree in a region indicated by a supplied 'Traversal', in a bottom-up manner, monadically.
--
-- @
-- 'transformMOn' :: ('Monad' m, 'Plated' a) => 'Traversal'' s a -> (a -> m a) -> s -> m s
-- @
transformMOn :: (Monad m, Plated a) => LensLike (WrappedMonad m) s t a a -> (a -> m a) -> s -> m t
transformMOn b = mapMOf b . transformM
{-# INLINE transformMOn #-}

-- | Transform every element in a tree using a user supplied 'Traversal' in a bottom-up manner with a monadic effect.
--
-- @
-- 'transformMOf' :: 'Monad' m => 'Traversal'' a a -> (a -> m a) -> a -> m a
-- @
transformMOf :: Monad m => LensLike (WrappedMonad m) a b a b -> (b -> m b) -> a -> m b
transformMOf l f = go where
  go t = mapMOf l go t >>= f
{-# INLINE transformMOf #-}

-- | Transform every element in a tree that lies in a region indicated by a supplied 'Traversal', walking with a user supplied 'Traversal' in
-- a bottom-up manner with a monadic effect.
--
-- @
-- 'transformMOnOf' :: 'Monad' m => 'Traversal'' s a -> 'Traversal'' a a -> (a -> m a) -> s -> m s
-- @
transformMOnOf :: Monad m => LensLike (WrappedMonad m) s t a b -> LensLike (WrappedMonad m) a b a b -> (b -> m b) -> s -> m t
transformMOnOf b l = mapMOf b . transformMOf l
{-# INLINE transformMOnOf #-}

-------------------------------------------------------------------------------
-- Holes and Contexts
-------------------------------------------------------------------------------

-- | Return a list of all of the editable contexts for every location in the structure, recursively.
--
-- @
-- propUniverse x = 'universe' x '==' 'map' 'Control.Comonad.Store.Class.pos' ('contexts' x)
-- propId x = 'all' ('==' x) ['Control.Lens.Internal.Context.extract' w | w <- 'contexts' x]
-- @
--
-- @
-- 'contexts' ≡ 'contextsOf' 'plate'
-- @
contexts :: Plated a => a -> [Context a a a]
contexts = contextsOf plate
{-# INLINE contexts #-}

-- | Return a list of all of the editable contexts for every location in the structure, recursively, using a user-specified 'Traversal' to walk each layer.
--
-- @
-- propUniverse l x = 'universeOf' l x '==' 'map' 'Control.Comonad.Store.Class.pos' ('contextsOf' l x)
-- propId l x = 'all' ('==' x) ['Control.Lens.Internal.Context.extract' w | w <- 'contextsOf' l x]
-- @
--
-- @
-- 'contextsOf' :: 'Traversal'' a a -> a -> ['Context' a a a]
-- @
contextsOf :: ATraversal' a a -> a -> [Context a a a]
contextsOf l x = sell x : f (map context (holesOf l x)) where
  f xs = do
    Context ctx child <- xs
    Context cont y <- contextsOf l child
    return $ Context (ctx . cont) y
{-# INLINE contextsOf #-}

-- | Return a list of all of the editable contexts for every location in the structure in an areas indicated by a user supplied 'Traversal', recursively using 'plate'.
--
-- @
-- 'contextsOn' b ≡ 'contextsOnOf' b 'plate'
-- @
--
-- @
-- 'contextsOn' :: 'Plated' a => 'Traversal'' s a -> s -> ['Context' a a s]
-- @
contextsOn :: Plated a => ATraversal s t a a -> s -> [Context a a t]
contextsOn b = contextsOnOf b plate
{-# INLINE contextsOn #-}

-- | Return a list of all of the editable contexts for every location in the structure in an areas indicated by a user supplied 'Traversal', recursively using
-- another user-supplied 'Traversal' to walk each layer.
--
-- @
-- 'contextsOnOf' :: 'Traversal'' s a -> 'Traversal'' a a -> s -> ['Context' a a s]
-- @
contextsOnOf :: ATraversal s t a a -> ATraversal' a a -> s -> [Context a a t]
contextsOnOf b l = f . map context . holesOf b where
  f xs = do
    Context ctx child <- xs
    Context cont y <- contextsOf l child
    return $ Context (ctx . cont) y
{-# INLINE contextsOnOf #-}

-- | The one-level version of 'context'. This extracts a list of the immediate children as editable contexts.
--
-- Given a context you can use 'Control.Comonad.Store.Class.pos' to see the values, 'Control.Comonad.Store.Class.peek' at what the structure would be like with an edited result, or simply 'Control.Lens.Internal.Context.extract' the original structure.
--
-- @
-- propChildren x = 'children' l x '==' 'map' 'Control.Comonad.Store.Class.pos' ('holes' l x)
-- propId x = 'all' ('==' x) ['Control.Lens.Internal.Context.extract' w | w <- 'holes' l x]
-- @
--
-- @
-- 'holes' = 'holesOf' 'plate'
-- @
holes :: Plated a => a -> [Pretext (->) a a a]
holes = holesOf plate
{-# INLINE holes #-}

-- | An alias for 'holesOf', provided for consistency with the other combinators.
--
-- @
-- 'holesOn' ≡ 'holesOf'
-- @
--
-- @
-- 'holesOn' :: 'Iso'' s a                -> s -> ['Pretext' (->) a a s]
-- 'holesOn' :: 'Lens'' s a               -> s -> ['Pretext' (->) a a s]
-- 'holesOn' :: 'Traversal'' s a          -> s -> ['Pretext' (->) a a s]
-- 'holesOn' :: 'IndexedLens'' i s a      -> s -> ['Pretext' ('Control.Lens.Internal.Indexed.Indexed' i) a a s]
-- 'holesOn' :: 'IndexedTraversal'' i s a -> s -> ['Pretext' ('Control.Lens.Internal.Indexed.Indexed' i) a a s]
-- @
holesOn :: Conjoined p => Over p (Bazaar p a a) s t a a -> s -> [Pretext p a a t]
holesOn = holesOf
{-# INLINE holesOn #-}

-- | Extract one level of 'holes' from a container in a region specified by one 'Traversal', using another.
--
-- @
-- 'holesOnOf' b l ≡ 'holesOf' (b '.' l)
-- @
--
-- @
-- 'holesOnOf' :: 'Iso'' s a       -> 'Iso'' a a                -> s -> ['Pretext' (->) a a s]
-- 'holesOnOf' :: 'Lens'' s a      -> 'Lens'' a a               -> s -> ['Pretext' (->) a a s]
-- 'holesOnOf' :: 'Traversal'' s a -> 'Traversal'' a a          -> s -> ['Pretext' (->) a a s]
-- 'holesOnOf' :: 'Lens'' s a      -> 'IndexedLens'' i a a      -> s -> ['Pretext' ('Control.Lens.Internal.Indexed.Indexed' i) a a s]
-- 'holesOnOf' :: 'Traversal'' s a -> 'IndexedTraversal'' i a a -> s -> ['Pretext' ('Control.Lens.Internal.Indexed.Indexed' i) a a s]
-- @
holesOnOf :: Conjoined p
          => LensLike (Bazaar p  r r) s t a b
          -> Over p (Bazaar p r r) a b r r
          -> s -> [Pretext p r r t]
holesOnOf b l = holesOf (b . l)
{-# INLINE holesOnOf #-}

-------------------------------------------------------------------------------
-- Paramorphisms
-------------------------------------------------------------------------------

-- | Perform a fold-like computation on each value, technically a paramorphism.
--
-- @
-- 'paraOf' :: 'Fold' a a -> (a -> [r] -> r) -> a -> r
-- @
paraOf :: Getting (Endo [a]) a a -> (a -> [r] -> r) -> a -> r
paraOf l f = go where
  go a = f a (go <$> toListOf l a)
{-# INLINE paraOf #-}

-- | Perform a fold-like computation on each value, technically a paramorphism.
--
-- @
-- 'para' ≡ 'paraOf' 'plate'
-- @
para :: Plated a => (a -> [r] -> r) -> a -> r
para = paraOf plate
{-# INLINE para #-}

-------------------------------------------------------------------------------
-- Compos
-------------------------------------------------------------------------------

-- $compos
--
-- Provided for compatibility with Björn Bringert's @compos@ library.
--
-- Note: Other operations from compos that were inherited by @uniplate@ are /not/ included
-- to avoid having even more redundant names for the same operators. For comparison:
--
-- @
-- 'composOpMonoid' ≡ 'foldMapOf' 'plate'
-- 'composOpMPlus' f ≡ 'msumOf' ('plate' '.' 'to' f)
-- 'composOp' ≡ 'descend' ≡ 'over' 'plate'
-- 'composOpM' ≡ 'descendM' ≡ 'mapMOf' 'plate'
-- 'composOpM_' ≡ 'descendM_' ≡ 'mapMOf_' 'plate'
-- @

-- | Fold the immediate children of a 'Plated' container.
--
-- @
-- 'composOpFold' z c f = 'foldrOf' 'plate' (c '.' f) z
-- @
composOpFold :: Plated a => b -> (b -> b -> b) -> (a -> b) -> a -> b
composOpFold z c f = foldrOf plate (c . f) z
{-# INLINE composOpFold #-}

-------------------------------------------------------------------------------
-- Parts
-------------------------------------------------------------------------------

-- | The original @uniplate@ combinator, implemented in terms of 'Plated' as a 'Lens'.
--
-- @
-- 'parts' ≡ 'partsOf' 'plate'
-- @
--
-- The resulting 'Lens' is safer to use as it ignores 'over-application' and deals gracefully with under-application,
-- but it is only a proper 'Lens' if you don't change the list 'length'!
parts :: Plated a => Lens' a [a]
parts = partsOf plate
{-# INLINE parts #-}

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

-- | Implement 'plate' operation for a type using its 'Generic' instance.
--
-- Note: the behavior may be different than with 'uniplate' in some special cases.
-- 'gplate' doesn't look through other types in a group of mutually
-- recursive types.
--
-- For example consider mutually recursive even and odd natural numbers:
--
-- >>> data Even = Z | E Odd deriving (Show, Generic, Data); data Odd = O Even deriving (Show, Generic, Data)
--
-- Then 'uniplate', which is based on `Data`, finds
-- all even numbers less or equal than four:
--
-- >>> import Data.Data.Lens (uniplate)
-- >>> universeOf uniplate (E (O (E (O Z))))
-- [E (O (E (O Z))),E (O Z),Z]
--
-- but 'gplate' doesn't see through @Odd@.
--
-- >>> universeOf gplate (E (O (E (O Z))))
-- [E (O (E (O Z)))]
--
-- If using 'Data' is not an option, you can still write the traversal manually.
-- It is sometimes useful to use helper traversals
--
-- >>> :{
-- let oddeven :: Traversal' Odd Even
--     oddeven f (O n) = O <$> f n
--     evenplate :: Traversal' Even Even
--     evenplate f Z     = pure Z
--     evenplate f (E n) = E <$> oddeven f n
-- :}
--
-- >>> universeOf evenplate (E (O (E (O Z))))
-- [E (O (E (O Z))),E (O Z),Z]
--
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
