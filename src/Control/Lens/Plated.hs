{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Plated
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- The name \"plate\" stems originally from \"boilerplate\", which was the term
-- used by the \"Scrap Your Boilerplate\" papers, and later inherited by Neil
-- Mitchell's \"Uniplate\".
--
-- <http://community.haskell.org/~ndm/uniplate/>
--
-- The combinators in here are designed to be compatible with and subsume the
-- 'uniplate' API with the notion of a 'Traversal' replacing a uniplate or
-- biplate.
--
-- By implementing these combinators in terms of 'plate' instead of 'uniplate'
-- additional type safety is gained, as the user is no longer responsible for
-- maintaining invariants such as the number of children he received.
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
  , transform, transformOf, transformOn, transformOnOf
  , transformM, transformMOf, transformMOn, transformMOnOf
  , contexts, contextsOf, contextsOn, contextsOnOf
  , holes, holesOf, holesOn, holesOnOf
  , para, paraOf

  -- * Compos
  -- $compos
  , composOpFold

  -- * Indexing into a Traversal
  , element
  , elementOf
  , indexed

  -- * Parts
  , parts
  , partsOf

  -- ** Unsafe Operations
  , unsafePartsOf
  )
  where

import Control.Applicative
import Control.Monad.State
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Indexed
import Control.Lens.IndexedLens
import Control.Lens.Internal
import Control.Lens.Setter
import Control.Lens.Traversal
import Control.Lens.Type
import Data.Tree
import Data.Data
import Data.Data.Lens
import Data.Traversable (sequenceA)

-- | A 'Plated' type is one where we know how to extract its immediate self-similar children.
--
-- /Example 1/:
--
-- @
-- import Control.Applicative
-- import Control.Lens
-- import Control.Plated
-- import Data.Data
-- import Data.Data.Lens ('Data.Data.Lens.uniplate')
-- @
--
-- @
-- data Expr
--   = Val 'Int'
--   | Neg Expr
--   | Add Expr Expr
--   deriving ('Eq','Ord','Show','Read','Data','Typeable')
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
-- import Control.Plated
-- import Data.Data
-- import Data.Data.Lens ('Data.Data.Lens.uniplate')
-- @
--
-- @
-- data Tree a
--   = Bin (Tree a) (Tree a)
--   | Tip a
--   deriving ('Eq','Ord','Show','Read','Data','Typeable')
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
-- in most cases by using the existing lens combinators! e.g.
--
-- @'toListOf' 'biplate' ≡ 'universeOnOf' 'biplate' 'ignored'@.
--
-- This same ability to explicitly pass the 'Traversal' in question is why there is no
-- analogue to uniplate's @Biplate@.
--
-- Moreover, since we can allow custom traversals, we implement reasonable defaults for
-- polymorphic data types, that only traverse into themselves, and /not/ their
-- polymorphic arguments.

class Plated a where
  -- | 'Traversal' of the immediate children of this structure.
  --
  -- The default definition finds no children.
  plate :: Simple Traversal a a
  default plate :: Data a => Simple Traversal a a
  plate = uniplate

instance Plated [a] where
  plate f (x:xs) = (x:) <$> f xs
  plate _ [] = pure []

instance Plated (Tree a) where
  plate f (Node a as) = Node a <$> traverse f as

-------------------------------------------------------------------------------
-- Children
-------------------------------------------------------------------------------

-- | Extract the immediate descendants of a 'Plated' container.
--
-- @'children' ≡ 'toListOf' 'plate'@
children :: Plated a => a -> [a]
children = toListOf plate
{-# INLINE children #-}

-------------------------------------------------------------------------------
-- Rewriting
-------------------------------------------------------------------------------

-- | Rewrite by applying a rule everywhere you can. Ensures that the rule cannot
-- be applied anywhere in the result:
--
-- @propRewrite r x = 'all' ('Data.Just.isNothing' . r) ('universe' ('rewrite' r x))@
--
-- Usually 'transform' is more appropriate, but 'rewrite' can give better
-- compositionality. Given two single transformations @f@ and @g@, you can
-- construct @\a -> f a `mplus` g a@ which performs both rewrites until a fixed point.
rewrite :: Plated a => (a -> Maybe a) -> a -> a
rewrite = rewriteOf plate
{-# INLINE rewrite #-}

-- | Rewrite by applying a rule everywhere you can. Ensures that the rule cannot
-- be applied anywhere in the result:
--
-- @propRewriteOf l r x = 'all' ('Data.Just.isNothing' . r) ('universeOf' l ('rewriteOf' l r x))@
--
-- Usually 'transformOf' is more appropriate, but 'rewriteOf' can give better
-- compositionality. Given two single transformations @f@ and @g@, you can
-- construct @\a -> f a `mplus` g a@ which performs both rewrites until a fixed point.
--
-- @
-- 'rewriteOf' :: 'Simple' 'Control.Lens.Iso.Iso' a a       -> (a -> 'Maybe' a) -> a -> a
-- 'rewriteOf' :: 'Simple' 'Lens' a a      -> (a -> 'Maybe' a) -> a -> a
-- 'rewriteOf' :: 'Simple' 'Traversal' a a -> (a -> 'Maybe' a) -> a -> a
-- 'rewriteOf' :: 'Simple' 'Setter' a a    -> (a -> 'Maybe' a) -> a -> a
-- @
rewriteOf :: SimpleSetting a a -> (a -> Maybe a) -> a -> a
rewriteOf l f = go where
  go = transformOf l (\x -> maybe x go (f x))
{-# INLINE rewriteOf #-}

-- | Rewrite recursively over part of a larger structure.
--
-- @
-- 'rewriteOn' :: 'Plated' a => 'Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> 'Maybe' a) -> s -> s
-- 'rewriteOn' :: 'Plated' a => 'Simple' 'Lens' s a      -> (a -> 'Maybe' a) -> s -> s
-- 'rewriteOn' :: 'Plated' a => 'Simple' 'Traversal' s a -> (a -> 'Maybe' a) -> s -> s
-- 'rewriteOn' :: 'Plated' a => 'Simple' 'Setting' s a   -> (a -> 'Maybe' a) -> s -> s
-- @
rewriteOn :: Plated a => Setting s t a a -> (a -> Maybe a) -> s -> t
rewriteOn b = over b . rewrite
{-# INLINE rewriteOn #-}

-- | Rewrite recursively over part of a larger structure using a specified setter.
--
-- @
-- 'rewriteOnOf' :: 'Plated' a => 'Simple' 'Control.Lens.Iso.Iso' s a       -> 'Simple' 'Control.Lens.Iso.Iso' a a       -> (a -> 'Maybe' a) -> s -> s
-- 'rewriteOnOf' :: 'Plated' a => 'Simple' 'Lens' s a      -> 'Simple' 'Lens' a a      -> (a -> 'Maybe' a) -> s -> s
-- 'rewriteOnOf' :: 'Plated' a => 'Simple' 'Traversal' s a -> 'Simple' 'Traversal' a a -> (a -> 'Maybe' a) -> s -> s
-- 'rewriteOnOf' :: 'Plated' a => 'Simple' 'Setter' s a    -> 'Simple' 'Setter' a a    -> (a -> 'Maybe' a) -> s -> s
-- @
rewriteOnOf :: Setting s t a a -> SimpleSetting a a -> (a -> Maybe a) -> s -> t
rewriteOnOf b l = over b . rewriteOf l
{-# INLINE rewriteOnOf #-}

-- | Rewrite by applying a monadic rule everywhere you can. Ensures that the rule cannot
-- be applied anywhere in the result.
rewriteM :: (Monad m, Plated a) => (a -> m (Maybe a)) -> a -> m a
rewriteM = rewriteMOf plate
{-# INLINE rewriteM #-}

-- | Rewrite by applying a monadic rule everywhere you recursing with a user-specified 'Traversal'.
-- Ensures that the rule cannot be applied anywhere in the result.
rewriteMOf :: Monad m => SimpleLensLike (WrappedMonad m) a a -> (a -> m (Maybe a)) -> a -> m a
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
rewriteMOnOf :: Monad m => LensLike (WrappedMonad m) s t a a -> SimpleLensLike (WrappedMonad m) a a -> (a -> m (Maybe a)) -> s -> m t
rewriteMOnOf b l = mapMOf b . rewriteMOf l
{-# INLINE rewriteMOnOf #-}

-------------------------------------------------------------------------------
-- Universe
-------------------------------------------------------------------------------

-- | Retrieve all of the transitive descendants of a 'Plated' container, including itself.
universe :: Plated a => a -> [a]
universe = universeOf plate
{-# INLINE universe #-}

-- | Given a fold that knows how to locate immediate children, retrieve all of the transitive descendants of a node, including itself.
--
-- @'universeOf' :: 'Fold' a a -> a -> [a]@
universeOf :: Getting [a] a b a b -> a -> [a]
universeOf l = go where
  go a = a : foldMapOf l go a
{-# INLINE universeOf #-}

-- | Given a 'Fold' that knows how to find 'Plated' parts of a container retrieve them and all of their descendants, recursively.
universeOn ::  Plated a => Getting [a] s t a a -> s -> [a]
universeOn b = universeOnOf b plate
{-# INLINE universeOn #-}

-- | Given a 'Fold' that knows how to locate immediate children, retrieve all of the transitive descendants of a node, including itself that lie
-- in a region indicated by another 'Fold'.
--
-- @'toListOf' l ≡ 'universeOnOf' l 'ignored'@
universeOnOf :: Getting [a] s t a b -> Getting [a] a b a b -> s -> [a]
universeOnOf b = foldMapOf b . universeOf
{-# INLINE universeOnOf #-}

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

-- | Transform every element in the tree, in a bottom-up manner.
--
-- For example, replacing negative literals with literals:
--
-- @
-- negLits = 'transform' $ \x -> case x of
--   Neg (Lit i) -> Lit ('negate' i)
--   _           -> x
-- @
transform :: Plated a => (a -> a) -> a -> a
transform = transformOf plate
{-# INLINE transform #-}

-- | Transform every element in the tree in a bottom-up manner over a region indicated by a 'Setter'.
--
-- @
-- 'transformOn' :: 'Plated' a => 'Simple' 'Traversal' s a -> (a -> a) -> s -> s
-- 'transformOn' :: 'Plated' a => 'Simple' 'Setter' s a    -> (a -> a) -> s -> s
-- @
transformOn :: Plated a => Setting s t a a -> (a -> a) -> s -> t
transformOn b = over b . transform
{-# INLINE transformOn #-}

-- | Transform every element by recursively applying a given 'Setter' in a bottom-up manner.
--
-- @
-- 'transformOf' :: 'Simple' 'Traversal' a a -> (a -> a) -> a -> a
-- 'transformOf' :: 'Simple' 'Setter' a a    -> (a -> a) -> a -> a
-- @
transformOf :: SimpleSetting a a -> (a -> a) -> a -> a
transformOf l f = go where
  go = f . over l go
{-# INLINE transformOf #-}

-- | Transform every element in a region indicated by a 'Setter' by recursively applying another 'Setter'
-- in a bottom-up manner.
--
-- @
-- 'transformOnOf' :: 'Simple' 'Setter' s a -> 'Simple' 'Traversal' a a -> (a -> a) -> s -> s
-- 'transformOnOf' :: 'Simple' 'Setter' s a -> 'Simple' 'Setter' a a    -> (a -> a) -> s -> s
-- @
transformOnOf :: Setting s t a a -> SimpleSetting a a -> (a -> a) -> s -> t
transformOnOf b l = over b . transformOf l
{-# INLINE transformOnOf #-}

-- | Transform every element in the tree, in a bottom-up manner, monadically.
transformM :: (Monad m, Plated a) => (a -> m a) -> a -> m a
transformM = transformMOf plate
{-# INLINE transformM #-}

-- | Transform every element in the tree in a region indicated by a supplied 'Traversal', in a bottom-up manner, monadically.
--
-- @'transformMOn' :: ('Monad' m, 'Plated' a) => 'Simple' 'Traversal' s a -> (a -> m a) -> s -> m s@
transformMOn :: (Monad m, Plated a) => LensLike (WrappedMonad m) s t a a -> (a -> m a) -> s -> m t
transformMOn b = mapMOf b . transformM
{-# INLINE transformMOn #-}

-- | Transform every element in a tree using a user supplied 'Traversal' in a bottom-up manner with a monadic effect.
--
-- @'transformMOf' :: 'Monad' m => 'Simple 'Traversal' a a -> (a -> m a) -> a -> m a@
transformMOf :: Monad m => SimpleLensLike (WrappedMonad m) a a -> (a -> m a) -> a -> m a
transformMOf l f = go where
  go t = mapMOf l go t >>= f
{-# INLINE transformMOf #-}

-- | Transform every element in a tree that lies in a region indicated by a supplied 'Traversal', walking with a user supplied 'Traversal' in
-- a bottom-up manner with a monadic effect.
--
-- @'transformMOnOf' :: 'Monad' m => 'Simple' 'Traversal' s a -> 'Simple' 'Traversal' a a -> (a -> m a) -> s -> m s@
transformMOnOf :: Monad m => LensLike (WrappedMonad m) s a a a -> SimpleLensLike (WrappedMonad m) a a -> (a -> m a) -> s -> m a
transformMOnOf b l = mapMOf b . transformMOf l
{-# INLINE transformMOnOf #-}

-------------------------------------------------------------------------------
-- Holes and Contexts
-------------------------------------------------------------------------------

-- | Return a list of all of the editable contexts for every location in the structure, recursively.
--
-- @
-- propUniverse x = 'universe' x == 'map' 'pos' ('contexts' x)
-- propId x = 'all' ('==' x) [extract w | w <- 'contexts' x]
-- @
--
-- @'contexts' ≡ 'contextsOf' 'plate'@
contexts :: Plated a => a -> [Context a a a]
contexts = contextsOf plate
{-# INLINE contexts #-}

-- | Return a list of all of the editable contexts for every location in the structure, recursively, using a user-specified 'Traversal' to walk each layer.
--
-- @
-- propUniverse l x = 'universeOf' l x == 'map' 'pos' ('contextsOf' l x)
-- propId l x = 'all' ('==' x) [extract w | w <- 'contextsOf' l x]
-- @
--
-- @'contextsOf' :: 'Simple' 'Traversal' a a -> a -> ['Context' a a]@
contextsOf :: SimpleLensLike (Bazaar a a) a a -> a -> [Context a a a]
contextsOf l x = Context id x : f (holesOf l x) where
  f xs = do
    Context ctx child <- xs
    Context context y <- contextsOf l child
    return $ Context (ctx . context) y
{-# INLINE contextsOf #-}

-- | Return a list of all of the editable contexts for every location in the structure in an areas indicated by a user supplied 'Traversal', recursively using 'plate'.
--
-- @'contextsOn' b ≡ 'contextsOnOf' b 'plate'@
--
-- @'contextsOn' :: 'Plated' a => 'Simple' 'Traversal' s a -> s -> ['Context' a a s]@
contextsOn :: Plated a => LensLike (Bazaar a a) s t a a -> s -> [Context a a t]
contextsOn b = contextsOnOf b plate
{-# INLINE contextsOn #-}

-- | Return a list of all of the editable contexts for every location in the structure in an areas indicated by a user supplied 'Traversal', recursively using
-- another user-supplied 'Traversal' to walk each layer.
--
-- @'contextsOnOf' :: 'Simple' 'Traversal' s a -> 'Simple' 'Traversal' a a -> s -> ['Context' a a s]@
contextsOnOf :: LensLike (Bazaar a a) s t a a -> SimpleLensLike (Bazaar a a) a a -> s -> [Context a a t]
contextsOnOf b l = f . holesOf b where
  f xs = do
    Context ctx child <- xs
    Context context y <- contextsOf l child
    return $ Context (ctx . context) y
{-# INLINE contextsOnOf #-}

-- | The one-level version of 'context'. This extracts a list of the immediate children as editable contexts.
--
-- Given a context you can use 'pos' to see the values, 'peek' at what the structure would be like with an edited result, or simply 'extract' the original structure.
--
-- @
-- propChildren x = 'children' l x '==' 'map' 'pos' ('holes' l x)
-- propId x = 'all' ('==' x) [extract w | w <- 'holes' l x]
-- @
--
-- @'holes' = 'holesOf' 'plate'@
holes :: Plated a => a -> [Context a a a]
holes = holesOf plate
{-# INLINE holes #-}

-- | The one-level version of 'contextsOf'. This extracts a list of the immediate children according to a given 'Traversal' as editable contexts.
--
-- Given a context you can use 'pos' to see the values, 'peek' at what the structure would be like with an edited result, or simply 'extract' the original structure.
--
-- @
-- propChildren l x = 'childrenOf' l x '==' 'map' 'pos' ('holesOf' l x)
-- propId l x = 'all' ('==' x) [extract w | w <- 'holesOf' l x]
-- @
--
-- @
-- 'holesOf' :: 'Simple' 'Iso' s a       -> s -> ['Context' a a s]
-- 'holesOf' :: 'Simple' 'Lens' s a      -> s -> ['Context' a a s]
-- 'holesOf' :: 'Simple' 'Traversal' s a -> s -> ['Context' a a s]
-- @
holesOf :: LensLike (Bazaar a a) s t a a -> s -> [Context a a t]
holesOf l a = f (ins b) (outs b) where
  b = l sell a
  f []     _ = []
  f (x:xs) g = Context (g . (:xs)) x : f xs (g . (x:))
{-# INLINE holesOf #-}


-- | An alias for 'holesOf', provided for consistency with the other combinators.
--
-- @'holesOn' ≡ 'holesOf'@
--
-- @
-- 'holesOn' :: 'Simple' 'Iso' s a       -> s -> ['Context' a a s]
-- 'holesOn' :: 'Simple' 'Lens' s a      -> s -> ['Context' a a s]
-- 'holesOn' :: 'Simple' 'Traversal' s a -> s -> ['Context' a a s]
-- @
holesOn :: LensLike (Bazaar a a) s t a a -> s -> [Context a a t]
holesOn = holesOf
{-# INLINE holesOn #-}

-- | Extract one level of holes from a container in a region specified by one 'Traversal', using another.
--
-- @'holesOnOf' b l ≡ 'holesOf' (b '.' l)@
--
-- @
-- 'holesOnOf' :: 'Simple' 'Iso' s a       -> 'Simple' 'Iso' a a       -> s -> ['Context' a a s]
-- 'holesOnOf' :: 'Simple' 'Lens' s a      -> 'Simple' 'Lens' a a      -> s -> ['Context' a a s]
-- 'holesOnOf' :: 'Simple' 'Traversal' s a -> 'Simple' 'Traversal' a a -> s -> ['Context' a a s]
-- @
holesOnOf :: LensLike (Bazaar r r) s t a b -> LensLike (Bazaar r r) a b r r -> s -> [Context r r t]
holesOnOf b l = holesOf (b.l)
{-# INLINE holesOnOf #-}

-------------------------------------------------------------------------------
-- Paramorphisms
-------------------------------------------------------------------------------

-- | Perform a fold-like computation on each value, technically a paramorphism.
--
-- @'paraOf' :: 'Fold' a a -> (a -> [r] -> r) -> a -> r@
paraOf :: Getting [a] a b a b -> (a -> [r] -> r) -> a -> r
paraOf l f = go where
  go a = f a (go <$> toListOf l a)
{-# INLINE paraOf #-}

-- | Perform a fold-like computation on each value, technically a paramorphism.
--
-- @'para' ≡ 'paraOf' 'plate'@
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
-- @'composOpFold' z c f = 'foldrOf' 'plate' (c '.' f) z@
composOpFold :: Plated a => b -> (b -> b -> b) -> (a -> b) -> a -> b
composOpFold z c f = foldrOf plate (c . f) z
{-# INLINE composOpFold #-}

-------------------------------------------------------------------------------
-- Parts
-------------------------------------------------------------------------------

-- | The original @uniplate@ combinator, implemented in terms of 'Plated' as a 'Lens'.
--
-- @'parts' ≡ 'partsOf' 'plate'@
--
-- The resulting lens is safer to use as it ignores 'over-application' and deals gracefully with under-application,
-- but it is only a proper lens if you don't change the list 'length'!
parts :: Plated a => Simple Lens a [a]
parts = partsOf plate
{-# INLINE parts #-}

-- | 'partsOf' turns a 'Traversal' into a lens that resembles an early version of the @uniplate@ (or @biplate@) type.
--
-- /Note:/ You should really try to maintain the invariant of the number of children in the list.
--
-- Any extras will be lost. If you do not supply enough, then the remainder will come from the original structure.
--
-- So technically, this is only a lens if you do not change the number of results it returns.
--
-- @
-- 'partsOf' :: 'Simple' 'Control.Lens.Iso.Iso' s a       -> 'Simple' 'Lens' s [a]
-- 'partsOf' :: 'Simple' 'Lens' s a      -> 'Simple' 'Lens' s [a]
-- 'partsOf' :: 'Simple' 'Traversal' s a -> 'Simple' 'Lens' s [a]
-- @
partsOf :: LensLike (Bazaar a a) s t a a -> Lens s t [a] [a]
partsOf l f s = outs b <$> f (ins b) where b = l sell s
{-# INLINE partsOf #-}

-- | 'unsafePartsOf' turns a 'Traversal' into a @uniplate@ (or @biplate@) family.
--
-- If you do not need the types of @s@ and @t@ to be different, it is recommended that
-- you use 'partsOf'
--
-- It is generally safer to traverse with the 'Bazaar' rather than use this
-- combinator. However, it is sometimes convenient.
--
-- This is unsafe because if you don't supply at least as many @b@'s as you were
-- given @a@'s, then the reconstruction of @t@ /will/ result in an error!
--
-- @
-- 'unsafePartsOf' :: 'Control.Lens.Iso.Iso' s t a b       -> 'Lens' s t [a] [b]
-- 'unsafePartsOf' :: 'Lens' s t a b      -> 'Lens' s t [a] [b]
-- 'unsafePartsOf' :: 'Traversal' s t a b -> 'Lens' s t [a] [b]
-- @
unsafePartsOf :: LensLike (Bazaar a b) s t a b -> Lens s t [a] [b]
unsafePartsOf l f s = unsafeOuts b <$> f (ins b) where b = l sell s
{-# INLINE unsafePartsOf #-}

------------------------------------------------------------------------------
-- Common Lenses
------------------------------------------------------------------------------

-- | A 'Lens' to 'Control.Lens.Getter.view'/'Control.Lens.Setter.set' the nth element 'elementOf' a 'Traversal', 'Lens' or 'Control.Lens.Iso.Iso'.
--
-- Attempts to access beyond the range of the 'Traversal' will cause an error.
--
-- >>> [[1],[3,4]]^.elementOf (traverse.traverse) 1
-- 3
elementOf :: Functor f => LensLike (Bazaar a a) s t a a -> Int -> LensLike f s t a a
elementOf l k f s = case holesOf l s !! k of
  Context g a -> g <$> f a

-- | Access the /nth/ element of a 'Traversable' container.
--
-- Attempts to access beyond the range of the 'Traversal' will cause an error.
--
-- @'element' ≡ 'elementOf' 'traverse'@
element :: Traversable t => Int -> Simple Lens (t a) a
element = elementOf traverse

-- | Transform an 'Traversal' into an 'Control.Lens.IndexedTraversal.IndexedTraversal', a 'Fold' into an 'Control.Lens.IndexedFold.IndexedFold', etc.
--
-- @
-- 'indexed' :: 'Control.Lens.Traversal.Traversal' s t a b -> 'Control.Lens.IndexedTraversal.IndexedTraversal' 'Int' s t a b
-- 'indexed' :: 'Control.Lens.Type.Lens' s t a b      -> 'Control.Lens.IndexedLens.IndexedLens' 'Int' s t a b
-- 'indexed' :: 'Control.Lens.Fold.Fold' s t          -> 'Control.Lens.IndexedFold.IndexedFold' 'Int' s t
-- 'indexed' :: 'Control.Lens.Iso.Iso' s t a b       -> 'Control.Lens.IndexedLens.IndexedLens' 'Int' s t a b
-- 'indexed' :: 'Control.Lens.Getter.Getter' s t        -> 'Control.Lens.IndexedGetter.IndexedGetter' 'Int' s t a b
-- @
indexed :: forall k f s t a b. (Applicative f, Indexed Int k) => LensLike (Bazaar a b) s t a b -> k (a -> f b) (s -> f t)
indexed l = index $ \(iafb :: Int -> a -> f b) s ->
  (\bs -> set (unsafePartsOf l) bs s) <$>
  sequenceA (zipWith iafb [(0 :: Int)..] (s^.unsafePartsOf l))
{-# INLINE indexed #-}

-------------------------------------------------------------------------------
-- Misc.
-------------------------------------------------------------------------------

ins :: Bazaar a b t -> [a]
ins = toListOf bazaar
{-# INLINE ins #-}

unconsWithDefault :: a -> [a] -> (a,[a])
unconsWithDefault d []     = (d,[])
unconsWithDefault _ (x:xs) = (x,xs)
{-# INLINE unconsWithDefault #-}

outs :: Bazaar a a t -> [a] -> t
outs = evalState . bazaar (\oldVal -> state (unconsWithDefault oldVal))
{-# INLINE outs #-}

unsafeOuts :: Bazaar a b t -> [b] -> t
unsafeOuts = evalState . bazaar (\_ -> state (unconsWithDefault fakeVal))
  where fakeVal = error "unsafePartsOf: not enough elements were supplied"
{-# INLINE unsafeOuts #-}

