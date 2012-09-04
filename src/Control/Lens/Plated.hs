{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , children, childrenOn
  , rewrite, rewriteOf, rewriteOn, rewriteOnOf
  , rewriteM, rewriteMOf, rewriteMOn, rewriteMOnOf
  , universe, universeOf, universeOn, universeOnOf
  , transform, transformOf, transformOn, transformOnOf
  , transformM, transformMOf, transformMOn, transformMOnOf
  , descend, descendOf, descendOn, descendOnOf
  , descendA, descendAOf, descendAOn, descendAOnOf
  , descendA_, descendAOf_, descendAOn_, descendAOnOf_
  , descendM, descendMOf, descendMOn, descendMOnOf
  , descendM_, descendMOf_, descendMOn_, descendMOnOf_
  , contexts, contextsOf, contextsOn, contextsOnOf
  , holes, holesOf, holesOn, holesOnOf
  , para, paraOf

  -- * Compos
  -- $compos
  , composOpFold

  -- * Parts
  , parts
  , partsOf

  -- ** Unsafe Operations
  , unsafePartsOf
  )
  where

import Control.Applicative
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Setter
import Control.Lens.Traversal
import Control.Lens.Type
import Data.Tree

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
  plate = ignored

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

-- | Provided for compatibility with @uniplate@.
--
-- @'childrenOn' ≡ 'toListOf'@
--
-- @'childrenOn' :: 'Fold' a c -> a -> [c]@
childrenOn :: Getting [c] a b c d -> a -> [c]
childrenOn = toListOf
{-# INLINE childrenOn #-}

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
-- 'rewriteOn' :: 'Plated' c => 'Simple' 'Control.Lens.Iso.Iso' a b       -> (b -> 'Maybe' b) -> a -> a
-- 'rewriteOn' :: 'Plated' c => 'Simple' 'Lens' a b      -> (b -> 'Maybe' b) -> a -> a
-- 'rewriteOn' :: 'Plated' c => 'Simple' 'Traversal' a b -> (b -> 'Maybe' b) -> a -> a
-- 'rewriteOn' :: 'Plated' c => 'Simple' 'Setting' a b   -> (b -> 'Maybe' b) -> a -> a
-- @
rewriteOn :: Plated c => Setting a b c c -> (c -> Maybe c) -> a -> b
rewriteOn b = over b . rewrite
{-# INLINE rewriteOn #-}

-- | Rewrite recursively over part of a larger structure using a specified setter.
--
-- @
-- 'rewriteOnOf' :: 'Plated' b => 'Simple' 'Control.Lens.Iso.Iso' a b       -> 'Simple' 'Control.Lens.Iso.Iso' b b       -> (b -> 'Maybe' b) -> a -> a
-- 'rewriteOnOf' :: 'Plated' b => 'Simple' 'Lens' a b      -> 'Simple' 'Lens' b b      -> (b -> 'Maybe' b) -> a -> a
-- 'rewriteOnOf' :: 'Plated' b => 'Simple' 'Traversal' a b -> 'Simple' 'Traversal' b b -> (b -> 'Maybe' b) -> a -> a
-- 'rewriteOnOf' :: 'Plated' b => 'Simple' 'Setter' a b    -> 'Simple' 'Setter' b b    -> (b -> 'Maybe' b) -> a -> a
-- @
rewriteOnOf :: Setting a b c c -> SimpleSetting c c -> (c -> Maybe c) -> a -> b
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
rewriteMOn :: (Monad m, Plated c) => LensLike (WrappedMonad m) a b c c -> (c -> m (Maybe c)) -> a -> m b
rewriteMOn b = mapMOf b . rewriteM
{-# INLINE rewriteMOn #-}

-- | Rewrite by applying a monadic rule everywhere inside of a structure located by a user-specified 'Traversal',
-- using a user-specified 'Traversal' for recursion. Ensures that the rule cannot be applied anywhere in the result.
rewriteMOnOf :: Monad m => LensLike (WrappedMonad m) a b c c -> SimpleLensLike (WrappedMonad m) c c -> (c -> m (Maybe c)) -> a -> m b
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
universeOn ::  Plated c => Getting [c] a b c c -> a -> [c]
universeOn b = universeOnOf b plate
{-# INLINE universeOn #-}

-- | Given a 'Fold' that knows how to locate immediate children, retrieve all of the transitive descendants of a node, including itself that lie
-- in a region indicated by another 'Fold'.
--
-- @'toListOf' l ≡ 'universeOnOf' l 'ignored'@
universeOnOf :: Getting [c] a b c d -> Getting [c] c d c d -> a -> [c]
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
-- 'transformOn' :: 'Plated' b => 'Simple' 'Traversal' a b -> (b -> b) -> a -> a
-- 'transformOn' :: 'Plated' b => 'Simple' 'Setter' a b    -> (b -> b) -> a -> a
-- @
transformOn :: Plated c => Setting a b c c -> (c -> c) -> a -> b
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
-- 'transformOnOf' :: 'Setter' a b -> 'Simple' 'Traversal' b b -> (b -> b) -> a -> a
-- 'transformOnOf' :: 'Setter' a b -> 'Simple' 'Setter' b b    -> (b -> b) -> a -> a
-- @
transformOnOf :: Setting a b c c -> SimpleSetting c c -> (c -> c) -> a -> b
transformOnOf b l = over b . transformOf l
{-# INLINE transformOnOf #-}

-- | Transform every element in the tree, in a bottom-up manner, monadically.
transformM :: (Monad m, Plated a) => (a -> m a) -> a -> m a
transformM = transformMOf plate
{-# INLINE transformM #-}

-- | Transform every element in the tree in a region indicated by a supplied 'Traversal', in a bottom-up manner, monadically.
--
-- @'transformMOn' :: ('Monad' m, 'Plated' c) => 'Simple' 'Traversal' a b -> (b -> m b) -> a -> m a@
transformMOn :: (Monad m, Plated c) => LensLike (WrappedMonad m) a b c c -> (c -> m c) -> a -> m b
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
-- @'transformMOnOf' :: 'Monad' m => 'Simple' 'Traversal' a b -> 'Simple' 'Traversal' b b -> (b -> m b) -> a -> m a@
transformMOnOf :: Monad m => LensLike (WrappedMonad m) a b c c -> SimpleLensLike (WrappedMonad m) c c -> (c -> m c) -> a -> m b
transformMOnOf b l = mapMOf b . transformMOf l
{-# INLINE transformMOnOf #-}

-------------------------------------------------------------------------------
-- Descent
-------------------------------------------------------------------------------

-- | Recurse one level into a structure. (a.k.a @composOp@ from Björn Bringert's @compos@)
--
-- @'descend' ≡ 'over' 'plate'@
descend :: Plated a => (a -> a) -> a -> a
descend = over plate
{-# INLINE descend #-}

-- | Recurse one level into a structure using a user specified recursion scheme. This is 'over', but it is supplied here
-- for consistency with the uniplate API.
--
-- @'descendOf' ≡ 'over'@
--
-- @
-- 'descendOf' :: 'Simple' 'Setter' a b -> (b -> b) -> a -> a
-- 'descendOf' :: 'Simple' 'Traversal' a b -> (b -> b) -> a -> a
-- @
descendOf :: Setting a b c d -> (c -> d) -> a -> b
descendOf = over
{-# INLINE descendOf #-}

-- | Recurse one level into the parts delimited by one 'Setter', using another.
--
-- @'descendOnOf' b l ≡ 'over' (b '.' l)@
--
-- @
-- 'descendOnOf' :: 'Simple' 'Setter' a b    -> 'Simple' 'Setter' b b    -> (b -> b) -> a -> a
-- 'descendOnOf' :: 'Simple' 'Traversal' a b -> 'Simple' 'Traversal' b b -> (b -> b) -> a -> a
-- @
--
descendOnOf :: Setting a b c d -> Setting c d e f -> (e -> f) -> a -> b
descendOnOf b l = over (b.l)
{-# INLINE descendOnOf #-}

-- | Recurse one level into the parts of the structure delimited by a 'Setter'.
--
-- @'descendOn' b ≡ 'over' (b '.' 'plate')@
--
-- @'descendOn' :: 'Plated' c => 'Setter' a b -> (b -> b) -> a -> a@
descendOn :: Plated c => Setting a b c c -> (c -> c) -> a -> b
descendOn b = over (b . plate)
{-# INLINE descendOn #-}

-------------------------------------------------------------------------------
-- Applicative Descent
-------------------------------------------------------------------------------

-- | Recurse one level into a structure with an 'Applicative' effect, this is 'plate', but it is supplied
-- for consistency with the uniplate API.
--
-- @'descendA' ≡ 'plate'@
descendA :: (Applicative f, Plated a) => (a -> f a) -> a -> f a
descendA = plate
{-# INLINE descendA #-}

-- | Recurse one level into a structure using a user specified recursion scheme and 'Applicative' effects. This is 'id', but it is supplied
-- for consistency with the uniplate API.
--
-- @'descendAOf' ≡ 'id'@
--
-- @'descendAOf' :: 'Applicative' m => 'Simple' 'Traversal' a b => (b -> m b) -> a -> m a@
descendAOf :: Applicative f => LensLike f a b c d -> (c -> f d) -> a -> f b
descendAOf = id
{-# INLINE descendAOf #-}

-- | Recurse one level into the parts delimited by one 'Traversal', using another with 'Applicative' effects.
--
-- @'descendAOnOf' ≡ ('.')@
--
-- @'descendAOnOf' :: 'Applicative' f => 'Simple' 'Traversal' a b -> 'Simple' 'Traversal' b b -> (b -> f b) -> a -> f a@
descendAOnOf :: Applicative g => LensLike g a b c d -> LensLike g c d e f -> (e -> g f) -> a -> g b
descendAOnOf = (.)
{-# INLINE descendAOnOf #-}

-- | Recurse one level into the parts of the structure delimited by a 'Traversal' with 'Applicative' effects.
--
-- @'descendAOn' b ≡ b '.' 'plate'@
--
-- @'descendAOn' :: ('Applicative' f, Plated' c) => 'Simple' 'Traversal' a b -> (b -> f b) -> a -> f a@
descendAOn :: (Applicative f, Plated c) => LensLike f a b c c -> (c -> f c) -> a -> f b
descendAOn b = b . plate
{-# INLINE descendAOn #-}

-- |
--
-- @'descendA_' ≡ traverseOf_' 'plate'@
descendA_ :: (Applicative f, Plated a) => (a -> f b) -> a -> f ()
descendA_ = traverseOf_ plate
{-# INLINE descendA_ #-}

-- | Recurse one level into a structure using a user specified recursion scheme and 'Applicative' effects, without reconstructing the structure behind you.
--
-- This is just 'traverseOf_', but is provided for consistency.
--
-- @'descendAOf_' ≡ 'traverseOf_'@
--
-- @'descendAOf_' :: 'Applicative' f => 'Fold' a b => (b -> f b) -> a -> f ()@
descendAOf_ :: Applicative f => Getting (Traversed f) a b c d -> (c -> f e) -> a -> f ()
descendAOf_ = traverseOf_
{-# INLINE descendAOf_ #-}

-- | Recurse one level into the parts delimited by one 'Fold', using another with 'Applicative' effects, without reconstructing the structure behind you.
--
-- @'descendAOnOf_' b l ≡ 'traverseOf_' (b '.' l)@
--
-- @'descendAOnOf_' :: 'Applicative' f => 'Fold' a b -> 'Fold' b b -> (b -> f c) -> a -> f ()@
descendAOnOf_ :: Applicative f => Getting (Traversed f) a b c d -> Getting (Traversed f) c d c d -> (c -> f e) -> a -> f ()
descendAOnOf_ b l = traverseOf_ (b . l)
{-# INLINE descendAOnOf_ #-}

-- | Recurse one level into the parts of the structure delimited by a 'Traversal' with monadic effects.
--
-- @'descendAOn_' b ≡ 'traverseOf_' (b '.' 'plate')@
--
-- @'descendAOn_' :: ('Applicative' f, 'Plated' b) => 'Simple' 'Traversal' a b -> (b -> f c) -> a -> f ()@
descendAOn_ :: (Applicative f, Plated c) => Getting (Traversed f) a b c c -> (c -> f e) -> a -> f ()
descendAOn_ b = traverseOf_ (b . plate)
{-# INLINE descendAOn_ #-}

-------------------------------------------------------------------------------
-- Monadic Descent
-------------------------------------------------------------------------------

-- | Recurse one level into a structure with a monadic effect. (a.k.a @composOpM@ from Björn Bringert's @compos@)
--
-- @'descendM' ≡ 'mapMOf' 'plate'@
descendM :: (Monad m, Plated a) => (a -> m a) -> a -> m a
descendM = mapMOf plate
{-# INLINE descendM #-}

-- | Recurse one level into a structure using a user specified recursion scheme and monadic effects. This is 'id', but it is
-- supplied for consistency with the uniplate API.
--
-- @'descendMOf' ≡ 'mapMOf'@
--
-- @'descendMOf' :: 'Monad' m => 'Simple' 'Traversal' a b => (b -> m b) -> a -> m a@
descendMOf :: Monad m => LensLike (WrappedMonad m) a b c d -> (c -> m d) -> a -> m b
descendMOf = mapMOf
{-# INLINE descendMOf #-}

-- | Recurse one level into the parts delimited by one 'Traversal', using another with monadic effects.
--
-- @'descendMOnOf' b l ≡ 'mapMOf' (b '.' l)@
--
-- @'descendMOnOf' :: 'Monad' m => 'Simple' 'Traversal' a b -> 'Simple' 'Traversal' b b -> (b -> m b) -> a -> m a@
descendMOnOf :: Monad m => LensLike (WrappedMonad m) a b c c -> SimpleLensLike (WrappedMonad m) c c -> (c -> m c) -> a -> m b
descendMOnOf b l = mapMOf (b . l)
{-# INLINE descendMOnOf #-}

-- | Recurse one level into the parts of the structure delimited by a 'Traversal' with monadic effects.
--
-- @'descendMOn' b ≡ 'mapMOf' (b . 'plate')@
--
-- @'descendMOn' :: ('Monad' m, 'Plated' c) => 'Simple' 'Traversal' a b -> (b -> m b) -> a -> m a@
descendMOn :: (Monad m, Plated c) => LensLike (WrappedMonad m) a b c c -> (c -> m c) -> a -> m b
descendMOn b = mapMOf (b . plate)
{-# INLINE descendMOn #-}

-- | Descend one level into a structure with monadic effects (a.k.a @composOpM@ from Björn Bringert's @compos@)
--
-- @'descendM_' ≡ mapMOf_' 'plate'@
descendM_ :: (Monad m, Plated a) => (a -> m b) -> a -> m ()
descendM_ = mapMOf_ plate
{-# INLINE descendM_ #-}

-- | Recurse one level into a structure using a user specified recursion scheme and monadic effects. This is just 'mapMOf_', but is provided for consistency.
--
-- @'descendMOf_' ≡ 'mapMOf_'@
--
-- @'descendMOf_' :: 'Monad' m => 'Fold' a b => (b -> m b) -> a -> m ()@
descendMOf_ :: Monad m => Getting (Sequenced m) a b c d -> (c -> m e) -> a -> m ()
descendMOf_ = mapMOf_
{-# INLINE descendMOf_ #-}

-- | Recurse one level into the parts delimited by one 'Traversal', using another with monadic effects.
--
-- @'descendMOnOf_' b l ≡ 'mapMOf_' (b '.' l)@
--
-- @'descendMOnOf_' :: 'Monad' m => 'Fold' a b -> 'Fold' b b -> (b -> m b) -> a -> m ()@
descendMOnOf_ :: Monad m => Getting (Sequenced m) a b c d -> Getting (Sequenced m) c d c d -> (c -> m e) -> a -> m ()
descendMOnOf_ b l = mapMOf_ (b . l)
{-# INLINE descendMOnOf_ #-}

-- | Recurse one level into the parts of the structure delimited by a 'Traversal' with monadic effects.
--
-- @'descendMOn_' b ≡ 'mapMOf_' (b '.' 'plate')@
--
-- @'descendMOn_' :: ('Monad' m, 'Plated' b) => 'Simple' 'Traversal' a b -> (b -> m c) -> a -> m ()@
descendMOn_ :: (Monad m, Plated c) => Getting (Sequenced m) a b c c -> (c -> m e) -> a -> m ()
descendMOn_ b = mapMOf_ (b . plate)
{-# INLINE descendMOn_ #-}

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
-- @'contextsOn' :: 'Plated' b => 'Simple' 'Traversal' a b -> a -> ['Context' b b a]@
contextsOn :: Plated c => LensLike (Bazaar c c) a b c c -> a -> [Context c c b]
contextsOn b = contextsOnOf b plate
{-# INLINE contextsOn #-}

-- | Return a list of all of the editable contexts for every location in the structure in an areas indicated by a user supplied 'Traversal', recursively using
-- another user-supplied 'Traversal' to walk each layer.
--
-- @'contextsOnOf' :: 'Simple' 'Traversal' a b -> 'Simple' 'Traversal' b b -> a -> ['Context' b b a]@
contextsOnOf :: LensLike (Bazaar c c) a b c c -> SimpleLensLike (Bazaar c c) c c -> a -> [Context c c b]
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
-- 'holesOf' :: 'Simple' 'Iso' a b       -> a -> ['Context' b a]
-- 'holesOf' :: 'Simple' 'Lens' a b      -> a -> ['Context' b a]
-- 'holesOf' :: 'Simple' 'Traversal' a b -> a -> ['Context' b a]
-- @
holesOf :: LensLike (Bazaar c c) a b c c -> a -> [Context c c b]
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
-- 'holesOn' :: 'Simple' 'Iso' a b       -> a -> ['Context' b b a]
-- 'holesOn' :: 'Simple' 'Lens' a b      -> a -> ['Context' b b a]
-- 'holesOn' :: 'Simple' 'Traversal' a b -> a -> ['Context' b b a]
-- @
holesOn :: LensLike (Bazaar c c) a b c c -> a -> [Context c c b]
holesOn = holesOf
{-# INLINE holesOn #-}

-- | Extract one level of holes from a container in a region specified by one 'Traversal', using another.
--
-- @'holesOnOf' b l ≡ 'holesOf' (b '.' l)@
--
-- @
-- 'holesOnOf' :: 'Simple' 'Iso' a b       -> 'Simple' 'Iso' b b       -> a -> ['Context' b a]
-- 'holesOnOf' :: 'Simple' 'Lens' a b      -> 'Simple' 'Lens' b b      -> a -> ['Context' b a]
-- 'holesOnOf' :: 'Simple' 'Traversal' a b -> 'Simple' 'Traversal' b b -> a -> ['Context' b a]
-- @
holesOnOf :: LensLike (Bazaar e e) a b c d -> LensLike (Bazaar e e) c d e e -> a -> [Context e e b]
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
-- 'partsOf' :: 'Simple' 'Control.Lens.Iso.Iso' a b       -> a -> 'Simple' 'Lens' a [b]
-- 'partsOf' :: 'Simple' 'Lens' a b      -> a -> 'Simple' 'Lens' a [b]
-- 'partsOf' :: 'Simple' 'Traversal' a b -> a -> 'Simple' 'Traversal' a [b]
-- @
partsOf :: LensLike (Bazaar c c) a b c c -> Lens a b [c] [c]
partsOf l f a = outs b <$> f (ins b) where b = l sell a
{-# INLINE partsOf #-}

-- | 'unsafePartsOf' turns a 'Traversal' into a @uniplate@ (or @biplate@) family.
--
-- If you do not need the types of @c@ and @d@ to be different, it is recommended that
-- you use 'partsOf'
--
-- It is generally safer to traverse with the 'Bazaar' rather than use this
-- combinator. However, it is sometimes convenient.
--
-- This is unsafe because if you don't supply at least as many @d@'s as you were
-- given @c@'s, then the reconstruction of @b@ /will/ result in an error!
--
unsafePartsOf :: LensLike (Bazaar c d) a b c d -> Lens a b [c] [d]
unsafePartsOf l f a = unsafeOuts b <$> f (ins b) where b = l sell a
{-# INLINE unsafePartsOf #-}



-------------------------------------------------------------------------------
-- Misc.
-------------------------------------------------------------------------------

ins :: Bazaar c d a -> [c]
ins (Bazaar m) = getConst (m (Const . return))
{-# INLINE ins #-}

newtype Out c a = Out { withOut :: [c] -> (a, [c]) }

instance Functor (Out c) where
  fmap f (Out m) = Out $ \cs -> case m cs of
    (as, ds) -> (f as, ds)
  {-# INLINE fmap #-}

instance Applicative (Out c) where
  pure a = Out $ \cs -> (a, cs)
  {-# INLINE pure #-}
  Out mf <*> Out ma = Out $ \cs -> case mf cs of
    (f,  ds) -> case ma ds of
       (a,  es) -> (f a, es)
  {-# INLINE (<*>) #-}

outs :: Bazaar c c a -> [c] -> a
outs (Bazaar m) = fst . withOut (m $ \c -> Out $ \cs -> case cs of
  [] -> (c, [])
  (d:ds) -> (d, ds))
{-# INLINE outs #-}

unsafeOuts :: Bazaar c d a -> [d] -> a
unsafeOuts (Bazaar m) = fst . withOut (m $ \_ -> Out $ \cs -> case cs of
  (d:ds) -> (d, ds)
  [] -> error "unsafePartsOf: not enough elements were supplied")
{-# INLINE unsafeOuts #-}
