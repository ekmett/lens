{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Plated
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
-- By implementing these combinators in terms of 'plates' instead of 'uniplate'
-- additional type safety is gained, as the user is no longer responsible for
-- maintaining invariants such as the number of children he received.
--
-- Biplates are deliberately excluded from the API here, with the intention
-- that you replace them with TH derived traversals.
-------------------------------------------------------------------------------
module Control.Plated
  (
  -- * Uniplates
    Plated(..)
  , uniplate
  , plateOf

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
  --
  -- $compos
  , composOpFold, composOpMonoid, composOpMPlus

  -- * Operations to be careful of
  , unsafePlateOf
  )
  where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Comonad
import Control.Monad
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Setter
import Control.Lens.Traversal
import Control.Lens.Type
import Data.Int
import Data.Monoid
import Data.Ratio
import Data.Word
-- import Data.Tree

-- | A 'Plated' type is one where we know how to extract its immediate self-similar children.
--
-- /Example 1/:
--
-- @
-- data Expr
--   = Val 'Int'
--   | Neg Expr
--   | Add Expr Expr
--   deriving ('Eq','Ord','Show','Read')
-- @
--
-- @
-- instance 'Plated' Expr where
--   'plates' f (Neg e) = Neg '<$>' f e
--   'plates' f (Add a b) = Add '<$>' f a '<*>' f b
--   'plates' _ a = 'pure' a
-- @
--
-- /Example 2/:
--
-- @
-- data Tree a
--   = Bin (Tree a) (Tree a)
--   | Tip a
--   deriving ('Eq','Ord','Show','Read')
-- @
--
-- @
-- instance 'Plated' (Tree a) where
--   'plates' f (Bin l r) = Bin '<$>' f l '<*>' f r
--   'plates' _ t = 'pure' t
-- @
class Plated a where
  -- | 'Traversal' of the immediate children of this structure.
  --
  -- The default definition finds no children.
  plates :: Simple Traversal a a
  plates _ = pure

instance Plated ()
instance Plated Bool
instance Plated Char
instance Plated Double
instance Plated Float
instance Plated Int
instance Plated Int8
instance Plated Int16
instance Plated Int32
instance Plated Int64
instance Plated Integer
instance Plated Word
instance Plated Word8
instance Plated Word16
instance Plated Word32
instance Plated Word64
instance Plated (Ratio Integer)
instance Plated [Char] where
  plates f (x:xs) = (x:) <$> f xs
  plates _ [] = pure []

{-
class Plated b => Biplated a b where
  -- When @a ~ b@, the default definition: @'biplates' = 'id'@ should be used.
  biplates :: Simple Traversal a b
  default biplates :: (a ~ b) => Simple Traversal a b
  biplates = id

instance Biplated [Char] [Char]
instance Biplated [Char] Char where biplates = traverse

instance Biplated (Ratio Integer) (Ratio Integer)
instance Biplated (Ratio Integer) Integer where
  biplates f x = (%) <$> f (numerator x) <*> f (denominator x)
-}

-- | Neil Mitchell's 'uniplate' combinator, implemented in terms of 'Plated'.
--
-- @'uniplate' = 'plateOf' 'plates'@
uniplate :: Plated a => a -> ([a], [a] -> a)
uniplate = plateOf plates
{-# INLINE uniplate #-}

{-
-- |
--
-- @'biplate' = 'plateOf' 'biplates'@
biplate :: Biplated a b => a -> ([b], [b] -> a)
biplate = plateOf biplates
{-# INLINE biplate #-}
-}

-- |
-- 'plateOf' turns a 'Traversal' into a uniplate (or biplate).
--
-- The resulting plate is actually safer to use as it replaces ignores 'over-application' and
-- deals gracefully with under-application.
--
-- @
-- 'plateOf' :: 'Simple' 'Control.Lens.Iso.Iso' a b       -> a -> ([b], [b] -> a)
-- 'plateOf' :: 'Simple' 'Lens' a b      -> a -> ([b], [b] -> a)
-- 'plateOf' :: 'Simple' 'Traversal' a b -> a -> ([b], [b] -> a)
-- @
plateOf :: LensLike (Kleene c c) a b c c -> a -> ([c], [c] -> b)
plateOf l = (ins &&& outs) . l (More (Done id))
{-# INLINE plateOf #-}

ins :: Kleene c d a -> [c]
ins (More ys c) = c : ins ys
ins _ = []

outs :: Kleene c c a -> [c] -> a
outs (More ys _) (c:cs) = outs (fmap ($c) ys) cs
outs xs          _      = extract xs

-- | 'unsafePlateOf' turns a 'Traversal' into a uniplate (or biplate) family.
--
-- If you do not need the types of @c@ and @d@ to be different, it is recommended that
-- you use 'plateOf'
--
-- It is generally safer to traverse with the 'Kleene' indexed store 'Comonad' rather
-- than use this combinator. However, it is sometimes convenient.
--
-- This is unsafe because if you don't supply at least as many @d@'s as you were
-- given @c@'s, then the reconstruction of @b@ /will/ result in an error.
unsafePlateOf :: LensLike (Kleene c d) a b c d -> a -> ([c], [d] -> b)
unsafePlateOf l = (ins &&& outs') . l (More (Done id))

outs' :: Kleene c d a -> [d] -> a
outs' (More ys _) (d:ds) = outs' (fmap ($d) ys) ds
outs' (Done a)    _      = a
outs' _           _      = error "unsafePlateOf: not enough elements were supplied"

-- | Extract the immediate descendants of a 'Plated' container.
children :: Plated a => a -> [a]
children = toListOf plates
{-# INLINE children #-}

-- | Provided for compatibility with @uniplate@.
--
-- @'childrenOn' = 'toListOf'@
--
-- @'childrenOn' :: 'Fold' a b -> a -> [b]@
childrenOn :: Getting [b] a b -> a -> [b]
childrenOn = toListOf

{-
-- | Extract the immediate descendants of a given type from a 'Biplated' container.
childrenBi :: Biplated a b => a -> [b]
childrenBi = toListOf biplates
{-# INLINE childrenBi #-}
-}

-- | Rewrite by applying a rule everywhere you can. Ensures that the rule cannot
-- be applied anywhere in the result:
--
-- @propRewrite r x = 'all' ('Data.Just.isNothing' . r) ('universe' ('rewrite' r x))@
--
-- Usually 'transform' is more appropriate, but 'rewrite' can give better
-- compositionality. Given two single transformations @f@ and @g@, you can
-- construct @\a -> f a `mplus` g a@ which performs both rewrites until a fixed point.
rewrite :: Plated a => (a -> Maybe a) -> a -> a
rewrite = rewriteOf plates
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

{-
rewriteBi :: Biplated a b => (b -> Maybe b) -> a -> a
rewriteBi = over biplates . rewrite
{-# INLINE rewriteBi #-}
-}

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
rewriteM = rewriteMOf plates
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

{-
-- | Rewrite by applying a monadic rule everywhere inside of a structure located by a user-specified 'Traversal'.
-- Ensures that the rule cannot be applied anywhere in the result.
rewriteMBi :: (Monad m, Biplated a b) => (b -> m (Maybe b)) -> a -> m a
rewriteMBi = mapMOf biplates . rewriteM
{-# INLINE rewriteMBi #-}
-}

-- | Get all the descendants of a node, including itself.
--
-- @
-- 'universe' (Add (Val 1) (Neg (Val 2))) = [Add (Val 1) (Neg (Val 2)), Val 1, Neg (Val 2), Val 2]
-- @
--
-- This method is often combined with a list comprehension, for example:
--
-- @vals x = [i | Val i <- 'universe' x]@
--
-- @'Data.List.tails' = 'universe'@
universe :: Plated a => a -> [a]
universe = universeOf plates
{-# INLINE universe #-}

-- | Get all of the descendants of a node that lie in a region indicated by a 'Fold' (or 'Getter')
--
-- @
-- 'universeOn' :: 'Plated' b => 'Getter' a b           -> a -> [b]
-- 'universeOn' :: 'Plated' b => 'Fold' a b             -> a -> [b]
-- 'universeOn' :: 'Plated' b => 'Simple' 'Lens' a b      -> a -> [b]
-- 'universeOn' :: 'Plated' b => 'Simple' 'Traversal' a b -> a -> [b]
-- 'universeOn' :: 'Plated' b => 'Simple' 'Iso' a b       -> a -> [b]
-- @
universeOn :: Plated b => Getting [b] a b -> a -> [b]
universeOn b = foldMapOf b universe
{-# INLINE universeOn #-}

{-
-- | Get all of the descendants of a node of a given type.
universeBi :: Biplated a b => a -> [b]
universeBi = foldMapOf biplates universe
{-# INLINE universeBi #-}
-}

-- | Given a traversal that knows how to locate immediate children, retrieve all of the transitive descendants of a node, including itself.
universeOf :: Getting [a] a a -> a -> [a]
universeOf l = go where
  go a = a : foldMapOf l go a
{-# INLINE universeOf #-}

-- | Given a traversal that knows how to locate immediate children, retrieve all of the transitive descendants of a node, including itself that lie
-- in a region indicated by another fold.
universeOnOf :: Getting [b] a b -> Getting [b] b b -> a -> [b]
universeOnOf b = foldMapOf b . universeOf
{-# INLINE universeOnOf #-}

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
transform = transformOf plates
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

{-
-- | Transform every element in the tree of a given type in a bottom-up manner.
transformBi :: Biplated a b => (b -> b) -> a -> a
transformBi = over biplates . transform
{-# INLINE transformBi #-}
-}

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
transformM = transformMOf plates
{-# INLINE transformM #-}

-- | Transform every element in the tree in a region indicated by a supplied 'Traversal', in a bottom-up manner, monadically.
--
-- @'transformMOn' :: ('Monad' m, 'Plated' c) => 'Simple' 'Traversal' a b -> (b -> m b) -> a -> m a@
transformMOn :: (Monad m, Plated c) => LensLike (WrappedMonad m) a b c c -> (c -> m c) -> a -> m b
transformMOn b = mapMOf b . transformM
{-# INLINE transformMOn #-}

{-
-- | Transform every element of a given type in the tree, in a bottom-up manner, monadically.
transformMBi :: (Monad m, Biplated a b) => (b -> m b) -> a -> m a
transformMBi = mapMOf biplates . transformM
{-# INLINE transformMBi #-}
-}

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

-- | Recurse one level into a structure.
--
-- @'descend' = 'over' 'plates'@
descend :: Plated a => (a -> a) -> a -> a
descend = over plates
{-# INLINE descend #-}

-- | Recurse one level into a structure using a user specified recursion scheme. This is 'over', but it is supplied here
-- for consistency with the uniplate API.
--
-- @'descendOf' = 'over'@
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
-- @'descendOnOf' b l = 'over' (b.l)@
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
-- @'descendOn' b = 'over' (b '.' 'plates')@
--
-- @'descendOn' :: 'Plated' c => 'Setter' a b -> (b -> b) -> a -> a@
descendOn :: Plated c => Setting a b c c -> (c -> c) -> a -> b
descendOn b = over (b . plates)
{-# INLINE descendOn #-}

{-
-- | Recurse one level into the parts of the structure delimited by a 'Setter'.
--
-- @'descendBi' b = 'over' ('biplates' '.' 'plates')@
descendBi :: Biplated a b => (b -> b) -> a -> a
descendBi = over (biplates . plates)
{-# INLINE descendBi #-}
-}

-- | Recurse one level into a structure with an 'Applicative' effect, this is 'plates', but it is supplied
-- for consistency with the uniplate API.
--
-- @'descendA' = 'plates'@
descendA :: (Applicative f, Plated a) => (a -> f a) -> a -> f a
descendA = plates
{-# INLINE descendA #-}

-- | Recurse one level into a structure using a user specified recursion scheme and 'Applicative' effects. This is 'id', but it is supplied
-- for consistency with the uniplate API.
--
-- @'descendAOf' = 'id'@
--
-- @'descendAOf' :: 'Applicative' m => 'Simple' 'Traversal' a b => (b -> m b) -> a -> m a@
descendAOf :: Applicative f => LensLike f a b c d -> (c -> f d) -> a -> f b
descendAOf = id
{-# INLINE descendAOf #-}

-- | Recurse one level into the parts delimited by one 'Traversal', using another with 'Applicative' effects.
--
-- @'descendAOnOf' = ('.')@
--
-- @'descendAOnOf' :: 'Applicative' f => 'Simple' 'Traversal' a b -> 'Simple' 'Traversal' b b -> (b -> f b) -> a -> f a@
descendAOnOf :: Applicative g => LensLike g a b c d -> LensLike g c d e f -> (e -> g f) -> a -> g b
descendAOnOf = (.)
{-# INLINE descendAOnOf #-}

-- | Recurse one level into the parts of the structure delimited by a 'Traversal' with 'Applicative' effects.
--
-- @'descendAOn' b = b '.' 'plates'@
--
-- @'descendAOn' :: ('Applicative' f, Plated' c) => 'Simple' 'Traversal' a b -> (b -> f b) -> a -> f a@
descendAOn :: (Applicative f, Plated c) => LensLike f a b c c -> (c -> f c) -> a -> f b
descendAOn b = b . plates
{-# INLINE descendAOn #-}

{-
-- | Recurse one level into specified parts of the structure.
--
-- @'descendABi' = 'biplates' '.' 'plates'@
descendABi :: (Applicative f, Biplated a b) => (b -> f b) -> a -> f a
descendABi = biplates . plates
{-# INLINE descendABi #-}
-}

-- |
--
-- @'descendA_' = traverseOf_' 'plates'@
descendA_ :: (Applicative f, Plated a) => (a -> f b) -> a -> f ()
descendA_ = traverseOf_ plates
{-# INLINE descendA_ #-}

-- | Recurse one level into a structure using a user specified recursion scheme and 'Applicative' effects, without reconstructing the structure behind you.
--
-- This is just 'traverseOf_', but is provided for consistency.
--
-- @'descendAOf_' = 'traverseOf_'@
--
-- @'descendAOf_' :: 'Applicative' f => 'Fold' a b => (b -> f b) -> a -> f ()@
descendAOf_ :: Applicative f => Getting (Traversed f) a b -> (b -> f c) -> a -> f ()
descendAOf_ = traverseOf_
{-# INLINE descendAOf_ #-}

-- | Recurse one level into the parts delimited by one 'Fold', using another with 'Applicative' effects, without reconstructing the structure behind you.
--
-- @'descendAOnOf_' b l = 'traverseOf_' (b '.' l)@
--
-- @'descendAOnOf_' :: 'Applicative' f => 'Fold' a b -> 'Fold' b b -> (b -> f c) -> a -> f ()@
descendAOnOf_ :: Applicative f => Getting (Traversed f) a b -> Getting (Traversed f) b b -> (b -> f c) -> a -> f ()
descendAOnOf_ b l = traverseOf_ (b . l)
{-# INLINE descendAOnOf_ #-}

-- | Recurse one level into the parts of the structure delimited by a 'Traversal' with monadic effects.
--
-- @'descendAOn_' b = 'traverseOf_' (b '.' 'plates')@
--
-- @'descendAOn_' :: ('Applicative' f, 'Plated' b) => 'Simple' 'Traversal' a b -> (b -> f c) -> a -> f ()@
descendAOn_ :: (Applicative f, Plated b) => Getting (Traversed f) a b -> (b -> f c) -> a -> f ()
descendAOn_ b = traverseOf_ (b . plates)
{-# INLINE descendAOn_ #-}

{-
-- | Recurse one level into parts of the structure.
--
-- @'descendABi_' = 'traverseOf_' ('biplates' '.' 'plates')@
descendABi_ :: (Applicative f, Biplated a b) => (b -> f c) -> a -> f ()
descendABi_ = traverseOf_ (biplates . plates)
{-# INLINE descendABi_ #-}
-}

-- | Recurse one level into a structure with a monadic effect.
--
-- @'descendM' = 'mapMOf' 'plates'@
descendM :: (Monad m, Plated a) => (a -> m a) -> a -> m a
descendM = mapMOf plates
{-# INLINE descendM #-}

-- | Recurse one level into a structure using a user specified recursion scheme and monadic effects. This is 'id', but it is
-- supplied for consistency with the uniplate API.
--
-- @'descendMOf' = 'mapMOf'@
--
-- @'descendMOf' :: 'Monad' m => 'Simple' 'Traversal' a b => (b -> m b) -> a -> m a@
descendMOf :: Monad m => LensLike (WrappedMonad m) a b c d -> (c -> m d) -> a -> m b
descendMOf = mapMOf
{-# INLINE descendMOf #-}

-- | Recurse one level into the parts delimited by one 'Traversal', using another with monadic effects.
--
-- @'descendMOnOf' b l = 'mapMOf' (b '.' l)@
--
-- @'descendMOnOf' :: 'Monad' m => 'Simple' 'Traversal' a b -> 'Simple' 'Traversal' b b -> (b -> m b) -> a -> m a@
descendMOnOf :: Monad m => LensLike (WrappedMonad m) a b c c -> SimpleLensLike (WrappedMonad m) c c -> (c -> m c) -> a -> m b
descendMOnOf b l = mapMOf (b . l)
{-# INLINE descendMOnOf #-}

-- | Recurse one level into the parts of the structure delimited by a 'Traversal' with monadic effects.
--
-- @'descendMOn' b = 'mapMOf' (b . 'plates')@
--
-- @'descendMOn' :: ('Monad' m, 'Plated' c) => 'Simple' 'Traversal' a b -> (b -> m b) -> a -> m a@
descendMOn :: (Monad m, Plated c) => LensLike (WrappedMonad m) a b c c -> (c -> m c) -> a -> m b
descendMOn b = mapMOf (b . plates)
{-# INLINE descendMOn #-}

{-
-- | Recurse one level into parts of the structure.
--
-- @'descendMBi' = 'mapMOf' ('biplates' . 'plates')@
descendMBi :: (Monad m, Biplated a b) => (b -> m b) -> a -> m a
descendMBi = mapMOf (biplates . plates)
{-# INLINE descendMBi #-}
-}

-- | Known as @composOpM_@ in Björn Bringert's @compos@ library.
--
-- @'descendM_' = mapMOf_' 'plates'@
descendM_ :: (Monad m, Plated a) => (a -> m b) -> a -> m ()
descendM_ = mapMOf_ plates
{-# INLINE descendM_ #-}

-- | Recurse one level into a structure using a user specified recursion scheme and monadic effects. This is just 'mapMOf_', but is provided for consistency.
--
-- @'descendMOf_' = 'mapMOf_'@
--
-- @'descendMOf_' :: 'Monad' m => 'Fold' a b => (b -> m b) -> a -> m ()@
descendMOf_ :: Monad m => Getting (Sequenced m) a b -> (b -> m c) -> a -> m ()
descendMOf_ = mapMOf_
{-# INLINE descendMOf_ #-}

-- | Recurse one level into the parts delimited by one 'Traversal', using another with monadic effects.
--
-- @'descendMOnOf_' b l = 'mapMOf_' (b '.' l)@
--
-- @'descendMOnOf_' :: 'Monad' m => 'Fold' a b -> 'Fold' b b -> (b -> m b) -> a -> m ()@
descendMOnOf_ :: Monad m => Getting (Sequenced m) a b -> Getting (Sequenced m) b b -> (b -> m c) -> a -> m ()
descendMOnOf_ b l = mapMOf_ (b . l)
{-# INLINE descendMOnOf_ #-}

-- | Recurse one level into the parts of the structure delimited by a 'Traversal' with monadic effects.
--
-- @'descendMOn_' b = 'mapMOf_' (b '.' 'plates')@
--
-- @'descendMOn_' :: ('Monad' m, 'Plated' b) => 'Simple' 'Traversal' a b -> (b -> m c) -> a -> m ()@
descendMOn_ :: (Monad m, Plated b) => Getting (Sequenced m) a b -> (b -> m c) -> a -> m ()
descendMOn_ b = mapMOf_ (b . plates)
{-# INLINE descendMOn_ #-}

{-
-- | Recurse one level into parts of the structure.
--
-- @'descendMBi_' = 'mapMOf_' ('biplates' '.' 'plates')@
descendMBi_ :: (Monad m, Biplated a b) => (b -> m c) -> a -> m ()
descendMBi_ = mapMOf_ (biplates . plates)
{-# INLINE descendMBi_ #-}
-}

-- | Return a list of all of the editable contexts for every location in the structure, recursively.
--
-- @
-- propUniverse x = 'universe' x == 'map' 'pos' ('contexts' x)
-- propId x = 'all' ('==' x) [extract w | w <- 'contexts' x]
-- @
--
-- @'contexts' = 'contextsOf' 'plates'@
contexts :: Plated a => a -> [Context a a a]
contexts = contextsOf plates
{-# INLINE contexts #-}

-- | Return a list of all of the editable contexts for every location in the structure, recursively, using a user-specified 'Traversal' to walk each layer.
--
-- @
-- propUniverse l x = 'universeOf' l x == 'map' 'pos' ('contextsOf' l x)
-- propId l x = 'all' ('==' x) [extract w | w <- 'contextsOf' l x]
-- @
--
-- @'contextsOf' :: 'Simple' 'Traversal' a a -> a -> ['Context' a a]@
contextsOf :: SimpleLensLike (Kleene a a) a a -> a -> [Context a a a]
contextsOf l x = Context id x : f (holesOf l x) where
  f xs = do
    Context ctx child <- xs
    Context context y <- contextsOf l child
    return $ Context (ctx . context) y
{-# INLINE contextsOf #-}

-- | Return a list of all of the editable contexts for every location in the structure in an areas indicated by a user supplied 'Traversal', recursively using 'plates'.
--
-- @'contextsOn' b = 'contextsOnOf' b 'plates'@
--
-- @'contextsOn' :: 'Plated' b => 'Simple' 'Traversal' a b -> a -> ['Context' b b a]@
contextsOn :: Plated c => LensLike (Kleene c c) a b c c -> a -> [Context c c b]
contextsOn b = contextsOnOf b plates
{-# INLINE contextsOn #-}

{-
-- | Return a list of all of the editable contexts for every location in the structure of a given type.
--
-- @'contextsBi' = 'contextsOn' 'biplates'@
contextsBi :: Biplated a b => a -> [Context b b a]
contextsBi = contextsOn biplates
{-# INLINE contextsBi #-}
-}

-- | Return a list of all of the editable contexts for every location in the structure in an areas indicated by a user supplied 'Traversal', recursively using
-- another user-supplied 'Traversal' to walk each layer.
--
-- @'contextsOnOf' :: 'Simple' 'Traversal' a b -> 'Simple' 'Traversal' b b -> a -> ['Context' b b a]@
contextsOnOf :: LensLike (Kleene c c) a b c c -> SimpleLensLike (Kleene c c) c c -> a -> [Context c c b]
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
-- @'holes' = 'holesOf' 'plates'@
holes :: Plated a => a -> [Context a a a]
holes = holesOf plates
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
holesOf :: LensLike (Kleene c c) a b c c -> a -> [Context c c b]
holesOf l = uncurry f . plateOf l where
  f []     _ = []
  f (x:xs) g = Context (g . (:xs)) x : f xs (g . (x:))
{-# INLINE holesOf #-}

-- | An alias for 'holesOf', provided for consistency with the other combinators.
--
-- @'holesOn' = 'holesOf'@
--
-- @
-- 'holesOn' :: 'Simple' 'Iso' a b       -> a -> ['Context' b b a]
-- 'holesOn' :: 'Simple' 'Lens' a b      -> a -> ['Context' b b a]
-- 'holesOn' :: 'Simple' 'Traversal' a b -> a -> ['Context' b b a]
-- @
holesOn :: LensLike (Kleene c c) a b c c -> a -> [Context c c b]
holesOn = holesOf

{-
-- |
--
-- @'holesBi' = 'holesOf' 'biplates'@
holesBi :: Biplated a b => a -> [Context b b a]
holesBi = holesOf biplates
-}

-- | Extract one level of holes from a container in a region specified by one 'Traversal', using another.
--
-- @'holesOnOf' b l = 'holesOf' (b.l)@
--
-- @
-- 'holesOnOf' :: 'Simple 'Iso' a b       -> 'Simple' 'Iso' b b       -> a -> ['Context' b a]
-- 'holesOnOf' :: 'Simple 'Lens' a b      -> 'Simple' 'Lens' b b      -> a -> ['Context' b a]
-- 'holesOnOf' :: 'Simple 'Traversal' a b -> 'Simple' 'Traversal' b b -> a -> ['Context' b a]
-- @
holesOnOf :: LensLike (Kleene e e) a b c d -> LensLike (Kleene e e) c d e e -> a -> [Context e e b]
holesOnOf b l = holesOf (b.l)

-- | Perform a fold-like computation on each value, technically a paramorphism.
--
-- @'paraOf' :: 'Fold' a a -> (a -> [r] -> r) -> a -> r@
paraOf :: Getting [a] a a -> (a -> [r] -> r) -> a -> r
paraOf l f = go where
  go a = f a (go <$> toListOf l a)
{-# INLINE paraOf #-}

-- | Perform a fold-like computation on each value, technically a paramorphism.
para :: Plated a => (a -> [r] -> r) -> a -> r
para = paraOf plates
{-# INLINE para #-}

-- $compos
--
-- Provided for compatibility with Björn Bringert's @compos@ library.

-- | Fold the immediate children of a 'Plated' container.
--
-- @'composOpFold' z c f = 'foldrOf' 'plates' (c '.' f) z@
composOpFold :: Plated a => b -> (b -> b -> b) -> (a -> b) -> a -> b
composOpFold z c f = foldrOf plates (c . f) z
{-# INLINE composOpFold #-}

-- |
--
-- @'composOpMonoid' = 'foldMapOf' 'plates'@
composOpMonoid :: (Plated a, Monoid m) => (a -> m) -> a -> m
composOpMonoid = foldMapOf plates
{-# INLINE composOpMonoid #-}

-- |
--
-- @'composOpMPlus' = 'msumOf' 'plates'@
composOpMPlus :: (MonadPlus m, Plated a) => (a -> m b) -> a -> m b
composOpMPlus f = msumOf (plates . to f)
{-# INLINE composOpMPlus #-}

{-
-- We could define plates using the Kleene store comonad, and retain more or less the existing API:

type Type a b = Kleene b b a

plateSelf :: a -> Kleene a a a
plateSelf = More (Done id)

infix 4 |-, |+, |*, ||*, ||+

(|-) :: Kleene c c (a -> b) -> a -> Kleene c c b
w |- a = fmap ($a) w
{-# INLINE (|-) #-}

(|+) :: Biplated a c => Kleene c c (a -> b) -> a -> Kleene c c b
w |+ a = w <*> biplates plateSelf a
{-# INLINE (|+) #-}

(|*) :: Kleene a a (a -> b) -> a -> Kleene a a b
(|*) = More
{-# INLINE (|*) #-}

(||*) :: Traversable f => Kleene a a (f a -> b) -> f a -> Kleene a a b
w ||* as = w <*> traverse plateSelf as
{-# INLINE (||*) #-}

(||+) :: (Traversable f, Biplated a c) => Kleene c c (f a -> b) -> f a -> Kleene c c b
w ||+ a = w <*> traverse (biplates plateSelf) a
{-# INLINE (||+) #-}

plated :: Applicative f => (a -> Kleene c d b) -> (c -> f d) -> a -> f b
plated = cloneTraversal . const
{-# INLINE plated #-}

-}
