{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-------------------------------------------------------------------------------
-- |
-------------------------------------------------------------------------------
module Control.Lens.Plated
  ( Plated(..), uniplate, plateOf
  , rewrite, rewriteOf, rewriteOn, rewriteOnOf
  , rewriteM, rewriteMOf, rewriteMOn, rewriteMOnOf
  , universe, universeOf, universeOn, universeOnOf
  , transform, transformOf, transformOn, transformOnOf
  , transformM, transformMOf, transformMOn, transformMOnOf
  , descend, descendOf, descendOn, descendOnOf
  , descendA, descendAOf, descendAOn, descendAOnOf
  , descendM, descendMOf, descendMOn, descendMOnOf
  , contexts, contextsOf, contextsOn, contextsOnOf
  , holes, holesOf
  )
  where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens.Setter
import Control.Lens.Type
import Control.Lens.Fold
import Control.Lens.Traversal
import Control.Lens.Internal

-- | A safer version of Neil Mitchell's @Uniplate@.
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
class Plated t where
  -- | 'Traversal' of the immediate children of this structure.
  plates :: Simple Traversal t t
  plates _ = pure

instance Plated [a] where
  plates _ [] = pure []
  plates f (x:xs) = (x :) <$> f xs

-- | Neil Mitchell's 'uniplate' combinator, implemented in terms of 'Plated'.
--
-- @'uniplate' = 'plateOf' 'plates'@
uniplate :: Plated a => a -> ([a], [a] -> a)
uniplate = plateOf plates
{-# INLINE uniplate #-}

-- |
-- 'plateOf' turns a 'Traversal' into a uniplate (or biplate).
--
-- The resulting plate is actually safer to use as it replaces ignores 'over-application' and
-- deals gracefully with under-application.
--
-- @
-- plateOf :: 'Simple' 'Control.Lens.Iso.Iso' a b       -> a -> ([b], [b] -> a)
-- plateOf :: 'Simple' 'Lens' a b      -> a -> ([b], [b] -> a)
-- plateOf :: 'Simple' 'Traversal' a b -> a -> ([b], [b] -> a)
-- @
plateOf :: LensLike (Kleene c c) a b c c -> a -> ([c], [c] -> b)
plateOf l = (ins &&& outs) . l (More (Done id))
{-# INLINE plateOf #-}

ins :: Kleene c d a -> [c]
ins (More ys c) = c : ins ys
ins _ = []

outs :: Kleene c c a -> [c] -> a
outs (More ys _) (c:cs) = outs (fmap ($c) ys) cs
outs xs          _      = extractKleene xs

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
-- rewriteOf :: 'Simple' 'Control.Lens.Iso.Iso' a a       -> (a -> 'Maybe' a) -> a -> a
-- rewriteOf :: 'Simple' 'Lens' a a      -> (a -> 'Maybe' a) -> a -> a
-- rewriteOf :: 'Simple' 'Traversal' a a -> (a -> 'Maybe' a) -> a -> a
-- rewriteOf :: 'Simple' 'Setter' a a    -> (a -> 'Maybe' a) -> a -> a
-- @
rewriteOf :: SimpleSetting a a -> (a -> Maybe a) -> a -> a
rewriteOf l f = go where
  go = transformOf l (\x -> maybe x go (f x))
{-# INLINE rewriteOf #-}

-- | Rewrite recursively over part of a larger structure.
--
-- @
-- rewriteOn :: 'Plated' c => 'Simple' 'Control.Lens.Iso.Iso' a b       -> (b -> 'Maybe' b) -> a -> a
-- rewriteOn :: 'Plated' c => 'Simple' 'Lens' a b      -> (b -> 'Maybe' b) -> a -> a
-- rewriteOn :: 'Plated' c => 'Simple' 'Traversal' a b -> (b -> 'Maybe' b) -> a -> a
-- rewriteOn :: 'Plated' c => 'Simple' 'Setting' a b   -> (b -> 'Maybe' b) -> a -> a
-- @
rewriteOn :: Plated c => Setting a b c c -> (c -> Maybe c) -> a -> b
rewriteOn b = over b . rewrite
{-# INLINE rewriteOn #-}

-- | Rewrite recursively over part of a larger structure using a specified setter.
--
-- @
-- rewriteOnOf :: 'Plated' b => 'Simple' 'Control.Lens.Iso.Iso' a b       -> 'Simple' 'Control.Lens.Iso.Iso' b b       -> (b -> 'Maybe' b) -> a -> a
-- rewriteOnOf :: 'Plated' b => 'Simple' 'Lens' a b      -> 'Simple' 'Lens' b b      -> (b -> 'Maybe' b) -> a -> a
-- rewriteOnOf :: 'Plated' b => 'Simple' 'Traversal' a b -> 'Simple' 'Traversal' b b -> (b -> 'Maybe' b) -> a -> a
-- rewriteOnOf :: 'Plated' b => 'Simple' 'Setter' a b    -> 'Simple' 'Setter' b b    -> (b -> 'Maybe' b) -> a -> a
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

-- | Get all the descendants of a node, including itself.
--
-- >>> data Expr = Val Int | Neg Expr | Add Expr Expr
-- >>> instance Plated Expr where plates f (Neg e) = Neg <$> f e; plates f (Add a b) = Add <$> f a <*> f b; plates _ a = pure a
-- >>> universe (Add (Val 1) (Neg (Val 2)))
-- [Add (Val 1) (Neg (Val 2)), Val 1, Neg (Val 2), Val 2]
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
-- universeOn :: 'Plated' b => 'Getter' a b           -> a -> [b]
-- universeOn :: 'Plated' b => 'Fold' a b             -> a -> [b]
-- universeOn :: 'Plated' b => 'Simple' 'Lens' a b      -> a -> [b]
-- universeOn :: 'Plated' b => 'Simple' 'Traversal' a b -> a -> [b]
-- universeOn :: 'Plated' b => 'Simple' 'Iso' a b       -> a -> [b]
-- @
universeOn :: Plated b => Fold a b -> a -> [b]
universeOn b = foldMapOf b universe
{-# INLINE universeOn #-}

-- | Given a traversal that knows how to locate immediate children, retrieve all of the transitive descendants of a node, including itself.
universeOf :: Fold a a -> a -> [a]
universeOf l = go where
  go a = a : foldMapOf l go a
{-# INLINE universeOf #-}

-- | Given a traversal that knows how to locate immediate children, retrieve all of the transitive descendants of a node, including itself that lie
-- in a region indicated by another fold.
universeOnOf :: Fold a b -> Fold b b -> a -> [b]
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
-- transformOn :: 'Plated' b => 'Simple' 'Iso' a b       -> (b -> b) -> a -> a
-- transformOn :: 'Plated' b => 'Simple' 'Lens' a b      -> (b -> b) -> a -> a
-- transformOn :: 'Plated' b => 'Simple' 'Traversal' a b -> (b -> b) -> a -> a
-- transformOn :: 'Plated' b => 'Simple' 'Setter' a b    -> (b -> b) -> a -> a
-- @
transformOn :: Plated c => Setting a b c c -> (c -> c) -> a -> b
transformOn b = over b . transform
{-# INLINE transformOn #-}

-- | Transform every element by recursively applying a given 'Setter' in a bottom-up manner.
--
-- @
-- transformOf :: 'Simple' 'Iso' a a       -> (a -> a) -> a -> a
-- transformOf :: 'Simple' 'Lens' a a      -> (a -> a) -> a -> a
-- transformOf :: 'Simple' 'Traversal' a a -> (a -> a) -> a -> a
-- transformOf :: 'Simple' 'Setter' a a    -> (a -> a) -> a -> a
-- @
transformOf :: SimpleSetting a a -> (a -> a) -> a -> a
transformOf l f = go where
  go = f . over l go
{-# INLINE transformOf #-}

-- | Transform every element in a region indicated by a 'Setter' by recursively applying another 'Setter'
-- in a bottom-up manner.
--
-- @
-- transformOnOf :: 'Setter' a b -> 'Simple' 'Iso' b b       -> (b -> b) -> a -> a
-- transformOnOf :: 'Setter' a b -> 'Simple' 'Lens' b b      -> (b -> b) -> a -> a
-- transformOnOf :: 'Setter' a b -> 'Simple' 'Traversal' b b -> (b -> b) -> a -> a
-- transformOnOf :: 'Setter' a b -> 'Simple' 'Setter' b b    -> (b -> b) -> a -> a
-- @
transformOnOf :: Setting a b c c -> SimpleSetting c c -> (c -> c) -> a -> b
transformOnOf b l = over b . transformOf l
{-# INLINE transformOnOf #-}

transformM :: (Monad m, Plated a) => (a -> m a) -> a -> m a
transformM = transformMOf plates
{-# INLINE transformM #-}

transformMOn :: (Monad m, Plated c) => LensLike (WrappedMonad m) a b c c -> (c -> m c) -> a -> m b
transformMOn b = mapMOf b . transformM
{-# INLINE transformMOn #-}

-- | Transform every element in a tree in a bottom up manner with a monadic effect
transformMOf :: Monad m => SimpleLensLike (WrappedMonad m) a a -> (a -> m a) -> a -> m a
transformMOf l f = go where
  go t = mapMOf l go t >>= f
{-# INLINE transformMOf #-}

transformMOnOf :: Monad m => LensLike (WrappedMonad m) a b c c -> SimpleLensLike (WrappedMonad m) c c -> (c -> m c) -> a -> m b
transformMOnOf b l = mapMOf b . transformMOf l
{-# INLINE transformMOnOf #-}

-- | @'descend' = 'over' 'plates'@
descend :: Plated a => (a -> a) -> a -> a
descend = over plates
{-# INLINE descend #-}

-- | @'descendOf' = 'over'@
descendOf :: Setting a b c d -> (c -> d) -> a -> b
descendOf = over
{-# INLINE descendOf #-}

-- | @'descendOnOf' b l = 'over' (b.l)@
descendOnOf :: Setting a b c d -> Setting c d e f -> (e -> f) -> a -> b
descendOnOf b l = over (b.l)
{-# INLINE descendOnOf #-}

-- | @'descendOn' b = 'over' (b . 'plates')@
descendOn :: Plated c => Setting a b c c -> (c -> c) -> a -> b
descendOn b = over (b . plates)
{-# INLINE descendOn #-}

-- | @'descendA' = 'plates'@
descendA :: (Applicative f, Plated a) => (a -> f a) -> a -> f a
descendA = plates
{-# INLINE descendA #-}

-- | @'descendAOf' = 'id'@
descendAOf :: Applicative f => LensLike f a b c d -> (c -> f d) -> a -> f b
descendAOf = id
{-# INLINE descendAOf #-}

-- | @'descendAOnOf' = ('.')@
descendAOnOf :: Applicative g => LensLike g a b c d -> LensLike g c d e f -> (e -> g f) -> a -> g b
descendAOnOf = (.)
{-# INLINE descendAOnOf #-}

-- | @'descendAOn' b = b . 'plates'@
descendAOn :: (Applicative f, Plated c) => LensLike f a b c c -> (c -> f c) -> a -> f b
descendAOn b = b . plates
{-# INLINE descendAOn #-}

-- | @'descendM' = 'mapMOf' 'plates'@
descendM :: (Monad m, Plated a) => (a -> m a) -> a -> m a
descendM = mapMOf plates
{-# INLINE descendM #-}

-- | @'descendMOf' = 'mapMOf'@
descendMOf :: Monad m => LensLike (WrappedMonad m) a b c d -> (c -> m d) -> a -> m b
descendMOf = mapMOf
{-# INLINE descendMOf #-}

-- | @'descendMOnOf' b l = 'mapMOf' (b '.' l)@
descendMOnOf :: Monad m => LensLike (WrappedMonad m) a b c c -> SimpleLensLike (WrappedMonad m) c c -> (c -> m c) -> a -> m b
descendMOnOf b l = mapMOf (b . l)
{-# INLINE descendMOnOf #-}

-- | @'descendMOn' b = 'mapMOf' (b . 'plates')@
descendMOn :: (Monad m, Plated c) => LensLike (WrappedMonad m) a b c c -> (c -> m c) -> a -> m b
descendMOn b = mapMOf (b . plates)
{-# INLINE descendMOn #-}

-- | Return all a list of editable contexts for every location in the structure.
--
-- @
-- propUniverse x = 'universe' x == 'map' 'fst' ('contexts' x)
-- propId x = 'all' ('==' x) [b a | (a,b) <- 'contexts' x]
-- @
contexts :: Plated a => a -> [(a, a -> a)]
contexts = contextsOf plates
{-# INLINE contexts #-}

contextsOf :: SimpleLensLike (Kleene a a) a a -> a -> [(a, a -> a)]
contextsOf l x = (x,id) : f (holesOf l x) where
  f xs = do
    (child, ctx) <- xs
    (y, context) <- contextsOf l child
    return (y, ctx . context)
{-# INLINE contextsOf #-}

contextsOn :: Plated c => LensLike (Kleene c c) a b c c -> a -> [(c, c -> b)]
contextsOn b = contextsOnOf b plates
{-# INLINE contextsOn #-}

contextsOnOf :: LensLike (Kleene c c) a b c c -> SimpleLensLike (Kleene c c) c c -> a -> [(c, c -> b)]
contextsOnOf b l = f . holesOf b where
  f xs = do
    (child, ctx) <- xs
    (y, context) <- contextsOf l child
    return (y, ctx . context)
{-# INLINE contextsOnOf #-}

holes :: Plated a => a -> [(a, a -> a)]
holes = holesOf plates
{-# INLINE holes #-}

-- | The one depth version of 'contextsOf'
--
-- @
-- propChildren l x = 'childrenOf' l x '==' 'map' 'fst' ('holesOf' l x)
-- propId l x = 'all' ('==' x) [b a | (a,b) <- 'holesOf' l x]
-- @
holesOf :: LensLike (Kleene c c) a b c c -> a -> [(c, c -> b)]
holesOf l = uncurry f . plateOf l where
  f [] _ = []
  f (x:xs) gen = (x, gen . (:xs)) : f xs (gen . (x:))
{-# INLINE holesOf #-}



-- $expr
--
-- >>> data Tree a = Bin (Tree a) (Tree a) | Tip a
-- >>> instance Plated (Tree a) where plates f (Bin l r) = Bin <$> f l <*> f r; plates _ a = pure a
