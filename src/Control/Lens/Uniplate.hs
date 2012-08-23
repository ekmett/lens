{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Lens.Uniplate
  ( Uniplate(..), uniplate, uniplateOf, biplateOf
  , rewrite, rewriteOf, rewriteOn, rewriteOnOf
  , rewriteM, rewriteMOf, rewriteMOn, rewriteMOnOf
  , universe, universeOf, universeOn, universeOnOf
  , transform, transformOf, transformOn, transformOnOf
  , transformM, transformMOf, transformMOn, transformMOnOf
  , descend, descendOf, descendOn, descendOnOf
  , descendA, descendAOf, descendAOn, descendAOnOf
  , descendM, descendMOf, descendMOn, descendMOnOf
  )
  where

import Control.Lens.Setter
import Control.Lens.Type
import Control.Lens.Fold
import Control.Lens.Traversal
import Control.Lens.Internal
import Control.Applicative

class Uniplate t where
  underneath :: Simple Traversal t t
  underneath _ = pure

rewrite :: Uniplate a => (a -> Maybe a) -> a -> a
rewrite = rewriteOf underneath
{-# INLINE rewrite #-}

-- | Rewrite by applying a rule everywhere you can. Ensures that the rule cannot
-- be applied anywhere in the result:
--
-- > propRewriteOf l r x = all (isNothing . r) (universeOf l (rewriteOf l r x))
--
-- Usually 'transformOf' is more appropriate, but 'rewriteOf' can give better
-- compositionality. Given two single transformations @f@ and @g@, you can
-- construct @f `mplus` g@ which performs both rewrites until a fixed point.
rewriteOf :: SimpleSetting a a -> (a -> Maybe a) -> a -> a
rewriteOf l f = go where
  go = transformOf l (\x -> maybe x go (f x))
{-# INLINE rewriteOf #-}

rewriteOn :: Uniplate b => SimpleSetting a b -> (b -> Maybe b) -> a -> a
rewriteOn b = over b . rewrite
{-# INLINE rewriteOn #-}

rewriteOnOf :: SimpleSetting a b -> SimpleSetting b b -> (b -> Maybe b) -> a -> a
rewriteOnOf b l = over b . rewriteOf l
{-# INLINE rewriteOnOf #-}

rewriteM :: (Monad m, Uniplate a) => (a -> m (Maybe a)) -> a -> m a
rewriteM = rewriteMOf underneath
{-# INLINE rewriteM #-}

rewriteMOf :: Monad m => SimpleLensLike (WrappedMonad m) a a -> (a -> m (Maybe a)) -> a -> m a
rewriteMOf l f = go where
  go = transformMOf l (\x -> f x >>= maybe (return x) go)
{-# INLINE rewriteMOf #-}

rewriteMOn :: (Monad m, Uniplate b) => SimpleLensLike (WrappedMonad m) a b -> (b -> m (Maybe b)) -> a -> m a
rewriteMOn b = mapMOf b . rewriteM
{-# INLINE rewriteMOn #-}

rewriteMOnOf :: Monad m => SimpleLensLike (WrappedMonad m) a b -> SimpleLensLike (WrappedMonad m) b b -> (b -> m (Maybe b)) -> a -> m a
rewriteMOnOf b l = mapMOf b . rewriteMOf l
{-# INLINE rewriteMOnOf #-}

universe :: Uniplate t => t -> [t]
universe = universeOf underneath
{-# INLINE universe #-}

universeOn :: Uniplate t => Fold f t -> f -> [t]
universeOn b = foldMapOf b universe
{-# INLINE universeOn #-}

-- | given a traversal that knows how to read the children,
-- get all of the descendants, including itself
universeOf :: Fold a a -> a -> [a]
universeOf l a = a : foldMapOf l (universeOf l) a
{-# INLINE universeOf #-}

universeOnOf :: Fold a b -> Fold b b -> a -> [b]
universeOnOf b = foldMapOf b . universeOf
{-# INLINE universeOnOf #-}

transform :: Uniplate a => (a -> a) -> a -> a
transform = transformOf underneath
{-# INLINE transform #-}

transformOn :: Uniplate b => SimpleSetting a b -> (b -> b) -> a -> a
transformOn b = over b . transform
{-# INLINE transformOn #-}

transformOf :: SimpleSetting a a -> (a -> a) -> a -> a
transformOf l f = go where
  go = f . over l go
{-# INLINE transformOf #-}

transformOnOf :: SimpleSetting a b -> SimpleSetting b b -> (b -> b) -> a -> a
transformOnOf b l = over b . transformOf l
{-# INLINE transformOnOf #-}

transformM :: (Monad m, Uniplate a) => (a -> m a) -> a -> m a
transformM = transformMOf underneath
{-# INLINE transformM #-}

transformMOn :: (Monad m, Uniplate b) => SimpleLensLike (WrappedMonad m) a b -> (b -> m b) -> a -> m a
transformMOn b = mapMOf b . transformM
{-# INLINE transformMOn #-}

-- | Transform every element in a tree in a bottom up manner with a monadic effect
transformMOf :: Monad m => SimpleLensLike (WrappedMonad m) a a -> (a -> m a) -> a -> m a
transformMOf l f = go where
  go t = mapMOf l go t >>= f
{-# INLINE transformMOf #-}

transformMOnOf :: Monad m => SimpleLensLike (WrappedMonad m) a b -> SimpleLensLike (WrappedMonad m) b b -> (b -> m b) -> a -> m a
transformMOnOf b l = mapMOf b . transformMOf l
{-# INLINE transformMOnOf #-}

-- | @'descend' = 'over' 'underneath'@
descend :: Uniplate a => (a -> a) -> a -> a
descend = over underneath
{-# INLINE descend #-}

-- | @'descendOf' = 'over'@
descendOf :: SimpleSetting a a -> (a -> a) -> a -> a
descendOf = over
{-# INLINE descendOf #-}

-- | @'descendOnOf' b l = 'over' (b.l)@
descendOnOf :: SimpleSetting a b -> SimpleSetting b b -> (b -> b) -> a -> a
descendOnOf b l = over (b.l)
{-# INLINE descendOnOf #-}

-- | @'descendOn' b = 'over' (b . 'underneath')@
descendOn :: Uniplate b => SimpleSetting a b -> (b -> b) -> a -> a
descendOn b = over (b.underneath)
{-# INLINE descendOn #-}

-- | @'descendA' = 'underneath'@
descendA :: (Applicative f, Uniplate a) => (a -> f a) -> a -> f a
descendA = underneath
{-# INLINE descendA #-}

-- | @'descendAOf' = 'id'@
descendAOf :: Applicative f => SimpleLensLike f a a -> (a -> f a) -> a -> f a
descendAOf = id
{-# INLINE descendAOf #-}

-- | @'descendAOnOf' = ('.')@
descendAOnOf :: Applicative f => SimpleLensLike f a b -> SimpleLensLike f b b -> (b -> f b) -> a -> f a
descendAOnOf = (.)
{-# INLINE descendAOnOf #-}

-- | @'descendAOn' b = b . 'underneath'@
descendAOn :: (Applicative f, Uniplate b) => SimpleLensLike f a b -> (b -> f b) -> a -> f a
descendAOn b = b . underneath
{-# INLINE descendAOn #-}

-- | @'descendM' = 'mapMOf' 'underneath'@
descendM :: (Monad m, Uniplate a) => (a -> m a) -> a -> m a
descendM = mapMOf underneath
{-# INLINE descendM #-}

-- | @'descendMOf' = 'mapMOf'@
descendMOf :: Monad m => SimpleLensLike (WrappedMonad m) a a -> (a -> m a) -> a -> m a
descendMOf = mapMOf
{-# INLINE descendMOf #-}

-- | @'descendMOnOf' b l = 'mapMOf' (b '.' l)@
descendMOnOf :: Monad m => SimpleLensLike (WrappedMonad m) a b -> SimpleLensLike (WrappedMonad m) b b -> (b -> m b) -> a -> m a
descendMOnOf b l = mapMOf (b . l)
{-# INLINE descendMOnOf #-}

-- | @'descendMOn' b = 'mapMOf' (b . 'underneath')@
descendMOn :: (Monad m, Uniplate b) => SimpleLensLike (WrappedMonad m) a b -> (b -> m b) -> a -> m a
descendMOn b = mapMOf (b . underneath)
{-# INLINE descendMOn #-}

-- holesOf :: SimpleLens

-- holesOf :: SimpleLensLike (Kleene a a) a a -> a -> [(a, a -> a)]
-- holesOf :: LensLike (Kleene c c) a b c c -> a -> [(c, c -> b)]
-- holesOf

-- | This is slightly safer than Neil Mitchell's uniplate, as it gracefully deals with under and over-supply.
uniplate :: Uniplate a => a -> ([a], [a] -> a)
uniplate = uniplateOf underneath
{-# INLINE uniplate #-}

uniplateOf :: LensLike (Kleene a a) a b a a -> a -> ([a], [a] -> b)
uniplateOf l a = (ins k, \cs -> outs cs k) where
  k = l (More (Done id)) a
{-# INLINE uniplateOf #-}

-- | This is rather dangerous. If you do not supply back (at least) as many elements of type @b@ as you are given then this
-- will silently error out!
biplateOf :: LensLike (Kleene c d) a b c d -> a -> ([c], [d] -> b)
biplateOf l a = (ins k, \cs -> outs2 cs k) where
  k = l (More (Done id)) a
{-# INLINE biplateOf #-}

ins :: Kleene c d a -> [c]
ins (More ys c) = c:ins ys
ins Done{} = []

outs :: [c] -> Kleene c c a -> a
outs (c:cs) (More ys _) = outs cs $ ($c) <$> ys
outs _ xs = extractKleene xs

outs2 :: [d] -> Kleene c d a -> a
outs2 (d:ds) (More ys _) = outs2 ds $ ($d) <$> ys
outs2 _      (Done a)    = a
outs2 []     More{}      = error "biplateOf: expected more children"

-- for testing
data Tree a = Bin (Tree a) (Tree a) | Tip a
  deriving (Eq,Ord,Show,Read)

instance Uniplate (Tree a) where
  underneath f (Bin l r) = Bin <$> f l <*> f r
  underneath _ t = pure t
