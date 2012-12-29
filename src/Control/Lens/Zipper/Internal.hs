{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Zipper.Internal
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides internal types and functions used in the implementation
-- of @Control.Lens.Zipper@. You shouldn't need to import it directly, and the
-- exported types can be used to break 'Zipper' invariants.
--
----------------------------------------------------------------------------
module Control.Lens.Zipper.Internal where

import Control.Applicative
import Control.Category ((>>>))
import Control.Monad
import Control.Lens.Magma
import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Lens
import Control.Lens.Setter
import Control.Lens.Traversal
import Control.Lens.Type
import Data.Maybe
import Data.Monoid

{-# ANN module "HLint: ignore Use foldl" #-}

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Char

data Path i a
  = ApL Int Bool Bool (Last i) !(Path i a) !(Magma i a)
  | ApR Int Bool Bool (Last i) !(Magma i a) !(Path i a)
  | Start
  deriving Show

instance Functor (Path i) where
  fmap f (ApL m nl nr li p q) = ApL m nl nr li (fmap f p) (fmap f q)
  fmap f (ApR m nl nr li p q) = ApR m nl nr li (fmap f p) (fmap f q)
  fmap _ Start = Start
  {-# INLINE fmap #-}

offset :: Path i a -> Int
offset Start           = 0
offset (ApL _ _ _ _ q _) = offset q
offset (ApR _ _ _ _ l q) = size l + offset q
{-# INLINE offset #-}

pathsize :: Path i a -> Int
pathsize = go 1 where
  go n Start = n
  go _ (ApL n _ _ _ p _) = go n p
  go _ (ApR n _ _ _ _ p) = go n p
{-# INLINE pathsize #-}

-- For several operations, we unroll the first step of the recursion (or part
-- of it) so GHC can inline better. There are two specific cases that we care
-- about: The "lens case", where the entire tree is just (Leaf (Identity x)), and the
-- "list case", where the traversal tree is right-biased, as in (Ap (Leaf (Identity x))
-- (Ap (Leaf (Identity y)) ...)). It should be safe to delete any of these cases.

recompress :: Path i a -> i -> a -> Magma i a
recompress Start i a = Leaf i a -- Unrolled: The lens case.
recompress (ApL m _ _ li Start r) i a = Ap m False False li (Leaf i a) r -- Unrolled: The list case. In particular, a right-biased tree that we haven't moved rightward in.
recompress p i a = go p (Leaf i a) where
  go Start              q = q
  go (ApL m _ _ li q r) l = go q (Ap m False False li l r)
  go (ApR m _ _ li l q) r = go q (Ap m False False li l r)
{-# INLINE recompress #-}

-- walk down the compressed tree to the leftmost child.
startl :: Path i a -> Magma i a -> r -> (Path i a -> i -> a -> r) -> r
startl p0 (Leaf i a) _ kp = kp p0 i a -- Unrolled: The lens case.
startl p0 (Ap m nl nr li (Leaf i a) r) _ kp = kp (ApL m nl nr li p0 r) i a -- Unrolled: The list case. (Is this one a good idea?)
startl p0 c0 kn kp = go p0 c0 where
  go p (Ap m nl nr li l r)
    | nullLeft l  = go (ApR m nl nr li Pure p) r
    | otherwise   = go (ApL m nl nr li p r) l
  go p (Leaf i a) = kp p i a
  go _ Pure       = kn
{-# INLINE startl #-}

startr :: Path i a -> Magma i a -> r -> (Path i a -> i -> a -> r) -> r
startr p0 (Leaf i a) _ kp = kp p0 i a -- Unrolled: The lens case.
startr p0 c0 kn kp = go p0 c0 where
  go p (Ap m nl nr li l r)
     | nullRight r = go (ApL m nl nr li p Pure) l
     | otherwise   = go (ApR m nl nr li l p) r
  go p (Leaf i a)  = kp p i a
  go _ Pure        = kn
{-# INLINE startr #-}

movel :: Path i a -> Magma i a -> r -> (Path i a -> i -> a -> r) -> r
movel p0 c0 kn kp = go p0 c0 where
  go Start _ = kn
  go (ApR m _ _ li l q) r
    | nullRight l = go q (Ap m False False li l Pure)
    | otherwise   = startr (ApL m False False li q r) l kn kp
  go (ApL m _ _ li p r) l = go p (Ap m False False li l r)
{-# INLINE movel #-}

mover :: Path i a -> Magma i a -> r -> (Path i a -> i -> a -> r) -> r
mover p0 c0 kn kp = go p0 c0 where
  go Start _ = kn
  go (ApL m _ _ li q r) l
    | nullLeft r  = go q (Ap m False False li Pure r)
    | otherwise   = startl (ApR m False False li l q) r kn kp
  go (ApR m _ _ li l p) r = go p (Ap m False False li l r)
{-# INLINE mover #-}

-----------------------------------------------------------------------------
-- * Zippers
-----------------------------------------------------------------------------

-- | This is used to represent the 'Top' of the 'Zipper'.
--
-- Every 'Zipper' starts with 'Top'.
--
-- /e.g./ @'Top' ':>' a@ is the type of the trivial 'Zipper'.
data Top


-- | This is the type of a 'Zipper'. It visually resembles a \"breadcrumb trail\" as
-- used in website navigation. Each breadcrumb in the trail represents a level you
-- can move up to.
--
-- This type operator associates to the left, so you can use a type like
--
-- @'Top' ':>' ('String','Double') ':>' 'String' ':>' 'Char'@
--
-- to represent a zipper from @('String','Double')@ down to 'Char' that has an intermediate
-- crumb for the 'String' containing the 'Char'.
--
-- You can construct a zipper into *any* data structure with 'zipper'.
--
-- You can repackage up the contents of a zipper with 'rezip'.
--
-- >>> rezip $ zipper 42
-- 42
--
-- The combinators in this module provide lot of things you can do to the zipper while you
-- have it open.
--
-- Note that a value of type @h ':>' s ':>' a@ doesn't actually contain a value
-- of type @h ':>' s@ -- as we descend into a level, the previous level is
-- unpacked and stored in 'Coil' form. Only one value of type @_ ':>' _@ exists
-- at any particular time for any particular 'Zipper'.

data Zipper h i a = Ord i => Zipper !(Coil h i a) !(Path i a) i a

-- Top :>> Map String Int :> Int :@ String :>> Bool

infixr 9 :@
data (:@) a i

infixl 8 :>
type family (:>) h p
type instance h :> (a :@ i) = Zipper h i a

infixl 8 :>>
type h :>> a = Zipper h Int a

-- | This represents the type a 'Zipper' will have when it is fully 'Zipped' back up.
type family Zipped h a
type instance Zipped Top a      = a
type instance Zipped (Zipper h i s) a = Zipped h s

-- | A 'Coil' is a linked list of the levels above the current one. The length
-- of a 'Coil' is known at compile time.
--
-- This is part of the internal structure of a zipper. You shouldn't need to manipulate this directly.
#ifndef HLINT
data Coil t i a where
  Coil :: Coil Top Int a
  Snoc :: Ord i => !(Coil h j s) -> AnIndexedTraversal' i s a -> !(Path j s) -> j -> (Magma i a -> s) -> Coil (Zipper h j s) i a
#endif

--downward :: forall j h s a. ALens' s a -> h :> s:@j -> h :> s:@j :> a:@Int
--downward l (Zipper h p j s) = Zipper (Snoc h l' p j go) Start 0 (s^.l')

-- | This 'Lens' views the current target of the 'Zipper'.
focus :: IndexedLens' i (Zipper h i a) a
focus f (Zipper h p i a) = Zipper h p i <$> indexed f i a
{-# INLINE focus #-}

-- | Construct a 'Zipper' that can explore anything, and start it at the top.
zipper :: a -> Top :>> a
zipper = Zipper Coil Start 0
{-# INLINE zipper #-}

-- | Return the index of the focus.
focalPoint :: Zipper h i a -> i
focalPoint (Zipper _ _ i _) = i
{-# INLINE focalPoint #-}

-- | Return the index into the current 'Traversal' within the current level of the 'Zipper'.
--
-- @'jerkTo' ('tooth' l) l = Just'@
--
-- Mnemonically, zippers have a number of 'teeth' within each level. This is which 'tooth' you are currently at.
--
-- This is based on ordinal position regardless of the underlying index type. It may be excessively expensive for a list.
--
-- 'focalPoint' may be much cheaper if you have a 'Traversal' indexed by ordinal position!
tooth :: Zipper h i a -> Int
tooth (Zipper _ p _ _) = offset p
{-# INLINE tooth #-}

-- | Move the 'Zipper' 'upward', closing the current level and focusing on the parent element.
--
-- NB: Attempts to move upward from the 'Top' of the 'Zipper' will fail to typecheck.
--
upward :: Ord j => h :> s:@j :> a:@i -> h :> s:@j
-- upward :: Zipper (Zipper h i s) j a -> Zipper h i s
upward (Zipper (Snoc h _ p j k) q i x) = Zipper h p j $ k $ recompress q i x
{-# INLINE upward #-}

-- | Jerk the 'Zipper' one 'tooth' to the 'rightward' within the current 'Lens' or 'Traversal'.
--
-- Attempts to move past the start of the current 'Traversal' (or trivially, the current 'Lens')
-- will return 'Nothing'.
--
-- >>> isNothing $ zipper "hello" & rightward
-- True
--
-- >>> zipper "hello" & fromWithin traverse & rightward <&> view focus
-- 'e'
--
-- >>> zipper "hello" & fromWithin traverse & rightward <&> focus .~ 'u' <&> rezip
-- "hullo"
--
-- >>> rezip $ zipper (1,2) & fromWithin both & tug rightward & focus .~ 3
-- (1,3)
rightward :: MonadPlus m => (h :> a:@i) -> m (h :> a:@i)
rightward (Zipper h p i a) = mover p (Leaf i a) mzero $ \q j b -> return $ Zipper h q j b
{-# INLINE rightward #-}

-- | Jerk the 'zipper' 'leftward' one 'tooth' within the current 'Lens' or 'Traversal'.
--
-- Attempts to move past the end of the current 'Traversal' (or trivially, the current 'Lens')
-- will return 'Nothing'.
--
-- >>> isNothing $ zipper "hello" & leftward
-- True

-- >>> isNothing $ zipper "hello" & within traverse >>= leftward
-- True
--
-- >>> zipper "hello" & within traverse <&> tug leftward
-- Just 'h'
--
-- >>> zipper "hello" & fromWithin traverse & tug rightward & tug leftward & view focus
-- 'h'
leftward :: MonadPlus m => (h :> a:@i) -> m (h :> a:@i)
leftward (Zipper h p i a) = movel p (Leaf i a) mzero $ \q j b -> return $ Zipper h q j b
{-# INLINE leftward #-}

-- | Move to the leftmost position of the current 'Traversal'.
--
-- This is just a convenient alias for @'farthest' 'leftward'@.
--
-- >>> zipper "hello" & fromWithin traverse & rightmost & focus .~ 'a' & rezip
-- "hella"
leftmost :: (a :> b:@i) -> a :> b:@i
leftmost (Zipper h p i a) = startl Start (recompress p i a) (error "leftmost: bad Magma structure") (Zipper h)
{-# INLINE leftmost #-}

-- | Move to the rightmost position of the current 'Traversal'.
--
-- This is just a convenient alias for @'farthest' 'rightward'@.
--
-- >>> zipper "hello" & fromWithin traverse & rightmost & focus .~ 'y' & leftmost & focus .~ 'j' & rezip
-- "jelly"
rightmost :: (a :> b:@i) -> a :> b:@i
rightmost (Zipper h p i a) = startr Start (recompress p i a) (error "rightmost: bad Magma structure") (Zipper h)
{-# INLINE rightmost #-}

-- | This allows you to safely 'tug leftward' or 'tug rightward' on a 'zipper'. This
-- will attempt the move, and stay where it was if it fails.
--
-- The more general signature allows its use in other circumstances, however.
--
-- @'tug' f x ≡ 'fromMaybe' a (f a)@
--
-- >>> fmap rezip $ zipper "hello" & within traverse <&> tug leftward <&> focus .~ 'j'
-- "jello"
--
-- >>> fmap rezip $ zipper "hello" & within traverse <&> tug rightward <&> focus .~ 'u'
-- "hullo"
tug :: (a -> Maybe a) -> a -> a
tug f a = fromMaybe a (f a)
{-# INLINE tug #-}

-- | This allows you to safely @'tug' 'leftward'@ or @'tug' 'rightward'@ multiple times on a 'zipper',
-- moving multiple steps in a given direction and stopping at the last place you
-- couldn't move from. This lets you safely move a zipper, because it will stop at either end.
--
-- >>> fmap rezip $ zipper "stale" & within traverse <&> tugs rightward 2 <&> focus .~ 'y'
-- "style"
--
-- >>> rezip $ zipper "want" & fromWithin traverse & tugs rightward 2 & focus .~ 'r' & tugs leftward 100 & focus .~ 'c'
-- "cart"
tugs :: (a -> Maybe a) -> Int -> a -> a
tugs f n0
  | n0 < 0    = error "tugs: negative tug count"
  | otherwise = go n0
  where
    go 0 a = a
    go n a = maybe a (go (n - 1)) (f a)
{-# INLINE tugs #-}

-- | Move in a direction as far as you can go, then stop there.
--
-- This repeatedly applies a function until it returns Nothing, and then returns the last answer.
--
-- >>> fmap rezip $ zipper ("hello","world") & downward _1 & within traverse <&> rightmost <&> focus .~ 'a'
-- ("hella","world")
--
-- >>> rezip $ zipper ("hello","there") & fromWithin (both.traverse) & rightmost & focus .~ 'm'
-- ("hello","therm")
farthest :: (a -> Maybe a) -> a -> a
farthest f = go where
  go a = maybe a go (f a)
{-# INLINE farthest #-}

-- | This allows for you to repeatedly pull a 'zipper' in a given direction, failing if it falls off the end.
--
-- >>> isNothing $ zipper "hello" & within traverse >>= jerks rightward 10
-- True
--
-- >>> fmap rezip $ zipper "silly" & within traverse >>= jerks rightward 3 <&> focus .~ 'k'
-- "silky"
jerks :: Monad m => (a -> m a) -> Int -> a -> m a
jerks f n0
  | n0 < 0    = fail "jerks: negative jerk count"
  | otherwise = go n0
  where
    go 0 a = return a
    go n a = f a >>= go (n - 1)
{-# INLINE jerks #-}

-- | Returns the number of siblings at the current level in the 'zipper'.
--
-- @'teeth' z '>=' 1@
--
-- /NB:/ If the current 'Traversal' targets an infinite number of elements then this may not terminate.
--
-- >>> zipper ("hello","world") & teeth
-- 1
--
-- >>> zipper ("hello","world") & fromWithin both & teeth
-- 2
--
-- >>> zipper ("hello","world") & downward _1 & teeth
-- 1
--
-- >>> zipper ("hello","world") & downward _1 & fromWithin traverse & teeth
-- 5
--
-- >>> zipper ("hello","world") & fromWithin (_1.traverse) & teeth
-- 5
--
-- >>> zipper ("hello","world") & fromWithin (both.traverse) & teeth
-- 10
teeth :: h :> a:@i -> Int
teeth (Zipper _ p _ _) = pathsize p
{-# INLINE teeth #-}

-- | Move the 'Zipper' horizontally to the element in the @n@th position in the
-- current level, absolutely indexed, starting with the 'farthest' 'leftward' as @0@.
--
-- This returns 'Nothing' if the target element doesn't exist.
--
-- @'jerkTo' n ≡ 'jerks' 'rightward' n . 'farthest' 'leftward'@
--
-- >>> isNothing $ zipper "not working." & jerkTo 20
-- True

-- >>> isNothing $ zipper "not working." & fromWithin traverse & jerkTo 20
-- True
--
-- >>> fmap rezip $ zipper "not working" & within traverse >>= jerkTo 2 <&> focus .~ 'w'
-- Just "now working"
jerkTo :: MonadPlus m => Int -> (h :> a:@i) -> m (h :> a:@i)
jerkTo n z = case compare k n of
  LT -> jerks rightward (n - k) z
  EQ -> return z
  GT -> jerks leftward (k - n) z
  where k = tooth z
{-# INLINE jerkTo #-}

-- | Move the 'Zipper' horizontally to the element in the @n@th position of the
-- current level, absolutely indexed, starting with the 'farthest' 'leftward' as @0@.
--
-- If the element at that position doesn't exist, then this will clamp to the range @0 <= n < 'teeth'@.
--
-- @'tugTo' n ≡ 'tugs' 'rightward' n . 'farthest' 'leftward'@
--
-- >>> rezip $ zipper "not working." & fromWithin traverse & tugTo 100 & focus .~ '!' & tugTo 1 & focus .~ 'u'
-- "nut working!"
tugTo :: Int -> (h :> a:@i) -> h :> a:@i
tugTo n z = case compare k n of
  LT -> tugs rightward (n - k) z
  EQ -> z
  GT -> tugs leftward (k - n) z
  where k = tooth z
{-# INLINE tugTo #-}

moveTo :: MonadPlus m => i -> (h :> a:@i) -> m (h :> a:@i)
moveTo _ = return -- TODO
{-# INLINE moveTo #-}

moveToward :: i -> (h :> a:@i) -> h :> a:@i
moveToward _ = id -- TODO
{-# INLINE moveToward #-}

lensed :: ALens' s a -> IndexedLens' Int s a
lensed l f = cloneLens l (indexed f (0 :: Int))
{-# INLINE lensed #-}

-- | Step down into a 'Lens'. This is a constrained form of 'fromWithin' for when you know
-- there is precisely one target that can never fail.
--
-- @
-- 'downward' :: 'Lens'' s a -> (h :> s) -> h :> s :> a
-- 'downward' :: 'Iso'' s a  -> (h :> s) -> h :> s :> a
-- @
downward :: forall j h s a. ALens' s a -> h :> s:@j -> h :> s:@j :>> a
downward l (Zipper h p j s) = Zipper (Snoc h l' p j go) Start 0 (s^.l')
  where l' :: IndexedLens' Int s a
        l' = lensed l
        go (Leaf _ b) = set l' b s
        go _ = error "downward: rezipping"
{-# INLINE downward #-}

idownward :: forall i j h s a. Ord i => AnIndexedLens' i s a -> h :> s:@j -> h :> s:@j :> a:@i
idownward l (Zipper h p j s) = Zipper (Snoc h l' p j go) Start i a
  where l' :: IndexedLens' i s a
        l' = cloneIndexedLens l
        (i, a) = iview l' s
        go (Leaf _ b) = set l' b s
        go _ = error "idownward: rezipping"
{-# INLINE idownward #-}

-- | Step down into the 'leftmost' entry of a 'Traversal'.
--
-- @
-- 'within' :: 'Traversal'' s a -> (h :> s) -> Maybe (h :> s :> a)
-- 'within' :: 'Prism'' s a     -> (h :> s) -> Maybe (h :> s :> a)
-- 'within' :: 'Lens'' s a      -> (h :> s) -> Maybe (h :> s :> a)
-- 'within' :: 'Iso'' s a       -> (h :> s) -> Maybe (h :> s :> a)
-- @

-- within :: MonadPlus m => ATraversal' s a -> (h :> s:@j) -> m (h :> s:@j :>> a)
within :: MonadPlus m => LensLike' (Indexing (Bazaar' (Indexed Int) a)) s a -> (h :> s:@j) -> m (h :> s:@j :>> a)
within = iwithin . indexing
{-# INLINE within #-}

iwithin :: (MonadPlus m, Ord i) => AnIndexedTraversal' i s a -> (h :> s:@j) -> m (h :> s:@j :> a:@i)
iwithin l (Zipper h p j s) = case magma l (Context id) s of
  Context k xs -> startl Start xs mzero $ \q i a -> return $ Zipper (Snoc h l p j k) q i a
{-# INLINE iwithin #-}

-- | Step down into every entry of a 'Traversal' simultaneously.
--
-- >>> zipper ("hello","world") & withins both >>= leftward >>= withins traverse >>= rightward <&> focus %~ toUpper <&> rezip :: [(String,String)]
-- [("hEllo","world"),("heLlo","world"),("helLo","world"),("hellO","world")]
--
-- @
-- 'withins' :: 'Traversal'' s a -> (h :> s) -> [h :> s :> a]
-- 'withins' :: 'Lens'' s a      -> (h :> s) -> [h :> s :> a]
-- 'withins' :: 'Iso'' s a       -> (h :> s) -> [h :> s :> a]
-- @
withins :: MonadPlus m => LensLike' (Indexing (Bazaar' (Indexed Int) a)) s a -> (h :> s:@j) -> m (h :> s:@j :>> a)
withins = iwithins . indexing
{-# INLINE withins #-}

iwithins :: (MonadPlus m, Ord i) => AnIndexedTraversal' i s a -> (h :> s:@j) -> m (h :> s:@j :> a:@i)
iwithins t (Zipper h p j s) = case magma t (Context id) s of
  Context k xs -> let up = Snoc h t p j k
                      go q (Ap m nl nr li l r) = go (ApL m nl nr li q r) l `mplus` go (ApR m nl nr li l q) r
                      go q (Leaf i a)       = return $ Zipper up q i a
                      go _ Pure             = mzero
                  in  go Start xs
{-# INLINE iwithins #-}

-- | Unsafely step down into a 'Traversal' that is /assumed/ to be non-empty.
--
-- If this invariant is not met then this will usually result in an error!
--
-- @
-- 'fromWithin' :: 'Traversal'' s a -> (h :> s) -> h :> s :> a
-- 'fromWithin' :: 'Lens'' s a      -> (h :> s) -> h :> s :> a
-- 'fromWithin' :: 'Iso'' s a       -> (h :> s) -> h :> s :> a
-- @
--
-- You can reason about this function as if the definition was:
--
-- @'fromWithin' l ≡ 'fromJust' '.' 'within' l@
fromWithin :: LensLike' (Indexing (Bazaar' (Indexed Int) a)) s a -> (h :> s:@j) -> h :> s:@j :>> a
fromWithin = ifromWithin . indexing
{-# INLINE fromWithin #-}

ifromWithin :: Ord i => AnIndexedTraversal' i s a -> (h :> s:@j) -> h :> s:@j :> a:@i
ifromWithin l (Zipper h p j s) = case magma l (Context id) s of
  Context k xs -> let up = Snoc h l p j k in
    startl Start xs (Zipper up Start (error "fromWithin an empty Traversal")
                                     (error "fromWithin an empty Traversal"))
                    (Zipper up)
{-# INLINE ifromWithin #-}

-- | This enables us to pull the 'Zipper' back up to the 'Top'.
class Zipping h a where
  recoil :: Coil h i a -> Magma i a -> Zipped h a

instance Zipping Top a where
  recoil Coil (Leaf _ a) = a
  recoil Coil _ = error "recoil: expected Leaf"
  {-# INLINE recoil #-}

instance Zipping h s => Zipping (Zipper h i s) a where
  recoil (Snoc h _ p i k) as = recoil h $ recompress p i (k as)
  {-# INLINE recoil #-}

-- | Close something back up that you opened as a 'Zipper'.
rezip :: Zipping h a => (h :> a:@i) -> Zipped h a
rezip (Zipper h p i a) = recoil h (recompress p i a)
{-# INLINE rezip #-}

-- | Extract the current 'focus' from a 'Zipper' as a 'Pretext', with access to the current index.
focusedContext :: (Indexable i p, Zipping h a) => (h :> a:@i) -> Pretext p a a (Zipped h a)
focusedContext (Zipper h p i a) = Pretext (\f -> rezip . Zipper h p i <$> indexed f i a)
{-# INLINE focusedContext #-}

-----------------------------------------------------------------------------
-- * Tapes
-----------------------------------------------------------------------------

-- | A 'Tape' is a recorded path through the 'Traversal' chain of a 'Zipper'.
data Tape h i a where
  Tape :: Track h i a -> i -> Tape h i a

-- | Save the current path as as a 'Tape' we can play back later.
saveTape :: Zipper h i a -> Tape h i a
saveTape (Zipper h _ i _) = Tape (peel h) i
{-# INLINE saveTape #-}

-- | Restore ourselves to a previously recorded position precisely.
--
-- If the position does not exist, then fail.
restoreTape :: MonadPlus m => Tape h i a -> Zipped h a -> m (Zipper h i a)
restoreTape (Tape h n) = restoreTrack h >=> moveTo n
{-# INLINE restoreTape #-}

-- | Restore ourselves to a location near our previously recorded position.
--
-- When moving left to right through a 'Traversal', if this will clamp at each level to the range @0 <= k < teeth@,
-- so the only failures will occur when one of the sequence of downward traversals find no targets.
restoreNearTape :: MonadPlus m => Tape h i a -> Zipped h a -> m (Zipper h i a)
restoreNearTape (Tape h n) a = liftM (moveToward n) (restoreNearTrack h a)
{-# INLINE restoreNearTape #-}

-- | Restore ourselves to a previously recorded position.
--
-- This *assumes* that nothing has been done in the meantime to affect the existence of anything on the entire path.
--
-- Motions leftward or rightward are clamped, but all traversals included on the 'Tape' are assumed to be non-empty.
--
-- Violate these assumptions at your own risk!
unsafelyRestoreTape :: Tape h i a -> Zipped h a -> Zipper h i a
unsafelyRestoreTape (Tape h n) = unsafelyRestoreTrack h >>> moveToward n
{-# INLINE unsafelyRestoreTape #-}

-----------------------------------------------------------------------------
-- * Tracks
-----------------------------------------------------------------------------

-- | This is used to peel off the path information from a 'Coil' for use when saving the current path for later replay.
peel :: Coil h i a -> Track h i a
peel Coil             = Track
peel (Snoc h l _ i _) = Fork (peel h) i l
{-# INLINE peel #-}

-- | The 'Track' forms the bulk of a 'Tape'.
data Track t i a where
  Track :: Track Top Int a
  Fork  :: Ord i => Track h j s -> j -> AnIndexedTraversal' i s a -> Track (Zipper h j s) i a

-- | Restore ourselves to a previously recorded position precisely.
--
-- If the position does not exist, then fail.
restoreTrack :: MonadPlus m => Track h i a -> Zipped h a -> m (Zipper h i a)
restoreTrack Track        = return . zipper
restoreTrack (Fork h n l) = restoreTrack h >=> moveTo n >=> iwithin l

-- | Restore ourselves to a location near our previously recorded position.
--
-- When moving leftward to rightward through a 'Traversal', if this will clamp at each level to the range @0 <= k < teeth@,
-- so the only failures will occur when one of the sequence of downward traversals find no targets.
restoreNearTrack :: MonadPlus m => Track h i a -> Zipped h a -> m (Zipper h i a)
restoreNearTrack Track        = return . zipper
restoreNearTrack (Fork h n l) = restoreNearTrack h >=> moveToward n >>> iwithin l

-- | Restore ourselves to a previously recorded position.
--
-- This *assumes* that nothing has been done in the meantime to affect the existence of anything on the entire path.
--
-- Motions leftward or rightward are clamped, but all traversals included on the 'Tape' are assumed to be non-empty.
--
-- Violate these assumptions at your own risk!
unsafelyRestoreTrack :: Track h i a -> Zipped h a -> Zipper h i a
unsafelyRestoreTrack Track = zipper
unsafelyRestoreTrack (Fork h n l) = unsafelyRestoreTrack h >>> moveToward n >>> ifromWithin l
