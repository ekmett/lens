{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Zipper
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides internal types and functions used in the implementation
-- of Control.Lens.Zipper. You shouldn't need to import it directly, and the
-- exported types can be used to break Zipper invariants.
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Zipper where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Category
import Control.Comonad
import Control.Comonad.Store.Class
import Control.Monad ((>=>))
import Control.Lens.Fold
import Control.Lens.Indexed
import Control.Lens.IndexedLens
import Control.Lens.Internal
import Control.Lens.Traversal
import Control.Lens.Type
import Data.Foldable
import Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Monoid
import Prelude hiding ((.),id)

-----------------------------------------------------------------------------
-- * Zippers
-----------------------------------------------------------------------------

-- | This is used to represent the 'Top' of the 'zipper'.
--
-- Every 'zipper' starts with 'Top'.
--
-- /e.g./ @'Top' ':>' a@ is the trivial zipper.
data Top

infixl 9 :>

-- | This is the type of a 'zipper'. It visually resembles a \"breadcrumb trail\" as
-- used in website navigation. Each breadcrumb in the trail represents a level you
-- can move up to.
--
-- This type operator associates to the left, so you can use a type like
--
-- @'Top' ':>' ('String','Double') ':>' 'String' ':>' 'Char'@
--
-- to represent a zipper from @('String','Double')@ down to 'Char' that has an intermediate
-- crumb for the 'String' containing the 'Char'.
data p :> a = Zipper (Coil p a) {-# UNPACK #-} !(Level a)

-- | This represents the type a zipper will have when it is fully 'Zipped' back up.
type family Zipped h a
type instance Zipped Top a      = a
type instance Zipped (h :> b) a = Zipped h b

-- | 'Coil' is used internally in the definition of a 'zipper'.
data Coil :: * -> * -> * where
  Coil :: Coil Top a
  Snoc :: Coil h b ->
          {-# UNPACK #-} !Int ->
          SimpleLensLike (Bazaar a a) b a ->
          [b] -> (NonEmpty a -> b) -> [b] ->
          Coil (h :> b) a

-- | This 'Lens' views the current target of the 'zipper'.
focus :: SimpleIndexedLens (Tape (h :> a)) (h :> a) a
focus = index $ \f (Zipper h (Level n l a r)) -> (\a' -> Zipper h (Level n l a' r)) <$> f (Tape (peel h) n) a
{-# INLINE focus #-}

-- | Construct a 'zipper' that can explore anything.
zipper :: a -> Top :> a
zipper a = Zipper Coil (Level 0 [] a [])
{-# INLINE zipper #-}

-- | Return the index into the current 'Traversal' within the current level of the zipper.
--
-- @'jerkTo' ('tooth' l) l = Just'@
tooth :: (a :> b) -> Int
tooth (Zipper _ (Level n _ _ _)) = n
{-# INLINE tooth #-}

-- | Move the 'zipper' 'up', closing the current level and focusing on the parent element.
up :: (a :> b :> c) -> a :> b
up (Zipper (Snoc h n _ ls k rs) w) = Zipper h (Level n ls (k (rezipLevel w)) rs)
{-# INLINE up #-}

-- | Pull the 'zipper' 'left' within the current 'Traversal'.
left  :: (a :> b) -> Maybe (a :> b)
left (Zipper h w) = Zipper h <$> leftLevel w
{-# INLINE left #-}

-- | Pull the entry one entry to the 'right'
right :: (a :> b) -> Maybe (a :> b)
right (Zipper h w) = Zipper h <$> rightLevel w
{-# INLINE right #-}

-- | This allows you to safely 'tug left' or 'tug right' on a 'zipper'.
--
-- The more general signature allows its use in other circumstances, however.
tug :: (a -> Maybe a) -> a -> a
tug f a = fromMaybe a (f a)
{-# INLINE tug #-}

-- | This allows you to safely 'tug left' or 'tug right' on a 'zipper', moving multiple steps in a given direction,
-- stopping at the last place you couldn't move from.
tugs :: (a -> Maybe a) -> Int -> a -> a
tugs f n0
  | n0 < 0    = error "tugs: negative tug count"
  | otherwise = go n0
  where
    go 0 a = a
    go n a = maybe a (go (n - 1)) (f a)
{-# INLINE tugs #-}

-- | Move in a direction as far as you can go, then stop.
farthest :: (a -> Maybe a) -> a -> a
farthest f = go where
  go a = maybe a go (f a)
{-# INLINE farthest #-}

-- | This allows for you to repeatedly pull a 'zipper' in a given direction, failing if it falls off the end.
jerks :: (a -> Maybe a) -> Int -> a -> Maybe a
jerks f n0
  | n0 < 0    = error "jerks: negative jerk count"
  | otherwise = go n0
  where
    go 0 a = Just a
    go n a = f a >>= go (n - 1)
{-# INLINE jerks #-}

-- | Returns the number of siblings at the current level in the 'zipper'.
--
-- @'teeth' z '>=' 1@
--
-- /NB:/ If the current 'Traversal' targets an infinite number of elements then this may not terminate.
teeth :: (a :> b) -> Int
teeth (Zipper _ w) = levelWidth w
{-# INLINE teeth #-}

-- | Move the 'zipper' horizontally to the element in the @n@th position in the current level, absolutely indexed, starting with the @'farthest' 'left'@ as @0@.
--
-- This returns 'Nothing' if the target element doesn't exist.
--
-- @'jerkTo' n ≡ 'jerks' 'right' n . 'farthest' 'left'@
jerkTo :: Int -> (a :> b) -> Maybe (a :> b)
jerkTo n z = case compare k n of
  LT -> jerks left (n - k) z
  EQ -> Just z
  GT -> jerks right (k - n) z
  where k = tooth z
{-# INLINE jerkTo #-}

-- | Move the 'zipper' horizontally to the element in the @n@th position of the current level, absolutely indexed, starting with the @'farthest' 'left'@ as @0@.
--
-- If the element at that position doesn't exist, then this will clamp to the range @0 <= n < 'teeth'@.
--
-- @'tugTo' n ≡ 'tugs' 'right' n . 'farthest' 'left'@
tugTo :: Int -> (a :> b) -> a :> b
tugTo n z = case compare k n of
  LT -> tugs left (n - k) z
  EQ -> z
  GT -> tugs right (k - n) z
  where k = tooth z
{-# INLINE tugTo #-}

-- | Step down into a 'Lens'. This is a constrained form of 'fromWithin' for when you know
-- there is precisely one target.
--
-- @
-- 'down' :: 'Simple' 'Lens' b c -> (a :> b) -> a :> b :> c
-- 'down' :: 'Simple' 'Iso' b c  -> (a :> b) -> a :> b :> c
-- @
down :: SimpleLensLike (Context c c) b c -> (a :> b) -> a :> b :> c
down l (Zipper h (Level n ls b rs)) = case l (Context id) b of
  Context k c -> Zipper (Snoc h n (cloneLens l) ls (k . extract) rs) (Level 0 [] c [])
{-# INLINE down #-}

-- | Step down into the 'leftmost' entry of a 'Traversal'.
--
-- @
-- 'within' :: 'Simple' 'Traversal' b c -> (a :> b) -> Maybe (a :> b :> c)
-- 'within' :: 'Simple' 'Lens' b c      -> (a :> b) -> Maybe (a :> b :> c)
-- 'within' :: 'Simple' 'Iso' b c       -> (a :> b) -> Maybe (a :> b :> c)
-- @
within :: SimpleLensLike (Bazaar c c) b c -> (a :> b) -> Maybe (a :> b :> c)
within l (Zipper h (Level n ls b rs)) = case partsOf' l (Context id) b of
  Context _ []     -> Nothing
  Context k (c:cs) -> Just (Zipper (Snoc h n l ls (k . NonEmpty.toList) rs) (Level 0 [] c cs))
{-# INLINE within #-}

-- | Unsafely step down into a 'Traversal' that is /assumed/ to be non-empty.
--
-- If this invariant is not met then this will usually result in an error!
--
-- @
-- 'fromWithin' :: 'Simple' 'Traversal' b c -> (a :> b) -> a :> b :> c
-- 'fromWithin' :: 'Simple' 'Lens' b c      -> (a :> b) -> a :> b :> c
-- 'fromWithin' :: 'Simple' 'Iso' b c       -> (a :> b) -> a :> b :> c
-- @
--
-- You can reason about this function as if the definition was:
--
-- @'fromWithin' l ≡ 'fromJust' '.' 'within' l@
--
-- but it is lazier in such a way that if this invariant is violated, some code
-- can still succeed if it is lazy enough in the use of the focused value.
fromWithin :: SimpleLensLike (Bazaar c c) b c -> (a :> b) -> a :> b :> c
fromWithin l (Zipper h (Level n ls b rs)) = case partsOf' l (Context id) b of
  Context k cs -> Zipper (Snoc h n l ls (k . NonEmpty.toList) rs)
                         (Level 0 [] (Prelude.head cs) (Prelude.tail cs))
{-# INLINE fromWithin #-}

-- | This enables us to pull the 'zipper' back up to the 'Top'.
class Zipper h a where
  recoil :: Coil h a -> NonEmpty a -> Zipped h a

instance Zipper Top a where
  recoil Coil = extract

instance Zipper h b => Zipper (h :> b) c where
  recoil (Snoc h _ _ ls k rs) as = recoil h (NonEmpty.fromList (Prelude.reverse ls ++ k as : rs))

-- | Close something back up that you opened as a 'zipper'.
rezip :: Zipper h a => (h :> a) -> Zipped h a
rezip (Zipper h w) = recoil h (rezipLevel w)
{-# INLINE rezip #-}

-----------------------------------------------------------------------------
-- * Tapes
-----------------------------------------------------------------------------

-- | A 'Tape' is a recorded path through the 'Traversal' chain of a 'zipper'.
data Tape k where
  Tape :: Track h a -> {-# UNPACK #-} !Int -> Tape (h :> a)

-- | Save the current path as as a 'Tape' we can play back later.
saveTape :: (a :> b) -> Tape (a :> b)
saveTape (Zipper h (Level n _ _ _)) = Tape (peel h) n
{-# INLINE saveTape #-}

-- | Restore ourselves to a previously recorded position precisely.
--
-- If the position does not exist, then fail.
restoreTape :: Tape (h :> a) -> Zipped h a -> Maybe (h :> a)
restoreTape (Tape h n) = restoreTrack h >=> jerks right n
{-# INLINE restoreTape #-}

-- | Restore ourselves to a location near our previously recorded position.
--
-- When moving left to right through a 'Traversal', if this will clamp at each level to the range @0 <= k < teeth@,
-- so the only failures will occur when one of the sequence of downward traversals find no targets.
restoreNearTape :: Tape (h :> a) -> Zipped h a -> Maybe (h :> a)
restoreNearTape (Tape h n) a = tugs right n <$> restoreNearTrack h a
{-# INLINE restoreNearTape #-}

-- | Restore ourselves to a previously recorded position.
--
-- This *assumes* that nothing has been done in the meantime to affect the existence of anything on the entire path.
--
-- Motions left or right are clamped, but all traversals included on the 'Tape' are assumed to be non-empty.
--
-- Violate these assumptions at your own risk!
unsafelyRestoreTape :: Tape (h :> a) -> Zipped h a -> h :> a
unsafelyRestoreTape (Tape h n) = unsafelyRestoreTrack h >>> tugs right n
{-# INLINE unsafelyRestoreTape #-}

-----------------------------------------------------------------------------
-- * Tracks
-----------------------------------------------------------------------------

-- | This is used to peel off the path information from a 'Coil' for use when saving the current path for later replay.
peel :: Coil h a -> Track h a
peel Coil               = Track
peel (Snoc h n l _ _ _) = Fork (peel h) n l

-- | The 'Track' forms the bulk of a 'Tape'.
data Track :: * -> * -> * where
  Track :: Track Top a
  Fork  :: Track h b -> {-# UNPACK #-} !Int -> SimpleLensLike (Bazaar a a) b a -> Track (h :> b) a

-- | Restore ourselves to a previously recorded position precisely.
--
-- If the position does not exist, then fail.
restoreTrack :: Track h a -> Zipped h a -> Maybe (h :> a)
restoreTrack Track = Just . zipper
restoreTrack (Fork h n l) = restoreTrack h >=> jerks right n >=> within l

-- | Restore ourselves to a location near our previously recorded position.
--
-- When moving left to right through a 'Traversal', if this will clamp at each level to the range @0 <= k < teeth@,
-- so the only failures will occur when one of the sequence of downward traversals find no targets.
restoreNearTrack :: Track h a -> Zipped h a -> Maybe (h :> a)
restoreNearTrack Track = Just . zipper
restoreNearTrack (Fork h n l) = restoreNearTrack h >=> tugs right n >>> within l

-- | Restore ourselves to a previously recorded position.
--
-- This *assumes* that nothing has been done in the meantime to affect the existence of anything on the entire path.
--
-- Motions left or right are clamped, but all traversals included on the 'Tape' are assumed to be non-empty.
--
-- Violate these assumptions at your own risk!
unsafelyRestoreTrack :: Track h a -> Zipped h a -> h :> a
unsafelyRestoreTrack Track = zipper
unsafelyRestoreTrack (Fork h n l) = unsafelyRestoreTrack h >>> tugs right n >>> fromWithin l

-----------------------------------------------------------------------------
-- * Levels
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

-- | Pull the non-empty list zipper left one entry
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
-- @'focusLevel' :: 'Simple' 'Lens' ('Level' a) a@
focusLevel :: Functor f => (a -> f a) -> Level a -> f (Level a)
focusLevel f (Level n ls a rs) = (\b -> Level n ls b rs) <$> f a
{-# INLINE focusLevel #-}

instance Functor Level where
  fmap f (Level n ls a rs) = Level n (f <$> ls) (f a) (f <$> rs)

instance Foldable Level where
  foldMap f (Level _ ls a rs) = foldMapOf (backwards folded) f ls <> f a <> foldMap f rs

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
