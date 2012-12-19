{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

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
-- of @Control.Lens.Zipper@. You shouldn't need to import it directly, and the
-- exported types can be used to break 'Zipper' invariants.
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Zipper where

import Control.Category
import Control.Monad
import Control.Lens.Classes
import Control.Lens.Combinators
import Control.Lens.Getter
import Control.Lens.IndexedLens
import Control.Lens.Internal
import Control.Lens.Setter
import Control.Lens.Traversal
import Control.Lens.Type
import Data.Maybe
import Prelude hiding ((.),id)

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Char

-----------------------------------------------------------------------------
-- * Zippers
-----------------------------------------------------------------------------

-- | This is used to represent the 'Top' of the 'Zipper'.
--
-- Every 'Zipper' starts with 'Top'.
--
-- /e.g./ @'Top' ':>' a@ is the type of the trivial 'Zipper'.
data Top

infixl 9 :>

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
data h :> a = Zipper (Coil h a) -- The 'Coil' storing the previous levels of the 'Zipper'.
      {-# UNPACK #-} !Int       -- Number of items to the left.
                     [a]        -- Items to the left (stored reversed).
                     a          -- Focused item.
                     [a]        -- Items to the right.

-- | This is an alias for '(:>)'. Provided mostly for convenience
type Zipper = (:>)

-- | This represents the type a 'Zipper' will have when it is fully 'Zipped' back up.
type family Zipped h a
type instance Zipped Top a      = a
type instance Zipped (h :> s) a = Zipped h s

-- | A 'Coil' is a linked list of the levels above the current one. The length
-- of a 'Coil' is known at compile time.
--
-- This is part of the internal structure of a zipper. You shouldn't need to manipulate this directly.
data Coil :: * -> * -> * where
  Coil :: Coil Top a
  Snoc :: Coil h s                        -- Previous 'Coil'.
       -> LensLike' (Bazaar a a) s a      -- The 'Traversal' used to descend into this level (used to build a 'Tape').
       -- The Zipper above us, unpacked:
       -> {-# UNPACK #-} !Int             -- Number of items to the left.
       -> [s]                             -- Previous level's items to the left (stored reverse).
       -> ([a] -> s)                      -- Function to rebuild the previous level's focused item from the entire current level.
                                          --   (Since the current level always has a focus, the list must be nonempty.)
       -> [s]                             -- Previous level's items to the right.
       -> Coil (h :> s) a

-- | This 'Lens' views the current target of the 'Zipper'.
--
-- A 'Tape' that can be used to get to the current location is available as the index of this 'Lens'.
focus :: IndexedLens' (Tape (h :> a)) (h :> a) a
focus f (Zipper h n l a r) = indexed f (Tape (peel h) n) a <&> \a' -> Zipper h n l a' r
{-# INLINE focus #-}

-- | Construct a 'Zipper' that can explore anything, and start it at the top.
zipper :: a -> Top :> a
zipper a = Zipper Coil 0 [] a []
{-# INLINE zipper #-}

-- | Return the index into the current 'Traversal' within the current level of the 'Zipper'.
--
-- @'jerkTo' ('tooth' l) l = Just'@
--
-- Mnemonically, zippers have a number of 'teeth' within each level. This is which 'tooth' you are currently at.
tooth :: (h :> a) -> Int
tooth (Zipper _ n _ _ _) = n
{-# INLINE tooth #-}

-- | Move the 'Zipper' 'upward', closing the current level and focusing on the parent element.
--
-- NB: Attempts to move upward from the 'Top' of the 'Zipper' will fail to typecheck.
--
upward :: (h :> s :> a) -> h :> s
upward (Zipper (Snoc h _ un uls k urs) _ ls x rs) = Zipper h un uls ux urs
  where ux = k (reverseList ls ++ x : rs)
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
rightward :: MonadPlus m => (h :> a) -> m (h :> a)
rightward (Zipper _ _ _  _ []    ) = mzero
rightward (Zipper h n ls a (r:rs)) = return (Zipper h (n + 1) (a:ls) r rs)
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
leftward :: MonadPlus m => (h :> a) -> m (h :> a)
leftward (Zipper _ _ []     _ _ ) = mzero
leftward (Zipper h n (l:ls) a rs) = return (Zipper h (n - 1) ls l (a:rs))
{-# INLINE leftward #-}

-- | Move to the leftmost position of the current 'Traversal'.
--
-- This is just a convenient alias for @'farthest' 'leftward'@.
--
-- >>> zipper "hello" & fromWithin traverse & rightmost & focus .~ 'a' & rezip
-- "hella"
leftmost :: (a :> b) -> a :> b
leftmost = farthest leftward

-- | Move to the rightmost position of the current 'Traversal'.
--
-- This is just a convenient alias for @'farthest' 'rightward'@.
--
-- >>> zipper "hello" & fromWithin traverse & rightmost & focus .~ 'y' & leftmost & focus .~ 'j' & rezip
-- "jelly"
rightmost :: (a :> b) -> a :> b
rightmost = farthest rightward

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
teeth :: (h :> a) -> Int
teeth (Zipper _ n _ _ rs) = n + 1 + length rs
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
jerkTo :: MonadPlus m => Int -> (h :> a) -> m (h :> a)
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
tugTo :: Int -> (h :> a) -> h :> a
tugTo n z = case compare k n of
  LT -> tugs rightward (n - k) z
  EQ -> z
  GT -> tugs leftward (k - n) z
  where k = tooth z
{-# INLINE tugTo #-}

-- | Step down into a 'Lens'. This is a constrained form of 'fromWithin' for when you know
-- there is precisely one target that can never fail.
--
-- @
-- 'downward' :: 'Lens'' s a -> (h :> s) -> h :> s :> a
-- 'downward' :: 'Iso'' s a  -> (h :> s) -> h :> s :> a
-- @
downward :: LensLike' (Context a a) s a -> (h :> s) -> h :> s :> a
downward l (Zipper h n ls s rs) = case l (Context id) s of
  Context k a -> Zipper (Snoc h (cloneLens l) n ls (k . head) rs) 0 [] a []
{-# INLINE downward #-}

-- | Step down into the 'leftmost' entry of a 'Traversal'.
--
-- @
-- 'within' :: 'Traversal'' s a -> (h :> s) -> Maybe (h :> s :> a)
-- 'within' :: 'Lens'' s a      -> (h :> s) -> Maybe (h :> s :> a)
-- 'within' :: 'Iso'' s a       -> (h :> s) -> Maybe (h :> s :> a)
-- @
within :: MonadPlus m => LensLike' (Bazaar a a) s a -> (h :> s) -> m (h :> s :> a)
within l (Zipper h n ls s rs) = case partsOf' l (Context id) s of
  Context _ []     -> mzero
  Context k (a:as) -> return (Zipper (Snoc h l n ls k rs) 0 [] a as)
{-# INLINE within #-}

-- | Step down into every entry of a 'Traversal' simultaneously.
--
-- >>> zipper ("hello","world") & withins both >>= leftward >>= withins traverse >>= rightward <&> focus %~ toUpper <&> rezip
-- [("hEllo","world"),("heLlo","world"),("helLo","world"),("hellO","world")]
--
-- @
-- 'withins' :: 'Traversal'' s a -> (h :> s) -> [h :> s :> a]
-- 'withins' :: 'Lens'' s a      -> (h :> s) -> [h :> s :> a]
-- 'withins' :: 'Iso'' s a       -> (h :> s) -> [h :> s :> a]
-- @
withins :: LensLike' (Bazaar a a) s a -> (h :> s) -> [h :> s :> a]
withins l (Zipper h n ls s rs) = case partsOf' l (Context id) s of
  Context k ys -> go k [] ys
  where go k xs (y:ys) = Zipper (Snoc h l n ls k rs) 0 xs y ys : go k (y:xs) ys
        go _ _  []     = []

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
--
-- but it is lazier in such a way that if this invariant is violated, some code
-- can still succeed if it is lazy enough in the use of the focused value.
fromWithin :: LensLike' (Bazaar a a) s a -> (h :> s) -> h :> s :> a
fromWithin l (Zipper h n ls s rs) = case partsOf' l (Context id) s of
  Context k ~(a:as) -> Zipper (Snoc h l n ls k rs) 0 [] a as
{-# INLINE fromWithin #-}

-- | This enables us to pull the 'Zipper' back up to the 'Top'.
class Zipping h a where
  recoil :: Coil h a -> [a] -> Zipped h a

instance Zipping Top a where
  recoil Coil = head
  {-# INLINE recoil #-}

instance Zipping h s => Zipping (h :> s) a where
  recoil (Snoc h _ _ ls k rs) as = recoil h (reverseList ls ++ k as : rs)
  {-# INLINE recoil #-}

-- | Close something back up that you opened as a 'Zipper'.
rezip :: Zipping h a => (h :> a) -> Zipped h a
rezip (Zipper h _ ls a rs) = recoil h (reverseList ls ++ a : rs)
{-# INLINE rezip #-}

-- | Extract the current 'focus' from a 'Zipper' as a 'Context'
focusedContext :: Zipping h a => (h :> a) -> Context a a (Zipped h a)
focusedContext z = Context (\a -> z & focus .~ a & rezip) (z^.focus)

-----------------------------------------------------------------------------
-- * Tapes
-----------------------------------------------------------------------------

-- | A 'Tape' is a recorded path through the 'Traversal' chain of a 'Zipper'.
data Tape k where
  Tape :: Track h a -> {-# UNPACK #-} !Int -> Tape (h :> a)

-- | Save the current path as as a 'Tape' we can play back later.
saveTape :: (h :> a) -> Tape (h :> a)
saveTape (Zipper h n _ _ _) = Tape (peel h) n
{-# INLINE saveTape #-}

-- | Restore ourselves to a previously recorded position precisely.
--
-- If the position does not exist, then fail.
restoreTape :: MonadPlus m => Tape (h :> a) -> Zipped h a -> m (h :> a)
restoreTape (Tape h n) = restoreTrack h >=> jerks rightward n
{-# INLINE restoreTape #-}

-- | Restore ourselves to a location near our previously recorded position.
--
-- When moving left to right through a 'Traversal', if this will clamp at each level to the range @0 <= k < teeth@,
-- so the only failures will occur when one of the sequence of downward traversals find no targets.
restoreNearTape :: MonadPlus m => Tape (h :> a) -> Zipped h a -> m (h :> a)
restoreNearTape (Tape h n) a = liftM (tugs rightward n) (restoreNearTrack h a)
{-# INLINE restoreNearTape #-}

-- | Restore ourselves to a previously recorded position.
--
-- This *assumes* that nothing has been done in the meantime to affect the existence of anything on the entire path.
--
-- Motions leftward or rightward are clamped, but all traversals included on the 'Tape' are assumed to be non-empty.
--
-- Violate these assumptions at your own risk!
unsafelyRestoreTape :: Tape (h :> a) -> Zipped h a -> h :> a
unsafelyRestoreTape (Tape h n) = unsafelyRestoreTrack h >>> tugs rightward n
{-# INLINE unsafelyRestoreTape #-}

-----------------------------------------------------------------------------
-- * Tracks
-----------------------------------------------------------------------------

-- | This is used to peel off the path information from a 'Coil' for use when saving the current path for later replay.
peel :: Coil h a -> Track h a
peel Coil               = Track
peel (Snoc h l n _ _ _) = Fork (peel h) n l

-- | The 'Track' forms the bulk of a 'Tape'.
data Track :: * -> * -> * where
  Track :: Track Top a
  Fork  :: Track h s -> {-# UNPACK #-} !Int -> LensLike' (Bazaar a a) s a -> Track (h :> s) a

-- | Restore ourselves to a previously recorded position precisely.
--
-- If the position does not exist, then fail.
restoreTrack :: MonadPlus m => Track h a -> Zipped h a -> m (h :> a)
restoreTrack Track = return . zipper
restoreTrack (Fork h n l) = restoreTrack h >=> jerks rightward n >=> within l

-- | Restore ourselves to a location near our previously recorded position.
--
-- When moving leftward to rightward through a 'Traversal', if this will clamp at each level to the range @0 <= k < teeth@,
-- so the only failures will occur when one of the sequence of downward traversals find no targets.
restoreNearTrack :: MonadPlus m => Track h a -> Zipped h a -> m (h :> a)
restoreNearTrack Track = return . zipper
restoreNearTrack (Fork h n l) = restoreNearTrack h >=> tugs rightward n >>> within l

-- | Restore ourselves to a previously recorded position.
--
-- This *assumes* that nothing has been done in the meantime to affect the existence of anything on the entire path.
--
-- Motions leftward or rightward are clamped, but all traversals included on the 'Tape' are assumed to be non-empty.
--
-- Violate these assumptions at your own risk!
unsafelyRestoreTrack :: Track h a -> Zipped h a -> h :> a
unsafelyRestoreTrack Track = zipper
unsafelyRestoreTrack (Fork h n l) = unsafelyRestoreTrack h >>> tugs rightward n >>> fromWithin l

-----------------------------------------------------------------------------
-- * Helper functions
-----------------------------------------------------------------------------

-- | Reverse a list.
--
-- GHC doesn't optimize @reverse []@ into @[]@, so we'll nudge it with our own
-- reverse function.
--
-- This is relevant when descending into a lens, for example -- we know the
-- unzipped part of the level will be empty.
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = go [x] xs
  where
    go a [] = a
    go a (y:ys) = go (y:a) ys
{-# INLINE reverseList #-}
