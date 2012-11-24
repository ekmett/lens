{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Zipper
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides a 'Zipper' with fairly strong type checking guarantees.
--
-- The code here is inspired by Brandon Simmons' @zippo@ package, but uses
-- a slightly different approach to represent the 'Zipper' that makes the whole thing
-- look like his breadcrumb trail, and can move side-to-side through traversals.
--
-- Some examples types:
--
-- [@'Top' ':>' a@] represents a trivial 'Zipper' with its focus at the root.
--
-- [@'Top' ':>' 'Data.Tree.Tree' a ':>' a@] represents a zipper that starts with a
--   'Data.Tree.Tree' and descends in a single step to values of type @a@.
--
-- [@'Top' ':>' 'Data.Tree.Tree' a ':>' 'Data.Tree.Tree' a ':>' 'Data.Tree.Tree' a@] represents a 'Zipper' into a
--   'Data.Tree.Tree' with an intermediate bookmarked 'Data.Tree.Tree',
--   focusing in yet another 'Data.Tree.Tree'.
--
-- Since individual levels of a zipper are managed by an arbitrary 'Traversal',
-- you can move left and right through the 'Traversal' selecting neighboring elements.
--
-- >>> zipper ("hello","world") & down _1 & fromWithin traverse & focus .~ 'J' & farthest right & focus .~ 'y' & rezip
-- ("Jelly","world")
--
-- This is particularly powerful when compiled with 'Control.Lens.Plated.plate',
-- 'Data.Data.Lens.uniplate' or 'Data.Data.Lens.biplate' for walking down into
-- self-similar children in syntax trees and other structures.
-----------------------------------------------------------------------------
module Control.Lens.Zipper
  (
  -- * Zippers
    Top()
  , (:>)()
  , zipper
  -- ** Focusing
  , focus
  -- ** Horizontal Movement
  , up
  , down
  , within
  -- ** Lateral Movement
  , left
  , right
  -- ** Movement Combinators
  , tug
  , tugs
  , jerks
  , farthest
  -- ** Absolute Positioning
  , tooth
  , teeth
  , jerkTo
  , tugTo
  -- ** Closing the zipper
  , rezip
  , Zipped
  , Zipper()
  -- ** Recording
  , Tape()
  , saveTape
  , restoreTape
  , restoreNearTape
  -- ** Unsafe Movement
  , fromWithin
  , unsafelyRestoreTape
  ) where

import Control.Applicative
import Control.Category
import Control.Comonad
import Control.Monad ((>=>))
import Control.Lens.Indexed
import Control.Lens.IndexedLens
import Control.Lens.Internal
import Control.Lens.Traversal
import Control.Lens.Type
import Data.Maybe
import Data.List.NonEmpty as NonEmpty
import Prelude hiding ((.),id)

-- $setup
-- >>> :m + Control.Lens

-- | This is used to represent the 'Top' of the 'Zipper'.
--
-- Every 'Zipper' starts with 'Top'.
--
-- /e.g./ @'Top' ':>' a@ is the trivial zipper.
data Top

infixl 9 :>

-- | This is the type of a 'Zipper'. It visually resembes a 'breadcrumb trail' as
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

-- | 'Coil' is used internally in the definition of a 'Zipper'.
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
tugs f = go where
  go 0 a = a
  go n a = maybe a (go (n - 1)) (f a)
{-# INLINE tugs #-}

-- | Move in a direction as far as you can go, then stop.
farthest :: (a -> Maybe a) -> a -> a
farthest f = go where
  go a = maybe a go (f a)
{-# INLINE farthest #-}

-- | This allows for you to repeatedly pull a 'zipper' in a given direction, failing if it falls of the end.
jerks :: (a -> Maybe a) -> Int -> a -> Maybe a
jerks f = go where
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
-- @'tooth' n = 'rights' n . 'leftmost'@
jerkTo :: Int -> (a :> b) -> Maybe (a :> b)
jerkTo n = jerks right n . farthest left
{-# INLINE jerkTo #-}

-- | Move the 'zipper' horizontally to the element in the @n@th position of the current level, absolutely indexed, starting with the @'farthest' 'left'@ as @0@.
--
-- If the element at that position doesn't exist, then this will clamp to the range @0 <= n < 'teeth'@.
tugTo :: Int -> (a :> b) -> a :> b
tugTo n = tugs right n . farthest left
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

-- | This is used to peel off the path information from a 'Coil' for use when saving the current path for later replay.
peel :: Coil h a -> Track h a
peel Coil               = Track
peel (Snoc h n l _ _ _) = Fork (peel h) n l

data Track :: * -> * -> * where
  Track :: Track Top a
  Fork  :: Track h b -> {-# UNPACK #-} !Int -> SimpleLensLike (Bazaar a a) b a -> Track (h :> b) a

restoreTrack :: Track h a -> Zipped h a -> Maybe (h :> a)
restoreTrack Track = Just . zipper
restoreTrack (Fork h n l) = restoreTrack h >=> jerks right n >=> within l

restoreNearTrack :: Track h a -> Zipped h a -> Maybe (h :> a)
restoreNearTrack Track = Just . zipper
restoreNearTrack (Fork h n l) = restoreNearTrack h >=> tugs right n >>> within l

unsafelyRestoreTrack :: Track h a -> Zipped h a -> h :> a
unsafelyRestoreTrack Track = zipper
unsafelyRestoreTrack (Fork h n l) = unsafelyRestoreTrack h >>> tugs right n >>> fromWithin l

-- | A 'Tape' is a recorded path through the 'Traversal' chain of a 'Zipper'.
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
