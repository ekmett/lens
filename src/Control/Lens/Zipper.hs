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
-- >>> zipper ("hello","world") % down _1 % fromWithin traverse % focus .~ 'J' % rightmost % focus .~ 'y' % rezip
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
  -- ** Horizontal movement
  , up
  , down
  , within, fromWithin
  -- ** Lateral movement
  , left, left1, lefts, lefts1, leftmost
  , right, right1, rights, rights1, rightmost
  , goto, goto1, coordinate, width
  -- ** Closing the Zipper
  , rezip
  , Zipped
  , Zipper()
  -- ** Saving your Progress
  , Tape()
  , save
  , restore
  , restore1
  , unsafelyRestore
  ) where

import Control.Applicative
import Control.Category
import Control.Comonad
import Control.Monad ((>=>))
import Control.Lens.Indexed
import Control.Lens.IndexedLens
import Control.Lens.Internal
import Control.Lens.Plated
import Control.Lens.Type
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
-- This type operator associates to the right, so you can use a type like
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

-- | Return the index into the current 'Traversal'.
--
-- @'goto' ('coordinate' l) l = Just'@
coordinate :: (a :> b) -> Int
coordinate (Zipper _ (Level n _ _ _)) = n
{-# INLINE coordinate #-}

-- | Move the 'zipper' 'up', closing the current level and focusing on the parent element.
up :: (a :> b :> c) -> a :> b
up (Zipper (Snoc h n _ ls k rs) w) = Zipper h (Level n ls (k (rezipLevel w)) rs)
{-# INLINE up #-}

-- | Pull the 'zipper' 'left' within the current 'Traversal'.
left  :: (a :> b) -> Maybe (a :> b)
left (Zipper h w) = Zipper h <$> leftLevel w
{-# INLINE left #-}

-- | Try to pull the 'zipper' one entry to the 'left'.
--
-- If the entry to the left doesn't exist, then stay still.
left1 :: (a :> b) -> a :> b
left1 (Zipper h w) = Zipper h $ left1Level w
{-# INLINE left1 #-}

-- | Pull the entry one entry to the 'right'
right :: (a :> b) -> Maybe (a :> b)
right (Zipper h w) = Zipper h <$> rightLevel w
{-# INLINE right #-}

-- | Try to pull the 'zipper' one entry to the 'right'.
--
-- If the entry doesn't exist, then stay still.
right1 :: (a :> b) -> a :> b
right1 (Zipper h w) = Zipper h $ right1Level w
{-# INLINE right1 #-}

-- | Try to pull the 'zipper' @n@ entries to the 'right', returning 'Nothing' if you pull too far and run out of entries.
--
-- Passing a negative @n@ will move @-n@ entries to the 'left'.
rights :: Int -> (h :> a) -> Maybe (h :> a)
rights n z
  | n < 0 = lefts (-n) z
  | otherwise = go n z
  where go 0 c = Just c
        go k c = case right c of
          Nothing -> Nothing
          Just c' -> go (k - 1) c'

-- | Try to pull the 'zipper' @n@ entries to the 'left', returning 'Nothing' if you pull too far and run out of entries.
lefts :: Int -> (h :> a) -> Maybe (h :> a)
lefts k z
  | coordinate z < k = Nothing
  | otherwise = Just (lefts1 k z)

-- | Try to pull the 'zipper' @n@ entries to the 'left'. Stopping at the first entry if you run out of entries.
--
-- Passing a negative @n@ will move to @-n@ entries the right, and will return the last entry if you run out of entries.
lefts1 :: Int -> (h :> a) -> h :> a
lefts1 n z
  | n < 0 = rights1 (-n) z
  | otherwise = go n z
  where go 0 c = c
        go k c = case left c of
          Nothing -> c
          Just c' -> go (k - 1) c'

-- | Try to pull the 'zipper' @n@ entries to the 'right'. Stopping at the last entry if you run out of entries.
--
-- Passing a negative number will move to the left and will return the first entry if you run out of entries.
rights1 :: Int -> (h :> a) -> h :> a
rights1 n z
  | n < 0 = lefts1 (-n) z
  | otherwise = go n z
  where go 0 c = c
        go k c = case right c of
          Nothing -> c
          Just c' -> go (k - 1) c'

-- | Returns the number of siblings at the current level in the 'zipper'.
--
-- @'width' z '>=' 1@
--
-- /NB:/ If the current 'Traversal' targets an infinite number of elements then this may not terminate.
width :: (a :> b) -> Int
width (Zipper _ w) = levelWidth w
{-# INLINE width #-}

-- | Move the 'zipper' horizontally to the element in the @n@th position in the current level. (absolutely indexed, starting with the 'leftmost' as @0@)
--
-- This returns 'Nothing' if the target element doesn't exist.
--
-- @'goto' n = 'rights' n . 'leftmost'@
goto :: Int -> (a :> b) -> Maybe (a :> b)
goto n = rights n . leftmost
{-# INLINE goto #-}

-- | Move the 'zipper' horizontally to the element in the @n@th position of the current level. (absolutely indexed, starting with the 'leftmost' as @0@)
--
-- If the element at that position doesn't exist, then this will clamp to the range @0 <= n < 'width' z@ and return the element there.
goto1 :: Int -> (a :> b) -> a :> b
goto1 n = rights1 n . leftmost
{-# INLINE goto1 #-}

-- | Move to the left-most position of the current 'Traversal'.
leftmost :: (a :> b) -> a :> b
leftmost (Zipper h w) = Zipper h $ leftmostLevel w
{-# INLINE leftmost #-}

-- | Move to the right-most position of the current 'Traversal'.
rightmost :: (a :> b) -> a :> b
rightmost (Zipper h w) = Zipper h $ rightmostLevel w
{-# INLINE rightmost #-}

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
within l (Zipper h (Level n ls b rs)) = case partsOf l (Context id) b of
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
-- @'fromWithin' l ≡ 'fromJust ' '.' 'within' l@
--
-- but it is lazier in such a way that if this invariant is violated, some code
-- can still succeed if it is lazy enough in the use of the focused value.
fromWithin :: SimpleLensLike (Bazaar c c) b c -> (a :> b) -> a :> b :> c
fromWithin l (Zipper h (Level n ls b rs)) = case partsOf l (Context id) b of
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
restoreTrack (Fork h n l) = restoreTrack h >=> rights n >=> within l

restoreTrack1 :: Track h a -> Zipped h a -> Maybe (h :> a)
restoreTrack1 Track = Just . zipper
restoreTrack1 (Fork h n l) = restoreTrack1 h >=> rights1 n >>> within l

unsafelyRestoreTrack :: Track h a -> Zipped h a -> h :> a
unsafelyRestoreTrack Track = zipper
unsafelyRestoreTrack (Fork h n l) = unsafelyRestoreTrack h >>> rights1 n >>> fromWithin l

-- | A 'Tape' is a recorded path through the 'Traversal' chain of a 'Zipper'.
data Tape k where
  Tape :: Track h a -> {-# UNPACK #-} !Int -> Tape (h :> a)

-- | Save the current path as as a 'Tape' we can play back later.
save :: (a :> b) -> Tape (a :> b)
save (Zipper h (Level n _ _ _)) = Tape (peel h) n
{-# INLINE save #-}

-- | Restore ourselves to a previously recorded position precisely.
--
-- If the position does not exist, then fail.
restore :: Tape (h :> a) -> Zipped h a -> Maybe (h :> a)
restore (Tape h n) = restoreTrack h >=> rights n
{-# INLINE restore #-}

-- | Restore ourselves to a previously recorded position.
--
-- When moving left to right through a 'Traversal', if this will clamp at each level to the range @0 <= k < width@,
-- so the only failures will occur when one of the sequence of downward traversals find no targets.
restore1 :: Tape (h :> a) -> Zipped h a -> Maybe (h :> a)
restore1 (Tape h n) a = rights1 n <$> restoreTrack1 h a
{-# INLINE restore1 #-}

-- | Restore ourselves to a previously recorded position.
--
-- This assumes that nothing has been done in the meantime to affect the existence of anything on the entire path.
--
-- Motions left or right are clamped, but all traversals included on the 'Tape' are assumed to be non-empty.
--
-- Violate these assumptions at your own risk.
unsafelyRestore :: Tape (h :> a) -> Zipped h a -> h :> a
unsafelyRestore (Tape h n) = unsafelyRestoreTrack h >>> rights1 n
{-# INLINE unsafelyRestore #-}
