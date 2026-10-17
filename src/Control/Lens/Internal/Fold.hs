{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Fold
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Fold
  (
  -- * Monoids for folding
    Folding(..)
  , Traversed(..)
  , TraversedF(..)
  , Sequenced(..)
  , Leftmost(..), getLeftmost
  , Rightmost(..), getRightmost
  , ReifiedMonoid(..)
  -- * Semigroups for folding
  , NonEmptyDList(..)
  ) where

import Prelude ()

import Control.Lens.Internal.Getter
import Control.Lens.Internal.Prelude
import Data.Functor.Bind
import Data.Reflection

import qualified Data.List.NonEmpty as NonEmpty

------------------------------------------------------------------------------
-- Folding
------------------------------------------------------------------------------

-- | A 'Monoid' for a 'Contravariant' 'Applicative'.
newtype Folding f a = Folding { getFolding :: f a }

instance (Contravariant f, Applicative f) => Semigroup (Folding f a) where
  Folding fr <> Folding fs = Folding (fr *> fs)
  {-# INLINE (<>) #-}

instance (Contravariant f, Applicative f) => Monoid (Folding f a) where
  mempty = Folding noEffect
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  Folding fr `mappend` Folding fs = Folding (fr *> fs)
  {-# INLINE mappend #-}
#endif

------------------------------------------------------------------------------
-- Traversed
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Traversal.traverseOf_' and the like.
--
-- The argument 'a' of the result should not be used!
newtype Traversed a f = Traversed { getTraversed :: f a }

-- See 4.16 Changelog entry for the explanation of "why not Apply f =>"?
instance Applicative f => Semigroup (Traversed a f) where
  Traversed ma <> Traversed mb = Traversed (ma *> mb)
  {-# INLINE (<>) #-}

instance Applicative f => Monoid (Traversed a f) where
  mempty = Traversed (pure (error "Traversed: value used"))
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  Traversed ma `mappend` Traversed mb = Traversed (ma *> mb)
  {-# INLINE mappend #-}
#endif

------------------------------------------------------------------------------
-- TraversedF
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Fold.traverse1Of_' and the like.
--
-- @since 4.16
newtype TraversedF a f = TraversedF { getTraversedF :: f a }

instance Apply f => Semigroup (TraversedF a f) where
  TraversedF ma <> TraversedF mb = TraversedF (ma .> mb)
  {-# INLINE (<>) #-}

instance (Apply f, Applicative f) => Monoid (TraversedF a f) where
  mempty = TraversedF (pure (error "TraversedF: value used"))
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  TraversedF ma `mappend` TraversedF mb = TraversedF (ma *> mb)
  {-# INLINE mappend #-}
#endif

------------------------------------------------------------------------------
-- Sequenced
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Traversal.mapM_' and the like.
--
-- The argument 'a' of the result should not be used!
--
-- See 4.16 Changelog entry for the explanation of "why not Apply f =>"?
newtype Sequenced a m = Sequenced { getSequenced :: m a }

instance Monad m => Semigroup (Sequenced a m) where
  Sequenced ma <> Sequenced mb = Sequenced (ma >> mb)
  {-# INLINE (<>) #-}

instance Monad m => Monoid (Sequenced a m) where
  mempty = Sequenced (return (error "Sequenced: value used"))
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  Sequenced ma `mappend` Sequenced mb = Sequenced (ma >> mb)
  {-# INLINE mappend #-}
#endif

------------------------------------------------------------------------------
-- NonEmptyDList
------------------------------------------------------------------------------

newtype NonEmptyDList a
  = NonEmptyDList { getNonEmptyDList :: [a] -> NonEmpty.NonEmpty a }

instance Semigroup (NonEmptyDList a) where
  NonEmptyDList f <> NonEmptyDList g = NonEmptyDList (f . NonEmpty.toList . g)

------------------------------------------------------------------------------
-- Leftmost and Rightmost
------------------------------------------------------------------------------

-- | Used for 'Control.Lens.Fold.firstOf'. '<>' only builds a node, and
-- 'getLeftmost' searches the tree.
data Leftmost a
  = LPure                           -- ^ No element.
  | LLeaf a                         -- ^ One element.
  | LNode (Leftmost a) (Leftmost a) -- ^ The elements of the first field, then those of the second.

instance Semigroup (Leftmost a) where
  (<>) = LNode
  {-# INLINE (<>) #-}

instance Monoid (Leftmost a) where
  mempty = LPure
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  mappend = LNode
  {-# INLINE mappend #-}
#endif

-- | Extract the 'Leftmost' element. The result is 'Just' as soon as the
-- search finds any element, so @firstOf (backwards folded) [1..]@ is
-- @Just _|_@.
getLeftmost :: Leftmost a -> Maybe a
getLeftmost = nearest

-- | Used for 'Control.Lens.Fold.lastOf'. The mirror image of 'Leftmost'.
data Rightmost a
  = RPure                             -- ^ No element.
  | RLeaf a                           -- ^ One element.
  | RNode (Rightmost a) (Rightmost a) -- ^ The elements of the first field, then those of the second.

instance Semigroup (Rightmost a) where
  (<>) = RNode
  {-# INLINE (<>) #-}

instance Monoid (Rightmost a) where
  mempty = RPure
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  mappend = RNode
  {-# INLINE mappend #-}
#endif

-- | Extract the 'Rightmost' element. The result is 'Just' as soon as the
-- search finds any element, so @lastOf folded [1..]@ is @Just _|_@.
getRightmost :: Rightmost a -> Maybe a
getRightmost = nearest

-- The search for the near-most element: the first one of a 'Leftmost', the
-- last one of a 'Rightmost'.
--
-- It searches the near child of each node first, so on a balanced tree, such
-- as a strict ByteString under bytes or a Map under traverse, it keeps one
-- path of the tree, not a whole level. 'work' stops at the first element it
-- finds, which gives 'Just'. Then 'nearest' searches the near side of that
-- element for a nearer one, and so on.
--
-- To stay fair on infinite folds, a node whose two children are both
-- unfinished pays its far child one unit for every 'pathRatio' units of work
-- in its near child. A node does not report these payments to the nodes
-- above it, so the payments do not grow from level to level. Inside a far
-- child the search is again near child first, up to 'depthBound', and its
-- own far children get one unit for every 'shareRatio' units. So an element
-- behind several far turns gets a very small share: fairness holds only in
-- the limit.
--
-- A chain, such as a lazy ByteString, has a far child (a chunk) at every
-- level. So that the search does not keep all the chunks, a node with
-- 'depthBound' or more unfinished far children above it works in reverse:
-- its far child gets the work first, and its near child gets one unit for
-- every 'reverseRatio' units. So an element behind many such levels also
-- gets a very small share.
--
-- The first look at a node forces its near child, and its far child unless
-- the near child is an element. The tests "firstOf partial inner list" and
-- "lastOf backwards partial inner list" in tests/hunit.hs check this.

-- | A tree that 'nearest' can search.
class Searchable t where
  -- | A node, with the child to search first on the left.
  look :: t a -> Look t a

data Look t a = LookPure | LookLeaf a | LookNode (t a) (t a)

instance Searchable Leftmost where
  look LPure       = LookPure
  look (LLeaf a)   = LookLeaf a
  look (LNode x y) = LookNode x y
  {-# INLINE look #-}

instance Searchable Rightmost where
  look RPure       = LookPure
  look (RLeaf a)   = LookLeaf a
  look (RNode x y) = LookNode y x
  {-# INLINE look #-}

-- | The state of the search in one subtree.
data Search t a
  = Unseen (t a)
    -- Not looked at yet.
  | Exhausted
    -- No element.
  | Found a
    -- The near-most element.
  | Both !Int !Int (Search t a) (Search t a)
    -- Both children unfinished, so neither is 'Exhausted', 'Found' or
    -- 'Fallback': a credit (see 'work'), the work the far child has used,
    -- the near child, the far child.
  | Fallback (Search t a) (Search t a)
    -- The near child is unfinished, with no element found yet; the far child
    -- has an element ('Found' or 'Fallback').

-- | 'Path': the main path of the search. 'Share': a far child that is paid a
-- share of the work.
data Mode = Path | Share

-- | The new state, and the work used so far.
data Step t a = Step !(Search t a) !Int

-- | On the main path, a far child that has used @w@ units of work is paid one
-- unit for this many units of near work. It is large, because the payments
-- are wasted when the element is on the near side, and it grows with @w@,
-- so that a large far child cannot take over.
pathRatio :: Int -> Int
pathRatio w = 1024 * (1 + w `quot` 1024)

-- | The same inside a far child. It is smaller than 'pathRatio', because the
-- share of an element behind several far turns gets smaller by this ratio
-- at each turn.
shareRatio :: Int -> Int
shareRatio w = 128 * (1 + w `quot` 1024)

-- | From this many unfinished far children above it, a node works in
-- reverse. The paths of balanced folds are shorter in practice (a balanced
-- tree of 2^32 elements); a chain reaches it after this many chunks.
depthBound :: Int
depthBound = 32

-- | In reverse, the near child is paid one unit for this many units of far
-- work. It is large, so a chain finishes its chunks about one at a time,
-- which is also faster for the garbage collector.
reverseRatio :: Int
reverseRatio = 128

-- | The work in one round of the search. Each round walks the path from the
-- root again, so it is large.
roundWork :: Int
roundWork = 4096

hasElement :: Search t a -> Bool
hasElement s = case s of
  Found _      -> True
  Fallback _ _ -> True
  _            -> False

nearest :: Searchable t => t a -> Maybe a
nearest t0 = case look t0 of
  LookPure -> Nothing
  LookLeaf a -> Just a
  LookNode x _ | LookLeaf a <- look x -> Just a
  _ -> looking (Unseen t0)
  where
    looking s = case s of
      Exhausted    -> Nothing
      Found a      -> Just a
      Fallback _ _ -> Just (finishing s)
      _            -> case work Path 0 roundWork 0 s of Step s' _ -> looking s'
    -- The state is 'Found' or 'Fallback'. The answer is an element of the
    -- near child of the 'Fallback' if it has one, and else one of the far
    -- child.
    finishing s = case s of
      Found a -> a
      Fallback (Unseen x) (Found a) -> finishing (fallback roundWork x a)
      Fallback near far -> case work Path 0 roundWork 0 near of
        Step near' _ -> case near' of
          Exhausted -> finishing far
          _ | hasElement near' -> finishing near'
            | otherwise        -> finishing (Fallback near' far)
      _ -> error "Control.Lens.Internal.Fold.nearest: internal error, an element was lost"
{-# SPECIALIZE nearest :: Leftmost a -> Maybe a #-}
{-# SPECIALIZE nearest :: Rightmost a -> Maybe a #-}

-- | @work mode depth budget used s@ does about @budget@ units of work on @s@,
-- and stops at the first element it finds. @depth@ is the number of
-- unfinished far children above @s@, and @used@ is the work used so far. The
-- result adds the work done here, except the payments of 'pay'.
--
-- While @depth@ is less than 'depthBound', on the main path the near child
-- gets the work, and then the far child is paid ('pay'). In 'Share' mode
-- @budget@ is all the work: first the far child is paid ('shareFar'), then
-- the near child gets the rest. The credit of a 'Both' node is the near work
-- not yet paid to its far child. From 'depthBound' on, the children change
-- roles ('reverseNear').
work :: Searchable t => Mode -> Int -> Int -> Int -> Search t a -> Step t a
work !mode !depth !budget !used s
  | budget <= 0 = Step s used
  | otherwise = case s of
      Unseen t -> case look t of
        LookPure     -> Step Exhausted (used + 1)
        LookLeaf a   -> Step (Found a) (used + 1)
        LookNode x y -> open mode depth (budget - 1) (used + 1) x y
      Exhausted    -> Step s used
      Found _      -> Step s used
      Fallback _ _ -> Step s used -- only 'finishing' goes on with a 'Fallback'
      Both credit farUsed near far
        | depth >= depthBound -> reverseNear mode depth budget used credit farUsed near far
        | Share <- mode -> shareFar depth budget used credit farUsed near far
        | otherwise -> case work Path (depth + 1) budget 0 near of
            Step near' nearWork -> case near' of
              Exhausted -> work Path depth (budget - nearWork) (used + nearWork) far
              _ | hasElement near' -> Step near' (used + nearWork)
                | otherwise -> pay depth (credit + nearWork) farUsed near' far (used + nearWork)

-- | 'Share' mode: pay the far child first, for the near work @credit@; then
-- the near child gets the rest ('shareNear').
shareFar
  :: Searchable t
  => Int -> Int -> Int -> Int -> Int -> Search t a -> Search t a -> Step t a
shareFar !depth !budget !used !credit !farUsed near far =
  let !ratio = shareRatio farUsed
      !payment = min budget (credit `quot` ratio)
  in if payment <= 0
       then shareNear depth budget used credit farUsed near far
       else case work Share (depth + 1) payment 0 far of
         Step far' farWork -> case far' of
           Exhausted -> work Share depth (budget - farWork) (used + farWork) near
           _ | hasElement far' -> Step (Fallback near far') (used + farWork)
             | otherwise ->
                 shareNear depth (budget - farWork) (used + farWork)
                   (max 0 (credit - farWork * ratio)) (farUsed + farWork) near far'

-- | 'Share' mode, after paying the far child: the near child gets the rest.
shareNear
  :: Searchable t
  => Int -> Int -> Int -> Int -> Int -> Search t a -> Search t a -> Step t a
shareNear !depth !budget !used !credit !farUsed near far
  | budget <= 0 = Step (Both credit farUsed near far) used
  | otherwise = case work Share (depth + 1) budget 0 near of
      Step near' nearWork -> case near' of
        Exhausted -> work Share depth (budget - nearWork) (used + nearWork) far
        _ | hasElement near' -> Step near' (used + nearWork)
          | otherwise -> Step (Both (credit + nearWork) farUsed near' far) (used + nearWork)

-- | On the main path: pay the far child for the near work @credit@, and rebuild
-- the node. The near child is unfinished. The payment is not added to
-- @used@, so it is not in the credit of the nodes above.
pay :: Searchable t => Int -> Int -> Int -> Search t a -> Search t a -> Int -> Step t a
pay !depth !credit !farUsed near far !used =
  let !ratio = pathRatio farUsed
      !payment = credit `quot` ratio
  in if payment <= 0
       then Step (Both credit farUsed near far) used
       else case work Share (depth + 1) payment 0 far of
         Step far' farWork -> case far' of
           Exhausted -> Step near used
           _ | hasElement far' -> Step (Fallback near far') used
             | otherwise ->
                 Step (Both (max 0 (credit - farWork * ratio)) (farUsed + farWork) near far') used

-- | From 'depthBound' on, the credit is the far work not yet paid to the
-- near child. Pay the near child first; then the far child gets the rest.
-- When nodes above it finish, a node can drop below 'depthBound', and its
-- credit then counts as near work; that costs at most a few units.
reverseNear
  :: Searchable t
  => Mode -> Int -> Int -> Int -> Int -> Int -> Search t a -> Search t a -> Step t a
reverseNear !mode !depth !budget !used !credit !farUsed near far =
  let !payment = min budget (credit `quot` reverseRatio)
  in if payment <= 0
       then reverseFar mode depth budget used credit farUsed near far
       else case work mode (depth + 1) payment 0 near of
         Step near' nearWork -> case near' of
           Exhausted -> work mode depth (budget - nearWork) (used + nearWork) far
           _ | hasElement near' -> Step near' (used + nearWork)
             | otherwise ->
                 reverseFar mode depth (budget - nearWork) (used + nearWork)
                   (max 0 (credit - nearWork * reverseRatio)) farUsed near' far

-- | From 'depthBound' on, after paying the near child: the far child gets
-- the rest.
reverseFar
  :: Searchable t
  => Mode -> Int -> Int -> Int -> Int -> Int -> Search t a -> Search t a -> Step t a
reverseFar !mode !depth !budget !used !credit !farUsed near far
  | budget <= 0 = Step (Both credit farUsed near far) used
  | otherwise = case work Share (depth + 1) budget 0 far of
      Step far' farWork -> case far' of
        Exhausted -> work mode depth (budget - farWork) (used + farWork) near
        _ | hasElement far' -> Step (Fallback near far') (used + farWork)
          | otherwise ->
              Step (Both (credit + farWork) (farUsed + farWork) near far') (used + farWork)

-- | Look at a node with near child @x@ and far child @y@.
open :: Searchable t => Mode -> Int -> Int -> Int -> t a -> t a -> Step t a
open !mode !depth !budget !used x y = case look x of
  LookLeaf a   -> Step (Found a) (used + 1)
  LookPure     -> work mode depth (budget - 1) (used + 1) (Unseen y)
  LookNode _ _ -> case look y of
    LookPure     -> work mode depth (budget - 2) (used + 2) (Unseen x)
    LookLeaf a   -> Step (Fallback (Unseen x) (Found a)) (used + 2)
    LookNode _ _ -> work mode depth (budget - 2) (used + 2) (Both 0 0 (Unseen x) (Unseen y))

-- | For 'nearest': search @x@ for about @budget@ units, with @a@ the answer
-- if @x@ has no element. A far child that is one element becomes the new
-- @a@, so a chain, such as a list, needs no search state.
fallback :: Searchable t => Int -> t a -> a -> Search t a
fallback !budget x a
  | budget <= 0 = Fallback (Unseen x) (Found a)
  | otherwise = case look x of
      LookPure       -> Found a
      LookLeaf a'    -> Found a'
      LookNode x' y' -> case look x' of
        LookLeaf a'  -> Found a'
        LookPure     -> fallback (budget - 2) y' a
        LookNode _ _ -> case look y' of
          LookPure     -> fallback (budget - 3) x' a
          LookLeaf a'  -> fallback (budget - 3) x' a'
          LookNode _ _ -> Fallback (Both 0 0 (Unseen x') (Unseen y')) (Found a)
