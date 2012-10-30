{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Traversals for manipulating parts of a list.
--
----------------------------------------------------------------------------
module Data.List.Lens
  ( _head
  , _tail
  , _last
  , _init
  , interspersed
  , intercalated
  -- * Traversals
  , traverseList
  , traverseHead
  , traverseTail
  , traverseInit
  , traverseLast
  , (~:), (=:)
  , (<~:), (<=:)
  , (++~), (<++~)
  , (++=), (<++=)
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State as State (MonadState, modify)
import Data.List

infixr 4 ++~, <++~
infixl 4 ~:, <~:
infix 4 =:, <=:, ++=, <++=

-- | A lens reading and writing to the head of a /non-empty/ list.
--
-- Attempting to read or write to the head of an /empty/ list will result in an 'error'.
--
-- >>> [1,2,3]^._head
-- 1
_head :: Simple Lens [a] a
_head _ [] = error "_head: empty list"
_head f (a:as) = (:as) <$> f a
{-# INLINE _head #-}

-- | A lens reading and writing to the tail of a /non-empty/ list
--
-- Attempting to read or write to the tail of an /empty/ list will result in an 'error'.
--
-- >>> _tail .~ [3,4,5] $ [1,2]
-- [1,3,4,5]
_tail :: Simple Lens [a] [a]
_tail _ [] = error "_tail: empty list"
_tail f (a:as) = (a:) <$> f as
{-# INLINE _tail #-}

-- | A lens reading and writing to the last element of a /non-empty/ list
--
-- Attempting to read or write to the last element of an /empty/ list will result in an 'error'.
--
-- >>> [1,2]^._last
-- 2
_last :: Simple Lens [a] a
_last _ []     = error "_last: empty list"
_last f [a]    = return <$> f a
_last f (a:as) = (a:) <$> _last f as
{-# INLINE _last #-}

-- | A lens reading and replacing all but the a last element of a /non-empty/ list
--
-- Attempting to read or write to all but the last element of an /empty/ list will result in an 'error'.
--
-- >>> [1,2,3,4]^._init
-- [1,2,3]
_init :: Simple Lens [a] [a]
_init _ [] = error "_init: empty list"
_init f as = (++ [Prelude.last as]) <$> f (Prelude.init as)
{-# INLINE _init #-}

-- | Obtain a version of the list with the supplied value interspersed.
--
-- >>> "abcde"^.interspersed ','
-- "a,b,c,d,e"
--
-- > xs^.interspersed a = intersperse a xs
interspersed :: a -> Getter [a] [a]
interspersed = to . intersperse
{-# INLINE interspersed #-}

-- | Obtain a version of the list with the supplied value intercalated.
intercalated :: [a] -> Getter [[a]] [a]
intercalated = to . intercalate
{-# INLINE intercalated #-}

-- | Indexed traversal of a list. The position in the list is available as the index.
traverseList :: IndexedTraversal Int [a] [b] a b
traverseList = index $ go (0::Int) where
  go !n f (x:xs) = (:) <$> f n x <*> go (n + 1) f xs
  go _ _ [] = pure []
{-# INLINE traverseList #-}

-- | A traversal for reading and writing to the head of a list
--
-- The position of the head in the original list (0) is available as the index.
--
-- >>> traverseHead +~ 1 $ [1,2,3]
-- [2,2,3]
--
-- @'traverseHead' :: 'Applicative' f => (a -> f a) -> [a] -> f [a]@
traverseHead :: SimpleIndexedTraversal Int [a] a
traverseHead = index $ \f aas -> case aas of
  []     -> pure []
  (a:as) -> (:as) <$> f (0::Int) a
{-# INLINE traverseHead #-}

-- | A traversal for editing the tail of a list
--
-- The position of each element /in the original list/ is available as the index.
--
-- >>> traverseTail +~ 1 $ [1,2,3]
-- [1,3,4]
--
-- @'traverseTail' :: 'Applicative' f => (a -> f a) -> [a] -> f [a]@
traverseTail :: SimpleIndexedTraversal Int [a] a
traverseTail = index $ \f aas -> case aas of
  []     -> pure []
  (a:as) -> (a:) <$> withIndex traverseList (f . (+1)) as
{-# INLINE traverseTail #-}

-- | A traversal the last element in a list
--
-- The position of the last element in the original list is available as the index.
--
-- >>> traverseLast +~ 1 $ [1,2,3]
-- [1,2,4]
--
-- @'traverseLast' :: 'Applicative' f => (a -> f a) -> [a] -> f [a]@
traverseLast :: SimpleIndexedTraversal Int [a] a
traverseLast = index $ \f xs0 -> let
    go [a]    n = return <$> f n a
    go (a:as) n = (a:) <$> (go as $! n + 1)
    go []     _ = pure []
  in go xs0 (0::Int) where
{-# INLINE traverseLast #-}

-- | A traversal of all but the last element of a list
--
-- The position of each element is available as the index.
--
-- >>> traverseInit +~ 1 $ [1,2,3]
-- [2,3,3]
--
-- @'traverseInit' :: 'Applicative' f => (a -> f a) -> [a] -> f [a]@
traverseInit :: SimpleIndexedTraversal Int [a] a
traverseInit = index $ \f aas -> case aas of
  [] -> pure []
  as -> (++ [Prelude.last as]) <$> withIndex traverseList f (Prelude.init as)
{-# INLINE traverseInit #-}

-- | Cons onto the list(s) referenced by a 'Setter'.
--
-- >>> 'h' ~: _1 $ ("ello","world")
-- ("hello","world")
--
-- @
-- ('~:') :: b -> 'Simple' 'Setter' s [a]    -> s -> s
-- ('~:') :: b -> 'Simple' 'Traversal' s [a] -> s -> s
-- ('~:') :: b -> 'Simple' 'Lens' s [a]      -> s -> s
-- ('~:') :: b -> 'Simple' 'Iso' s [a]       -> s -> s
-- @
(~:) :: a -> Setting s t [a] [a] -> s -> t
n ~: l = over l (n :)
{-# INLINE (~:) #-}

-- | Cons onto the list(s) referenced by a 'Setter' in your monad state
--
-- @
-- ('=:') :: 'MonadState' s m => a -> 'Simple' 'Setter' s [a]    -> m ()
-- ('=:') :: 'MonadState' s m => a -> 'Simple' 'Traversal' s [a] -> m ()
-- ('=:') :: 'MonadState' s m => a -> 'Simple' 'Lens' s [a]      -> m ()
-- ('=:') :: 'MonadState' s m => a -> 'Simple' 'Iso' s [a]       -> m ()
-- @
(=:) :: MonadState s m => a -> SimpleSetting s [a] -> m ()
n =: l = modify (n ~: l)
{-# INLINE (=:) #-}

-- | Cons onto the list(s) referenced by a 'Lens' (or 'Traversal'), returning the result.
--
-- If you use this with a 'Traversal' you will receive back the concatenation of all of
-- the resulting lists instead of an individual result.
--
-- >>> 'h' <~: _1 $ ("ello","world")
-- ("hello",("hello","world"))
--
-- @
-- ('<~:') :: b -> 'Simple' 'Lens' s [a]       -> s -> ([a], s)
-- ('<~:') :: b -> 'Simple' 'Iso' s [a]        -> s -> ([a], s)
-- ('<~:') :: b -> 'Simple' 'Traversal' s [a]  -> s -> ([a], s)
-- @
(<~:) :: a -> LensLike ((,)[a]) s t [a] [a] -> s -> ([a], t)
n <~: l = l <%~ (n :)
{-# INLINE (<~:) #-}

-- | Cons onto the list(s) referenced by a 'Lens' (or 'Traversal') into your monad state,
-- returning the result.
--
-- If you use this with a 'Traversal', you will receive back the concatenation of all
-- of the resulting lists instead of an individual result.
--
-- @
-- ('<=:') :: 'MonadState' s m => 'Simple' 'Lens' s [a]      -> a -> m [a]
-- ('<=:') :: 'MonadState' s m => 'Simple' 'Iso' s [a]       -> a -> m [a]
-- ('<=:') :: 'MonadState' s m => 'Simple' 'Traversal' s [a] -> a -> m [a]
-- @
(<=:) :: MonadState s m => a -> SimpleLensLike ((,)[a]) s [a] -> m [a]
n <=: l = l <%= (n :)
{-# INLINE (<=:) #-}


-- | Append to the target of a list-valued setter by appending to it with ('++').
--
-- ('Data.Monoid.<>~') generalizes this operation to an arbitrary 'Monoid'.
--
-- >>> :m + Control.Lens
-- >>> both ++~ "!!!" $ ("hello","world")
-- ("hello!!!","world!!!")
--
-- @
-- ('++~') :: 'Simple' 'Setter' s [a] -> [a] -> s -> s
-- ('++~') :: 'Simple' 'Iso' s [a] -> [a] -> s -> s
-- ('++~') :: 'Simple' 'Lens' s [a] -> [a] -> s -> s
-- ('++~') :: 'Simple' 'Traversal' s [a] -> [a] -> s -> s
-- @
(++~) :: Setting s t [a] [a] -> [a] -> s -> t
l ++~ n = over l (++ n)
{-# INLINE (++~) #-}

-- | Append to the target(s) of a 'Simple' 'Lens', 'Iso', 'Setter' or 'Traversal' with ('++') in the current state.
--
-- ('Data.Monoid.<>=') generalizes this operation to an arbitrary 'Monoid'.
--
-- @
-- ('++=') :: 'MonadState' s m => 'Simple' 'Setter' s [a] -> [a] -> m ()
-- ('++=') :: 'MonadState' s m => 'Simple' 'Iso' s [a] -> [a] -> m ()
-- ('++=') :: 'MonadState' s m => 'Simple' 'Lens' s [a] -> [a] -> m ()
-- ('++=') :: 'MonadState' s m => 'Simple' 'Traversal' s [a] -> [a] -> m ()
-- @
(++=) :: MonadState s m => SimpleSetting s [a] -> [a] -> m ()
l ++= b = State.modify (l ++~ b)
{-# INLINE (++=) #-}

-- | Append onto the end of the list targeted by a 'Lens' and return the result.
--
-- ('Data.Monoid.<<>~') generalizes this operation to an arbitrary 'Monoid'.
--
-- When using a 'Traversal', the result returned is actually the concatenation of all of the results.
--
-- When you do not need the result of the operation, ('++~') is more flexible.
(<++~) :: LensLike ((,)[a]) s t [a] [a] -> [a] -> s -> ([a], t)
l <++~ m = l <%~ (++ m)
{-# INLINE (<++~) #-}

-- | Append onto the end of the list targeted by a 'Lens' into the current monadic state, and return the result.
--
-- ('Data.Monoid.<<>=') generalizes this operation to an arbitrary 'Monoid'.
--
-- When using a 'Traversal', the result returned is actually the concatenation of all of the results.
--
-- When you do not need the result of the operation, ('++=') is more flexible.
(<++=) :: MonadState s m => SimpleLensLike ((,)[a]) s [a] -> [a] -> m [a]
l <++= m = l <%= (++ m)
{-# INLINE (<++=) #-}
