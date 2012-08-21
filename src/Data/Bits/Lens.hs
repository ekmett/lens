{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bits.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  LiberalTypeSynonyms
--
----------------------------------------------------------------------------
module Data.Bits.Lens
  ( (|~), (&~), (<|~), (<&~)
  , (|=), (&=), (<|=), (<&=)
  , bitAt
  , traverseBits
  ) where

import Control.Lens
import Control.Monad.State.Class
import Data.Bits
import Data.Functor

infixr 4 |~, &~, <|~, <&~
infix 4 |=, &=, <|=, <&=

-- | Bitwise '.|.' the target(s) of a 'Lens' or 'Setter'
--
-- >>> _2 |~ 6 $ ("hello",3)
-- ("hello",7)
--
-- @
-- (|~) :: 'Bits' c => 'Setter' a b c c -> c -> a -> b
-- (|~) :: 'Bits' c => 'Iso' a b c c -> c -> a -> b
-- (|~) :: 'Bits' c => 'Lens' a b c c -> c -> a -> b
-- (|~) :: ('Monoid c', 'Bits' c) => 'Traversal' a b c c -> c -> a -> b
-- @
(|~):: Bits c => Setting a b c c -> c -> a -> b
l |~ n = over l (.|. n)
{-# INLINE (|~) #-}

-- | Bitwise '.&.' the target(s) of a 'Lens' or 'Setter'
--
-- >>> _2 &~ 7 $ ("hello",254)
-- ("hello",6)
--
-- @
-- (&~) :: 'Bits' c => 'Setter' a b c c -> c -> a -> b
-- (&~) :: 'Bits' c => 'Iso' a b c c -> c -> a -> b
-- (&~) :: 'Bits' c => 'Lens' a b c c -> c -> a -> b
-- (&~) :: ('Monoid c', 'Bits' c) => 'Traversal' a b c c -> c -> a -> b
-- @
(&~) :: Bits c => Setting a b c c -> c -> a -> b
l &~ n = over l (.&. n)
{-# INLINE (&~) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by computing its bitwise '.&.' with another value.
--
-- @
-- (&=):: ('MonadState' a m, 'Bits' b) => 'Simple' 'Setter' a b -> b -> m ()
-- (&=):: ('MonadState' a m, 'Bits' b) => 'Simple' 'Iso' a b -> b -> m ()
-- (&=):: ('MonadState' a m, 'Bits' b) => 'Simple' 'Lens' a b -> b -> m ()
-- (&=):: ('MonadState' a m, 'Bits' b) => 'Simple' 'Traversal' a b -> b -> m ()
-- @
(&=):: (MonadState a m, Bits b) => Simple Setting a b -> b -> m ()
l &= b = modify (l &~ b)
{-# INLINE (&=) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by computing its bitwise '.|.' with another value.
--
-- @
-- (|=):: ('MonadState' a m, 'Bits' b) => 'Simple' 'Setter' a b -> b -> m ()
-- (|=):: ('MonadState' a m, 'Bits' b) => 'Simple' 'Iso' a b -> b -> m ()
-- (|=):: ('MonadState' a m, 'Bits' b) => 'Simple' 'Lens' a b -> b -> m ()
-- (|=):: ('MonadState' a m, 'Bits' b) => 'Simple' 'Traversal' a b -> b -> m ()
-- @
(|=) :: (MonadState a m, Bits b) => Simple Setting a b -> b -> m ()
l |= b = modify (l |~ b)
{-# INLINE (|=) #-}

-- | Bitwise '.|.' the target(s) of a 'Lens' (or 'Traversal'), returning the result
-- (or a monoidal summary of all of the results).
--
-- >>> _2 <|~ 6 $ ("hello",3)
-- (7,("hello",7))
--
-- @
-- (\<|~) :: 'Bits' c => 'Iso' a b c c -> c -> a -> (c, b)
-- (\<|~) :: 'Bits' c => 'Lens' a b c c -> c -> a -> (c, b)
-- (\<|~) :: ('Bits' c, 'Monoid c) => 'Traversal' a b c c -> c -> a -> (c, b)
-- @
(<|~):: Bits c => LensLike ((,) c) a b c c -> c -> a -> (c, b)
l <|~ n = l <%~ (.|. n)
{-# INLINE (<|~) #-}

-- | Bitwise '.&.' the target(s) of a 'Lens' or 'Traversal', returning the result
-- (or a monoidal summary of all of the results).
--
-- >>> _2 <&~ 7 $ ("hello",254)
-- (6,("hello",6))
--
-- @
-- (\<&~) :: 'Bits' c => 'Iso' a b c c -> c -> a -> (c, b)
-- (\<&~) :: 'Bits' c => 'Lens' a b c c -> c -> a -> (c, b)
-- (\<&~) :: ('Bits' c, 'Monoid c) => 'Traversal' a b c c -> c -> a -> (c, b)
-- @
(<&~) :: Bits c => LensLike ((,) c) a b c c -> c -> a -> (c, b)
l <&~ n = l <%~ (.&. n)
{-# INLINE (<&~) #-}

-- | Modify the target(s) of a 'Simple' 'Lens' (or 'Traversal') by computing its bitwise '.&.' with another value,
-- returning the result (or a monoidal summary of all of the results traversed)
--
-- @
-- (\<&=) :: ('MonadState' a m, 'Bits' b) => 'Simple' 'Lens' a b -> b -> m b
-- (\<&=) :: ('MonadState' a m, 'Bits' b, 'Monoid' b) => 'Simple' 'Traversal' a b -> b -> m b
-- @
(<&=):: (MonadState a m, Bits b) => SimpleLensLike ((,)b) a b -> b -> m b
l <&= b = l <%= (.&. b)
{-# INLINE (<&=) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', (or 'Traversal') by computing its bitwise '.|.' with another value,
-- returning the result (or a monoidal summary of all of the results traversed)
--
-- @
-- (\<|=) :: ('MonadState' a m, 'Bits' b) => 'Simple' 'Lens' a b -> b -> m b
-- (\<|=) :: ('MonadState' a m, 'Bits' b, 'Monoid' b) => 'Simple' 'Traversal' a b -> b -> m b
-- @
(<|=) :: (MonadState a m, Bits b) => SimpleLensLike ((,)b) a b -> b -> m b
l <|= b = l <%= (.|. b)
{-# INLINE (<|=) #-}

-- | This lens can be used to access the value of the nth bit in a number.
--
-- @'bitAt' n@ is only a legal 'Lens' into @b@ if @0 <= n < 'bitSize' ('undefined' :: b)@
--
-- >>> 16^.bitAt 4
-- True
--
-- >>> 15^.bitAt 4
-- False
bitAt :: Bits b => Int -> SimpleIndexedLens Int b Bool
bitAt n = index $ \f b -> (\x -> if x then setBit b n else clearBit b n) <$> f n (testBit b n)
{-# INLINE bitAt #-}

-- | Traverse over all bits in a numeric type.
--
-- The bit position is available as the index.
--
-- >>> import Data.Word
-- >>> toListOf traverseBits (5 :: Word8)
-- [True,False,True,False,False,False,False,False]
--
-- If you supply this an 'Integer', the result will
-- be an infinite 'Traversal' that can be productively consumed.
traverseBits :: (Num b, Bits b) => SimpleIndexedTraversal Int b Bool
traverseBits = index $ \f b -> let
    g n      = (,) n <$> f n (testBit b n)
    bits     = Prelude.takeWhile hasBit [0..]
    hasBit n = complementBit b n /= b -- test to make sure that complementing this bit actually changes the value
    step (n,True) r = setBit r n
    step _        r = r
  in Prelude.foldr step 0 <$> traverse g bits
{-# INLINE traverseBits #-}
