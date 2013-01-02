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
  ( (.|.~), (.&.~), (<.|.~), (<.&.~)
  , (.|.=), (.&.=), (<.|.=), (<.&.=)
  , bitAt
  , bits
  , byteAt
  ) where

import Control.Lens
import Control.Monad.State
import Data.Bits
import Data.Functor
import Data.Word

-- $setup
-- >>> import Data.Word

infixr 4 .|.~, .&.~, <.|.~, <.&.~
infix 4 .|.=, .&.=, <.|.=, <.&.=

-- | Bitwise '.|.' the target(s) of a 'Lens' or 'Setter'
--
-- >>> _2 .|.~ 6 $ ("hello",3)
-- ("hello",7)
--
-- @
-- ('.|.~') :: 'Bits' a => 'Setter' s t a a -> a -> s -> t
-- ('.|.~') :: 'Bits' a => 'Iso' s t a a -> a -> s -> t
-- ('.|.~') :: 'Bits' a => 'Lens' s t a a -> a -> s -> t
-- ('.|.~') :: ('Monoid a', 'Bits' a) => 'Traversal' s t a a -> a -> s -> t
-- @
(.|.~):: (Profunctor q, Bits a) => Overloading (->) q Mutator s t a a -> a -> q s t
l .|.~ n = over l (.|. n)
{-# INLINE (.|.~) #-}

-- | Bitwise '.&.' the target(s) of a 'Lens' or 'Setter'
--
-- >>> _2 .&.~ 7 $ ("hello",254)
-- ("hello",6)
--
-- @
-- ('.&.~') :: 'Bits' a => 'Setter' s t a a -> a -> s -> t
-- ('.&.~') :: 'Bits' a => 'Iso' s t a a -> a -> s -> t
-- ('.&.~') :: 'Bits' a => 'Lens' s t a a -> a -> s -> t
-- ('.&.~') :: ('Monoid a', 'Bits' a) => 'Traversal' s t a a -> a -> s -> t
-- @
(.&.~) :: (Profunctor q, Bits a) => Overloading (->) q Mutator s t a a -> a -> q s t
l .&.~ n = over l (.&. n)
{-# INLINE (.&.~) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by computing its bitwise '.&.' with another value.
--
-- >>> execState (do _1 .&.= 15; _2 .&.= 3) (7,7)
-- (7,3)
--
-- @
-- ('.&.=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Setter' s a -> a -> m ()
-- ('.&.=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Iso' s a -> a -> m ()
-- ('.&.=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Lens' s a -> a -> m ()
-- ('.&.=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Traversal' s a -> a -> m ()
-- @
(.&.=):: (MonadState s m, Bits a) => ASetter' s a -> a -> m ()
l .&.= a = modify (l .&.~ a)
{-# INLINE (.&.=) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by computing its bitwise '.|.' with another value.
--
-- >>> execState (do _1 .|.= 15; _2 .|.= 3) (7,7)
-- (15,7)
--
-- @
-- ('.|.=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Setter' s a -> a -> m ()
-- ('.|.=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Iso' s a -> a -> m ()
-- ('.|.=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Lens' s a -> a -> m ()
-- ('.|.=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Traversal' s a -> a -> m ()
-- @
(.|.=) :: (MonadState s m, Bits a) => ASetter' s a -> a -> m ()
l .|.= a = modify (l .|.~ a)
{-# INLINE (.|.=) #-}

-- | Bitwise '.|.' the target(s) of a 'Lens' (or 'Traversal'), returning the result
-- (or a monoidal summary of all of the results).
--
-- >>> _2 <.|.~ 6 $ ("hello",3)
-- (7,("hello",7))
--
-- @
-- ('<.|.~') :: 'Bits' a => 'Iso' s t a a -> a -> s -> (a, t)
-- ('<.|.~') :: 'Bits' a => 'Lens' s t a a -> a -> s -> (a, t)
-- ('<.|.~') :: ('Bits' a, 'Monoid a) => 'Traversal' s t a a -> a -> s -> (a, t)
-- @
(<.|.~):: (Profunctor q, Bits a) => Overloading (->) q ((,) a) s t a a -> a -> q s (a, t)
l <.|.~ n = l <%~ (.|. n)
{-# INLINE (<.|.~) #-}

-- | Bitwise '.&.' the target(s) of a 'Lens' or 'Traversal', returning the result
-- (or a monoidal summary of all of the results).
--
-- >>> _2 <.&.~ 7 $ ("hello",254)
-- (6,("hello",6))
--
-- @
-- ('<.&.~') :: 'Bits' a => 'Iso' s t a a -> a -> s -> (a, t)
-- ('<.&.~') :: 'Bits' a => 'Lens' s t a a -> a -> s -> (a, t)
-- ('<.&.~') :: ('Bits' a, 'Monoid a) => 'Traversal' s t a a -> a -> s -> (a, t)
-- @
(<.&.~) :: (Profunctor q, Bits a) => Overloading (->) q ((,) a) s t a a -> a -> q s (a, t)
l <.&.~ n = l <%~ (.&. n)
{-# INLINE (<.&.~) #-}

-- | Modify the target(s) of a 'Simple' 'Lens' (or 'Traversal') by computing its bitwise '.&.' with another value,
-- returning the result (or a monoidal summary of all of the results traversed)
--
-- >>> runState (_1 <.&.= 15) (31,0)
-- (15,(15,0))
--
-- @
-- ('<.&.=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Lens' s a -> a -> m a
-- ('<.&.=') :: ('MonadState' s m, 'Bits' a, 'Monoid' a) => 'Simple' 'Traversal' s a -> a -> m a
-- @
(<.&.=):: (MonadState s m, Bits a) => LensLike' ((,)a) s a -> a -> m a
l <.&.= b = l <%= (.&. b)
{-# INLINE (<.&.=) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', (or 'Traversal') by computing its bitwise '.|.' with another value,
-- returning the result (or a monoidal summary of all of the results traversed)
--
-- >>> runState (_1 <.|.= 7) (28,0)
-- (31,(31,0))
--
-- @
-- ('<.|.=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Lens' s a -> a -> m a
-- ('<.|.=') :: ('MonadState' s m, 'Bits' a, 'Monoid' a) => 'Simple' 'Traversal' s a -> a -> m a
-- @
(<.|.=) :: (MonadState s m, Bits a) => LensLike' ((,)a) s a -> a -> m a
l <.|.= b = l <%= (.|. b)
{-# INLINE (<.|.=) #-}

-- | This lens can be used to access the value of the nth bit in a number.
--
-- @'bitAt' n@ is only a legal 'Lens' into @b@ if @0 <= n < 'bitSize' ('undefined' :: b)@
--
-- >>> 16^.bitAt 4
-- True
--
-- >>> 15^.bitAt 4
-- False
--
-- >>> 15 & bitAt 4 .~ True
-- 31
--
-- >>> 16 & bitAt 4 .~ False
-- 0
bitAt :: Bits b => Int -> IndexedLens' Int b Bool
bitAt n f b = indexed f n (testBit b n) <&> \x -> if x then setBit b n else clearBit b n
{-# INLINE bitAt #-}

-- | Get the nth byte, counting from the high-end and starting from 0.
--
-- @'byteAt' n@ is only a legal 'Lens' into @b@ if @0 <= n < ('bitSize' ('undefined' :: b) `div` 8)@
--
-- >>> (0xff :: Word8)^.byteAt 0
-- 255
--
-- >>> (0xff00 :: Word16)^.byteAt 0
-- 255
--
-- >>> (0xff00 :: Word16)^.byteAt 1
-- 0
--
-- >>> (0xaabbccdd :: Word32)^.byteAt 0
-- 170
--
-- >>> (0xaabbccdd :: Word32)^.byteAt 1
-- 187
--
-- >>> (0xaabbccdd :: Word32)^.byteAt 2
-- 204
--
-- >>> (0xaabbccdd :: Word32)^.byteAt 3
-- 221
byteAt :: (Integral b, Bits b) => Int -> IndexedLens' Int b Word8
byteAt i f b = back <$> indexed f i (fromIntegral (255 .&. shiftR b offset)) where
  offset = bitSize b - (i + 1) * 8
  back w8 = b `xor` shiftL 255 offset .|. shiftL (fromIntegral w8) offset

-- | Traverse over all bits in a numeric type.
--
-- The bit position is available as the index.
--
-- >>> toListOf bits (5 :: Word8)
-- [True,False,True,False,False,False,False,False]
--
-- If you supply this an 'Integer', the result will be an infinite 'Traversal', which
-- can be productively consumed, but not reassembled.
bits :: (Num b, Bits b) => IndexedTraversal' Int b Bool
bits f b = Prelude.foldr step 0 <$> traverse g bs where
  g n      = (,) n <$> indexed f n (testBit b n)
  bs       = Prelude.takeWhile hasBit [0..]
  hasBit n = complementBit b n /= b -- test to make sure that complementing this bit actually changes the value
  step (n,True) r = setBit r n
  step _        r = r
{-# INLINE bits #-}
