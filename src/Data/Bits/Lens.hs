{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bits.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  LiberalTypeSynonyms
--
----------------------------------------------------------------------------
module Data.Bits.Lens
  ( (.|.~), (.&.~), (<.|.~), (<.&.~), (<<.|.~), (<<.&.~)
  , (.|.=), (.&.=), (<.|.=), (<.&.=), (<<.|.=), (<<.&.=)
  , bitAt
  , bits
  , byteAt
  , bytewise
  ) where

import Control.Lens
import Control.Monad.State
import Data.Bits
import Data.Word

#if !MIN_VERSION_base(4,8,0)
import Data.Functor
#endif

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Data.Word

infixr 4 .|.~, .&.~, <.|.~, <.&.~, <<.|.~, <<.&.~
infix 4 .|.=, .&.=, <.|.=, <.&.=, <<.|.=, <<.&.=

-- | Bitwise '.|.' the target(s) of a 'Lens' or 'Setter'.
--
-- >>> _2 .|.~ 6 $ ("hello",3)
-- ("hello",7)
--
-- @
-- ('.|.~') :: 'Bits' a             => 'Setter' s t a a    -> a -> s -> t
-- ('.|.~') :: 'Bits' a             => 'Iso' s t a a       -> a -> s -> t
-- ('.|.~') :: 'Bits' a             => 'Lens' s t a a      -> a -> s -> t
-- ('.|.~') :: ('Data.Monoid.Monoid' a, 'Bits' a) => 'Traversal' s t a a -> a -> s -> t
-- @
(.|.~):: Bits a => ASetter s t a a -> a -> s -> t
l .|.~ n = over l (.|. n)
{-# INLINE (.|.~) #-}

-- | Bitwise '.&.' the target(s) of a 'Lens' or 'Setter'.
--
-- >>> _2 .&.~ 7 $ ("hello",254)
-- ("hello",6)
--
-- @
-- ('.&.~') :: 'Bits' a             => 'Setter' s t a a    -> a -> s -> t
-- ('.&.~') :: 'Bits' a             => 'Iso' s t a a       -> a -> s -> t
-- ('.&.~') :: 'Bits' a             => 'Lens' s t a a      -> a -> s -> t
-- ('.&.~') :: ('Data.Monoid.Monoid' a, 'Bits' a) => 'Traversal' s t a a -> a -> s -> t
-- @
(.&.~) :: Bits a => ASetter s t a a -> a -> s -> t
l .&.~ n = over l (.&. n)
{-# INLINE (.&.~) #-}

-- | Modify the target(s) of a 'Lens'', 'Setter'' or 'Traversal'' by computing its bitwise '.&.' with another value.
--
-- >>> execState (do _1 .&.= 15; _2 .&.= 3) (7,7)
-- (7,3)
--
-- @
-- ('.&.=') :: ('MonadState' s m, 'Bits' a) => 'Setter'' s a    -> a -> m ()
-- ('.&.=') :: ('MonadState' s m, 'Bits' a) => 'Iso'' s a       -> a -> m ()
-- ('.&.=') :: ('MonadState' s m, 'Bits' a) => 'Lens'' s a      -> a -> m ()
-- ('.&.=') :: ('MonadState' s m, 'Bits' a) => 'Traversal'' s a -> a -> m ()
-- @
(.&.=):: (MonadState s m, Bits a) => ASetter' s a -> a -> m ()
l .&.= a = modify (l .&.~ a)
{-# INLINE (.&.=) #-}

-- | Modify the target(s) of a 'Lens'', 'Setter' or 'Traversal' by computing its bitwise '.|.' with another value.
--
-- >>> execState (do _1 .|.= 15; _2 .|.= 3) (7,7)
-- (15,7)
--
-- @
-- ('.|.=') :: ('MonadState' s m, 'Bits' a) => 'Setter'' s a    -> a -> m ()
-- ('.|.=') :: ('MonadState' s m, 'Bits' a) => 'Iso'' s a       -> a -> m ()
-- ('.|.=') :: ('MonadState' s m, 'Bits' a) => 'Lens'' s a      -> a -> m ()
-- ('.|.=') :: ('MonadState' s m, 'Bits' a) => 'Traversal'' s a -> a -> m ()
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
-- ('<.|.~') :: 'Bits' a             => 'Iso' s t a a       -> a -> s -> (a, t)
-- ('<.|.~') :: 'Bits' a             => 'Lens' s t a a      -> a -> s -> (a, t)
-- ('<.|.~') :: ('Bits' a, 'Data.Monoid.Monoid' a) => 'Traversal' s t a a -> a -> s -> (a, t)
-- @
(<.|.~):: Bits a => LensLike ((,) a) s t a a -> a -> s -> (a, t)
l <.|.~ n = l <%~ (.|. n)
{-# INLINE (<.|.~) #-}

-- | Bitwise '.&.' the target(s) of a 'Lens' or 'Traversal', returning the result
-- (or a monoidal summary of all of the results).
--
-- >>> _2 <.&.~ 7 $ ("hello",254)
-- (6,("hello",6))
--
-- @
-- ('<.&.~') :: 'Bits' a             => 'Iso'       s t a a -> a -> s -> (a, t)
-- ('<.&.~') :: 'Bits' a             => 'Lens'      s t a a -> a -> s -> (a, t)
-- ('<.&.~') :: ('Bits' a, 'Data.Monoid.Monoid' a) => 'Traversal' s t a a -> a -> s -> (a, t)
-- @
(<.&.~) :: Bits a => LensLike ((,) a) s t a a -> a -> s -> (a, t)
l <.&.~ n = l <%~ (.&. n)
{-# INLINE (<.&.~) #-}

-- | Modify the target(s) of a 'Lens'' (or 'Traversal'') by computing its bitwise '.&.' with another value,
-- returning the result (or a monoidal summary of all of the results traversed).
--
-- >>> runState (_1 <.&.= 15) (31,0)
-- (15,(15,0))
--
-- @
-- ('<.&.=') :: ('MonadState' s m, 'Bits' a)           => 'Lens'' s a      -> a -> m a
-- ('<.&.=') :: ('MonadState' s m, 'Bits' a, 'Data.Monoid.Monoid' a) => 'Traversal'' s a -> a -> m a
-- @
(<.&.=):: (MonadState s m, Bits a) => LensLike' ((,)a) s a -> a -> m a
l <.&.= b = l <%= (.&. b)
{-# INLINE (<.&.=) #-}

-- | Modify the target(s) of a 'Lens'', (or 'Traversal') by computing its bitwise '.|.' with another value,
-- returning the result (or a monoidal summary of all of the results traversed).
--
-- >>> runState (_1 <.|.= 7) (28,0)
-- (31,(31,0))
--
-- @
-- ('<.|.=') :: ('MonadState' s m, 'Bits' a)           => 'Lens'' s a      -> a -> m a
-- ('<.|.=') :: ('MonadState' s m, 'Bits' a, 'Data.Monoid.Monoid' a) => 'Traversal'' s a -> a -> m a
-- @
(<.|.=) :: (MonadState s m, Bits a) => LensLike' ((,)a) s a -> a -> m a
l <.|.= b = l <%= (.|. b)
{-# INLINE (<.|.=) #-}

(<<.&.~) :: Bits a => Optical' (->) q ((,)a) s a -> a -> q s (a, s)
l <<.&.~ b = l $ \a -> (a, a .&. b)
{-# INLINE (<<.&.~) #-}

(<<.|.~) :: Bits a => Optical' (->) q ((,)a) s a -> a -> q s (a, s)
l <<.|.~ b = l $ \a -> (a, a .|. b)
{-# INLINE (<<.|.~) #-}

(<<.&.=) :: (MonadState s m, Bits a) => LensLike' ((,) a) s a -> a -> m a
l <<.&.= b = l %%= \a -> (a, a .&. b)
{-# INLINE (<<.&.=) #-}

(<<.|.=) :: (MonadState s m, Bits a) => LensLike' ((,) a) s a -> a -> m a
l <<.|.= b = l %%= \a -> (a, a .|. b)
{-# INLINE (<<.|.=) #-}

-- | This 'Lens' can be used to access the value of the nth bit in a number.
--
-- @'bitAt' n@ is only a legal 'Lens' into @b@ if @0 '<=' n '<' 'bitSize' ('undefined' :: b)@.
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

-- | Get the nth byte, counting from the low end.
--
-- @'byteAt' n@ is a legal 'Lens' into @b@ iff @0 '<=' n '<' 'div' ('bitSize' ('undefined' :: b)) 8@
--
-- >>> (0xff00 :: Word16)^.byteAt 0
-- 0
--
-- >>> (0xff00 :: Word16)^.byteAt 1
-- 255
--
-- >>> byteAt 1 .~ 0 $ 0xff00 :: Word16
-- 0
--
-- >>> byteAt 0 .~ 0xff $ 0 :: Word16
-- 255
byteAt :: (Integral b, Bits b) => Int -> IndexedLens' Int b Word8
byteAt i f b = back <$> indexed f i (forward b) where
  back w8 = (fromIntegral w8 `shiftL` (i * 8))
    .|. (complement (255 `shiftL` (i * 8)) .&. b)
  forward = fromIntegral . (.&.) 0xff . flip shiftR (i * 8)


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

-- | Traverse over all the bytes in an integral type, from the low end.
--
-- The byte position is available as the index.
--
-- >>> toListOf bytewise (1312301580 :: Word32)
-- [12,34,56,78]
--
-- If you supply this an 'Integer', the result will be an infinite 'Traversal',
-- which can be productively consumed, but not reassembled.
--
-- Why isn't this function called @bytes@ to match 'bits'? Alas, there
-- is already a function by that name in "Data.ByteString.Lens".
bytewise :: (Integral b, Bits b) => IndexedTraversal' Int b Word8
bytewise f b = Prelude.foldr step 0 <$> traverse g bs where
  g n = (,) n <$> indexed f n (fromIntegral $ b `shiftR` (n*8))
  bs = Prelude.takeWhile hasByte [0..]
  hasByte n = complementBit b (n*8) /= b
  step (n,x) r = r .|. (fromIntegral x `shiftL` (n*8))
{-# INLINE bytewise #-}
