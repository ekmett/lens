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
  , bits
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
-- ('|~') :: 'Bits' a => 'Setter' s t a a -> a -> s -> t
-- ('|~') :: 'Bits' a => 'Iso' s t a a -> a -> s -> t
-- ('|~') :: 'Bits' a => 'Lens' s t a a -> a -> s -> t
-- ('|~') :: ('Monoid a', 'Bits' a) => 'Traversal' s t a a -> a -> s -> t
-- @
(|~):: Bits a => Setting s t a a -> a -> s -> t
l |~ n = over l (.|. n)
{-# INLINE (|~) #-}

-- | Bitwise '.&.' the target(s) of a 'Lens' or 'Setter'
--
-- >>> _2 &~ 7 $ ("hello",254)
-- ("hello",6)
--
-- @
-- ('&~') :: 'Bits' a => 'Setter' s t a a -> a -> s -> t
-- ('&~') :: 'Bits' a => 'Iso' s t a a -> a -> s -> t
-- ('&~') :: 'Bits' a => 'Lens' s t a a -> a -> s -> t
-- ('&~') :: ('Monoid a', 'Bits' a) => 'Traversal' s t a a -> a -> s -> t
-- @
(&~) :: Bits a => Setting s t a a -> a -> s -> t
l &~ n = over l (.&. n)
{-# INLINE (&~) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by computing its bitwise '.&.' with another value.
--
-- @
-- ('&=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Setter' s a -> a -> m ()
-- ('&=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Iso' s a -> a -> m ()
-- ('&=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Lens' s a -> a -> m ()
-- ('&=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Traversal' s a -> a -> m ()
-- @
(&=):: (MonadState s m, Bits a) => Simple Setting s a -> a -> m ()
l &= a = modify (l &~ a)
{-# INLINE (&=) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by computing its bitwise '.|.' with another value.
--
-- @
-- ('|=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Setter' s a -> a -> m ()
-- ('|=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Iso' s a -> a -> m ()
-- ('|=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Lens' s a -> a -> m ()
-- ('|=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Traversal' s a -> a -> m ()
-- @
(|=) :: (MonadState s m, Bits a) => Simple Setting s a -> a -> m ()
l |= a = modify (l |~ a)
{-# INLINE (|=) #-}

-- | Bitwise '.|.' the target(s) of a 'Lens' (or 'Traversal'), returning the result
-- (or a monoidal summary of all of the results).
--
-- >>> _2 <|~ 6 $ ("hello",3)
-- (7,("hello",7))
--
-- @
-- ('<|~') :: 'Bits' a => 'Iso' s t a a -> a -> s -> (a, t)
-- ('<|~') :: 'Bits' a => 'Lens' s t a a -> a -> s -> (a, t)
-- ('<|~') :: ('Bits' a, 'Monoid a) => 'Traversal' s t a a -> a -> s -> (a, t)
-- @
(<|~):: Bits a => LensLike ((,) a) s t a a -> a -> s -> (a, t)
l <|~ n = l <%~ (.|. n)
{-# INLINE (<|~) #-}

-- | Bitwise '.&.' the target(s) of a 'Lens' or 'Traversal', returning the result
-- (or a monoidal summary of all of the results).
--
-- >>> _2 <&~ 7 $ ("hello",254)
-- (6,("hello",6))
--
-- @
-- ('<&~') :: 'Bits' a => 'Iso' s t a a -> a -> s -> (a, t)
-- ('<&~') :: 'Bits' a => 'Lens' s t a a -> a -> s -> (a, t)
-- ('<&~') :: ('Bits' a, 'Monoid a) => 'Traversal' s t a a -> a -> s -> (a, t)
-- @
(<&~) :: Bits a => LensLike ((,) a) s t a a -> a -> s -> (a, t)
l <&~ n = l <%~ (.&. n)
{-# INLINE (<&~) #-}

-- | Modify the target(s) of a 'Simple' 'Lens' (or 'Traversal') by computing its bitwise '.&.' with another value,
-- returning the result (or a monoidal summary of all of the results traversed)
--
-- @
-- ('<&=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Lens' s a -> a -> m a
-- ('<&=') :: ('MonadState' s m, 'Bits' a, 'Monoid' a) => 'Simple' 'Traversal' s a -> a -> m a
-- @
(<&=):: (MonadState s m, Bits a) => SimpleLensLike ((,)a) s a -> a -> m a
l <&= b = l <%= (.&. b)
{-# INLINE (<&=) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', (or 'Traversal') by computing its bitwise '.|.' with another value,
-- returning the result (or a monoidal summary of all of the results traversed)
--
-- @
-- ('<|=') :: ('MonadState' s m, 'Bits' a) => 'Simple' 'Lens' s a -> a -> m a
-- ('<|=') :: ('MonadState' s m, 'Bits' a, 'Monoid' a) => 'Simple' 'Traversal' s a -> a -> m a
-- @
(<|=) :: (MonadState s m, Bits a) => SimpleLensLike ((,)a) s a -> a -> m a
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
-- >>> toListOf bits (5 :: Word8)
-- [True,False,True,False,False,False,False,False]
--
-- If you supply this an 'Integer', the result will
-- be an infinite 'Traversal' that can be productively consumed.
bits :: (Num b, Bits b) => SimpleIndexedTraversal Int b Bool
bits = index $ \f b -> let
    g n      = (,) n <$> f n (testBit b n)
    bs       = Prelude.takeWhile hasBit [0..]
    hasBit n = complementBit b n /= b -- test to make sure that complementing this bit actually changes the value
    step (n,True) r = setBit r n
    step _        r = r
  in Prelude.foldr step 0 <$> traverse g bs
{-# INLINE bits #-}
