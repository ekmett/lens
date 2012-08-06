{-# LANGUAGE LiberalTypeSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bits.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  LiberalTypeSynonyms
--
----------------------------------------------------------------------------
module Data.Bits.Lens
  ( (|~), (&~)
  , (|=), (&=)
  , bitAt
  , traverseBits
  ) where

import Control.Lens
import Control.Monad.State.Class
import Data.Bits
import Data.Functor

infixr 4 |~, &~
infix 4 |=, &=

-- | Bitwise '.|.' the target(s) of a 'Bool'-valued 'Lens' or 'Setter'
(|~):: Bits c => Setting a b c c -> c -> a -> b
l |~ n = adjust l (.|. n)
{-# INLINE (|~) #-}

-- | Bitwise '.&.' the target(s) of a 'Bool'-valued 'Lens' or 'Setter'
(&~) :: Bits c => Setting a b c c -> c -> a -> b
l &~ n = adjust l (.&. n)
{-# INLINE (&~) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by computing its bitwise '.&.' with another value.
(&=):: (MonadState a m, Bits b) => Simple Setting a b -> b -> m ()
l &= b = modify (l &~ b)
{-# INLINE (&=) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Setter' or 'Traversal' by computing its bitwise '.|.' with another value.
(|=) :: (MonadState a m, Bits b) => Simple Setting a b -> b -> m ()
l |= b = modify (l |~ b)
{-# INLINE (|=) #-}

-- | This lens can be used to access the value of the nth bit in a number.
--
-- @bitsAt n@ is only a legal 'Lens' into @b@ if @0 <= n < bitSize (undefined :: b)@
bitAt :: Bits b => Int -> Simple Lens b Bool
bitAt n f b = (\x -> if x then setBit b n else clearBit b n) <$> f (testBit b n)
{-# INLINE bitAt #-}

-- | Traverse over all bits in a numeric type.
--
-- > ghci> toListOf traverseBits (5 :: Word8)
-- > [True,False,True,False,False,False,False,False]
--
-- If you supply this an Integer, it won't crash, but the result will
-- be an infinite traversal that can be productively consumed.
--
-- > ghci> toListOf traverseBits 5
-- > [True,False,True,False,False,False,False,False,False,False,False,False...
traverseBits :: Bits b => Simple Traversal b Bool
traverseBits f b = Prelude.foldr step 0 <$> traverse g bits
  where
    g n      = (,) n <$> f (testBit b n)
    bits     = Prelude.takeWhile hasBit [0..]
    hasBit n = complementBit b n /= b -- test to make sure that complementing this bit actually changes the value
    step (n,True) r = setBit r n
    step _        r = r
{-# INLINE traverseBits #-}
