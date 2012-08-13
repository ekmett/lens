{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
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
--
-- >>> _2 |~ 6 $ ("hello",3)
-- ("hello",7)
(|~):: Bits c => Setting a b c c -> c -> a -> b
l |~ n = over l (.|. n)
{-# INLINE (|~) #-}

-- | Bitwise '.&.' the target(s) of a 'Bool'-valued 'Lens' or 'Setter'
--
-- >>> _2 &~ 7 $ ("hello",254)
-- ("hello",6)
(&~) :: Bits c => Setting a b c c -> c -> a -> b
l &~ n = over l (.&. n)
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
traverseBits :: Bits b => SimpleIndexedTraversal Int b Bool
traverseBits = index $ \f b -> let
    g n      = (,) n <$> f n (testBit b n)
    bits     = Prelude.takeWhile hasBit [0..]
    hasBit n = complementBit b n /= b -- test to make sure that complementing this bit actually changes the value
    step (n,True) r = setBit r n
    step _        r = r
  in Prelude.foldr step 0 <$> traverse g bits
{-# INLINE traverseBits #-}
