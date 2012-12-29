{-# LANGUAGE CPP #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 1
#endif

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Loupe
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- A 'Loupe' is a minimalist 'Lens' suitable for storing in containers
-- or returning monadically that can still be composed with other lenses.
-------------------------------------------------------------------------------
module Control.Lens.Loupe
  (
  -- * Lenses
    Loupe, Loupe'
  , storing
  , (^#)
  , ( #~ ), ( #%~ ), ( #%%~ ), (<#~), (<#%~)
  , ( #= ), ( #%= ), ( #%%= ), (<#=), (<#%=)

  -- * Deprecated
  , SimpleLoupe
  ) where

import Control.Lens.Internal
import Control.Lens.Lens
import Control.Monad.State.Class as State

-- $setup
-- >>> import Control.Lens

infixl 8 ^#
infixr 4 <#~, #~, #%~, <#%~, #%%~
infix  4 <#=, #=, #%=, <#%=, #%%=

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

-- | A 'Loupe'-specific version of ('Control.Lens.Getter.^.')
--
-- >>> ("hello","world")^#_2
-- "world"
(^#) :: s -> ALens s t a b -> a
s ^# l = ipos (l sell s)
{-# INLINE (^#) #-}

-- | A 'Loupe'-specific version of 'Control.Lens.Setter.set'
--
-- >>> storing _2 "world" ("hello","there")
-- ("hello","world")
storing :: ALens s t a b -> b -> s -> t
storing l b s = ipeek b (l sell s)
{-# INLINE storing #-}

-- | A 'Loupe'-specific version of ('Control.Lens.Setter..~')
--
-- >>> ("hello","there") & _2 #~ "world"
-- ("hello","world")
( #~ ) :: ALens s t a b -> b -> s -> t
( #~ ) l b s = ipeek b (l sell s)
{-# INLINE ( #~ ) #-}

-- | A 'Loupe'-specific version of ('Control.Lens.Setter.%~')
--
-- >>> ("hello","world") & _2 #%~ length
-- ("hello",5)
( #%~ ) :: ALens s t a b -> (a -> b) -> s -> t
( #%~ ) l f s = ipeeks f (l sell s)
{-# INLINE ( #%~ ) #-}

-- | A 'Loupe'-specific version of ('Control.Lens.Type.%%~')
--
-- >>> ("hello","world") & _2 #%%~ \x -> (length x, x ++ "!")
-- (5,("hello","world!"))
( #%%~ ) :: Functor f => ALens s t a b -> (a -> f b) -> s -> f t
( #%%~ ) l f s = runPretext (l sell s) f
{-# INLINE ( #%%~ ) #-}

-- | A 'Loupe'-specific version of ('Control.Lens.Setter..=')
( #= ) :: MonadState s m => ALens s s a b -> b -> m ()
l #= f = modify (l #~ f)
{-# INLINE ( #= ) #-}

-- | A 'Loupe'-specific version of ('Control.Lens.Setter.%=')
( #%= ) :: MonadState s m => ALens s s a b -> (a -> b) -> m ()
l #%= f = modify (l #%~ f)
{-# INLINE ( #%= ) #-}

-- | Modify the target of a 'Loupe' and return the result.
--
-- >>> ("hello","world") & _2 <#%~ length
-- (5,("hello",5))
(<#%~) :: ALens s t a b -> (a -> b) -> s -> (b, t)
l <#%~ f = \s -> runPretext (l sell s) $ \a -> let b = f a in (b, b)
{-# INLINE (<#%~) #-}

-- | Modify the target of a 'Loupe' into your monad's state by a user supplied function and return the result.
(<#%=) :: MonadState s m => ALens s s a b -> (a -> b) -> m b
l <#%= f = l #%%= \a -> let b = f a in (b, b)
{-# INLINE (<#%=) #-}

-- | Modify the target of a 'Loupe' in the current monadic state, returning an auxiliary result.
( #%%= ) :: MonadState s m => ALens s s a b -> (a -> (r, b)) -> m r
#if MIN_VERSION_mtl(2,1,1)
l #%%= f = State.state $ \s -> runPretext (l sell s) f
#else
l #%%= f = do
  p <- State.gets (l sell)
  let (r, t) = runPretext p f
  State.put t
  return r
#endif

-- | Replace the target of a 'Loupe' and return the new value.
--
-- >>> ("hello","there") & _2 <#~ "world"
-- ("world",("hello","world"))
(<#~) :: ALens s t a b -> b -> s -> (b, t)
l <#~ b = \s -> (b, storing l b s)

-- | Replace the target of a 'Loupe' in the current monadic state, returning the new value.
(<#=) :: MonadState s m => ALens s s a b -> b -> m b
l <#= b = do
  l #= b
  return b

------------------------------------------------------------------------------
-- Deprecated
------------------------------------------------------------------------------

-- | @type 'SimpleLoupe' = 'Simple' 'Loupe'@
type SimpleLoupe s a = Loupe s s a a
{-# DEPRECATED SimpleLoupe "use ALens'" #-}
