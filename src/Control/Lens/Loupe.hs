{-# LANGUAGE CPP #-}

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
    Loupe
  , storing
  , (^#)
  , (#~), (#%~), (#%%~), (<#~), (<#%~)
  , (#=), (#%=), (#%%=), (<#=), (<#%=)

  -- * Simplified
  , SimpleLoupe
  ) where

import Control.Applicative              as Applicative
import Control.Lens.Internal
import Control.Lens.Type
import Control.Monad.State.Class        as State

-- $setup
-- >>> import Control.Lens

infixl 8 ^#
infixr 4 <#~, #~, #%~, <#%~, #%%~
infix  4 <#=, #=, #%=, <#%=, #%%=

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

-- |
-- A @'Loupe' s t a b@ is almost a 'Lens'. It can be composed on the left of other lenses,
-- you can use 'cloneLens' to promote it to a 'Lens', and it provides a minimalist lens-like
-- interface. They can be used in an API where you need to pass around lenses inside containers
-- or as monadic results. Unlike a 'ReifiedLens' they can be composed and used directly, but
-- they are slightly lower performance.

-- 1) You get back what you put in:
--
-- @'Control.Lens.Setter.set' l b a '^#' l ≡ b@
--
-- 2) Putting back what you got doesn't change anything:
--
-- @'storing' l (a '^#' l) a  ≡ a@
--
-- 3) Setting twice is the same as setting once:
--
-- @'storing' l c ('storing' l b a) ≡ 'storing' l c a@
--
-- These laws are strong enough that the 4 type parameters of a 'Loupe' cannot
-- vary fully independently. For more on how they interact, read the \"Why is
-- it a Lens Family?\" section of <http://comonad.com/reader/2012/mirrored-lenses/>.

type Loupe s t a b = LensLike (Context a b) s t a b

-- | @type 'SimpleLoupe' = 'Simple' 'Loupe'@
type SimpleLoupe s a = Loupe s s a a

-- | A 'Loupe'-specific version of ('Control.Lens.Getter.^.')
(^#) :: s -> Loupe s t a b -> a
s ^# l = case l (Context id) s of
  Context _ a -> a
{-# INLINE (^#) #-}

-- | A 'Loupe'-specific version of 'Control.Lens.Setter.set'
storing :: Loupe s t a b -> b -> s -> t
storing l b s = case l (Context id) s of
  Context g _ -> g b
{-# INLINE storing #-}

-- | A 'Loupe'-specific version of ('Control.Lens.Setter..~')
(#~) :: Loupe s t a b -> b -> s -> t
(#~) l b s = case l (Context id ) s of
  Context g _ -> g b
{-# INLINE (#~) #-}

-- | A 'Loupe'-specific version of ('Control.Lens.Setter.%~')
(#%~) :: Loupe s t a b -> (a -> b) -> s -> t
(#%~) l f s = case l (Context id) s of
  Context g a -> g (f a)
{-# INLINE (#%~) #-}

-- | A 'Loupe'-specific version of ('Control.Lens.Type.%%~')
(#%%~) :: Functor f => Loupe s t a b -> (a -> f b) -> s -> f t
(#%%~) l f s = case l (Context id) s of
  Context g a -> g <$> f a

-- | A 'Loupe'-specific version of ('Control.Lens.Setter..=')
(#=) :: MonadState s m => Loupe s s a b -> b -> m ()
l #= f = modify (l #~ f)
{-# INLINE (#=) #-}

-- | A 'Loupe'-specific version of ('Control.Lens.Setter.%=')
(#%=) :: MonadState s m => Loupe s s a b -> (a -> b) -> m ()
l #%= f = modify (l #%~ f)
{-# INLINE (#%=) #-}

-- | Modify the target of a 'Loupe' and return the result.
(<#%~) :: Loupe s t a b -> (a -> b) -> s -> (b, t)
l <#%~ f = \s -> case l (Context id) s of
  Context g a -> let b = f a in (b, g b)
{-# INLINE (<#%~) #-}

-- | Modify the target of a 'Loupe' into your monad's state by a user supplied function and return the result.
(<#%=) :: MonadState s m => Loupe s s a b -> (a -> b) -> m b
l <#%= f = l #%%= \a -> let b = f a in (b,b)
{-# INLINE (<#%=) #-}

-- | Modify the target of a 'Loupe' in the current monadic state, returning an auxillary result.
(#%%=) :: MonadState s m => Loupe s s a b -> (a -> (r, b)) -> m r
#if MIN_VERSION_mtl(2,1,1)
l #%%= f = State.state $ \s -> case l (Context id) s of
  Context g a -> g <$> f a
#else
l #%%= f = do
  Context g a <- State.gets (l (Context id))
  let (r, b) = f a
  State.put (g b)
  return r
#endif

-- | Replace the target of a 'Loupe' and return the new value.
(<#~) :: Loupe s t a b -> b -> s -> (b, t)
l <#~ b = \s -> (b, storing l b s)

-- | Replace the target of a 'Loupe' in the current monadic state, returning the new value.
(<#=) :: MonadState s m => Loupe s s a b -> b -> m b
l <#= b = do
  l #= b
  return b
