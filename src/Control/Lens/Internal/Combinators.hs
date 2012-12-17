{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
#if defined(TRUSTWORTHY) && !defined(SAFE)
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Combinators
-- Copyright   :  (C) 2012 Edward Kmett, Shachaf Ben-Kiki
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types, KindSignatures
--
-- This module is not exported from this package.
--
-- These combinators are used to reduce eta-expansion in the resulting code
-- which could otherwise cause both a constant and asymptotic slowdown to
-- code execution.
--
-- Many micro-benchmarks are improved up to 50%, and larger benchmarks can
-- win asymptotically.
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Combinators
  (
    (#)
  {-
  -- * Safe "Unsafe" Coercions
  , const#, getConst#
  , zipList#, getZipList#
  , wrapMonad#, unwrapMonad#
  , last#, getLast#
  , first#, getFirst#
  , product#, getProduct#
  , sum#, getSum#
  , any#, getAny#
  , all#, getAll#
  , dual#, getDual#
  , endo#, appEndo#
  , may#, getMay#
  , folding#, getFolding#
  , effect#, getEffect#
  , effectRWS#, getEffectRWS#
  , accessor#, runAccessor#
  , err#, getErr#
  , traversed#, getTraversed#
  , sequenced#, getSequenced#
  , focusing#, unfocusing#
  , focusingWith#, unfocusingWith#
  , focusingPlus#, unfocusingPlus#
  , focusingOn#, unfocusingOn#
  , focusingMay#, unfocusingMay#
  , focusingErr#, unfocusingErr#
  , mutator#, runMutator#
  , backwards#, forwards#
  -}
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens.Internal
import Data.Monoid
#ifndef SAFE
import Unsafe.Coerce
#endif

#ifndef SAFE
#define UNSAFELY(x) unsafeCoerce
#else
#define UNSAFELY(f) (\g -> g `seq` \x -> (f) (g x))
#endif

infixr 9 #
(#) :: (b -> c) -> (a -> b) -> a -> c
f # g = f `seq` g `seq` \x -> f (g x)
{-# INLINE (#) #-}

{-
const# :: (a -> b) -> a -> Const b r
const# = UNSAFELY(Const)

getConst# :: (a -> Const b r) -> a -> b
getConst# = UNSAFELY(getConst)

zipList# :: (a -> [b]) -> a -> ZipList b
zipList# = UNSAFELY(ZipList)

getZipList# :: (a -> ZipList b) -> a -> [b]
getZipList# = UNSAFELY(getZipList)

wrapMonad# :: (a -> m b) -> a -> WrappedMonad m b
wrapMonad# = UNSAFELY(WrapMonad)

unwrapMonad# :: (a -> WrappedMonad m b) -> a -> m b
unwrapMonad# = UNSAFELY(unwrapMonad)

last# :: (a -> Maybe b) -> a -> Last b
last# = UNSAFELY(Last)

getLast# :: (a -> Last b) -> a -> Maybe b
getLast# = UNSAFELY(getLast)

first# :: (a -> Maybe b) -> a -> First b
first# = UNSAFELY(First)

getFirst# :: (a -> First b) -> a -> Maybe b
getFirst# = UNSAFELY(getFirst)

product# :: (a -> b) -> a -> Product b
product# = UNSAFELY(Product)

getProduct# :: (a -> Product b) -> a -> b
getProduct# = UNSAFELY(getProduct)

sum# :: (a -> b) -> a -> Sum b
sum# = UNSAFELY(Sum)

getSum# :: (a -> Sum b) -> a -> b
getSum# = UNSAFELY(getSum)

any# :: (a -> Bool) -> a -> Any
any# = UNSAFELY(Any)

getAny# :: (a -> Any) -> a -> Bool
getAny# = UNSAFELY(getAny)

all# :: (a -> Bool) -> a -> All
all# = UNSAFELY(All)

getAll# :: (a -> All) -> a -> Bool
getAll# = UNSAFELY(getAll)

dual# :: (a -> b) -> a -> Dual b
dual# = UNSAFELY(Dual)

getDual# :: (a -> Dual b) -> a -> b
getDual# = UNSAFELY(getDual)

endo# :: (a -> b -> b) -> a -> Endo b
endo# = UNSAFELY(Endo)

appEndo# :: (a -> Endo b) -> a -> b -> b
appEndo# = UNSAFELY(appEndo)

may# :: (a -> Maybe b) -> a -> May b
may# = UNSAFELY(May)

getMay# :: (a -> May b) -> a -> Maybe b
getMay# = UNSAFELY(getMay)

folding# :: (a -> f b) -> a -> Folding f b
folding# = UNSAFELY(Folding)

getFolding# :: (a -> Folding f b) -> a -> f b
getFolding# = UNSAFELY(getFolding)

effect# :: (a -> m r) -> a -> Effect m r b
effect# = UNSAFELY(Effect)

getEffect# :: (a -> Effect m r b) -> a -> m r
getEffect# = UNSAFELY(getEffect)

effectRWS# :: (a -> st -> m (s, st, w)) -> a -> EffectRWS w st m s b
effectRWS# = UNSAFELY(EffectRWS)

getEffectRWS# :: (a -> EffectRWS w st m s b) -> a -> st -> m (s, st, w)
getEffectRWS# = UNSAFELY(getEffectRWS)

accessor# :: (a -> r) -> a -> Accessor r b
accessor# = UNSAFELY(Accessor)

runAccessor# :: (a -> Accessor r b) -> a -> r
runAccessor# = UNSAFELY(runAccessor)

err# :: (a -> Either e b) -> a -> Err e b
err# = UNSAFELY(Err)

getErr# :: (a -> Err e b) -> a -> Either e b
getErr# = UNSAFELY(getErr)

traversed# :: (a -> f ()) -> a -> Traversed f
traversed# = UNSAFELY(Traversed)

getTraversed# :: (a -> Traversed f) -> a -> f ()
getTraversed# = UNSAFELY(getTraversed)

sequenced# :: (a -> f ()) -> a -> Sequenced f
sequenced# = UNSAFELY(Sequenced)

getSequenced# :: (a -> Sequenced f) -> a -> f ()
getSequenced# = UNSAFELY(getSequenced)

focusing# :: (a -> m (s, b)) -> a -> Focusing m s b
focusing# = UNSAFELY(Focusing)

unfocusing# :: (a -> Focusing m s b) -> a -> m (s, b)
unfocusing# = UNSAFELY(unfocusing)

focusingWith# :: (a -> m (s, b, w)) -> a -> FocusingWith w m s b
focusingWith# = UNSAFELY(FocusingWith)

unfocusingWith# :: (a -> FocusingWith w m s b) -> a -> m (s, b, w)
unfocusingWith# = UNSAFELY(unfocusingWith)

focusingPlus# :: (a -> k (s, w) b) -> a -> FocusingPlus w k s b
focusingPlus# = UNSAFELY(FocusingPlus)

unfocusingPlus# :: (a -> FocusingPlus w k s b) -> a -> k (s, w) b
unfocusingPlus# = UNSAFELY(unfocusingPlus)

focusingOn# :: (a -> k (f s) b) -> a -> FocusingOn f k s b
focusingOn# = UNSAFELY(FocusingOn)

unfocusingOn# :: (a -> FocusingOn f k s b) -> a -> k (f s) b
unfocusingOn# = UNSAFELY(unfocusingOn)

focusingMay# :: (a -> k (May s) b) -> a -> FocusingMay k s b
focusingMay# = UNSAFELY(FocusingMay)

unfocusingMay# :: (a -> FocusingMay k s b) -> a -> k (May s) b
unfocusingMay# = UNSAFELY(unfocusingMay)

focusingErr# :: (a -> k (Err e s) b) -> a -> FocusingErr e k s b
focusingErr# = UNSAFELY(FocusingErr)

unfocusingErr# :: (a -> FocusingErr e k s b) -> a -> k (Err e s) b
unfocusingErr# = UNSAFELY(unfocusingErr)

mutator# :: (a -> b) -> a -> Mutator b
mutator# = UNSAFELY(Mutator)

runMutator# :: (a -> Mutator b) -> a -> b
runMutator# = UNSAFELY(runMutator)

backwards# :: (a -> f b) -> a -> Backwards f b
backwards# = UNSAFELY(Backwards)

forwards# :: (a -> Backwards f b) -> a -> f b
forwards# = UNSAFELY(forwards)
-}
