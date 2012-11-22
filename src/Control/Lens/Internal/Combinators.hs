{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
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
  -- * Safe "Unsafe" Coercions
    const#, getConst#
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
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens.Internal
import Data.Monoid
import Unsafe.Coerce

const# :: (a -> b) -> a -> Const b r
const# = unsafeCoerce

getConst# :: (a -> Const b r) -> a -> b
getConst# = unsafeCoerce

zipList# :: (a -> [b]) -> a -> ZipList b
zipList# = unsafeCoerce

getZipList# :: (a -> ZipList b) -> a -> [b]
getZipList# = unsafeCoerce

wrapMonad# :: (a -> m b) -> a -> WrappedMonad m b
wrapMonad# = unsafeCoerce

unwrapMonad# :: (a -> WrappedMonad m b) -> a -> m b
unwrapMonad# = unsafeCoerce

last# :: (a -> Maybe b) -> a -> Last b
last# = unsafeCoerce

getLast# :: (a -> Last b) -> a -> Maybe b
getLast# = unsafeCoerce

first# :: (a -> Maybe b) -> a -> First b
first# = unsafeCoerce

getFirst# :: (a -> First b) -> a -> Maybe b
getFirst# = unsafeCoerce

product# :: (a -> b) -> a -> Product b
product# = unsafeCoerce

getProduct# :: (a -> Product b) -> a -> b
getProduct# = unsafeCoerce

sum# :: (a -> b) -> a -> Sum b
sum# = unsafeCoerce

getSum# :: (a -> Sum b) -> a -> b
getSum# = unsafeCoerce

any# :: (a -> Bool) -> a -> Any
any# = unsafeCoerce

getAny# :: (a -> Any) -> a -> Bool
getAny# = unsafeCoerce

all# :: (a -> Bool) -> a -> All
all# = unsafeCoerce

getAll# :: (a -> All) -> a -> Bool
getAll# = unsafeCoerce

dual# :: (a -> b) -> a -> Dual b
dual# = unsafeCoerce

getDual# :: (a -> Dual b) -> a -> b
getDual# = unsafeCoerce

endo# :: (a -> b -> b) -> a -> Endo b
endo# = unsafeCoerce

appEndo# :: (a -> Endo b) -> a -> b -> b
appEndo# = unsafeCoerce

may# :: (a -> Maybe b) -> a -> May b
may# = unsafeCoerce

getMay# :: (a -> May b) -> a -> Maybe b
getMay# = unsafeCoerce

folding# :: (a -> f b) -> a -> Folding f b
folding# = unsafeCoerce

getFolding# :: (a -> Folding f b) -> a -> f b
getFolding# = unsafeCoerce

effect# :: (a -> m r) -> a -> Effect m r b
effect# = unsafeCoerce

getEffect# :: (a -> Effect m r b) -> a -> m r
getEffect# = unsafeCoerce

effectRWS# :: (a -> st -> m (s, st, w)) -> a -> EffectRWS w st m s b
effectRWS# = unsafeCoerce

getEffectRWS# :: (a -> EffectRWS w st m s b) -> a -> st -> m (s, st, w)
getEffectRWS# = unsafeCoerce

accessor# :: (a -> r) -> a -> Accessor r b
accessor# = unsafeCoerce

runAccessor# :: (a -> Accessor r b) -> a -> r
runAccessor# = unsafeCoerce

err# :: (a -> Either e b) -> a -> Err e b
err# = unsafeCoerce

getErr# :: (a -> Err e b) -> a -> Either e b
getErr# = unsafeCoerce

traversed# :: (a -> f ()) -> a -> Traversed f
traversed# = unsafeCoerce

getTraversed# :: (a -> Traversed f) -> a -> f ()
getTraversed# = unsafeCoerce

sequenced# :: (a -> f ()) -> a -> Sequenced f
sequenced# = unsafeCoerce

getSequenced# :: (a -> Sequenced f) -> a -> f ()
getSequenced# = unsafeCoerce

focusing# :: (a -> m (s, b)) -> a -> Focusing m s b
focusing# = unsafeCoerce

unfocusing# :: (a -> Focusing m s b) -> a -> m (s, b)
unfocusing# = unsafeCoerce

focusingWith# :: (a -> m (s, b, w)) -> a -> FocusingWith w m s b
focusingWith# = unsafeCoerce

unfocusingWith# :: (a -> FocusingWith w m s b) -> a -> m (s, b, w)
unfocusingWith# = unsafeCoerce

focusingPlus# :: (a -> k (s, w) b) -> a -> FocusingPlus w k s b
focusingPlus# = unsafeCoerce

unfocusingPlus# :: (a -> FocusingPlus w k s b) -> a -> k (s, w) b
unfocusingPlus# = unsafeCoerce

focusingOn# :: (a -> k (f s) b) -> a -> FocusingOn f k s b
focusingOn# = unsafeCoerce

unfocusingOn# :: (a -> FocusingOn f k s b) -> a -> k (f s) b
unfocusingOn# = unsafeCoerce

focusingMay# :: (a -> k (May s) b) -> a -> FocusingMay k s b
focusingMay# = unsafeCoerce

unfocusingMay# :: (a -> FocusingMay k s b) -> a -> k (May s) b
unfocusingMay# = unsafeCoerce

focusingErr# :: (a -> k (Err e s) b) -> a -> FocusingErr e k s b
focusingErr# = unsafeCoerce

unfocusingErr# :: (a -> FocusingErr e k s b) -> a -> k (Err e s) b
unfocusingErr# = unsafeCoerce

mutator# :: (a -> b) -> a -> Mutator b
mutator# = unsafeCoerce

runMutator# :: (a -> Mutator b) -> a -> b
runMutator# = unsafeCoerce

backwards# :: (a -> f b) -> a -> Backwards f b
backwards# = unsafeCoerce

forwards# :: (a -> Backwards f b) -> a -> f b
forwards# = unsafeCoerce
