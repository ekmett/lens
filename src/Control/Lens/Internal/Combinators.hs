{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704 && !defined(SAFE)
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
-- This module exports a strict composition operator and rewrite rules (and
-- constructor aliases) to reduce eta-expansion in the generated code for
-- chains of newtype constructor/accessor compositions which could otherwise
-- cause both a constant and asymptotic slowdown to code execution.
--
-- Many micro-benchmarks are improved up to 50%, and larger benchmarks can
-- win asymptotically.
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Combinators
  (
  -- * Strict coercion
    (#)
  -- * Constructor aliases
  , _Const
  , _ZipList
  , _WrapMonad
  , _Last
  , _First
  , _Product
  , _Sum
  , _Any
  , _All
  , _Dual
  , _Endo
  , _May
  , _Folding
  , _Effect
  , _EffectRWS
  , _Accessor
  , _Err
  , _Traversed
  , _Sequenced
  , _Focusing
  , _FocusingWith
  , _FocusingPlus
  , _FocusingOn
  , _FocusingMay
  , _FocusingErr
  , _Mutator
  , _Backwards
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens.Internal
import Data.Monoid
#ifndef SAFE
import Unsafe.Coerce
#endif

-- | Strict composition.
--
-- @'id' '.' f@ becomes @\x -> f x@ rather than @f@, which stops GHC from doing
-- some optimizations when we use a newtype constructor/accessor instead of
-- 'id'. ('#') is a strict version of ('.') such that @'id' '#' f@ behaves
-- identically to @f@.
infixr 9 #
(#) :: (b -> c) -> (a -> b) -> a -> c
(#) = \f -> f `seq` \g -> g `seq` \x -> f (g x)

-----------------------------------------------------------------------------
-- Constructor aliases
-----------------------------------------------------------------------------

-- GHC has trouble with rewrite rules involving newtype constructors, so we
-- define an alias for each one that
--
-- '_Mutator' is defined in a separate module because Mutator's Applicative
-- instance needs to refer to it.

_Const :: a -> Const a b
_Const = Const

_ZipList :: [a] -> ZipList a
_ZipList = ZipList

_WrapMonad :: m a -> WrappedMonad m a
_WrapMonad = WrapMonad

_Last :: Maybe a -> Last a
_Last = Last

_First :: Maybe a -> First a
_First = First

_Product :: a -> Product a
_Product = Product

_Sum :: a -> Sum a
_Sum = Sum

_Any :: Bool -> Any
_Any = Any

_All :: Bool -> All
_All = All

_Dual :: a -> Dual a
_Dual = Dual

_Endo :: (a -> a) -> Endo a
_Endo = Endo

_May :: Maybe a -> May a
_May = May

_Folding :: f a -> Folding f a
_Folding = Folding

_Effect :: m r -> Effect m r a
_Effect = Effect

_EffectRWS :: (st -> m (s, st, w)) -> EffectRWS w st m s a
_EffectRWS = EffectRWS

_Accessor :: r -> Accessor r a
_Accessor = Accessor

_Err :: Either e a -> Err e a
_Err = Err

_Traversed :: f () -> Traversed f
_Traversed = Traversed

_Sequenced :: m () -> Sequenced m
_Sequenced = Sequenced

_Focusing :: m (s, a) -> Focusing m s a
_Focusing = Focusing

_FocusingWith :: m (s, a, w) -> FocusingWith w m s a
_FocusingWith = FocusingWith

_FocusingPlus :: k (s, w) a -> FocusingPlus w k s a
_FocusingPlus = FocusingPlus

_FocusingOn :: k (f s) a -> FocusingOn f k s a
_FocusingOn = FocusingOn

_FocusingMay :: k (May s) a -> FocusingMay k s a
_FocusingMay = FocusingMay

_FocusingErr :: k (Err e s) a -> FocusingErr e k s a
_FocusingErr = FocusingErr

_Backwards :: f a -> Backwards f a
_Backwards = Backwards

-----------------------------------------------------------------------------
-- RULES
-----------------------------------------------------------------------------

-- When not compiling with -fsafe, we can unsafely coerce @Foo # f@ to @f@,
-- where @Foo@ is a newtype constructor or accessor. This has identical
-- semantics with strict composition, but GHC doesn't manage the optimization
-- itself.

#ifndef SAFE

{-# RULES "_Const#"         (#) _Const         = unsafeCoerce #-}
{-# RULES "getConst#"       (#) getConst       = unsafeCoerce #-}

{-# RULES "_ZipList#"       (#) _ZipList       = unsafeCoerce #-}
{-# RULES "getZipList#"     (#) getZipList     = unsafeCoerce #-}

{-# RULES "_WrapMonad#"     (#) _WrapMonad     = unsafeCoerce #-}
{-# RULES "unwrapMonad#"    (#) unwrapMonad    = unsafeCoerce #-}

{-# RULES "_Last#"          (#) _Last          = unsafeCoerce #-}
{-# RULES "getLast#"        (#) getLast        = unsafeCoerce #-}

{-# RULES "_First#"         (#) _First         = unsafeCoerce #-}
{-# RULES "getFirst#"       (#) getFirst       = unsafeCoerce #-}

{-# RULES "_Product#"       (#) _Product       = unsafeCoerce #-}
{-# RULES "getProduct#"     (#) getProduct     = unsafeCoerce #-}

{-# RULES "_Sum#"           (#) _Sum           = unsafeCoerce #-}
{-# RULES "getSum#"         (#) getSum         = unsafeCoerce #-}

{-# RULES "_Any#"           (#) _Any           = unsafeCoerce #-}
{-# RULES "getAny#"         (#) getAny         = unsafeCoerce #-}

{-# RULES "_All#"           (#) _All           = unsafeCoerce #-}
{-# RULES "getAll#"         (#) getAll         = unsafeCoerce #-}

{-# RULES "_Dual#"          (#) _Dual          = unsafeCoerce #-}
{-# RULES "getDual#"        (#) getDual        = unsafeCoerce #-}

{-# RULES "_Endo#"          (#) _Endo          = unsafeCoerce #-}
{-# RULES "appEndo#"        (#) appEndo        = unsafeCoerce #-}

{-# RULES "_May#"           (#) _May           = unsafeCoerce #-}
{-# RULES "getMay#"         (#) getMay         = unsafeCoerce #-}

{-# RULES "_Folding#"       (#) _Folding       = unsafeCoerce #-}
{-# RULES "getFolding#"     (#) getFolding     = unsafeCoerce #-}

{-# RULES "_Effect#"        (#) _Effect        = unsafeCoerce #-}
{-# RULES "getEffect#"      (#) getEffect      = unsafeCoerce #-}

{-# RULES "_EffectRWS#"     (#) _EffectRWS     = unsafeCoerce #-}
{-# RULES "getEffectRWS#"   (#) getEffectRWS   = unsafeCoerce #-}

{-# RULES "_Accessor#"      (#) _Accessor      = unsafeCoerce #-}
{-# RULES "runAccessor#"    (#) runAccessor    = unsafeCoerce #-}

{-# RULES "_Err#"           (#) _Err           = unsafeCoerce #-}
{-# RULES "getErr#"         (#) getErr         = unsafeCoerce #-}

{-# RULES "_Traversed#"     (#) _Traversed     = unsafeCoerce #-}
{-# RULES "getTraversed#"   (#) getTraversed   = unsafeCoerce #-}

{-# RULES "_Sequenced#"     (#) _Sequenced     = unsafeCoerce #-}
{-# RULES "getSequenced#"   (#) getSequenced   = unsafeCoerce #-}

{-# RULES "_Focusing#"      (#) _Focusing      = unsafeCoerce #-}
{-# RULES "unfocusing#"     (#) unfocusing     = unsafeCoerce #-}

{-# RULES "_FocusingWith#"  (#) _FocusingWith  = unsafeCoerce #-}
{-# RULES "unfocusingWith#" (#) unfocusingWith = unsafeCoerce #-}

{-# RULES "_FocusingPlus#"  (#) _FocusingPlus  = unsafeCoerce #-}
{-# RULES "unfocusingPlus#" (#) unfocusingPlus = unsafeCoerce #-}

{-# RULES "_FocusingOn#"    (#) _FocusingOn    = unsafeCoerce #-}
{-# RULES "unfocusingOn#"   (#) unfocusingOn   = unsafeCoerce #-}

{-# RULES "_FocusingMay#"   (#) _FocusingMay   = unsafeCoerce #-}
{-# RULES "unfocusingMay#"  (#) unfocusingMay  = unsafeCoerce #-}

{-# RULES "_FocusingErr#"   (#) _FocusingErr   = unsafeCoerce #-}
{-# RULES "unfocusingErr#"  (#) unfocusingErr  = unsafeCoerce #-}

{-# RULES "_Mutator#"       (#) _Mutator       = unsafeCoerce #-}
{-# RULES "runMutator#"     (#) runMutator     = unsafeCoerce #-}

{-# RULES "_Backwards#"     (#) _Backwards     = unsafeCoerce #-}
{-# RULES "forwards#"       (#) forwards       = unsafeCoerce #-}

#endif
