{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#endif

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

#ifndef MIN_VERSION_exceptions
#define MIN_VERSION_exceptions 1
#endif

#if !(MIN_VERSION_exceptions(0,4,0))
#define MonadThrow MonadCatch
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Exception.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Control.Exception
--
-- @Control.Exception@ provides an example of a large open hierarchy
-- that we can model with prisms and isomorphisms.
--
-- Additional combinators for working with 'IOException' results can
-- be found in "System.IO.Error.Lens".
--
-- The combinators in this module have been generalized to work with
-- 'MonadCatch' instead of just 'IO'. This enables them to be used
-- more easily in 'Monad' transformer stacks.
----------------------------------------------------------------------------
module Control.Exception.Lens
  (
  -- * Handling
    catching, catching_
  , handling, handling_
  -- * Trying
  , trying, trying_
  -- * Throwing
  , throwing
  , throwing_
  , throwingM
  , throwingTo
  -- * Mapping
  , mappedException, mappedException'
  -- * Exceptions
  , exception
#if __GLASGOW_HASKELL__ >= 710
  , pattern Exception
#endif
  -- * Exception Handlers
  , Handleable(..)
  -- ** IOExceptions
  , AsIOException(..)
#if __GLASGOW_HASKELL__ >= 710
  , pattern IOException_
#endif
  -- ** Arithmetic Exceptions
  , AsArithException(..)
  , _Overflow, _Underflow, _LossOfPrecision, _DivideByZero, _Denormal
#if MIN_VERSION_base(4,6,0)
  , _RatioZeroDenominator
#endif
#if __GLASGOW_HASKELL__ >= 710
  , pattern ArithException_
  , pattern Overflow_
  , pattern Underflow_
  , pattern LossOfPrecision_
  , pattern DivideByZero_
  , pattern Denormal_
  , pattern RatioZeroDenominator_
#endif
  -- ** Array Exceptions
  , AsArrayException(..)
  , _IndexOutOfBounds
  , _UndefinedElement
#if __GLASGOW_HASKELL__ >= 710
  , pattern ArrayException_
  , pattern IndexOutOfBounds_
  , pattern UndefinedElement_
#endif
  -- ** Assertion Failed
  , AsAssertionFailed(..)
#if __GLASGOW_HASKELL__ >= 710
  , pattern AssertionFailed_
#endif
  -- ** Async Exceptions
  , AsAsyncException(..)
  , _StackOverflow
  , _HeapOverflow
  , _ThreadKilled
  , _UserInterrupt
#if __GLASGOW_HASKELL__ >= 710
  , pattern AsyncException_
  , pattern StackOverflow_
  , pattern HeapOverflow_
  , pattern ThreadKilled_
  , pattern UserInterrupt_
#endif
  -- ** Non-Termination
  , AsNonTermination(..)
#if __GLASGOW_HASKELL__ >= 710
  , pattern NonTermination_
#endif
  -- ** Nested Atomically
  , AsNestedAtomically(..)
#if __GLASGOW_HASKELL__ >= 710
  , pattern NestedAtomically_
#endif
  -- ** Blocked Indefinitely
  -- *** on MVar
  , AsBlockedIndefinitelyOnMVar(..)
#if __GLASGOW_HASKELL__ >= 710
  , pattern BlockedIndefinitelyOnMVar_
#endif
  -- *** on STM
  , AsBlockedIndefinitelyOnSTM(..)
#if __GLASGOW_HASKELL__ >= 710
  , pattern BlockedIndefinitelyOnSTM_
#endif
  -- ** Deadlock
  , AsDeadlock(..)
#if __GLASGOW_HASKELL__ >= 710
  , pattern Deadlock_
#endif
  -- ** No Such Method
  , AsNoMethodError(..)
#if __GLASGOW_HASKELL__ >= 710
  , pattern NoMethodError_
#endif
  -- ** Pattern Match Failure
  , AsPatternMatchFail(..)
#if __GLASGOW_HASKELL__ >= 710
  , pattern PatternMatchFail_
#endif
  -- ** Record
  , AsRecConError(..)
  , AsRecSelError(..)
  , AsRecUpdError(..)
#if __GLASGOW_HASKELL__ >= 710
  , pattern RecConError_
  , pattern RecSelError_
  , pattern RecUpdError_
#endif
  -- ** Error Call
  , AsErrorCall(..)
#if __GLASGOW_HASKELL__ >= 710
  , pattern ErrorCall_
#endif
#if MIN_VERSION_base(4,8,0)
  -- ** Allocation Limit Exceeded
  , AsAllocationLimitExceeded(..)
  , pattern AllocationLimitExceeded_
#endif
#if MIN_VERSION_base(4,9,0)
  -- ** Type Error
  , AsTypeError(..)
  , pattern TypeError_
#endif
#if MIN_VERSION_base(4,10,0)
  -- ** Compaction Failed
  , AsCompactionFailed(..)
  , pattern CompactionFailed_
#endif
  -- * Handling Exceptions
  , AsHandlingException(..)
#if __GLASGOW_HASKELL__ >= 710
  , pattern HandlingException_
#endif
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch as Catch
import Control.Exception as Exception hiding (try, tryJust, catchJust)
import Control.Lens
import Control.Lens.Internal.Exception
import Data.Monoid
import GHC.Conc (ThreadId)
import Prelude
  ( const, either, flip, id
  , (.)
  , Maybe(..), Either(..), String
#if __GLASGOW_HASKELL__ >= 710
  , Bool(..)
#endif
  )

#ifdef HLINT
{-# ANN module "HLint: ignore Use Control.Exception.catch" #-}
#endif

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> :m + Control.Exception Control.Monad Data.List Prelude

------------------------------------------------------------------------------
-- Exceptions as Prisms
------------------------------------------------------------------------------

-- | Traverse the strongly typed 'Exception' contained in 'SomeException' where the type of your function matches
-- the desired 'Exception'.
--
-- @
-- 'exception' :: ('Applicative' f, 'Exception' a)
--           => (a -> f a) -> 'SomeException' -> f 'SomeException'
-- @
exception :: Exception a => Prism' SomeException a
exception = prism' toException fromException
{-# INLINE exception #-}

#if __GLASGOW_HASKELL__ >= 710
pattern Exception e <- (preview exception -> Just e) where
  Exception e = review exception e
#endif

------------------------------------------------------------------------------
-- Catching
------------------------------------------------------------------------------

-- | Catch exceptions that match a given 'Prism' (or any 'Fold', really).
--
-- >>> catching _AssertionFailed (assert False (return "uncaught")) $ \ _ -> return "caught"
-- "caught"
--
-- @
-- 'catching' :: 'MonadCatch' m => 'Prism'' 'SomeException' a     -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadCatch' m => 'Lens'' 'SomeException' a      -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadCatch' m => 'Traversal'' 'SomeException' a -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadCatch' m => 'Iso'' 'SomeException' a       -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadCatch' m => 'Getter' 'SomeException' a     -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadCatch' m => 'Fold' 'SomeException' a       -> m r -> (a -> m r) -> m r
-- @
catching :: MonadCatch m => Getting (First a) SomeException a -> m r -> (a -> m r) -> m r
catching l = catchJust (preview l)
{-# INLINE catching #-}

-- | Catch exceptions that match a given 'Prism' (or any 'Getter'), discarding
-- the information about the match. This is particuarly useful when you have
-- a @'Prism'' e ()@ where the result of the 'Prism' or 'Fold' isn't
-- particularly valuable, just the fact that it matches.
--
-- >>> catching_ _AssertionFailed (assert False (return "uncaught")) $ return "caught"
-- "caught"
--
-- @
-- 'catching_' :: 'MonadCatch' m => 'Prism'' 'SomeException' a     -> m r -> m r -> m r
-- 'catching_' :: 'MonadCatch' m => 'Lens'' 'SomeException' a      -> m r -> m r -> m r
-- 'catching_' :: 'MonadCatch' m => 'Traversal'' 'SomeException' a -> m r -> m r -> m r
-- 'catching_' :: 'MonadCatch' m => 'Iso'' 'SomeException' a       -> m r -> m r -> m r
-- 'catching_' :: 'MonadCatch' m => 'Getter' 'SomeException' a     -> m r -> m r -> m r
-- 'catching_' :: 'MonadCatch' m => 'Fold' 'SomeException' a       -> m r -> m r -> m r
-- @
catching_ :: MonadCatch m => Getting (First a) SomeException a -> m r -> m r -> m r
catching_ l a b = catchJust (preview l) a (const b)
{-# INLINE catching_ #-}

------------------------------------------------------------------------------
-- Handling
------------------------------------------------------------------------------

-- | A version of 'catching' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.
--
-- >>> handling _NonTermination (\_ -> return "caught") $ throwIO NonTermination
-- "caught"
--
-- @
-- 'handling' :: 'MonadCatch' m => 'Prism'' 'SomeException' a     -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadCatch' m => 'Lens'' 'SomeException' a      -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadCatch' m => 'Traversal'' 'SomeException' a -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadCatch' m => 'Iso'' 'SomeException' a       -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadCatch' m => 'Fold' 'SomeException' a       -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadCatch' m => 'Getter' 'SomeException' a     -> (a -> m r) -> m r -> m r
-- @
handling :: MonadCatch m => Getting (First a) SomeException a -> (a -> m r) -> m r -> m r
handling l = flip (catching l)
{-# INLINE handling #-}

-- | A version of 'catching_' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.
--
-- >>> handling_ _NonTermination (return "caught") $ throwIO NonTermination
-- "caught"
--
-- @
-- 'handling_' :: 'MonadCatch' m => 'Prism'' 'SomeException' a     -> m r -> m r -> m r
-- 'handling_' :: 'MonadCatch' m => 'Lens'' 'SomeException' a      -> m r -> m r -> m r
-- 'handling_' :: 'MonadCatch' m => 'Traversal'' 'SomeException' a -> m r -> m r -> m r
-- 'handling_' :: 'MonadCatch' m => 'Iso'' 'SomeException' a       -> m r -> m r -> m r
-- 'handling_' :: 'MonadCatch' m => 'Getter' 'SomeException' a     -> m r -> m r -> m r
-- 'handling_' :: 'MonadCatch' m => 'Fold' 'SomeException' a       -> m r -> m r -> m r
-- @
handling_ :: MonadCatch m => Getting (First a) SomeException a -> m r -> m r -> m r
handling_ l = flip (catching_ l)
{-# INLINE handling_ #-}

------------------------------------------------------------------------------
-- Trying
------------------------------------------------------------------------------

-- | A variant of 'Control.Exception.try' that takes a 'Prism' (or any 'Fold') to select which
-- exceptions are caught (c.f. 'Control.Exception.tryJust', 'Control.Exception.catchJust'). If the
-- 'Exception' does not match the predicate, it is re-thrown.
--
-- @
-- 'trying' :: 'MonadCatch' m => 'Prism''     'SomeException' a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadCatch' m => 'Lens''      'SomeException' a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadCatch' m => 'Traversal'' 'SomeException' a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadCatch' m => 'Iso''       'SomeException' a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadCatch' m => 'Getter'     'SomeException' a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadCatch' m => 'Fold'       'SomeException' a -> m r -> m ('Either' a r)
-- @
trying :: MonadCatch m => Getting (First a) SomeException a -> m r -> m (Either a r)
trying l = tryJust (preview l)
{-# INLINE trying #-}

-- | A version of 'trying' that discards the specific exception thrown.
--
-- @
-- 'trying_' :: 'MonadCatch' m => 'Prism''     'SomeException' a -> m r -> m (Maybe r)
-- 'trying_' :: 'MonadCatch' m => 'Lens''      'SomeException' a -> m r -> m (Maybe r)
-- 'trying_' :: 'MonadCatch' m => 'Traversal'' 'SomeException' a -> m r -> m (Maybe r)
-- 'trying_' :: 'MonadCatch' m => 'Iso''       'SomeException' a -> m r -> m (Maybe r)
-- 'trying_' :: 'MonadCatch' m => 'Getter'     'SomeException' a -> m r -> m (Maybe r)
-- 'trying_' :: 'MonadCatch' m => 'Fold'       'SomeException' a -> m r -> m (Maybe r)
-- @
trying_ :: MonadCatch m => Getting (First a) SomeException a -> m r -> m (Maybe r)
trying_ l m = preview _Right `liftM` trying l m
{-# INLINE trying_ #-}

------------------------------------------------------------------------------
-- Throwing
------------------------------------------------------------------------------

-- | Throw an 'Exception' described by a 'Prism'. Exceptions may be thrown from
-- purely functional code, but may only be caught within the 'IO' 'Monad'.
--
-- @
-- 'throwing' l ≡ 'reviews' l 'throw'
-- @
--
-- @
-- 'throwing' :: 'Prism'' 'SomeException' t -> t -> r
-- 'throwing' :: 'Iso'' 'SomeException' t   -> t -> r
-- @
throwing :: AReview SomeException b -> b -> r
throwing l = reviews l Exception.throw
{-# INLINE throwing #-}

-- | Similar to 'throwing' but specialised for the common case of
--   error constructors with no arguments.
--
-- @
-- data MyError = Foo | Bar
-- makePrisms ''MyError
-- 'throwing_' _Foo :: 'MonadError' MyError m => m a
-- @
throwing_ :: AReview SomeException () -> m x
throwing_ l = throwing l ()
{-# INLINE throwing_ #-}

-- | A variant of 'throwing' that can only be used within the 'IO' 'Monad'
-- (or any other 'MonadCatch' instance) to throw an 'Exception' described
-- by a 'Prism'.
--
-- Although 'throwingM' has a type that is a specialization of the type of
-- 'throwing', the two functions are subtly different:
--
-- @
-- 'throwing' l e \`seq\` x  ≡ 'throwing' e
-- 'throwingM' l e \`seq\` x ≡ x
-- @
--
-- The first example will cause the 'Exception' @e@ to be raised, whereas the
-- second one won't. In fact, 'throwingM' will only cause an 'Exception' to
-- be raised when it is used within the 'MonadCatch' instance. The 'throwingM'
-- variant should be used in preference to 'throwing' to raise an 'Exception'
-- within the 'Monad' because it guarantees ordering with respect to other
-- monadic operations, whereas 'throwing' does not.
--
-- @
-- 'throwingM' l ≡ 'reviews' l 'CatchIO.throw'
-- @
--
-- @
-- 'throwingM' :: 'MonadThrow' m => 'Prism'' 'SomeException' t -> t -> m r
-- 'throwingM' :: 'MonadThrow' m => 'Iso'' 'SomeException' t   -> t -> m r
-- @
throwingM :: MonadThrow m => AReview SomeException b -> b -> m r
throwingM l = reviews l throwM
{-# INLINE throwingM #-}

-- | 'throwingTo' raises an 'Exception' specified by a 'Prism' in the target thread.
--
-- @
-- 'throwingTo' thread l ≡ 'reviews' l ('throwTo' thread)
-- @
--
-- @
-- 'throwingTo' :: 'ThreadId' -> 'Prism'' 'SomeException' t -> t -> m a
-- 'throwingTo' :: 'ThreadId' -> 'Iso'' 'SomeException' t   -> t -> m a
-- @
throwingTo :: MonadIO m => ThreadId -> AReview SomeException b -> b -> m ()
throwingTo tid l = reviews l (liftIO . throwTo tid)
{-# INLINE throwingTo #-}

----------------------------------------------------------------------------
-- Mapping
----------------------------------------------------------------------------

-- | This 'Setter' can be used to purely map over the 'Exception's an
-- arbitrary expression might throw; it is a variant of 'mapException' in
-- the same way that 'mapped' is a variant of 'fmap'.
--
-- > 'mapException' ≡ 'over' 'mappedException'
--
-- This view that every Haskell expression can be regarded as carrying a bag
-- of 'Exception's is detailed in “A Semantics for Imprecise Exceptions” by
-- Peyton Jones & al. at PLDI ’99.
--
-- The following maps failed assertions to arithmetic overflow:
--
-- >>> handling _Overflow (\_ -> return "caught") $ assert False (return "uncaught") & mappedException %~ \ (AssertionFailed _) -> Overflow
-- "caught"
mappedException :: (Exception e, Exception e') => Setter s s e e'
mappedException = sets mapException
{-# INLINE mappedException #-}

-- | This is a type restricted version of 'mappedException', which avoids
-- the type ambiguity in the input 'Exception' when using 'set'.
--
-- The following maps any exception to arithmetic overflow:
--
-- >>> handling _Overflow (\_ -> return "caught") $ assert False (return "uncaught") & mappedException' .~ Overflow
-- "caught"
mappedException' :: Exception e' => Setter s s SomeException e'
mappedException' = mappedException
{-# INLINE mappedException' #-}

----------------------------------------------------------------------------
-- IOException
----------------------------------------------------------------------------

-- | Exceptions that occur in the 'IO' 'Monad'. An 'IOException' records a
-- more specific error type, a descriptive string and maybe the handle that was
-- used when the error was flagged.
--
-- Due to their richer structure relative to other exceptions, these have
-- a more carefully overloaded signature.
class AsIOException t where
  -- | Unfortunately the name 'ioException' is taken by @base@ for
  -- throwing IOExceptions.
  --
  -- @
  -- '_IOException' :: 'Prism'' 'IOException' 'IOException'
  -- '_IOException' :: 'Prism'' 'SomeException' 'IOException'
  -- @
  --
  -- Many combinators for working with an 'IOException' are available
  -- in "System.IO.Error.Lens".
  _IOException :: Prism' t IOException

instance AsIOException IOException where
  _IOException = id
  {-# INLINE _IOException #-}

instance AsIOException SomeException where
  _IOException = exception
  {-# INLINE _IOException #-}

#if __GLASGOW_HASKELL__ >= 710
pattern IOException_ a <- (preview _IOException -> Just a) where
  IOException_ a = review _IOException a
#endif

----------------------------------------------------------------------------
-- ArithException
----------------------------------------------------------------------------

-- | Arithmetic exceptions.
class AsArithException t where
  -- '_ArithException' :: 'Prism'' 'ArithException' 'ArithException'
  -- '_ArithException' :: 'Prism'' 'SomeException'  'ArithException'
  _ArithException :: Prism' t ArithException

#if __GLASGOW_HASKELL__ >= 710
pattern ArithException_ a <- (preview _ArithException -> Just a) where
  ArithException_ a = review _ArithException a
#endif

instance AsArithException ArithException where
  _ArithException = id
  {-# INLINE _ArithException #-}

instance AsArithException SomeException where
  _ArithException = exception
  {-# INLINE _ArithException #-}

-- | Handle arithmetic '_Overflow'.
--
-- @
-- '_Overflow' ≡ '_ArithException' '.' '_Overflow'
-- @
--
-- @
-- '_Overflow' :: 'Prism'' 'ArithException' 'ArithException'
-- '_Overflow' :: 'Prism'' 'SomeException'  'ArithException'
-- @
_Overflow :: AsArithException t => Prism' t ()
_Overflow = _ArithException . dimap seta (either id id) . right' . rmap (Overflow <$) where
  seta Overflow = Right ()
  seta t        = Left  (pure t)
{-# INLINE _Overflow #-}

#if __GLASGOW_HASKELL__ >= 710
pattern Overflow_ <- (has _Overflow -> True) where
  Overflow_ = review _Overflow ()
#endif

-- | Handle arithmetic '_Underflow'.
--
-- @
-- '_Underflow' ≡ '_ArithException' '.' '_Underflow'
-- @
--
-- @
-- '_Underflow' :: 'Prism'' 'ArithException' 'ArithException'
-- '_Underflow' :: 'Prism'' 'SomeException'  'ArithException'
-- @
_Underflow :: AsArithException t => Prism' t ()
_Underflow = _ArithException . dimap seta (either id id) . right' . rmap (Underflow <$) where
  seta Underflow = Right ()
  seta t        = Left  (pure t)
{-# INLINE _Underflow #-}

#if __GLASGOW_HASKELL__ >= 710
pattern Underflow_ <- (has _Underflow -> True) where
  Underflow_ = review _Underflow ()
#endif

-- | Handle arithmetic loss of precision.
--
-- @
-- '_LossOfPrecision' ≡ '_ArithException' '.' '_LossOfPrecision'
-- @
--
-- @
-- '_LossOfPrecision' :: 'Prism'' 'ArithException' 'ArithException'
-- '_LossOfPrecision' :: 'Prism'' 'SomeException'  'ArithException'
-- @
_LossOfPrecision :: AsArithException t => Prism' t ()
_LossOfPrecision = _ArithException . dimap seta (either id id) . right' . rmap (LossOfPrecision <$) where
  seta LossOfPrecision = Right ()
  seta t        = Left  (pure t)
{-# INLINE _LossOfPrecision #-}

#if __GLASGOW_HASKELL__ >= 710
pattern LossOfPrecision_ <- (has _LossOfPrecision -> True) where
  LossOfPrecision_ = review _LossOfPrecision ()
#endif

-- | Handle division by zero.
--
-- @
-- '_DivideByZero' ≡ '_ArithException' '.' '_DivideByZero'
-- @
--
-- @
-- '_DivideByZero' :: 'Prism'' 'ArithException' 'ArithException'
-- '_DivideByZero' :: 'Prism'' 'SomeException'  'ArithException'
-- @
_DivideByZero :: AsArithException t => Prism' t ()
_DivideByZero = _ArithException . dimap seta (either id id) . right' . rmap (DivideByZero <$) where
  seta DivideByZero = Right ()
  seta t        = Left  (pure t)
{-# INLINE _DivideByZero #-}

#if __GLASGOW_HASKELL__ >= 710
pattern DivideByZero_ <- (has _DivideByZero -> True) where
  DivideByZero_ = review _DivideByZero ()
#endif

-- | Handle exceptional _Denormalized floating pure.
--
-- @
-- '_Denormal' ≡ '_ArithException' '.' '_Denormal'
-- @
--
-- @
-- '_Denormal' :: 'Prism'' 'ArithException' 'ArithException'
-- '_Denormal' :: 'Prism'' 'SomeException'  'ArithException'
-- @
_Denormal :: AsArithException t => Prism' t ()
_Denormal = _ArithException . dimap seta (either id id) . right' . rmap (Denormal <$) where
  seta Denormal = Right ()
  seta t        = Left  (pure t)
{-# INLINE _Denormal #-}

#if __GLASGOW_HASKELL__ >= 710
pattern Denormal_ <- (has _Denormal -> True) where
  Denormal_ = review _Denormal ()
#endif

#if MIN_VERSION_base(4,6,0)
-- | Added in @base@ 4.6 in response to this libraries discussion:
--
-- <http://haskell.1045720.n5.nabble.com/Data-Ratio-and-exceptions-td5711246.html>
--
-- @
-- '_RatioZeroDenominator' ≡ '_ArithException' '.' '_RatioZeroDenominator'
-- @
--
-- @
-- '_RatioZeroDenominator' :: 'Prism'' 'ArithException' 'ArithException'
-- '_RatioZeroDenominator' :: 'Prism'' 'SomeException'  'ArithException'
-- @
_RatioZeroDenominator :: AsArithException t => Prism' t ()
_RatioZeroDenominator = _ArithException . dimap seta (either id id) . right' . rmap (RatioZeroDenominator <$) where
  seta RatioZeroDenominator = Right ()
  seta t        = Left  (pure t)
{-# INLINE _RatioZeroDenominator #-}

#if __GLASGOW_HASKELL__ >= 710
pattern RatioZeroDenominator_ <- (has _RatioZeroDenominator -> True) where
  RatioZeroDenominator_ = review _RatioZeroDenominator ()
#endif

#endif

----------------------------------------------------------------------------
-- ArrayException
----------------------------------------------------------------------------

-- | Exceptions generated by array operations.
class AsArrayException t where
  -- | Extract information about an 'ArrayException'.
  --
  -- @
  -- '_ArrayException' :: 'Prism'' 'ArrayException' 'ArrayException'
  -- '_ArrayException' :: 'Prism'' 'SomeException'  'ArrayException'
  -- @
  _ArrayException :: Prism' t ArrayException

instance AsArrayException ArrayException where
  _ArrayException = id
  {-# INLINE _ArrayException #-}

instance AsArrayException SomeException where
  _ArrayException = exception
  {-# INLINE _ArrayException #-}

#if __GLASGOW_HASKELL__ >= 710
pattern ArrayException_ e <- (preview _ArrayException -> Just e) where
  ArrayException_ e = review _ArrayException e
#endif

-- | An attempt was made to index an array outside its declared bounds.
--
-- @
-- '_IndexOutOfBounds' ≡ '_ArrayException' '.' '_IndexOutOfBounds'
-- @
--
-- @
-- '_IndexOutOfBounds' :: 'Prism'' 'ArrayException' 'String'
-- '_IndexOutOfBounds' :: 'Prism'' 'SomeException'  'String'
-- @
_IndexOutOfBounds :: AsArrayException t => Prism' t String
_IndexOutOfBounds = _ArrayException . dimap seta (either id id) . right' . rmap (fmap IndexOutOfBounds) where
  seta (IndexOutOfBounds r) = Right r
  seta t                    = Left  (pure t)
{-# INLINE _IndexOutOfBounds #-}

#if __GLASGOW_HASKELL__ >= 710
pattern IndexOutOfBounds_ e <- (preview _IndexOutOfBounds -> Just e) where
  IndexOutOfBounds_ e = review _IndexOutOfBounds e
#endif

-- | An attempt was made to evaluate an element of an array that had not been initialized.
--
-- @
-- '_UndefinedElement' ≡ '_ArrayException' '.' '_UndefinedElement'
-- @
--
-- @
-- '_UndefinedElement' :: 'Prism'' 'ArrayException' 'String'
-- '_UndefinedElement' :: 'Prism'' 'SomeException'  'String'
-- @
_UndefinedElement :: AsArrayException t => Prism' t String
_UndefinedElement = _ArrayException . dimap seta (either id id) . right' . rmap (fmap UndefinedElement) where
  seta (UndefinedElement r) = Right r
  seta t                    = Left  (pure t)
{-# INLINE _UndefinedElement #-}

#if __GLASGOW_HASKELL__ >= 710
pattern UndefinedElement_ e <- (preview _UndefinedElement -> Just e) where
  UndefinedElement_ e = review _UndefinedElement e
#endif

----------------------------------------------------------------------------
-- AssertionFailed
----------------------------------------------------------------------------

-- | 'assert' was applied to 'Prelude.False'.
class AsAssertionFailed t where
  -- | This 'Exception' contains provides information about what assertion failed in the 'String'.
  --
  -- >>> handling _AssertionFailed (\ xs -> "caught" <$ guard ("<interactive>" `isInfixOf` xs) ) $ assert False (return "uncaught")
  -- "caught"
  --
  -- @
  -- '_AssertionFailed' :: 'Prism'' 'AssertionFailed' 'String'
  -- '_AssertionFailed' :: 'Prism'' 'SomeException'   'String'
  -- @
  _AssertionFailed :: Prism' t String

instance AsAssertionFailed AssertionFailed where
  _AssertionFailed = _Wrapping AssertionFailed
  {-# INLINE _AssertionFailed #-}

instance AsAssertionFailed SomeException where
  _AssertionFailed = exception._Wrapping AssertionFailed
  {-# INLINE _AssertionFailed #-}

#if __GLASGOW_HASKELL__ >= 710
pattern AssertionFailed_ e <- (preview _AssertionFailed -> Just e) where
  AssertionFailed_ e = review _AssertionFailed e
#endif

----------------------------------------------------------------------------
-- AsyncException
----------------------------------------------------------------------------

-- | Asynchronous exceptions.
class AsAsyncException t where
  -- | There are several types of 'AsyncException'.
  --
  -- @
  -- '_AsyncException' :: 'Equality'' 'AsyncException' 'AsyncException'
  -- '_AsyncException' :: 'Prism''    'SomeException'  'AsyncException'
  -- @
  _AsyncException :: Prism' t AsyncException

instance AsAsyncException AsyncException where
  _AsyncException = id
  {-# INLINE _AsyncException #-}

instance AsAsyncException SomeException where
  _AsyncException = exception
  {-# INLINE _AsyncException #-}

#if __GLASGOW_HASKELL__ >= 710
pattern AsyncException_ e <- (preview _AsyncException -> Just e) where
  AsyncException_ e = review _AsyncException e
#endif

-- | The current thread's stack exceeded its limit. Since an 'Exception' has
-- been raised, the thread's stack will certainly be below its limit again,
-- but the programmer should take remedial action immediately.
--
-- @
-- '_StackOverflow' :: 'Prism'' 'AsyncException' ()
-- '_StackOverflow' :: 'Prism'' 'SomeException'  ()
-- @
_StackOverflow :: AsAsyncException t => Prism' t ()
_StackOverflow = _AsyncException . dimap seta (either id id) . right' . rmap (StackOverflow <$) where
  seta StackOverflow = Right ()
  seta t             = Left  (pure t)
{-# INLINE _StackOverflow #-}

#if __GLASGOW_HASKELL__ >= 710
pattern StackOverflow_ <- (has _StackOverflow -> True) where
  StackOverflow_ = review _StackOverflow ()
#endif

-- | The program's heap is reaching its limit, and the program should take action
-- to reduce the amount of live data it has.
--
-- Notes:
--
-- * It is undefined which thread receives this 'Exception'.
--
-- * GHC currently does not throw 'HeapOverflow' exceptions.
--
-- @
-- '_HeapOverflow' :: 'Prism'' 'AsyncException' ()
-- '_HeapOverflow' :: 'Prism'' 'SomeException'  ()
-- @
_HeapOverflow :: AsAsyncException t => Prism' t ()
_HeapOverflow = _AsyncException . dimap seta (either id id) . right' . rmap (HeapOverflow <$) where
  seta HeapOverflow = Right ()
  seta t            = Left  (pure t)
{-# INLINE _HeapOverflow #-}

#if __GLASGOW_HASKELL__ >= 710
pattern HeapOverflow_ <- (has _HeapOverflow -> True) where
  HeapOverflow_ = review _HeapOverflow ()
#endif

-- | This 'Exception' is raised by another thread calling
-- 'Control.Concurrent.killThread', or by the system if it needs to terminate
-- the thread for some reason.
--
-- @
-- '_ThreadKilled' :: 'Prism'' 'AsyncException' ()
-- '_ThreadKilled' :: 'Prism'' 'SomeException'  ()
-- @
_ThreadKilled :: AsAsyncException t => Prism' t ()
_ThreadKilled = _AsyncException . dimap seta (either id id) . right' . rmap (ThreadKilled <$) where
  seta ThreadKilled = Right ()
  seta t            = Left  (pure t)
{-# INLINE _ThreadKilled #-}

#if __GLASGOW_HASKELL__ >= 710
pattern ThreadKilled_ <- (has _ThreadKilled -> True) where
  ThreadKilled_ = review _ThreadKilled ()
#endif

-- | This 'Exception' is raised by default in the main thread of the program when
-- the user requests to terminate the program via the usual mechanism(s)
-- (/e.g./ Control-C in the console).
--
-- @
-- '_UserInterrupt' :: 'Prism'' 'AsyncException' ()
-- '_UserInterrupt' :: 'Prism'' 'SomeException'  ()
-- @
_UserInterrupt :: AsAsyncException t => Prism' t ()
_UserInterrupt = _AsyncException . dimap seta (either id id) . right' . rmap (UserInterrupt <$) where
  seta UserInterrupt = Right ()
  seta t             = Left  (pure t)
{-# INLINE _UserInterrupt #-}

#if __GLASGOW_HASKELL__ >= 710
pattern UserInterrupt_ <- (has _UserInterrupt -> True) where
  UserInterrupt_ = review _UserInterrupt ()
#endif

----------------------------------------------------------------------------
-- AsyncException
----------------------------------------------------------------------------

-- | Thrown when the runtime system detects that the computation is guaranteed
-- not to terminate. Note that there is no guarantee that the runtime system
-- will notice whether any given computation is guaranteed to terminate or not.
class AsNonTermination t where
  -- | There is no additional information carried in a 'NonTermination' 'Exception'.
  --
  -- @
  -- '_NonTermination' :: 'Prism'' 'NonTermination' ()
  -- '_NonTermination' :: 'Prism'' 'SomeException'  ()
  -- @
  _NonTermination :: Prism' t ()

instance AsNonTermination NonTermination where
  _NonTermination = trivial NonTermination
  {-# INLINE _NonTermination #-}

instance AsNonTermination SomeException where
  _NonTermination = exception.trivial NonTermination
  {-# INLINE _NonTermination #-}

#if __GLASGOW_HASKELL__ >= 710
pattern NonTermination_ <- (has _NonTermination -> True) where
  NonTermination_ = review _NonTermination ()
#endif

----------------------------------------------------------------------------
-- NestedAtomically
----------------------------------------------------------------------------

-- | Thrown when the program attempts to call atomically, from the
-- 'Control.Monad.STM' package, inside another call to atomically.
class AsNestedAtomically t where
  -- | There is no additional information carried in a 'NestedAtomically' 'Exception'.
  --
  -- @
  -- '_NestedAtomically' :: 'Prism'' 'NestedAtomically' ()
  -- '_NestedAtomically' :: 'Prism'' 'SomeException'    ()
  -- @
  _NestedAtomically :: Prism' t ()

instance AsNestedAtomically NestedAtomically where
  _NestedAtomically = trivial NestedAtomically
  {-# INLINE _NestedAtomically #-}

instance AsNestedAtomically SomeException where
  _NestedAtomically = exception.trivial NestedAtomically
  {-# INLINE _NestedAtomically #-}

#if __GLASGOW_HASKELL__ >= 710
pattern NestedAtomically_ <- (has _NestedAtomically -> True) where
  NestedAtomically_ = review _NestedAtomically ()
#endif

----------------------------------------------------------------------------
-- BlockedIndefinitelyOnMVar
----------------------------------------------------------------------------

-- | The thread is blocked on an 'Control.Concurrent.MVar.MVar', but there
-- are no other references to the 'Control.Concurrent.MVar.MVar' so it can't
-- ever continue.
class AsBlockedIndefinitelyOnMVar t where
  -- | There is no additional information carried in a 'BlockedIndefinitelyOnMVar' 'Exception'.
  --
  -- @
  -- '_BlockedIndefinitelyOnMVar' :: 'Prism'' 'BlockedIndefinitelyOnMVar' ()
  -- '_BlockedIndefinitelyOnMVar' :: 'Prism'' 'SomeException'             ()
  -- @
  _BlockedIndefinitelyOnMVar :: Prism' t ()

instance AsBlockedIndefinitelyOnMVar BlockedIndefinitelyOnMVar where
  _BlockedIndefinitelyOnMVar = trivial BlockedIndefinitelyOnMVar
  {-# INLINE _BlockedIndefinitelyOnMVar #-}

instance AsBlockedIndefinitelyOnMVar SomeException where
  _BlockedIndefinitelyOnMVar = exception.trivial BlockedIndefinitelyOnMVar
  {-# INLINE _BlockedIndefinitelyOnMVar #-}

#if __GLASGOW_HASKELL__ >= 710
pattern BlockedIndefinitelyOnMVar_ <- (has _BlockedIndefinitelyOnMVar -> True) where
  BlockedIndefinitelyOnMVar_ = review _BlockedIndefinitelyOnMVar ()
#endif

----------------------------------------------------------------------------
-- BlockedIndefinitelyOnSTM
----------------------------------------------------------------------------

-- | The thread is waiting to retry an 'Control.Monad.STM.STM' transaction,
-- but there are no other references to any TVars involved, so it can't ever
-- continue.
class AsBlockedIndefinitelyOnSTM t where
  -- | There is no additional information carried in a 'BlockedIndefinitelyOnSTM' 'Exception'.
  --
  -- @
  -- '_BlockedIndefinitelyOnSTM' :: 'Prism'' 'BlockedIndefinitelyOnSTM' ()
  -- '_BlockedIndefinitelyOnSTM' :: 'Prism'' 'SomeException'            ()
  -- @
  _BlockedIndefinitelyOnSTM :: Prism' t ()

instance AsBlockedIndefinitelyOnSTM BlockedIndefinitelyOnSTM where
  _BlockedIndefinitelyOnSTM = trivial BlockedIndefinitelyOnSTM
  {-# INLINE _BlockedIndefinitelyOnSTM #-}

instance AsBlockedIndefinitelyOnSTM SomeException where
  _BlockedIndefinitelyOnSTM = exception.trivial BlockedIndefinitelyOnSTM
  {-# INLINE _BlockedIndefinitelyOnSTM #-}

#if __GLASGOW_HASKELL__ >= 710
pattern BlockedIndefinitelyOnSTM_ <- (has _BlockedIndefinitelyOnSTM -> True) where
  BlockedIndefinitelyOnSTM_ = review _BlockedIndefinitelyOnSTM ()
#endif

----------------------------------------------------------------------------
-- Deadlock
----------------------------------------------------------------------------

-- | There are no runnable threads, so the program is deadlocked. The
-- 'Deadlock' 'Exception' is raised in the main thread only.
class AsDeadlock t where
  -- | There is no information carried in a 'Deadlock' 'Exception'.
  --
  -- @
  -- '_Deadlock' :: 'Prism'' 'Deadlock'      ()
  -- '_Deadlock' :: 'Prism'' 'SomeException' ()
  -- @
  _Deadlock :: Prism' t ()

instance AsDeadlock Deadlock where
  _Deadlock = trivial Deadlock
  {-# INLINE _Deadlock #-}

instance AsDeadlock SomeException where
  _Deadlock = exception.trivial Deadlock
  {-# INLINE _Deadlock #-}

#if __GLASGOW_HASKELL__ >= 710
pattern Deadlock_ <- (has _Deadlock -> True) where
  Deadlock_ = review _Deadlock ()
#endif

----------------------------------------------------------------------------
-- NoMethodError
----------------------------------------------------------------------------

-- | A class method without a definition (neither a default definition,
-- nor a definition in the appropriate instance) was called.
class AsNoMethodError t where
  -- | Extract a description of the missing method.
  --
  -- @
  -- '_NoMethodError' :: 'Prism'' 'NoMethodError' 'String'
  -- '_NoMethodError' :: 'Prism'' 'SomeException' 'String'
  -- @
  _NoMethodError :: Prism' t String

instance AsNoMethodError NoMethodError where
  _NoMethodError = _Wrapping NoMethodError
  {-# INLINE _NoMethodError #-}

instance AsNoMethodError SomeException where
  _NoMethodError = exception._Wrapping NoMethodError
  {-# INLINE _NoMethodError #-}

#if __GLASGOW_HASKELL__ >= 710
pattern NoMethodError_ e <- (preview _NoMethodError -> Just e) where
  NoMethodError_ e = review _NoMethodError e
#endif

----------------------------------------------------------------------------
-- PatternMatchFail
----------------------------------------------------------------------------

-- | A pattern match failed.
class AsPatternMatchFail t where
  -- | Information about the source location of the pattern.
  --
  -- @
  -- '_PatternMatchFail' :: 'Prism'' 'PatternMatchFail' 'String'
  -- '_PatternMatchFail' :: 'Prism'' 'SomeException'    'String'
  -- @
  _PatternMatchFail :: Prism' t String

instance AsPatternMatchFail PatternMatchFail where
  _PatternMatchFail = _Wrapping PatternMatchFail
  {-# INLINE _PatternMatchFail #-}

instance AsPatternMatchFail SomeException where
  _PatternMatchFail = exception._Wrapping PatternMatchFail
  {-# INLINE _PatternMatchFail #-}

#if __GLASGOW_HASKELL__ >= 710
pattern PatternMatchFail_ e <- (preview _PatternMatchFail -> Just e) where
  PatternMatchFail_ e = review _PatternMatchFail e
#endif

----------------------------------------------------------------------------
-- RecConError
----------------------------------------------------------------------------

-- | An uninitialised record field was used.
class AsRecConError t where
  -- | Information about the source location where the record was
  -- constructed.
  --
  -- @
  -- '_RecConError' :: 'Prism'' 'RecConError'   'String'
  -- '_RecConError' :: 'Prism'' 'SomeException' 'String'
  -- @
  _RecConError :: Prism' t String

instance AsRecConError RecConError where
  _RecConError = _Wrapping RecConError
  {-# INLINE _RecConError #-}

instance AsRecConError SomeException where
  _RecConError = exception._Wrapping RecConError
  {-# INLINE _RecConError #-}

#if __GLASGOW_HASKELL__ >= 710
pattern RecConError_ e <- (preview _RecConError -> Just e) where
  RecConError_ e = review _RecConError e
#endif

----------------------------------------------------------------------------
-- RecSelError
----------------------------------------------------------------------------

-- | A record selector was applied to a constructor without the appropriate
-- field. This can only happen with a datatype with multiple constructors,
-- where some fields are in one constructor but not another.
class AsRecSelError t where
  -- | Information about the source location where the record selection occurred.
  _RecSelError :: Prism' t String

instance AsRecSelError RecSelError where
  _RecSelError = _Wrapping RecSelError
  {-# INLINE _RecSelError #-}

instance AsRecSelError SomeException where
  _RecSelError = exception._Wrapping RecSelError
  {-# INLINE _RecSelError #-}

#if __GLASGOW_HASKELL__ >= 710
pattern RecSelError_ e <- (preview _RecSelError -> Just e) where
  RecSelError_ e = review _RecSelError e
#endif

----------------------------------------------------------------------------
-- RecUpdError
----------------------------------------------------------------------------

-- | A record update was performed on a constructor without the
-- appropriate field. This can only happen with a datatype with multiple
-- constructors, where some fields are in one constructor but not another.
class AsRecUpdError t where
  -- | Information about the source location where the record was updated.
  _RecUpdError :: Prism' t String

instance AsRecUpdError RecUpdError where
  _RecUpdError = _Wrapping RecUpdError
  {-# INLINE _RecUpdError #-}

instance AsRecUpdError SomeException where
  _RecUpdError = exception._Wrapping RecUpdError
  {-# INLINE _RecUpdError #-}

#if __GLASGOW_HASKELL__ >= 710
pattern RecUpdError_ e <- (preview _RecUpdError -> Just e) where
  RecUpdError_ e = review _RecUpdError e
#endif

----------------------------------------------------------------------------
-- ErrorCall
----------------------------------------------------------------------------

-- | This is thrown when the user calls 'Prelude.error'.
class AsErrorCall t where
  -- | Retrieve the argument given to 'Prelude.error'.
  --
  -- 'ErrorCall' is isomorphic to a 'String'.
  --
  -- >>> catching _ErrorCall (error "touch down!") return
  -- "touch down!"
  _ErrorCall :: Prism' t String

instance AsErrorCall ErrorCall where
  _ErrorCall = _Wrapping ErrorCall
  {-# INLINE _ErrorCall #-}

instance AsErrorCall SomeException where
  _ErrorCall = exception._Wrapping ErrorCall
  {-# INLINE _ErrorCall #-}

#if __GLASGOW_HASKELL__ >= 710
pattern ErrorCall_ e <- (preview _ErrorCall -> Just e) where
  ErrorCall_ e = review _ErrorCall e
#endif

#if MIN_VERSION_base(4,8,0)
----------------------------------------------------------------------------
-- AllocationLimitExceeded
----------------------------------------------------------------------------

-- | This thread has exceeded its allocation limit.
class AsAllocationLimitExceeded t where
  -- | There is no additional information carried in an
  -- 'AllocationLimitExceeded' 'Exception'.
  --
  -- @
  -- '_AllocationLimitExceeded' :: 'Prism'' 'AllocationLimitExceeded' ()
  -- '_AllocationLimitExceeded' :: 'Prism'' 'SomeException'           ()
  -- @
  _AllocationLimitExceeded :: Prism' t ()

instance AsAllocationLimitExceeded AllocationLimitExceeded where
  _AllocationLimitExceeded = trivial AllocationLimitExceeded
  {-# INLINE _AllocationLimitExceeded #-}

instance AsAllocationLimitExceeded SomeException where
  _AllocationLimitExceeded = exception.trivial AllocationLimitExceeded
  {-# INLINE _AllocationLimitExceeded #-}

pattern AllocationLimitExceeded_ <- (has _AllocationLimitExceeded -> True) where
  AllocationLimitExceeded_ = review _AllocationLimitExceeded ()
#endif

#if MIN_VERSION_base(4,9,0)
----------------------------------------------------------------------------
-- TypeError
----------------------------------------------------------------------------

-- | An expression that didn't typecheck during compile time was called.
-- This is only possible with @-fdefer-type-errors@.
class AsTypeError t where
  -- | Details about the failed type check.
  --
  -- @
  -- '_TypeError' :: 'Prism'' 'TypeError'     ()
  -- '_TypeError' :: 'Prism'' 'SomeException' ()
  -- @
  _TypeError :: Prism' t String

instance AsTypeError TypeError where
  _TypeError = _Wrapping TypeError
  {-# INLINE _TypeError #-}

instance AsTypeError SomeException where
  _TypeError = exception._Wrapping TypeError
  {-# INLINE _TypeError #-}

pattern TypeError_ e <- (preview _TypeError -> Just e) where
  TypeError_ e = review _TypeError e
#endif

#if MIN_VERSION_base(4,10,0)
----------------------------------------------------------------------------
-- CompactionFailed
----------------------------------------------------------------------------

-- | Compaction found an object that cannot be compacted.
-- Functions cannot be compacted, nor can mutable objects or pinned objects.
class AsCompactionFailed t where
  -- | Information about why a compaction failed.
  --
  -- @
  -- '_CompactionFailed' :: 'Prism'' 'CompactionFailed' ()
  -- '_CompactionFailed' :: 'Prism'' 'SomeException'    ()
  -- @
  _CompactionFailed :: Prism' t String

instance AsCompactionFailed CompactionFailed where
  _CompactionFailed = _Wrapping CompactionFailed
  {-# INLINE _CompactionFailed #-}

instance AsCompactionFailed SomeException where
  _CompactionFailed = exception._Wrapping CompactionFailed
  {-# INLINE _CompactionFailed #-}

pattern CompactionFailed_ e <- (preview _CompactionFailed -> Just e) where
  CompactionFailed_ e = review _CompactionFailed e
#endif

------------------------------------------------------------------------------
-- HandlingException
------------------------------------------------------------------------------

-- | This 'Exception' is thrown by @lens@ when the user somehow manages to rethrow
-- an internal 'HandlingException'.
class AsHandlingException t where
  -- | There is no information carried in a 'HandlingException'.
  --
  -- @
  -- '_HandlingException' :: 'Prism'' 'HandlingException' ()
  -- '_HandlingException' :: 'Prism'' 'SomeException'     ()
  -- @
  _HandlingException :: Prism' t ()

instance AsHandlingException HandlingException where
  _HandlingException = trivial HandlingException
  {-# INLINE _HandlingException #-}

instance AsHandlingException SomeException where
  _HandlingException = exception.trivial HandlingException
  {-# INLINE _HandlingException #-}

#if __GLASGOW_HASKELL__ >= 710
pattern HandlingException_ <- (has _HandlingException -> True) where
  HandlingException_ = review _HandlingException ()
#endif

------------------------------------------------------------------------------
-- Helper Functions
------------------------------------------------------------------------------

trivial :: t -> Iso' t ()
trivial t = const () `iso` const t

