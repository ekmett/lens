{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#include "lens-common.h"

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
  , pattern Exception
  -- * Exception Handlers
  , Handleable(..)
  -- ** IOExceptions
  , AsIOException(..)
  , pattern IOException_
  -- ** Arithmetic Exceptions
  , AsArithException(..)
  , _Overflow, _Underflow, _LossOfPrecision, _DivideByZero, _Denormal
  , _RatioZeroDenominator
  , pattern ArithException_
  , pattern Overflow_
  , pattern Underflow_
  , pattern LossOfPrecision_
  , pattern DivideByZero_
  , pattern Denormal_
  , pattern RatioZeroDenominator_
  -- ** Array Exceptions
  , AsArrayException(..)
  , _IndexOutOfBounds
  , _UndefinedElement
  , pattern ArrayException_
  , pattern IndexOutOfBounds_
  , pattern UndefinedElement_
  -- ** Assertion Failed
  , AsAssertionFailed(..)
  , pattern AssertionFailed__
  , pattern AssertionFailed_
  -- ** Async Exceptions
  , AsAsyncException(..)
  , _StackOverflow
  , _HeapOverflow
  , _ThreadKilled
  , _UserInterrupt
  , pattern AsyncException_
  , pattern StackOverflow_
  , pattern HeapOverflow_
  , pattern ThreadKilled_
  , pattern UserInterrupt_
  -- ** Non-Termination
  , AsNonTermination(..)
  , pattern NonTermination__
  , pattern NonTermination_
  -- ** Nested Atomically
  , AsNestedAtomically(..)
  , pattern NestedAtomically__
  , pattern NestedAtomically_
  -- ** Blocked Indefinitely
  -- *** on MVar
  , AsBlockedIndefinitelyOnMVar(..)
  , pattern BlockedIndefinitelyOnMVar__
  , pattern BlockedIndefinitelyOnMVar_
  -- *** on STM
  , AsBlockedIndefinitelyOnSTM(..)
  , pattern BlockedIndefinitelyOnSTM__
  , pattern BlockedIndefinitelyOnSTM_
  -- ** Deadlock
  , AsDeadlock(..)
  , pattern Deadlock__
  , pattern Deadlock_
  -- ** No Such Method
  , AsNoMethodError(..)
  , pattern NoMethodError__
  , pattern NoMethodError_
  -- ** Pattern Match Failure
  , AsPatternMatchFail(..)
  , pattern PatternMatchFail__
  , pattern PatternMatchFail_
  -- ** Record
  , AsRecConError(..)
  , AsRecSelError(..)
  , AsRecUpdError(..)
  , pattern RecConError__
  , pattern RecConError_
  , pattern RecSelError__
  , pattern RecSelError_
  , pattern RecUpdError__
  , pattern RecUpdError_
  -- ** Error Call
  , AsErrorCall(..)
  , pattern ErrorCall__
  , pattern ErrorCall_
  -- ** Allocation Limit Exceeded
  , AsAllocationLimitExceeded(..)
  , pattern AllocationLimitExceeded__
  , pattern AllocationLimitExceeded_
  -- ** Type Error
  , AsTypeError(..)
  , pattern TypeError__
  , pattern TypeError_
#if MIN_VERSION_base(4,10,0)
  -- ** Compaction Failed
  , AsCompactionFailed(..)
  , pattern CompactionFailed__
  , pattern CompactionFailed_
#endif
  -- * Handling Exceptions
  , AsHandlingException(..)
  , pattern HandlingException__
  , pattern HandlingException_
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
  , Bool(..)
  )

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Control.Applicative
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

pattern Exception :: Exception a => a -> SomeException
pattern Exception e <- (preview exception -> Just e) where
  Exception e = review exception e

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
-- the information about the match. This is particularly useful when you have
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

pattern IOException_ :: AsIOException s => IOException -> s
pattern IOException_ a <- (preview _IOException -> Just a) where
  IOException_ a = review _IOException a

----------------------------------------------------------------------------
-- ArithException
----------------------------------------------------------------------------

-- | Arithmetic exceptions.
class AsArithException t where
  -- |
  -- @
  -- '_ArithException' :: 'Prism'' 'ArithException' 'ArithException'
  -- '_ArithException' :: 'Prism'' 'SomeException'  'ArithException'
  -- @
  _ArithException :: Prism' t ArithException

pattern ArithException_ :: AsArithException s => ArithException -> s
pattern ArithException_ a <- (preview _ArithException -> Just a) where
  ArithException_ a = review _ArithException a

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

pattern Overflow_ :: AsArithException s => s
pattern Overflow_ <- (has _Overflow -> True) where
  Overflow_ = review _Overflow ()

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

pattern Underflow_ :: AsArithException s => s
pattern Underflow_ <- (has _Underflow -> True) where
  Underflow_ = review _Underflow ()

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

pattern LossOfPrecision_ :: AsArithException s => s
pattern LossOfPrecision_ <- (has _LossOfPrecision -> True) where
  LossOfPrecision_ = review _LossOfPrecision ()

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

pattern DivideByZero_ :: AsArithException s => s
pattern DivideByZero_ <- (has _DivideByZero -> True) where
  DivideByZero_ = review _DivideByZero ()

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

pattern Denormal_ :: AsArithException s => s
pattern Denormal_ <- (has _Denormal -> True) where
  Denormal_ = review _Denormal ()

-- |
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

pattern RatioZeroDenominator_ :: AsArithException s => s
pattern RatioZeroDenominator_ <- (has _RatioZeroDenominator -> True) where
  RatioZeroDenominator_ = review _RatioZeroDenominator ()

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

pattern ArrayException_ :: AsArrayException s => ArrayException -> s
pattern ArrayException_ e <- (preview _ArrayException -> Just e) where
  ArrayException_ e = review _ArrayException e

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

pattern IndexOutOfBounds_ :: AsArrayException s => String -> s
pattern IndexOutOfBounds_ e <- (preview _IndexOutOfBounds -> Just e) where
  IndexOutOfBounds_ e = review _IndexOutOfBounds e

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

pattern UndefinedElement_ :: AsArrayException s => String -> s
pattern UndefinedElement_ e <- (preview _UndefinedElement -> Just e) where
  UndefinedElement_ e = review _UndefinedElement e

----------------------------------------------------------------------------
-- AssertionFailed
----------------------------------------------------------------------------

-- | 'assert' was applied to 'Prelude.False'.
class AsAssertionFailed t where
  -- |
  -- @
  -- '__AssertionFailed' :: 'Prism'' 'AssertionFailed' 'AssertionFailed'
  -- '__AssertionFailed' :: 'Prism'' 'SomeException'   'AssertionFailed'
  -- @
  __AssertionFailed :: Prism' t AssertionFailed

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
  _AssertionFailed = __AssertionFailed._AssertionFailed
  {-# INLINE _AssertionFailed #-}

instance AsAssertionFailed AssertionFailed where
  __AssertionFailed = id
  {-# INLINE __AssertionFailed #-}

  _AssertionFailed = _Wrapping AssertionFailed
  {-# INLINE _AssertionFailed #-}

instance AsAssertionFailed SomeException where
  __AssertionFailed = exception
  {-# INLINE __AssertionFailed #-}

pattern AssertionFailed__ :: AsAssertionFailed s => AssertionFailed -> s
pattern AssertionFailed__ e <- (preview __AssertionFailed -> Just e) where
  AssertionFailed__ e = review __AssertionFailed e

pattern AssertionFailed_ :: AsAssertionFailed s => String -> s
pattern AssertionFailed_ e <- (preview _AssertionFailed -> Just e) where
  AssertionFailed_ e = review _AssertionFailed e

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

pattern AsyncException_ :: AsAsyncException s => AsyncException -> s
pattern AsyncException_ e <- (preview _AsyncException -> Just e) where
  AsyncException_ e = review _AsyncException e

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

pattern StackOverflow_ :: AsAsyncException s => s
pattern StackOverflow_ <- (has _StackOverflow -> True) where
  StackOverflow_ = review _StackOverflow ()

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

pattern HeapOverflow_ :: AsAsyncException s => s
pattern HeapOverflow_ <- (has _HeapOverflow -> True) where
  HeapOverflow_ = review _HeapOverflow ()

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

pattern ThreadKilled_ :: AsAsyncException s => s
pattern ThreadKilled_ <- (has _ThreadKilled -> True) where
  ThreadKilled_ = review _ThreadKilled ()

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

pattern UserInterrupt_ :: AsAsyncException s => s
pattern UserInterrupt_ <- (has _UserInterrupt -> True) where
  UserInterrupt_ = review _UserInterrupt ()

----------------------------------------------------------------------------
-- AsyncException
----------------------------------------------------------------------------

-- | Thrown when the runtime system detects that the computation is guaranteed
-- not to terminate. Note that there is no guarantee that the runtime system
-- will notice whether any given computation is guaranteed to terminate or not.
class AsNonTermination t where
  -- |
  -- @
  -- '__NonTermination' :: 'Prism'' 'NonTermination' 'NonTermination'
  -- '__NonTermination' :: 'Prism'' 'SomeException'  'NonTermination'
  -- @
  __NonTermination :: Prism' t NonTermination

  -- | There is no additional information carried in a 'NonTermination' 'Exception'.
  --
  -- @
  -- '_NonTermination' :: 'Prism'' 'NonTermination' ()
  -- '_NonTermination' :: 'Prism'' 'SomeException'  ()
  -- @
  _NonTermination :: Prism' t ()
  _NonTermination = __NonTermination._NonTermination
  {-# INLINE _NonTermination #-}

instance AsNonTermination NonTermination where
  __NonTermination = id
  {-# INLINE __NonTermination #-}

  _NonTermination = trivial NonTermination
  {-# INLINE _NonTermination #-}

instance AsNonTermination SomeException where
  __NonTermination = exception
  {-# INLINE __NonTermination #-}

pattern NonTermination__ :: AsNonTermination s => NonTermination -> s
pattern NonTermination__ e <- (preview __NonTermination -> Just e) where
  NonTermination__ e = review __NonTermination e

pattern NonTermination_ :: AsNonTermination s => s
pattern NonTermination_ <- (has _NonTermination -> True) where
  NonTermination_ = review _NonTermination ()

----------------------------------------------------------------------------
-- NestedAtomically
----------------------------------------------------------------------------

-- | Thrown when the program attempts to call atomically, from the
-- 'Control.Monad.STM' package, inside another call to atomically.
class AsNestedAtomically t where
  -- |
  -- @
  -- '__NestedAtomically' :: 'Prism'' 'NestedAtomically' 'NestedAtomically'
  -- '__NestedAtomically' :: 'Prism'' 'SomeException'    'NestedAtomically'
  -- @
  __NestedAtomically :: Prism' t NestedAtomically

  -- | There is no additional information carried in a 'NestedAtomically' 'Exception'.
  --
  -- @
  -- '_NestedAtomically' :: 'Prism'' 'NestedAtomically' ()
  -- '_NestedAtomically' :: 'Prism'' 'SomeException'    ()
  -- @
  _NestedAtomically :: Prism' t ()
  _NestedAtomically = __NestedAtomically._NestedAtomically
  {-# INLINE _NestedAtomically #-}

instance AsNestedAtomically NestedAtomically where
  __NestedAtomically = id
  {-# INLINE __NestedAtomically #-}

  _NestedAtomically = trivial NestedAtomically
  {-# INLINE _NestedAtomically #-}

instance AsNestedAtomically SomeException where
  __NestedAtomically = exception
  {-# INLINE __NestedAtomically #-}

pattern NestedAtomically__ :: AsNestedAtomically s => NestedAtomically -> s
pattern NestedAtomically__ e <- (preview __NestedAtomically -> Just e) where
  NestedAtomically__ e = review __NestedAtomically e

pattern NestedAtomically_ :: AsNestedAtomically s => s
pattern NestedAtomically_ <- (has _NestedAtomically -> True) where
  NestedAtomically_ = review _NestedAtomically ()

----------------------------------------------------------------------------
-- BlockedIndefinitelyOnMVar
----------------------------------------------------------------------------

-- | The thread is blocked on an 'Control.Concurrent.MVar.MVar', but there
-- are no other references to the 'Control.Concurrent.MVar.MVar' so it can't
-- ever continue.
class AsBlockedIndefinitelyOnMVar t where
  -- |
  -- @
  -- '__BlockedIndefinitelyOnMVar' :: 'Prism'' 'BlockedIndefinitelyOnMVar' 'BlockedIndefinitelyOnMVar'
  -- '__BlockedIndefinitelyOnMVar' :: 'Prism'' 'SomeException'             'BlockedIndefinitelyOnMVar'
  -- @
  __BlockedIndefinitelyOnMVar :: Prism' t BlockedIndefinitelyOnMVar

  -- | There is no additional information carried in a 'BlockedIndefinitelyOnMVar' 'Exception'.
  --
  -- @
  -- '_BlockedIndefinitelyOnMVar' :: 'Prism'' 'BlockedIndefinitelyOnMVar' ()
  -- '_BlockedIndefinitelyOnMVar' :: 'Prism'' 'SomeException'             ()
  -- @
  _BlockedIndefinitelyOnMVar :: Prism' t ()
  _BlockedIndefinitelyOnMVar = __BlockedIndefinitelyOnMVar._BlockedIndefinitelyOnMVar
  {-# INLINE _BlockedIndefinitelyOnMVar #-}

instance AsBlockedIndefinitelyOnMVar BlockedIndefinitelyOnMVar where
  __BlockedIndefinitelyOnMVar = id
  {-# INLINE __BlockedIndefinitelyOnMVar #-}

  _BlockedIndefinitelyOnMVar = trivial BlockedIndefinitelyOnMVar
  {-# INLINE _BlockedIndefinitelyOnMVar #-}

instance AsBlockedIndefinitelyOnMVar SomeException where
  __BlockedIndefinitelyOnMVar = exception
  {-# INLINE __BlockedIndefinitelyOnMVar #-}

pattern BlockedIndefinitelyOnMVar__ :: AsBlockedIndefinitelyOnMVar s => BlockedIndefinitelyOnMVar -> s
pattern BlockedIndefinitelyOnMVar__ e <- (preview __BlockedIndefinitelyOnMVar -> Just e) where
  BlockedIndefinitelyOnMVar__ e = review __BlockedIndefinitelyOnMVar e

pattern BlockedIndefinitelyOnMVar_ :: AsBlockedIndefinitelyOnMVar s => s
pattern BlockedIndefinitelyOnMVar_ <- (has _BlockedIndefinitelyOnMVar -> True) where
  BlockedIndefinitelyOnMVar_ = review _BlockedIndefinitelyOnMVar ()

----------------------------------------------------------------------------
-- BlockedIndefinitelyOnSTM
----------------------------------------------------------------------------

-- | The thread is waiting to retry an 'Control.Monad.STM.STM' transaction,
-- but there are no other references to any TVars involved, so it can't ever
-- continue.
class AsBlockedIndefinitelyOnSTM t where
  -- |
  -- @
  -- '__BlockedIndefinitelyOnSTM' :: 'Prism'' 'BlockedIndefinitelyOnSTM' 'BlockedIndefinitelyOnSTM'
  -- '__BlockedIndefinitelyOnSTM' :: 'Prism'' 'SomeException'            'BlockedIndefinitelyOnSTM'
  -- @
  __BlockedIndefinitelyOnSTM :: Prism' t BlockedIndefinitelyOnSTM

  -- | There is no additional information carried in a 'BlockedIndefinitelyOnSTM' 'Exception'.
  --
  -- @
  -- '_BlockedIndefinitelyOnSTM' :: 'Prism'' 'BlockedIndefinitelyOnSTM' ()
  -- '_BlockedIndefinitelyOnSTM' :: 'Prism'' 'SomeException'            ()
  -- @
  _BlockedIndefinitelyOnSTM :: Prism' t ()
  _BlockedIndefinitelyOnSTM = __BlockedIndefinitelyOnSTM._BlockedIndefinitelyOnSTM
  {-# INLINE _BlockedIndefinitelyOnSTM #-}

instance AsBlockedIndefinitelyOnSTM BlockedIndefinitelyOnSTM where
  __BlockedIndefinitelyOnSTM = id
  {-# INLINE __BlockedIndefinitelyOnSTM #-}

  _BlockedIndefinitelyOnSTM = trivial BlockedIndefinitelyOnSTM
  {-# INLINE _BlockedIndefinitelyOnSTM #-}

instance AsBlockedIndefinitelyOnSTM SomeException where
  __BlockedIndefinitelyOnSTM = exception
  {-# INLINE __BlockedIndefinitelyOnSTM #-}

pattern BlockedIndefinitelyOnSTM__ :: AsBlockedIndefinitelyOnSTM s => BlockedIndefinitelyOnSTM -> s
pattern BlockedIndefinitelyOnSTM__ e <- (preview __BlockedIndefinitelyOnSTM -> Just e) where
  BlockedIndefinitelyOnSTM__ e = review __BlockedIndefinitelyOnSTM e

pattern BlockedIndefinitelyOnSTM_ :: AsBlockedIndefinitelyOnSTM s => s
pattern BlockedIndefinitelyOnSTM_ <- (has _BlockedIndefinitelyOnSTM -> True) where
  BlockedIndefinitelyOnSTM_ = review _BlockedIndefinitelyOnSTM ()

----------------------------------------------------------------------------
-- Deadlock
----------------------------------------------------------------------------

-- | There are no runnable threads, so the program is deadlocked. The
-- 'Deadlock' 'Exception' is raised in the main thread only.
class AsDeadlock t where
  -- |
  -- @
  -- '__Deadlock' :: 'Prism'' 'Deadlock'      'Deadlock'
  -- '__Deadlock' :: 'Prism'' 'SomeException' 'Deadlock'
  -- @
  __Deadlock :: Prism' t Deadlock

  -- | There is no information carried in a 'Deadlock' 'Exception'.
  --
  -- @
  -- '_Deadlock' :: 'Prism'' 'Deadlock'      ()
  -- '_Deadlock' :: 'Prism'' 'SomeException' ()
  -- @
  _Deadlock :: Prism' t ()
  _Deadlock = __Deadlock._Deadlock
  {-# INLINE _Deadlock #-}

instance AsDeadlock Deadlock where
  __Deadlock = id
  {-# INLINE __Deadlock #-}

  _Deadlock = trivial Deadlock
  {-# INLINE _Deadlock #-}

instance AsDeadlock SomeException where
  __Deadlock = exception
  {-# INLINE __Deadlock #-}

pattern Deadlock__ :: AsDeadlock s => Deadlock -> s
pattern Deadlock__ e <- (preview __Deadlock -> Just e) where
  Deadlock__ e = review __Deadlock e

pattern Deadlock_ :: AsDeadlock s => s
pattern Deadlock_ <- (has _Deadlock -> True) where
  Deadlock_ = review _Deadlock ()

----------------------------------------------------------------------------
-- NoMethodError
----------------------------------------------------------------------------

-- | A class method without a definition (neither a default definition,
-- nor a definition in the appropriate instance) was called.
class AsNoMethodError t where
  -- |
  -- @
  -- '__NoMethodError' :: 'Prism'' 'NoMethodError' 'NoMethodError'
  -- '__NoMethodError' :: 'Prism'' 'SomeException' 'NoMethodError'
  -- @
  __NoMethodError :: Prism' t NoMethodError

  -- | Extract a description of the missing method.
  --
  -- @
  -- '_NoMethodError' :: 'Prism'' 'NoMethodError' 'String'
  -- '_NoMethodError' :: 'Prism'' 'SomeException' 'String'
  -- @
  _NoMethodError :: Prism' t String
  _NoMethodError = __NoMethodError._NoMethodError
  {-# INLINE _NoMethodError #-}

instance AsNoMethodError NoMethodError where
  __NoMethodError = id
  {-# INLINE __NoMethodError #-}

  _NoMethodError = _Wrapping NoMethodError
  {-# INLINE _NoMethodError #-}

instance AsNoMethodError SomeException where
  __NoMethodError = exception
  {-# INLINE __NoMethodError #-}

pattern NoMethodError__ :: AsNoMethodError s => NoMethodError -> s
pattern NoMethodError__ e <- (preview __NoMethodError -> Just e) where
  NoMethodError__ e = review __NoMethodError e

pattern NoMethodError_ :: AsNoMethodError s => String -> s
pattern NoMethodError_ e <- (preview _NoMethodError -> Just e) where
  NoMethodError_ e = review _NoMethodError e

----------------------------------------------------------------------------
-- PatternMatchFail
----------------------------------------------------------------------------

-- | A pattern match failed.
class AsPatternMatchFail t where
  -- |
  -- @
  -- '__PatternMatchFail' :: 'Prism'' 'PatternMatchFail' 'PatternMatchFail'
  -- '__PatternMatchFail' :: 'Prism'' 'SomeException'    'PatternMatchFail'
  -- @
  __PatternMatchFail :: Prism' t PatternMatchFail

  -- | Information about the source location of the pattern.
  --
  -- @
  -- '_PatternMatchFail' :: 'Prism'' 'PatternMatchFail' 'String'
  -- '_PatternMatchFail' :: 'Prism'' 'SomeException'    'String'
  -- @
  _PatternMatchFail :: Prism' t String
  _PatternMatchFail = __PatternMatchFail._PatternMatchFail
  {-# INLINE _PatternMatchFail #-}

instance AsPatternMatchFail PatternMatchFail where
  __PatternMatchFail = id
  {-# INLINE __PatternMatchFail #-}

  _PatternMatchFail = _Wrapping PatternMatchFail
  {-# INLINE _PatternMatchFail #-}

instance AsPatternMatchFail SomeException where
  __PatternMatchFail = exception
  {-# INLINE __PatternMatchFail #-}

pattern PatternMatchFail__ :: AsPatternMatchFail s => PatternMatchFail -> s
pattern PatternMatchFail__ e <- (preview __PatternMatchFail -> Just e) where
  PatternMatchFail__ e = review __PatternMatchFail e

pattern PatternMatchFail_ :: AsPatternMatchFail s => String -> s
pattern PatternMatchFail_ e <- (preview _PatternMatchFail -> Just e) where
  PatternMatchFail_ e = review _PatternMatchFail e

----------------------------------------------------------------------------
-- RecConError
----------------------------------------------------------------------------

-- | An uninitialised record field was used.
class AsRecConError t where
  -- |
  -- @
  -- '__RecConError' :: 'Prism'' 'RecConError'   'RecConError'
  -- '__RecConError' :: 'Prism'' 'SomeException' 'RecConError'
  -- @
  __RecConError :: Prism' t RecConError

  -- | Information about the source location where the record was
  -- constructed.
  --
  -- @
  -- '_RecConError' :: 'Prism'' 'RecConError'   'String'
  -- '_RecConError' :: 'Prism'' 'SomeException' 'String'
  -- @
  _RecConError :: Prism' t String
  _RecConError = __RecConError._RecConError
  {-# INLINE _RecConError #-}

instance AsRecConError RecConError where
  __RecConError = id
  {-# INLINE __RecConError #-}

  _RecConError = _Wrapping RecConError
  {-# INLINE _RecConError #-}

instance AsRecConError SomeException where
  __RecConError = exception
  {-# INLINE __RecConError #-}

pattern RecConError__ :: AsRecConError s => RecConError -> s
pattern RecConError__ e <- (preview __RecConError -> Just e) where
  RecConError__ e = review __RecConError e

pattern RecConError_ :: AsRecConError s => String -> s
pattern RecConError_ e <- (preview _RecConError -> Just e) where
  RecConError_ e = review _RecConError e

----------------------------------------------------------------------------
-- RecSelError
----------------------------------------------------------------------------

-- | A record selector was applied to a constructor without the appropriate
-- field. This can only happen with a datatype with multiple constructors,
-- where some fields are in one constructor but not another.
class AsRecSelError t where
  -- |
  -- @
  -- '__RecSelError' :: 'Prism'' 'RecSelError'   'RecSelError'
  -- '__RecSelError' :: 'Prism'' 'SomeException' 'RecSelError'
  -- @
  __RecSelError :: Prism' t RecSelError

  -- | Information about the source location where the record selection occurred.
  --
  -- @
  -- '_RecSelError' :: 'Prism'' 'RecSelError'   'String'
  -- '_RecSelError' :: 'Prism'' 'SomeException' 'String'
  -- @
  _RecSelError :: Prism' t String
  _RecSelError = __RecSelError._RecSelError
  {-# INLINE _RecSelError #-}

instance AsRecSelError RecSelError where
  __RecSelError = id
  {-# INLINE __RecSelError #-}

  _RecSelError = _Wrapping RecSelError
  {-# INLINE _RecSelError #-}

instance AsRecSelError SomeException where
  __RecSelError = exception
  {-# INLINE __RecSelError #-}

pattern RecSelError__ :: AsRecSelError s => RecSelError -> s
pattern RecSelError__ e <- (preview __RecSelError -> Just e) where
  RecSelError__ e = review __RecSelError e

pattern RecSelError_ :: AsRecSelError s => String -> s
pattern RecSelError_ e <- (preview _RecSelError -> Just e) where
  RecSelError_ e = review _RecSelError e

----------------------------------------------------------------------------
-- RecUpdError
----------------------------------------------------------------------------

-- | A record update was performed on a constructor without the
-- appropriate field. This can only happen with a datatype with multiple
-- constructors, where some fields are in one constructor but not another.
class AsRecUpdError t where
  -- |
  -- @
  -- '__RecUpdError' :: 'Prism'' 'RecUpdError'   'RecUpdError'
  -- '__RecUpdError' :: 'Prism'' 'SomeException' 'RecUpdError'
  -- @
  __RecUpdError :: Prism' t RecUpdError

  -- | Information about the source location where the record was updated.
  --
  -- @
  -- '_RecUpdError' :: 'Prism'' 'RecUpdError'   'String'
  -- '_RecUpdError' :: 'Prism'' 'SomeException' 'String'
  -- @
  _RecUpdError :: Prism' t String
  _RecUpdError = __RecUpdError._RecUpdError
  {-# INLINE _RecUpdError #-}

instance AsRecUpdError RecUpdError where
  __RecUpdError = id
  {-# INLINE __RecUpdError #-}

  _RecUpdError = _Wrapping RecUpdError
  {-# INLINE _RecUpdError #-}

instance AsRecUpdError SomeException where
  __RecUpdError = exception
  {-# INLINE __RecUpdError #-}

pattern RecUpdError__ :: AsRecUpdError s => RecUpdError -> s
pattern RecUpdError__ e <- (preview __RecUpdError -> Just e) where
  RecUpdError__ e = review __RecUpdError e

pattern RecUpdError_ :: AsRecUpdError s => String -> s
pattern RecUpdError_ e <- (preview _RecUpdError -> Just e) where
  RecUpdError_ e = review _RecUpdError e

----------------------------------------------------------------------------
-- ErrorCall
----------------------------------------------------------------------------

-- | This is thrown when the user calls 'Prelude.error'.
class AsErrorCall t where
  -- |
  -- @
  -- '__ErrorCall' :: 'Prism'' 'ErrorCall'     'ErrorCall'
  -- '__ErrorCall' :: 'Prism'' 'SomeException' 'ErrorCall'
  -- @
  __ErrorCall :: Prism' t ErrorCall

  -- | Retrieve the argument given to 'Prelude.error'.
  --
  -- 'ErrorCall' is isomorphic to a 'String'.
  --
  -- >>> catching _ErrorCall (error "touch down!") return
  -- "touch down!"
  --
  -- @
  -- '_ErrorCall' :: 'Prism'' 'ErrorCall'     'String'
  -- '_ErrorCall' :: 'Prism'' 'SomeException' 'String'
  -- @
  _ErrorCall :: Prism' t String
  _ErrorCall = __ErrorCall._ErrorCall
  {-# INLINE _ErrorCall #-}

instance AsErrorCall ErrorCall where
  __ErrorCall = id
  {-# INLINE __ErrorCall #-}

  _ErrorCall = _Wrapping ErrorCall
  {-# INLINE _ErrorCall #-}

instance AsErrorCall SomeException where
  __ErrorCall = exception
  {-# INLINE __ErrorCall #-}

pattern ErrorCall__ :: AsErrorCall s => ErrorCall -> s
pattern ErrorCall__ e <- (preview __ErrorCall -> Just e) where
  ErrorCall__ e = review __ErrorCall e

pattern ErrorCall_ :: AsErrorCall s => String -> s
pattern ErrorCall_ e <- (preview _ErrorCall -> Just e) where
  ErrorCall_ e = review _ErrorCall e

----------------------------------------------------------------------------
-- AllocationLimitExceeded
----------------------------------------------------------------------------

-- | This thread has exceeded its allocation limit.
class AsAllocationLimitExceeded t where
  -- |
  -- @
  -- '__AllocationLimitExceeded' :: 'Prism'' 'AllocationLimitExceeded' 'AllocationLimitExceeded'
  -- '__AllocationLimitExceeded' :: 'Prism'' 'SomeException'           'AllocationLimitExceeded'
  -- @
  __AllocationLimitExceeded :: Prism' t AllocationLimitExceeded

  -- | There is no additional information carried in an
  -- 'AllocationLimitExceeded' 'Exception'.
  --
  -- @
  -- '_AllocationLimitExceeded' :: 'Prism'' 'AllocationLimitExceeded' ()
  -- '_AllocationLimitExceeded' :: 'Prism'' 'SomeException'           ()
  -- @
  _AllocationLimitExceeded :: Prism' t ()
  _AllocationLimitExceeded = __AllocationLimitExceeded._AllocationLimitExceeded
  {-# INLINE _AllocationLimitExceeded #-}

instance AsAllocationLimitExceeded AllocationLimitExceeded where
  __AllocationLimitExceeded = id
  {-# INLINE __AllocationLimitExceeded #-}

  _AllocationLimitExceeded = trivial AllocationLimitExceeded
  {-# INLINE _AllocationLimitExceeded #-}

instance AsAllocationLimitExceeded SomeException where
  __AllocationLimitExceeded = exception
  {-# INLINE __AllocationLimitExceeded #-}

pattern AllocationLimitExceeded__ :: AsAllocationLimitExceeded s => AllocationLimitExceeded -> s
pattern AllocationLimitExceeded__ e <- (preview __AllocationLimitExceeded -> Just e) where
  AllocationLimitExceeded__ e = review __AllocationLimitExceeded e

pattern AllocationLimitExceeded_ :: AsAllocationLimitExceeded s => s
pattern AllocationLimitExceeded_ <- (has _AllocationLimitExceeded -> True) where
  AllocationLimitExceeded_ = review _AllocationLimitExceeded ()

----------------------------------------------------------------------------
-- TypeError
----------------------------------------------------------------------------

-- | An expression that didn't typecheck during compile time was called.
-- This is only possible with @-fdefer-type-errors@.
class AsTypeError t where
  -- |
  -- @
  -- '__TypeError' :: 'Prism'' 'TypeError'     'TypeError'
  -- '__TypeError' :: 'Prism'' 'SomeException' 'TypeError'
  -- @
  __TypeError :: Prism' t TypeError

  -- | Details about the failed type check.
  --
  -- @
  -- '_TypeError' :: 'Prism'' 'TypeError'     'String'
  -- '_TypeError' :: 'Prism'' 'SomeException' 'String'
  -- @
  _TypeError :: Prism' t String
  _TypeError = __TypeError._TypeError
  {-# INLINE _TypeError #-}

instance AsTypeError TypeError where
  __TypeError = id
  {-# INLINE __TypeError #-}

  _TypeError = _Wrapping TypeError
  {-# INLINE _TypeError #-}

instance AsTypeError SomeException where
  __TypeError = exception
  {-# INLINE __TypeError #-}

pattern TypeError__ :: AsTypeError s => TypeError -> s
pattern TypeError__ e <- (preview __TypeError -> Just e) where
  TypeError__ e = review __TypeError e

pattern TypeError_ :: AsTypeError s => String -> s
pattern TypeError_ e <- (preview _TypeError -> Just e) where
  TypeError_ e = review _TypeError e

#if MIN_VERSION_base(4,10,0)
----------------------------------------------------------------------------
-- CompactionFailed
----------------------------------------------------------------------------

-- | Compaction found an object that cannot be compacted.
-- Functions cannot be compacted, nor can mutable objects or pinned objects.
class AsCompactionFailed t where
  -- |
  -- @
  -- '__CompactionFailed' :: 'Prism'' 'CompactionFailed' 'CompactionFailed'
  -- '__CompactionFailed' :: 'Prism'' 'SomeException'    'CompactionFailed'
  -- @
  __CompactionFailed :: Prism' t CompactionFailed

  -- | Information about why a compaction failed.
  --
  -- @
  -- '_CompactionFailed' :: 'Prism'' 'CompactionFailed' 'String'
  -- '_CompactionFailed' :: 'Prism'' 'SomeException'    'String'
  -- @
  _CompactionFailed :: Prism' t String
  _CompactionFailed = __CompactionFailed._CompactionFailed
  {-# INLINE _CompactionFailed #-}

instance AsCompactionFailed CompactionFailed where
  __CompactionFailed = id
  {-# INLINE __CompactionFailed #-}

  _CompactionFailed = _Wrapping CompactionFailed
  {-# INLINE _CompactionFailed #-}

instance AsCompactionFailed SomeException where
  __CompactionFailed = exception
  {-# INLINE __CompactionFailed #-}

pattern CompactionFailed__ :: AsCompactionFailed s => CompactionFailed -> s
pattern CompactionFailed__ e <- (preview __CompactionFailed -> Just e) where
  CompactionFailed__ e = review __CompactionFailed e

pattern CompactionFailed_ :: AsCompactionFailed s => String -> s
pattern CompactionFailed_ e <- (preview _CompactionFailed -> Just e) where
  CompactionFailed_ e = review _CompactionFailed e
#endif

------------------------------------------------------------------------------
-- HandlingException
------------------------------------------------------------------------------

-- | This 'Exception' is thrown by @lens@ when the user somehow manages to rethrow
-- an internal 'HandlingException'.
class AsHandlingException t where
  -- |
  -- @
  -- '__HandlingException' :: 'Prism'' 'HandlingException' 'HandlingException'
  -- '__HandlingException' :: 'Prism'' 'SomeException'     'HandlingException'
  -- @
  __HandlingException :: Prism' t HandlingException

  -- | There is no information carried in a 'HandlingException'.
  --
  -- @
  -- '_HandlingException' :: 'Prism'' 'HandlingException' ()
  -- '_HandlingException' :: 'Prism'' 'SomeException'     ()
  -- @
  _HandlingException :: Prism' t ()
  _HandlingException = __HandlingException._HandlingException
  {-# INLINE _HandlingException #-}

instance AsHandlingException HandlingException where
  __HandlingException = id
  {-# INLINE __HandlingException #-}

  _HandlingException = trivial HandlingException
  {-# INLINE _HandlingException #-}

instance AsHandlingException SomeException where
  __HandlingException = exception
  {-# INLINE __HandlingException #-}

pattern HandlingException__ :: AsHandlingException s => HandlingException -> s
pattern HandlingException__ e <- (preview __HandlingException -> Just e) where
  HandlingException__ e = review __HandlingException e

pattern HandlingException_ :: AsHandlingException s => s
pattern HandlingException_ <- (has _HandlingException -> True) where
  HandlingException_ = review _HandlingException ()

------------------------------------------------------------------------------
-- Helper Functions
------------------------------------------------------------------------------

trivial :: t -> Iso' t ()
trivial t = const () `iso` const t

