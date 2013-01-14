{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Exception.Lens
-- Copyright   :  (C) 2012 Edward Kmett
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
----------------------------------------------------------------------------
module Control.Exception.Lens
  (
  -- * Handling
    catching, catching_
  , handling, handling_
  -- * Throwing
  , throwing, throwingIO, throwingTo
  -- * Exceptions
  , exception
  -- ** IOExceptions
  , AsIOException(..)
  -- ** Arithmetic Exceptions
  , AsArithException(..)
  , _Overflow
  , _Underflow
  , _LossOfPrecision
  , _DivideByZero
  , _Denormal
#if MIN_VERSION_base(4,6,0)
  , _RatioZeroDenominator
#endif
  -- ** Array Exceptions
  , AsArrayException(..)
  , _IndexOutOfBounds
  , _UndefinedElement
  -- ** Assertion Failed
  , AsAssertionFailed(..)
  -- ** Async Exceptions
  , AsAsyncException(..)
  , _StackOverflow
  , _HeapOverflow
  , _ThreadKilled
  , _UserInterrupt
  -- ** Non-Termination
  , AsNonTermination(..)
  -- ** Nested Atomically
  , AsNestedAtomically(..)
  -- ** Blocked Indefinitely
  -- *** on MVar
  , AsBlockedIndefinitelyOnMVar(..)
  -- *** on STM
  , AsBlockedIndefinitelyOnSTM(..)
  -- ** Deadlock
  , AsDeadlock(..)
  -- ** No Such Method
  , AsNoMethodError(..)
  -- ** Pattern Match Failure
  , AsPatternMatchFail(..)
  -- ** Record
  , AsRecConError(..)
  , AsRecSelError(..)
  , AsRecUpdError(..)
  -- ** Error Call
  , AsErrorCall(..)
  ) where

import Control.Applicative
import Control.Exception
import Control.Lens
import GHC.Conc (ThreadId)

-- $setup
-- >>> import Data.List
-- >>> import Control.Monad

-- |
-- Traverse the strongly typed 'Exception' contained in 'SomeException' where the type of your function matches
-- the desired 'Exception'.
--
-- @
-- exception :: ('Applicative' f, 'Exception' a, 'Exception' b)
--           => (a -> f b) -> 'SomeException' -> f 'SomeException'
-- @
exception :: Exception a => Prism' SomeException a
exception = prism toException $ \ e -> maybe (Left e) Right $ fromException e
{-# INLINE exception #-}

-- | Catch exceptions that match a given 'Prism' (or any 'Getter', really).
--
-- >>> catching _AssertionFailed (assert False (return "uncaught")) $ \ _ -> return "caught"
-- "caught"
--
-- @
-- 'catching' :: 'Prism'' 'SomeException' a     -> 'IO' r -> (a -> 'IO' r) -> 'IO' r
-- 'catching' :: 'Lens'' 'SomeException' a      -> 'IO' r -> (a -> 'IO' r) -> 'IO' r
-- 'catching' :: 'Traversal'' 'SomeException' a -> 'IO' r -> (a -> 'IO' r) -> 'IO' r
-- 'catching' :: 'Iso'' 'SomeException' a       -> 'IO' r -> (a -> 'IO' r) -> 'IO' r
-- 'catching' :: 'Getter' 'SomeException' a     -> 'IO' r -> (a -> 'IO' r) -> 'IO' r
-- 'catching' :: 'Fold' 'SomeException' a       -> 'IO' r -> (a -> 'IO' r) -> 'IO' r
-- @
catching :: Getting (Leftmost a) SomeException t a b -> IO r -> (a -> IO r) -> IO r
catching l = catchJust (preview l)
{-# INLINE catching #-}

-- | Catch exceptions that match a given 'Prism' (or any 'Getter'), discarding
-- the information about the match. This is particuarly useful when you have
-- a @'Prism'' 'SomeException' ()@ where the result of the prism or fold isn't
-- particularly valuable, just the fact that it matches.
--
-- >>> catching_ _AssertionFailed (assert False (return "uncaught")) $ return "caught"
-- "caught"
--
-- @
-- 'catching_' :: 'Prism'' 'SomeException' a     -> 'IO' r -> 'IO' r -> 'IO' r
-- 'catching_' :: 'Lens'' 'SomeException' a      -> 'IO' r -> 'IO' r -> 'IO' r
-- 'catching_' :: 'Traversal'' 'SomeException' a -> 'IO' r -> 'IO' r -> 'IO' r
-- 'catching_' :: 'Iso'' 'SomeException' a       -> 'IO' r -> 'IO' r -> 'IO' r
-- 'catching_' :: 'Getter' 'SomeException' a     -> 'IO' r -> 'IO' r -> 'IO' r
-- 'catching_' :: 'Fold' 'SomeException' a       -> 'IO' r -> 'IO' r -> 'IO' r
-- @
catching_ :: Getting (Leftmost a) SomeException t a b -> IO r -> IO r -> IO r
catching_ l a b = catchJust (preview l) a (const b)
{-# INLINE catching_ #-}

-- | A version of 'catching' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.
--
-- >>> handling _NonTermination (\_ -> return "caught") $ throwIO NonTermination
-- "caught"
--
-- @
-- 'handling' :: 'Prism'' 'SomeException' a     -> (a -> 'IO' r) -> 'IO' r -> 'IO' r
-- 'handling' :: 'Lens'' 'SomeException' a      -> (a -> 'IO' r) -> 'IO' r -> 'IO' r
-- 'handling' :: 'Traversal'' 'SomeException' a -> (a -> 'IO' r) -> 'IO' r -> 'IO' r
-- 'handling' :: 'Iso'' 'SomeException' a       -> (a -> 'IO' r) -> 'IO' r -> 'IO' r
-- 'handling' :: 'Fold' 'SomeException' a       -> (a -> 'IO' r) -> 'IO' r -> 'IO' r
-- 'handling' :: 'Getter' 'SomeException' a     -> (a -> 'IO' r) -> 'IO' r -> 'IO' r
-- @
handling :: Getting (Leftmost a) SomeException t a b -> (a -> IO r) -> IO r -> IO r
handling l = handleJust (preview l)
{-# INLINE handling #-}

-- | A version of 'catching_' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.
--
-- >>> handling_ _NonTermination (return "caught") $ throwIO NonTermination
-- "caught"
--
-- @
-- 'handling_' :: 'Prism'' 'SomeException' a     -> 'IO' r -> 'IO' r -> 'IO' r
-- 'handling_' :: 'Lens'' 'SomeException' a      -> 'IO' r -> 'IO' r -> 'IO' r
-- 'handling_' :: 'Traversal'' 'SomeException' a -> 'IO' r -> 'IO' r -> 'IO' r
-- 'handling_' :: 'Iso'' 'SomeException' a       -> 'IO' r -> 'IO' r -> 'IO' r
-- 'handling_' :: 'Getter' 'SomeException' a     -> 'IO' r -> 'IO' r -> 'IO' r
-- 'handling_' :: 'Fold' 'SomeException' a       -> 'IO' r -> 'IO' r -> 'IO' r
-- @
handling_ :: Getting (Leftmost a) SomeException t a b -> IO r -> IO r -> IO r
handling_ l b = handling l (const b)
{-# INLINE handling_ #-}

-- |
-- Throw an 'Exception' described by a 'Prism'. Exceptions may be thrown from
-- purely functional code, but may only be caught within the 'IO' monad.
--
-- @'throwing' l ≡ 'reviews' l 'throw'@
--
-- @
-- 'throwing' :: 'Prism'' 'SomeException' t -> t -> a
-- 'throwing' :: 'Iso'' 'SomeException' t   -> t -> a
-- @
throwing :: AReview s SomeException a b -> b -> a
throwing l = reviews l throw
{-# INLINE throwing #-}

-- |
-- A variant of 'throwing' that can only be used within the 'IO' monad.
--
-- @'throwingIO' l ≡ 'reviews' l 'throwIO'@
--
-- Although 'throwingIO' has a type that is a specialization of the type of
-- 'throwing', the two functions are subtly different:
--
-- @
-- throwing l e `seq` x   ≡ throwing e
-- throwingIO l e `seq` x ≡ x
-- @
--
-- The first example will cause the exception @e@ to be raised, whereas the
-- second one won't. In fact, 'throwingIO' will only cause an exception to be
-- raised when it is used within the 'IO' monad. The 'throwingIO' variant should
-- be used in preference to 'throwing' to raise an exception within the 'IO' monad
-- because it guarantees ordering with respect to other 'IO' operations, whereas
-- 'throwing' does not.
--
-- @
-- 'throwingIO' :: 'Prism'' 'SomeException' t -> t -> 'IO' a
-- 'throwingIO' :: 'Iso'' 'SomeException' t   -> t -> 'IO' a
-- @
throwingIO :: AReview s SomeException a b -> b -> IO a
throwingIO l = reviews l throwIO
{-# INLINE throwingIO #-}

-- |
-- 'throwingTo' raises an exception specified by a 'Prism' in the target thread
--
-- @'throwingTo' thread l ≡ 'reviews' l ('throwTo' thread)@
--
--
-- @
-- 'throwingTo' :: 'ThreadId' -> 'Prism'' 'SomeException' t -> t -> 'IO' a
-- 'throwingTo' :: 'ThreadId' -> 'Iso'' 'SomeException' t   -> t -> 'IO' a
-- @
throwingTo :: ThreadId -> AReview s SomeException a b -> b -> IO ()
throwingTo tid l = reviews l (throwTo tid)
{-# INLINE throwingTo #-}

----------------------------------------------------------------------------
-- IOException
----------------------------------------------------------------------------

-- | Exceptions that occur in the IO monad. An IOException records a more
-- specific error type, a descriptive string and maybe the handle that was
-- used when the error was flagged.
--
-- Due to their richer structure relative to other exceptions, these have
-- a more carefully overloaded signature.
class AsIOException p f t where
  -- | Unfortunately the name 'ioException' is taken by @base@ for
  -- throwing IOExceptions.
  --
  -- @
  -- '_IOException' :: 'Equality'' 'IOException'   'IOException'
  -- '_IOException' :: 'Prism'' 'SomeException' 'IOException'
  -- @
  --
  -- Many combinators for working with an 'IOException' are available
  -- in "System.IO.Error.Lens".
  _IOException :: Overloaded' p f t IOException

instance AsIOException p f IOException where
  _IOException = id
  {-# INLINE _IOException #-}

instance (Choice p, Applicative f) => AsIOException p f SomeException where
  _IOException = exception
  {-# INLINE _IOException #-}

----------------------------------------------------------------------------
-- ArithException
----------------------------------------------------------------------------

-- | Arithmetic exceptions.
class AsArithException p f t where
  -- '_ArithException' :: 'Equality'' 'ArithException' 'ArithException'
  -- '_ArithException' :: 'Prism''    'SomeException'  'ArithException'
  _ArithException :: Overloaded' p f t ArithException

instance AsArithException p f ArithException where
  _ArithException = id
  {-# INLINE _ArithException #-}

instance (Choice p, Applicative f) => AsArithException p f SomeException where
  _ArithException = exception
  {-# INLINE _ArithException #-}

-- | Handle arithmetic '_Overflow'.
--
-- @'_Overflow' ≡ '_ArithException' . '_Overflow'@
--
-- @
-- '_Overflow' :: 'Prism'' 'ArithException' 'ArithException'
-- '_Overflow' :: 'Prism'' 'SomeException'  'ArithException'
-- @
_Overflow :: (AsArithException p f t, Choice p, Applicative f) => Overloaded' p f t ()
_Overflow = _ArithException . dimap seta (either id id) . right' . rmap (Overflow <$) where
  seta Overflow = Right ()
  seta t        = Left  (pure t)
{-# INLINE _Overflow #-}

-- | Handle arithmetic '_Underflow'.
--
-- @'_Underflow' ≡ '_ArithException' . '_Underflow'@
--
-- @
-- '_Underflow' :: 'Prism'' 'ArithException' 'ArithException'
-- '_Underflow' :: 'Prism'' 'SomeException'  'ArithException'
-- @
_Underflow :: (AsArithException p f t, Choice p, Applicative f) => Overloaded' p f t ()
_Underflow = _ArithException . dimap seta (either id id) . right' . rmap (Underflow <$) where
  seta Underflow = Right ()
  seta t        = Left  (pure t)
{-# INLINE _Underflow #-}

-- | Handle arithmetic loss of precision.
--
-- @'_LossOfPrecision' ≡ '_ArithException' . '_LossOfPrecision'@
--
-- @
-- '_LossOfPrecision' :: 'Prism'' 'ArithException' 'ArithException'
-- '_LossOfPrecision' :: 'Prism'' 'SomeException'  'ArithException'
-- @
_LossOfPrecision :: (AsArithException p f t, Choice p, Applicative f) => Overloaded' p f t ()
_LossOfPrecision = _ArithException . dimap seta (either id id) . right' . rmap (LossOfPrecision <$) where
  seta LossOfPrecision = Right ()
  seta t        = Left  (pure t)
{-# INLINE _LossOfPrecision #-}

-- | Handle division by zero.
--
-- @'_DivideByZero' ≡ '_ArithException' . '_DivideByZero'@
--
-- @
-- '_DivideByZero' :: 'Prism'' 'ArithException' 'ArithException'
-- '_DivideByZero' :: 'Prism'' 'SomeException'  'ArithException'
-- @
_DivideByZero :: (AsArithException p f t, Choice p, Applicative f) => Overloaded' p f t ()
_DivideByZero = _ArithException . dimap seta (either id id) . right' . rmap (DivideByZero <$) where
  seta DivideByZero = Right ()
  seta t        = Left  (pure t)
{-# INLINE _DivideByZero #-}

-- | Handle exceptional _Denormalized floating point.
--
-- @'_Denormal' ≡ '_ArithException' . '_Denormal'@
--
-- @
-- '_Denormal' :: 'Prism'' 'ArithException' 'ArithException'
-- '_Denormal' :: 'Prism'' 'SomeException'  'ArithException'
-- @
_Denormal :: (AsArithException p f t, Choice p, Applicative f) => Overloaded' p f t ()
_Denormal = _ArithException . dimap seta (either id id) . right' . rmap (Denormal <$) where
  seta Denormal = Right ()
  seta t        = Left  (pure t)
{-# INLINE _Denormal #-}

#if MIN_VERSION_base(4,6,0)
-- | Added in @base@ 4.6 in response to this libraries discussion:
--
-- <http://haskell.1045720.n5.nabble.com/Data-Ratio-and-exceptions-td5711246.html>
--
-- @'_RatioZeroDenominator' ≡ '_ArithException' . '_RatioZeroDenominator'@
--
-- @
-- '_RatioZeroDenominator' :: 'Prism'' 'ArithException' 'ArithException'
-- '_RatioZeroDenominator' :: 'Prism'' 'SomeException'  'ArithException'
-- @
_RatioZeroDenominator :: (AsArithException p f t, Choice p, Applicative f) => Overloaded' p f t ()
_RatioZeroDenominator = _ArithException . dimap seta (either id id) . right' . rmap (RatioZeroDenominator <$) where
  seta RatioZeroDenominator = Right ()
  seta t        = Left  (pure t)
{-# INLINE _RatioZeroDenominator #-}

#endif

----------------------------------------------------------------------------
-- ArrayException
----------------------------------------------------------------------------

-- | Exceptions generated by array operations
class AsArrayException p f t where
  -- | Extract information about an array exception.
  --
  -- @
  -- '_ArrayException' :: 'Equality'' 'ArrayException' 'ArrayException'
  -- '_ArrayException' :: 'Prism''    'SomeException'  'ArrayException'
  -- @
  _ArrayException :: Overloaded' p f t ArrayException

instance AsArrayException p f ArrayException where
  _ArrayException = id
  {-# INLINE _ArrayException #-}

instance (Choice p, Applicative f) => AsArrayException p f SomeException where
  _ArrayException = exception
  {-# INLINE _ArrayException #-}

-- | An attempt was made to index an array outside its declared bounds.
--
-- @'_IndexOutOfBounds' ≡ '_ArrayException' . '_IndexOutOfBounds'@
--
-- @
-- '_IndexOutOfBounds' :: 'Prism'' 'ArrayException' 'String'
-- '_IndexOutOfBounds' :: 'Prism'' 'SomeException'  'String'
-- @
_IndexOutOfBounds :: (AsArrayException p f t, Choice p, Applicative f) => Overloaded' p f t String
_IndexOutOfBounds = _ArrayException . dimap seta (either id id) . right' . rmap (fmap IndexOutOfBounds) where
  seta (IndexOutOfBounds r) = Right r
  seta t                    = Left  (pure t)
{-# INLINE _IndexOutOfBounds #-}

-- | An attempt was made to evaluate an element of an array that had not been initialized.
--
-- @'_UndefinedElement' ≡ '_ArrayException' . '_UndefinedElement'@
--
-- @
-- '_UndefinedElement' :: 'Prism'' 'ArrayException' 'String'
-- '_UndefinedElement' :: 'Prism'' 'SomeException'  'String'
-- @
_UndefinedElement :: (AsArrayException p f t, Choice p, Applicative f) => Overloaded' p f t String
_UndefinedElement = _ArrayException . dimap seta (either id id) . right' . rmap (fmap UndefinedElement) where
  seta (UndefinedElement r) = Right r
  seta t                    = Left  (pure t)
{-# INLINE _UndefinedElement #-}

----------------------------------------------------------------------------
-- AssertionFailed
----------------------------------------------------------------------------

-- | 'assert' was applied to 'False'.
class AsAssertionFailed p f t where
  -- |
  -- >>> handling _AssertionFailed (\ xs -> "caught" <$ guard ("<interactive>" `isInfixOf` xs) ) $ assert False (return "uncaught")
  -- "caught"
  --
  -- @
  -- '_AssertionFailed' :: 'Iso''   'AssertionFailed' 'String'
  -- '_AssertionFailed' :: 'Prism'' 'SomeException'   'String'
  -- @
  _AssertionFailed :: Overloaded' p f t String

instance (Profunctor p, Functor f) => AsAssertionFailed p f AssertionFailed where
  _AssertionFailed = unwrapping AssertionFailed
  {-# INLINE _AssertionFailed #-}

instance (Choice p, Applicative f) => AsAssertionFailed p f SomeException where
  _AssertionFailed = exception.unwrapping AssertionFailed
  {-# INLINE _AssertionFailed #-}

----------------------------------------------------------------------------
-- AsyncException
----------------------------------------------------------------------------

-- | Asynchronous exceptions.
class AsAsyncException p f t where
  -- |
  -- @
  -- '_AsyncException' :: 'Equality'' 'AsyncException' 'AsyncException'
  -- '_AsyncException' :: 'Prism''    'SomeException'  'AsyncException'
  -- @
  _AsyncException :: Overloaded' p f t AsyncException

instance AsAsyncException p f AsyncException where
  _AsyncException = id
  {-# INLINE _AsyncException #-}

instance (Choice p, Applicative f) => AsAsyncException p f SomeException where
  _AsyncException = exception
  {-# INLINE _AsyncException #-}

-- | The current thread's stack exceeded its limit. Since an exception has been
-- raised, the thread's stack will certainly be below its limit again, but the
-- programmer should take remedial action immediately.
--
-- @
-- '_StackOverflow' :: 'Prism'' 'AsyncException' ()
-- '_StackOverflow' :: 'Prism'' 'SomeException'  ()
-- @
_StackOverflow :: (AsAsyncException p f t, Choice p, Applicative f) => Overloaded' p f t ()
_StackOverflow = _AsyncException . dimap seta (either id id) . right' . rmap (StackOverflow <$) where
  seta StackOverflow = Right ()
  seta t             = Left  (pure t)
{-# INLINE _StackOverflow #-}

-- | The program's heap is reaching its limit, and the program should take action
-- to reduce the amount of live data it has.
--
-- Notes:
--
-- * It is undefined which thread receives this exception.
--
-- * GHC currently does not throw 'HeapOverflow' exceptions.
--
-- @
-- '_HeapOverflow' :: 'Prism'' 'AsyncException' ()
-- '_HeapOverflow' :: 'Prism'' 'SomeException'  ()
-- @
_HeapOverflow :: (AsAsyncException p f t, Choice p, Applicative f) => Overloaded' p f t ()
_HeapOverflow = _AsyncException . dimap seta (either id id) . right' . rmap (HeapOverflow <$) where
  seta HeapOverflow = Right ()
  seta t             = Left  (pure t)
{-# INLINE _HeapOverflow #-}

-- | This exception is raised by another thread calling 'killThread', or by the
-- system if it needs to terminate the thread for some reason.
--
-- @
-- '_ThreadKilled' :: 'Prism'' 'AsyncException' ()
-- '_ThreadKilled' :: 'Prism'' 'SomeException'  ()
-- @
_ThreadKilled :: (AsAsyncException p f t, Choice p, Applicative f) => Overloaded' p f t ()
_ThreadKilled = _AsyncException . dimap seta (either id id) . right' . rmap (ThreadKilled <$) where
  seta ThreadKilled = Right ()
  seta t             = Left  (pure t)
{-# INLINE _ThreadKilled #-}

-- | This exception is raised by default in the main thread of the program when
-- the user requests to terminate the program via the usual mechanism(s)
-- (/e.g./ Control-C in the console).
--
-- @
-- '_UserInterrupt' :: 'Prism'' 'AsyncException' ()
-- '_UserInterrupt' :: 'Prism'' 'SomeException'  ()
-- @
_UserInterrupt :: (AsAsyncException p f t, Choice p, Applicative f) => Overloaded' p f t ()
_UserInterrupt = _AsyncException . dimap seta (either id id) . right' . rmap (UserInterrupt <$) where
  seta UserInterrupt = Right ()
  seta t             = Left  (pure t)
{-# INLINE _UserInterrupt #-}

----------------------------------------------------------------------------
-- AsyncException
----------------------------------------------------------------------------

class (Profunctor p, Functor f) => AsNonTermination p f t where
  -- | Thrown when the runtime system detects that the computation is guaranteed
  -- not to terminate. Note that there is no guarantee that the runtime system
  -- will notice whether any given computation is guaranteed to terminate or not.
  _NonTermination :: Overloaded' p f t ()

instance (Profunctor p, Functor f) => AsNonTermination p f NonTermination where
  _NonTermination = trivial NonTermination
  {-# INLINE _NonTermination #-}

instance (Choice p, Applicative f) => AsNonTermination p f SomeException where
  _NonTermination = exception.trivial NonTermination
  {-# INLINE _NonTermination #-}

----------------------------------------------------------------------------
-- NestedAtomically
----------------------------------------------------------------------------

class (Profunctor p, Functor f) => AsNestedAtomically p f t where
  -- | Thrown when the program attempts to call atomically, from the stm package,
  -- inside another call to atomically.
  _NestedAtomically :: Overloaded' p f t ()

instance (Profunctor p, Functor f) => AsNestedAtomically p f NestedAtomically where
  _NestedAtomically = trivial NestedAtomically
  {-# INLINE _NestedAtomically #-}

instance (Choice p, Applicative f) => AsNestedAtomically p f SomeException where
  _NestedAtomically = exception.trivial NestedAtomically
  {-# INLINE _NestedAtomically #-}

----------------------------------------------------------------------------
-- BlockedIndefinitelyOnMVar
----------------------------------------------------------------------------

class (Profunctor p, Functor f) => AsBlockedIndefinitelyOnMVar p f t where
  -- | The thread is blocked on an MVar, but there are no other references
  -- to the MVar so it can't ever continue.
  _BlockedIndefinitelyOnMVar :: Overloaded' p f t ()

instance (Profunctor p, Functor f) => AsBlockedIndefinitelyOnMVar p f BlockedIndefinitelyOnMVar where
  _BlockedIndefinitelyOnMVar = trivial BlockedIndefinitelyOnMVar
  {-# INLINE _BlockedIndefinitelyOnMVar #-}

instance (Choice p, Applicative f) => AsBlockedIndefinitelyOnMVar p f SomeException where
  _BlockedIndefinitelyOnMVar = exception.trivial BlockedIndefinitelyOnMVar
  {-# INLINE _BlockedIndefinitelyOnMVar #-}

----------------------------------------------------------------------------
-- BlockedIndefinitelyOnSTM
----------------------------------------------------------------------------

class (Profunctor p, Functor f) => AsBlockedIndefinitelyOnSTM p f t where
  -- | The thread is waiting to retry an STM transaction, but there are no
  -- other references to any TVars involved, so it can't ever continue.
  _BlockedIndefinitelyOnSTM :: Overloaded' p f t ()

instance (Profunctor p, Functor f) => AsBlockedIndefinitelyOnSTM p f BlockedIndefinitelyOnSTM where
  _BlockedIndefinitelyOnSTM = trivial BlockedIndefinitelyOnSTM
  {-# INLINE _BlockedIndefinitelyOnSTM #-}

instance (Choice p, Applicative f) => AsBlockedIndefinitelyOnSTM p f SomeException where
  _BlockedIndefinitelyOnSTM = exception.trivial BlockedIndefinitelyOnSTM
  {-# INLINE _BlockedIndefinitelyOnSTM #-}

----------------------------------------------------------------------------
-- Deadlock
----------------------------------------------------------------------------

class (Profunctor p, Functor f) => AsDeadlock p f t where
  -- | There are no runnable threads, so the program is deadlocked. The 'Deadlock' exception
  -- is raised in the main thread only.
  _Deadlock :: Overloaded' p f t ()

instance (Profunctor p, Functor f) => AsDeadlock p f Deadlock where
  _Deadlock = trivial Deadlock
  {-# INLINE _Deadlock #-}

instance (Choice p, Applicative f) => AsDeadlock p f SomeException where
  _Deadlock = exception.trivial Deadlock
  {-# INLINE _Deadlock #-}

----------------------------------------------------------------------------
-- NoMethodError
----------------------------------------------------------------------------

class (Profunctor p, Functor f) => AsNoMethodError p f t where
  -- | A class method without a definition (neither a default definition,
  -- nor a definition in the appropriate instance) was called.
  _NoMethodError :: Overloaded' p f t String

instance (Profunctor p, Functor f) => AsNoMethodError p f NoMethodError where
  _NoMethodError = unwrapping NoMethodError
  {-# INLINE _NoMethodError #-}

instance (Choice p, Applicative f) => AsNoMethodError p f SomeException where
  _NoMethodError = exception.unwrapping NoMethodError
  {-# INLINE _NoMethodError #-}

----------------------------------------------------------------------------
-- PatternMatchFail
----------------------------------------------------------------------------

class (Profunctor p, Functor f) => AsPatternMatchFail p f t where
  -- | A pattern match failed.
  --
  -- Information about the source location of the pattern
  _PatternMatchFail :: Overloaded' p f t String

instance (Profunctor p, Functor f) => AsPatternMatchFail p f PatternMatchFail where
  _PatternMatchFail = unwrapping PatternMatchFail
  {-# INLINE _PatternMatchFail #-}

instance (Choice p, Applicative f) => AsPatternMatchFail p f SomeException where
  _PatternMatchFail = exception.unwrapping PatternMatchFail
  {-# INLINE _PatternMatchFail #-}

----------------------------------------------------------------------------
-- RecConError
----------------------------------------------------------------------------

class (Profunctor p, Functor f) => AsRecConError p f t where
  -- | An uninitialised record field was used.
  --
  -- Information about the source location where the record was constructed
  _RecConError :: Overloaded' p f t String

instance (Profunctor p, Functor f) => AsRecConError p f RecConError where
  _RecConError = unwrapping RecConError
  {-# INLINE _RecConError #-}

instance (Choice p, Applicative f) => AsRecConError p f SomeException where
  _RecConError = exception.unwrapping RecConError
  {-# INLINE _RecConError #-}

----------------------------------------------------------------------------
-- RecSelError
----------------------------------------------------------------------------

-- | A record selector was applied to a constructor without the appropriate
-- field. This can only happen with a datatype with multiple constructors,
-- where some fields are in one constructor but not another.
class (Profunctor p, Functor f) => AsRecSelError p f t where
  -- | Information about the source location where the record selection occurred
  _RecSelError :: Overloaded' p f t String

instance (Profunctor p, Functor f) => AsRecSelError p f RecSelError where
  _RecSelError = unwrapping RecSelError
  {-# INLINE _RecSelError #-}

instance (Choice p, Applicative f) => AsRecSelError p f SomeException where
  _RecSelError = exception.unwrapping RecSelError
  {-# INLINE _RecSelError #-}

----------------------------------------------------------------------------
-- RecUpdError
----------------------------------------------------------------------------

-- | A record update was performed on a constructor without the
-- appropriate field. This can only happen with a datatype with multiple
-- constructors, where some fields are in one constructor but not another.
class (Profunctor p, Functor f) => AsRecUpdError p f t where
  -- | Information about the source location where the record was updated
  _RecUpdError :: Overloaded' p f t String

instance (Profunctor p, Functor f) => AsRecUpdError p f RecUpdError where
  _RecUpdError = unwrapping RecUpdError
  {-# INLINE _RecUpdError #-}

instance (Choice p, Applicative f) => AsRecUpdError p f SomeException where
  _RecUpdError = exception.unwrapping RecUpdError
  {-# INLINE _RecUpdError #-}

----------------------------------------------------------------------------
-- ErrorCall
----------------------------------------------------------------------------

-- | This is thrown when the user calls 'error'.
class (Profunctor p, Functor f) => AsErrorCall p f t where
  -- | Retrieve the argument given to 'error'.
  --
  -- 'ErrorCall' is isomorphic to a 'String'
  --
  -- >>> catching _ErrorCall (error "touch down!") return
  -- "touch down!"
  _ErrorCall :: Overloaded' p f t String

instance (Profunctor p, Functor f) => AsErrorCall p f ErrorCall where
  _ErrorCall = unwrapping ErrorCall
  {-# INLINE _ErrorCall #-}

instance (Choice p, Applicative f) => AsErrorCall p f SomeException where
  _ErrorCall = exception.unwrapping ErrorCall
  {-# INLINE _ErrorCall #-}

------------------------------------------------------------------------------
-- Helper Functions
------------------------------------------------------------------------------

trivial :: t -> Iso' t ()
trivial t = const () `iso` const t
