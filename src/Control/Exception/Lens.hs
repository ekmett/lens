{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_base
#defined MIN_VERSION_base(x,y,z) 1
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
  , overflow
  , underflow
  , lossOfPrecision
  , divideByZero
  , denormal
#if MIN_VERSION_base(4,6,0)
  , ratioZeroDenominator
#endif
  -- ** Array Exceptions
  , AsArrayException(..)
  , indexOutOfBounds
  , undefinedElement
  -- ** Assertion Failed
  , AsAssertionFailed(..)
  , _assertionFailed
  -- ** Async Exceptions
  , AsAsyncException(..)
  , stackOverflow
  , heapOverflow
  , threadKilled
  , userInterrupt
  -- ** Non-Termination
  , AsNonTermination(..)
  , _nonTermination
  -- ** Nested Atomically
  , AsNestedAtomically(..)
  , _nestedAtomically
  -- ** Blocked Indefinitely
  -- *** on MVar
  , AsBlockedIndefinitelyOnMVar(..)
  , _blockedIndefinitelyOnMVar
  -- *** on STM
  , AsBlockedIndefinitelyOnSTM(..)
  , _blockedIndefinitelyOnSTM
  -- ** Deadlock
  , AsDeadlock(..)
  , _deadlock
  -- ** No Such Method
  , AsNoMethodError(..)
  , _noMethodError
  -- ** Pattern Match Failure
  , AsPatternMatchFail(..)
  , _patternMatchFail
  -- ** Record
  -- *** Constructor Error
  , AsRecConError(..)
  , _recConError
  -- *** Selection Error
  , AsRecSelError(..)
  , _recSelError
  -- *** Update Error
  , AsRecUpdError(..)
  , _recUpdError
  -- ** Error Call
  , AsErrorCall(..)
  , _errorCall
  ) where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Lens.Internal
import Data.Monoid
import Data.Profunctor
import GHC.Conc (ThreadId)

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

-- |
-- @
-- 'catching' :: 'Prism'' 'SomeException' a     -> 'IO' r -> (a -> 'IO' r) -> 'IO' r
-- 'catching' :: 'Lens'' 'SomeException' a      -> 'IO' r -> (a -> 'IO' r) -> 'IO' r
-- 'catching' :: 'Traversal'' 'SomeException' a -> 'IO' r -> (a -> 'IO' r) -> 'IO' r
-- 'catching' :: 'Iso'' 'SomeException' a       -> 'IO' r -> (a -> 'IO' r) -> 'IO' r
-- 'catching' :: 'Getter' 'SomeException' a     -> 'IO' r -> (a -> 'IO' r) -> 'IO' r
-- 'catching' :: 'Fold' 'SomeException' a       -> 'IO' r -> (a -> 'IO' r) -> 'IO' r
-- @
catching :: Getting (First a) SomeException t a b -> IO r -> (a -> IO r) -> IO r
catching l = catchJust (preview l)
{-# INLINE catching #-}

-- |
-- @
-- 'catching_' :: 'Prism'' 'SomeException' a     -> 'IO' r -> 'IO' r -> 'IO' r
-- 'catching_' :: 'Lens'' 'SomeException' a      -> 'IO' r -> 'IO' r -> 'IO' r
-- 'catching_' :: 'Traversal'' 'SomeException' a -> 'IO' r -> 'IO' r -> 'IO' r
-- 'catching_' :: 'Iso'' 'SomeException' a       -> 'IO' r -> 'IO' r -> 'IO' r
-- 'catching_' :: 'Getter' 'SomeException' a     -> 'IO' r -> 'IO' r -> 'IO' r
-- 'catching_' :: 'Fold' 'SomeException' a       -> 'IO' r -> 'IO' r -> 'IO' r
-- @
catching_ :: Getting (First a) SomeException t a b -> IO r -> IO r -> IO r
catching_ l a b = catchJust (preview l) a (const b)
{-# INLINE catching_ #-}

-- |
--
-- @
-- 'handling' :: 'Prism'' 'SomeException' a     -> (a -> 'IO' r) -> 'IO' r -> 'IO' r
-- 'handling' :: 'Lens'' 'SomeException' a      -> (a -> 'IO' r) -> 'IO' r -> 'IO' r
-- 'handling' :: 'Traversal'' 'SomeException' a -> (a -> 'IO' r) -> 'IO' r -> 'IO' r
-- 'handling' :: 'Iso'' 'SomeException' a       -> (a -> 'IO' r) -> 'IO' r -> 'IO' r
-- 'handling' :: 'Fold' 'SomeException' a       -> (a -> 'IO' r) -> 'IO' r -> 'IO' r
-- 'handling' :: 'Getter' 'SomeException' a     -> (a -> 'IO' r) -> 'IO' r -> 'IO' r
-- @
handling :: Getting (First a) SomeException t a b -> (a -> IO r) -> IO r -> IO r
handling l = handleJust (preview l)
{-# INLINE handling #-}

-- |
-- >>> handling_ nonTermination (return "caught") $ throwIO NonTermination
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
handling_ :: Getting (First a) SomeException t a b -> IO r -> IO r -> IO r
handling_ l b = handling l (const b)
{-# INLINE handling_ #-}

-- |
-- Throw an 'Exception' described by a 'Prism'. Exceptions may be thrown from
-- purely functional code, but may only be caught within the 'IO' monad.
--
-- @'throwing' l ≡ 'reviews' l 'throw'@
--
-- >>> throwing _nonTermination ()
-- *** Exception: <<loop>>
--
-- @
-- 'throwing' :: 'Prism'' 'SomeException' t -> t -> a
-- 'throwing' :: 'Iso'' 'SomeException' t   -> t -> a
-- @
throwing :: Reviewing' SomeException t -> t -> a
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
throwingIO :: Reviewing' SomeException t -> t -> IO a
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
throwingTo :: ThreadId -> Reviewing' SomeException t -> t -> IO ()
throwingTo tid l = reviews l (throwTo tid)
{-# INLINE throwingTo #-}

----------------------------------------------------------------------------
-- IOException
----------------------------------------------------------------------------

-- Exceptions that occur in the IO monad. An IOException records a more specific error type, a descriptive string and maybe the handle that was used when the error was flagged.
class AsIOException p f t where
  ioException :: Overloaded' p f t IOException

-- | @'ioException' :: 'Equality'' 'IOException' 'IOException'@
instance AsIOException p f IOException where
  ioException = id
  {-# INLINE ioException #-}

-- | @'ioException' :: 'Prism'' 'SomeException' 'IOException'@
instance (Prismatic p, Applicative f) => AsIOException p f SomeException where
  ioException = exception
  {-# INLINE ioException #-}

----------------------------------------------------------------------------
-- ArithException
----------------------------------------------------------------------------

-- | Arithmetic exceptions.
class AsArithException p f t where
  -- |
  -- @
  -- 'arithException' :: 'Equality'' 'ArithException' 'ArithException'
  -- 'arithException' :: 'Prism''    'SomeException' 'ArithException'
  -- @
  arithException :: Overloaded' p f t ArithException

-- | @'arithException' :: 'Equality'' 'ArithException' 'ArithException'@
instance AsArithException p f ArithException where
  arithException = id
  {-# INLINE arithException #-}

-- | @'arithException' :: 'Prism'' 'SomeException' 'ArithException'@
instance (Prismatic p, Applicative f) => AsArithException p f SomeException where
  arithException = exception
  {-# INLINE arithException #-}

-- |
-- @'overflow' ≡ 'arithException' . 'overflow'@
--
-- @
-- 'overflow' :: 'Prism'' 'SomeException' 'ArithException'
-- 'overflow' :: 'Prism'' 'SomeException' 'ArithException'
-- @
overflow :: AsArithException (Market' ArithException) Mutator t => Prism' t ()
overflow = case runPrism arithException of
  (bt, seta) | bto <- bt Overflow -> prism (const bto) $ \s -> case seta s of
    Left t -> Left t
    Right Overflow -> Right ()
    Right a -> Left (bt a)
{-# INLINE overflow #-}

-- |
-- @'underflow' ≡ 'arithException' . 'underflow'@
--
-- @
-- 'underflow' :: 'Prism'' 'SomeException' 'ArithException'
-- 'underflow' :: 'Prism'' 'SomeException' 'ArithException'
-- @
underflow :: AsArithException (Market' ArithException) Mutator t => Prism' t ()
underflow = case runPrism arithException of
  (bt, seta) | btu <- bt Underflow -> prism (const btu) $ \s -> case seta s of
    Left t -> Left t
    Right Underflow -> Right ()
    Right a -> Left (bt a)
{-# INLINE underflow #-}

-- |
-- @'lossOfPrecision' ≡ 'arithException' . 'lossOfPrecision'@
--
-- @
-- 'lossOfPrecision' :: 'Prism'' 'ArithException' 'ArithException'
-- 'lossOfPrecision' :: 'Prism'' 'SomeException' 'ArithException'
-- @
lossOfPrecision :: AsArithException (Market' ArithException) Mutator t => Prism' t ()
lossOfPrecision = case runPrism arithException of
  (bt, seta) | btu <- bt LossOfPrecision -> prism (const btu) $ \s -> case seta s of
    Left t -> Left t
    Right LossOfPrecision -> Right ()
    Right a -> Left (bt a)
{-# INLINE lossOfPrecision #-}

-- |
-- @'divideByZero' ≡ 'arithException' . 'divideByZero'@
--
-- @
-- 'divideByZero' :: 'Prism'' 'ArithException' 'ArithException'
-- 'divideByZero' :: 'Prism'' 'SomeException' 'ArithException'
-- @
divideByZero :: AsArithException (Market' ArithException) Mutator t => Prism' t ()
divideByZero = case runPrism arithException of
  (bt, seta) | btu <- bt DivideByZero -> prism (const btu) $ \s -> case seta s of
    Left t -> Left t
    Right DivideByZero -> Right ()
    Right a -> Left (bt a)
{-# INLINE divideByZero #-}

-- |
-- @'denormal' ≡ 'arithException' . 'denormal'@
--
-- @
-- 'denormal' :: 'Prism'' 'ArithException' 'ArithException'
-- 'denormal' :: 'Prism'' 'SomeException' 'ArithException'
-- @
denormal :: AsArithException (Market' ArithException) Mutator t => Prism' t ()
denormal = case runPrism arithException of
  (bt, seta) | btu <- bt Denormal -> prism (const btu) $ \s -> case seta s of
    Left t -> Left t
    Right Denormal -> Right ()
    Right a -> Left (bt a)
{-# INLINE denormal #-}

#if MIN_VERSION_base(4,6,0)
-- |
-- @'ratioZeroDenominator' ≡ 'arithException' . 'ratioZeroDenominator'@
--
-- @
-- 'ratioZeroDenominator' :: 'Prism'' 'ArithException' 'ArithException'
-- 'ratioZeroDenominator' :: 'Prism'' 'SomeException' 'ArithException'
-- @
ratioZeroDenominator :: AsArithException (Market' ArithException) Mutator t => Prism' t ()
ratioZeroDenominator = case runPrism arithException of
  (bt, seta) | btu <- bt RatioZeroDenominator -> prism (const btu) $ \s -> case seta s of
    Left t -> Left t
    Right RatioZeroDenominator -> Right ()
    Right a -> Left (bt a)
{-# INLINE ratioZeroDenominator #-}
#endif

----------------------------------------------------------------------------
-- ArrayException
----------------------------------------------------------------------------

-- | Exceptions generated by array operations
class AsArrayException p f t where
  -- |
  -- @
  -- 'arrayException' :: 'Equality'' 'ArrayException' 'ArrayException'
  -- 'arrayException' :: 'Prism''    'SomeException'  'ArrayException'
  -- @
  arrayException :: Overloaded' p f t ArrayException

-- | @'arrayException' :: 'Equality'' 'ArrayException' 'ArrayException'@
instance AsArrayException p f ArrayException where
  arrayException = id
  {-# INLINE arrayException #-}

-- | @'arrayException' :: 'Prism'' 'SomeException' 'ArrayException'@
instance (Prismatic p, Applicative f) => AsArrayException p f SomeException where
  arrayException = exception
  {-# INLINE arrayException #-}

-- | An attempt was made to index an array outside its declared bounds.
--
-- @'indexOutOfBounds' ≡ 'arrayException' . 'indexOutOfBounds'@
indexOutOfBounds :: AsArrayException (Market' ArrayException) Mutator t => Prism' t String
indexOutOfBounds = case runPrism arrayException of
  (bt, seta) -> prism (bt . IndexOutOfBounds) $ \s -> case seta s of
    Left t -> Left t
    Right (IndexOutOfBounds r) -> Right r
    Right a -> Left (bt a)
{-# INLINE indexOutOfBounds #-}

-- | An attempt was made to evaluate an element of an array that had not been initialized.
--
-- @'undefinedElement' ≡ 'arrayException' . 'undefinedElement'@
undefinedElement :: AsArrayException (Market' ArrayException) Mutator t => Prism' t String
undefinedElement = case runPrism arrayException of
  (bt, seta) -> prism (bt . UndefinedElement) $ \s -> case seta s of
    Left t -> Left t
    Right (UndefinedElement r) -> Right r
    Right a -> Left (bt a)
{-# INLINE undefinedElement #-}

----------------------------------------------------------------------------
-- AssertionFailed
----------------------------------------------------------------------------

-- | 'assert' was applied to 'False'.
class AsAssertionFailed p f t where
  -- |
  -- @
  -- 'assertionFailed' :: 'Equality'' 'AssertionFailed' 'AssertionFailed'
  -- 'assertionFailed' :: 'Prism''    'SomeException'   'AssertionFailed'
  -- @
  assertionFailed :: Overloaded' p f t AssertionFailed

-- | Retrieve the text of a failed assertion
--
-- 'AssertionFailed' is isomorphic to a 'String'
--
-- @
-- '_assertionFailed' :: 'Iso'' 'AssertionFailed' 'String'@
-- '_assertionFailed' :: 'Prism'' 'SomeException' 'String'@
-- @
_assertionFailed :: (AsAssertionFailed p f t, Profunctor p, Functor f) => Overloaded' p f t String
_assertionFailed = assertionFailed . unwrapped
{-# INLINE _assertionFailed #-}

-- | @'assertionFailed' :: 'Equality'' 'AssertionFailed' 'AssertionFailed'@
instance AsAssertionFailed p f AssertionFailed where
  assertionFailed = id
  {-# INLINE assertionFailed #-}

-- | @'assertionFailed' :: 'Prism'' 'SomeException' 'AssertionFailed'@
instance (Prismatic p, Applicative f) => AsAssertionFailed p f SomeException where
  assertionFailed = exception
  {-# INLINE assertionFailed #-}

----------------------------------------------------------------------------
-- AsyncException
----------------------------------------------------------------------------

-- | Asynchronous exceptions.
class AsAsyncException p f t where
  -- @
  -- 'asyncException' :: 'Equality'' 'AsyncException' 'AsyncException'
  -- 'asyncException' :: 'Prism''    'SomeException'  'AsyncException'
  -- @
  asyncException :: Overloaded' p f t AsyncException

-- | @'asyncException' :: 'Equality'' 'AsyncException' 'AsyncException'@
instance AsAsyncException p f AsyncException where
  asyncException = id
  {-# INLINE asyncException #-}

-- | @'asyncException' :: 'Prism'' 'SomeException' 'AsyncException'@
instance (Prismatic p, Applicative f) => AsAsyncException p f SomeException where
  asyncException = exception
  {-# INLINE asyncException #-}

-- | The current thread's stack exceeded its limit. Since an exception has been
-- raised, the thread's stack will certainly be below its limit again, but the
-- programmer should take remedial action immediately.
--
-- @
-- 'stackOverflow' :: 'Prism'' 'AssertionFailed' 'String'@
-- 'stackOverflow' :: 'Prism'' 'SomeException'   'String'@
-- @
stackOverflow :: AsAsyncException (Market' AsyncException) Mutator t => Prism' t ()
stackOverflow = case runPrism asyncException of
  (bt, seta) | btu <- bt StackOverflow -> prism (const btu) $ \s -> case seta s of
    Left t -> Left t
    Right StackOverflow -> Right ()
    Right a -> Left (bt a)
{-# INLINE stackOverflow #-}

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
-- 'heapOverflow' :: 'Prism'' 'AssertionFailed' 'String'@
-- 'heapOverflow' :: 'Prism'' 'SomeException'   'String'@
-- @
heapOverflow :: AsAsyncException (Market' AsyncException) Mutator t => Prism' t ()
heapOverflow = case runPrism asyncException of
  (bt, seta) | btu <- bt HeapOverflow -> prism (const btu) $ \s -> case seta s of
    Left t -> Left t
    Right HeapOverflow -> Right ()
    Right a -> Left (bt a)
{-# INLINE heapOverflow #-}

-- | This exception is raised by another thread calling 'killThread', or by the
-- system if it needs to terminate the thread for some reason.
--
-- @
-- 'threadKilled' :: 'Prism'' 'AssertionFailed' 'String'@
-- 'threadKilled' :: 'Prism'' 'SomeException'   'String'@
-- @
threadKilled :: AsAsyncException (Market' AsyncException) Mutator t => Prism' t ()
threadKilled = case runPrism asyncException of
  (bt, seta) | btu <- bt ThreadKilled -> prism (const btu) $ \s -> case seta s of
    Left t -> Left t
    Right ThreadKilled -> Right ()
    Right a -> Left (bt a)
{-# INLINE threadKilled #-}

-- | This exception is raised by default in the main thread of the program when
-- the user requests to terminate the program via the usual mechanism(s)
-- (/e.g./ Control-C in the console).
--
-- @
-- 'userInterrupt' :: 'Prism'' 'AssertionFailed' 'String'@
-- 'userInterrupt' :: 'Prism'' 'SomeException'   'String'@
-- @
userInterrupt :: AsAsyncException (Market' AsyncException) Mutator t => Prism' t ()
userInterrupt = case runPrism asyncException of
  (bt, seta) | btu <- bt UserInterrupt -> prism (const btu) $ \s -> case seta s of
    Left t -> Left t
    Right UserInterrupt -> Right ()
    Right a -> Left (bt a)
{-# INLINE userInterrupt #-}

----------------------------------------------------------------------------
-- AsyncException
----------------------------------------------------------------------------

class AsNonTermination p f t where
  -- | Thrown when the runtime system detects that the computation is guaranteed
  -- not to terminate. Note that there is no guarantee that the runtime system
  -- will notice whether any given computation is guaranteed to terminate or not.
  nonTermination :: Overloaded' p f t NonTermination

-- | @'nonTermination' :: 'Equality'' 'NonTermination' 'NonTermination'@
instance AsNonTermination p f NonTermination where
  nonTermination = id
  {-# INLINE nonTermination #-}

-- | @'nonTermination' :: 'Prism'' 'SomeException' 'NonTermination'@
instance (Prismatic p, Applicative f) => AsNonTermination p f SomeException where
  nonTermination = exception
  {-# INLINE nonTermination #-}

-- | 'NonTermination' is isomorphic to ()
--
-- @
-- '_nonTermination' :: 'Iso''   'NonTermination' ()
-- '_nonTermination' :: 'Prism'' 'SomeException'  ()
-- @
_nonTermination :: (AsNonTermination p f t, Profunctor p, Functor f) => Overloaded' p f t ()
_nonTermination = nonTermination . iso (const ()) (const NonTermination)
{-# INLINE _nonTermination #-}

----------------------------------------------------------------------------
-- NestedAtomically
----------------------------------------------------------------------------

class AsNestedAtomically p f t where
  -- | Thrown when the program attempts to call atomically, from the stm package,
  -- inside another call to atomically.
  --
  -- @
  -- 'nestedAtomically' :: 'Equality'' 'SomeException' 'NestedAtomically'
  -- 'nestedAtomically' :: 'Prism''    'SomeException' 'NestedAtomically'
  -- @
  nestedAtomically :: Overloaded' p f t NestedAtomically

-- | @'nestedAtomically' :: 'Equality'' 'SomeException' 'NestedAtomically'@
instance AsNestedAtomically p f NestedAtomically where
  nestedAtomically = id
  {-# INLINE nestedAtomically #-}

-- | @'nestedAtomically' :: 'Prism'' 'SomeException' 'NestedAtomically'@
instance (Prismatic p, Applicative f) => AsNestedAtomically p f SomeException where
  nestedAtomically = exception
  {-# INLINE nestedAtomically #-}

-- | 'NestedAtomically' is isomorphic to ()
--
-- @
-- '_nestedAtomically' :: 'Iso''   'NestedAtomically' ()
-- '_nestedAtomically' :: 'Prism'' 'SomeException'  ()
-- @
_nestedAtomically :: (AsNestedAtomically p f t, Profunctor p, Functor f) => Overloaded' p f t ()
_nestedAtomically = nestedAtomically . iso (const ()) (const NestedAtomically)
{-# INLINE _nestedAtomically #-}

----------------------------------------------------------------------------
-- BlockedIndefinitelyOnMVar
----------------------------------------------------------------------------

class AsBlockedIndefinitelyOnMVar p f t where
  -- | The thread is blocked on an MVar, but there are no other references
  -- to the MVar so it can't ever continue.
  --
  -- @
  -- 'blockedIndefinitelyOnMVar' :: 'Equality'' 'BlockedIndefinitelyOnMVar' 'BlockedIndefinitelyOnMVar'
  -- 'blockedIndefinitelyOnMVar' :: 'Prism''    'SomeException'             'BlockedIndefinitelyOnMVar'
  -- @
  blockedIndefinitelyOnMVar :: Overloaded' p f t BlockedIndefinitelyOnMVar

-- | @'blockedIndefinitelyOnMVar' :: 'Equality'' 'BlockedIndefinitelyOnMVar' 'BlockedIndefinitelyOnMVar'@
instance AsBlockedIndefinitelyOnMVar p f BlockedIndefinitelyOnMVar where
  blockedIndefinitelyOnMVar = id
  {-# INLINE blockedIndefinitelyOnMVar #-}

-- | @'blockedIndefinitelyOnMVar' :: 'Prism'' 'SomeException' 'BlockedIndefinitelyOnMVar'@
instance (Prismatic p, Applicative f) => AsBlockedIndefinitelyOnMVar p f SomeException where
  blockedIndefinitelyOnMVar = exception
  {-# INLINE blockedIndefinitelyOnMVar #-}

-- | 'BlockedIndefinetelyOnMVar' is isomorphic to ()
--
-- @
-- '_blockedIndefinitelyOnMVar' :: 'Iso''   'BlockedIndefinitelyOnMVar' ()
-- '_blockedIndefinitelyOnMVar' :: 'Prism'' 'SomeException'  ()
-- @
_blockedIndefinitelyOnMVar :: (AsBlockedIndefinitelyOnMVar p f t, Profunctor p, Functor f) => Overloaded' p f t ()
_blockedIndefinitelyOnMVar = blockedIndefinitelyOnMVar . iso (const ()) (const BlockedIndefinitelyOnMVar)
{-# INLINE _blockedIndefinitelyOnMVar #-}

----------------------------------------------------------------------------
-- BlockedIndefinitelyOnSTM
----------------------------------------------------------------------------

class AsBlockedIndefinitelyOnSTM p f t where
  -- | The thread is waiting to retry an STM transaction, but there are no
  -- other references to any TVars involved, so it can't ever continue.
  --
  -- @
  -- 'blockedIndefinitelyOnSTM' :: 'Equality'' 'BlockedIndefinitelyOnSTM' 'BlockedIndefinitelyOnSTM'
  -- 'blockedIndefinitelyOnSTM' :: 'Prism''    'SomeException'            'BlockedIndefinitelyOnSTM'
  -- @
  blockedIndefinitelyOnSTM :: Overloaded' p f t BlockedIndefinitelyOnSTM

-- | @'blockedIndefinitelyOnSTM' :: 'Equality'' 'BlockedIndefinitelyOnSTM' 'BlockedIndefinitelyOnSTM'@
instance AsBlockedIndefinitelyOnSTM p f BlockedIndefinitelyOnSTM where
  blockedIndefinitelyOnSTM = id
  {-# INLINE blockedIndefinitelyOnSTM #-}

-- | @'blockedIndefinitelyOnSTM' :: 'Prism'' 'SomeException' 'BlockedIndefinitelyOnSTM'@
instance (Prismatic p, Applicative f) => AsBlockedIndefinitelyOnSTM p f SomeException where
  blockedIndefinitelyOnSTM = exception
  {-# INLINE blockedIndefinitelyOnSTM #-}

-- | 'BlockedIndefinetelyOnSTM' is isomorphic to ()
--
-- @
-- '_blockedIndefinitelyOnSTM' :: 'Iso''   'BlockedIndefinitelyOnSTM' ()
-- '_blockedIndefinitelyOnSTM' :: 'Prism'' 'SomeException'  ()
-- @
_blockedIndefinitelyOnSTM :: (AsBlockedIndefinitelyOnSTM p f t, Profunctor p, Functor f) => Overloaded' p f t ()
_blockedIndefinitelyOnSTM = blockedIndefinitelyOnSTM . iso (const ()) (const BlockedIndefinitelyOnSTM)
{-# INLINE _blockedIndefinitelyOnSTM #-}

----------------------------------------------------------------------------
-- Deadlock
----------------------------------------------------------------------------

class AsDeadlock p f t where
  -- | There are no runnable threads, so the program is deadlocked. The Deadlock exception
  -- is raised in the main thread only.
  --
  -- @
  -- 'deadlock' :: 'Equality'' 'Deadlock'      'Deadlock'
  -- 'deadlock' :: 'Prism''    'SomeException' 'Deadlock'
  -- @
  deadlock :: Overloaded' p f t Deadlock

-- | @'deadlock' :: 'Equality'' 'Deadlock' 'Deadlock'@
instance AsDeadlock p f Deadlock where
  deadlock = id
  {-# INLINE deadlock #-}

-- | 'deadlock' :: 'Prism'' 'SomeException' 'Deadlock'@
instance (Prismatic p, Applicative f) => AsDeadlock p f SomeException where
  deadlock = exception
  {-# INLINE deadlock #-}

-- | 'Deadlock' is isomorphic to ()
--
-- @
-- '_deadlock' :: 'Iso''   'Deadlock'      ()
-- '_deadlock' :: 'Prism'' 'SomeException' ()
-- @
_deadlock :: (AsDeadlock p f t, Profunctor p, Functor f) => Overloaded' p f t ()
_deadlock = deadlock . iso (const ()) (const Deadlock)
{-# INLINE _deadlock #-}

----------------------------------------------------------------------------
-- NoMethodError
----------------------------------------------------------------------------

class AsNoMethodError p f t where
  -- | A class method without a definition (neither a default definition,
  -- nor a definition in the appropriate instance) was called.
  --
  -- @
  -- 'noMethodError' :: 'Equality'' 'NoMethodError' 'NoMethodError'@
  -- 'noMethodError' :: 'Prism''    'SomeException' 'NoMethodError'@
  -- @
  noMethodError :: Overloaded' p f t NoMethodError

-- | @'noMethodError' :: 'Equality'' 'NoMethodError' 'NoMethodError'@
instance AsNoMethodError p f NoMethodError where
  noMethodError = id
  {-# INLINE noMethodError #-}

-- | @'noMethodError' :: 'Prism'' 'SomeException' 'NoMethodError'@
instance (Prismatic p, Applicative f) => AsNoMethodError p f SomeException where
  noMethodError = exception
  {-# INLINE noMethodError #-}

-- | Information about which method it was
--
-- 'NoMethodError' is isomorphic to a 'String'
--
-- @
-- '_noMethodError' :: 'Iso''   'NoMethodError' 'String'
-- '_noMethodError' :: 'Prism'' 'SomeException' 'String'
-- @
_noMethodError :: (AsNoMethodError p f t, Profunctor p, Functor f) => Overloaded' p f t String
_noMethodError = noMethodError . unwrapped
{-# INLINE _noMethodError #-}

----------------------------------------------------------------------------
-- PatternMatchFail
----------------------------------------------------------------------------

class AsPatternMatchFail p f t where
  -- | A pattern match failed.
  --
  -- @
  -- 'patternMatchFail' :: 'Equality'' 'PatternMatchFail' 'PatternMatchFail'
  -- 'patternMatchFail' :: 'Prism''    'SomeException'    'PatternMatchFail'
  -- @
  patternMatchFail :: Overloaded' p f t PatternMatchFail

-- | @'patternMatchFail' :: 'Equality'' 'PatternMatchFail' 'PatternMatchFail'@
instance AsPatternMatchFail p f PatternMatchFail where
  patternMatchFail = id
  {-# INLINE patternMatchFail #-}

-- | @'patternMatchFail' :: 'Prism'' 'SomeException' 'PatternMatchFail'@
instance (Prismatic p, Applicative f) => AsPatternMatchFail p f SomeException where
  patternMatchFail = exception
  {-# INLINE patternMatchFail #-}

-- | Information about the source location of the pattern
--
-- 'PatternMatchFail' is isomorphic to a 'String'
--
-- @
-- '_patternMatchFail' :: 'Iso''   'PatternMatchFail' String
-- '_patternMatchFail' :: 'Prism'' 'SomeException'    String
-- @
_patternMatchFail :: (AsPatternMatchFail p f t, Profunctor p, Functor f) => Overloaded' p f t String
_patternMatchFail = patternMatchFail . unwrapped
{-# INLINE _patternMatchFail #-}

----------------------------------------------------------------------------
-- RecConError
----------------------------------------------------------------------------

class AsRecConError p f t where
  -- | An uninitialised record field was used.
  --
  -- @
  -- 'recConError' :: 'Equality'' 'RecConError' 'RecConError'
  -- 'recConError' :: 'Prism'' 'SomeException' 'RecConError'
  -- @
  recConError :: Overloaded' p f t RecConError

-- | @'recConError' :: 'Equality'' 'RecConError' 'RecConError'@
instance AsRecConError p f RecConError where
  recConError = id
  {-# INLINE recConError #-}

-- | @'recConError' :: 'Prism'' 'SomeException' 'RecConError'@
instance (Prismatic p, Applicative f) => AsRecConError p f SomeException where
  recConError = exception
  {-# INLINE recConError #-}

-- | Information about the source location where the record was constructed
--
-- 'RecConError' is isomorphic to a 'String'
--
-- @
-- '_recConError' :: 'Iso''   'RecConError'   String
-- '_recConError' :: 'Prism'' 'SomeException' String
-- @
_recConError :: (AsRecConError p f t, Profunctor p, Functor f) => Overloaded' p f t String
_recConError = recConError . unwrapped
{-# INLINE _recConError #-}

----------------------------------------------------------------------------
-- RecSelError
----------------------------------------------------------------------------

-- | A record selector was applied to a constructor without the appropriate
-- field. This can only happen with a datatype with multiple constructors,
-- where some fields are in one constructor but not another.
class AsRecSelError p f t where
  -- |
  -- @
  -- 'recSelError' :: 'Equality'' 'RecSelError' 'RecSelError'
  -- 'recSelError' :: 'Prism'' 'SomeException' 'RecSelError'
  -- @
  recSelError :: Overloaded' p f t RecSelError

-- | @'recSelError' :: 'Equality'' 'RecSelError' 'RecSelError'@
instance AsRecSelError p f RecSelError where
  recSelError = id
  {-# INLINE recSelError #-}

-- | @'recSelError' :: 'Prism'' 'SomeException' 'RecSelError'@
instance (Prismatic p, Applicative f) => AsRecSelError p f SomeException where
  recSelError = exception
  {-# INLINE recSelError #-}

-- | Information about the source location where the record selection occurred
--
-- 'RecSelError' is isomorphic to a 'String'
--
-- @
-- '_recSelError' :: 'Iso''   'RecSelError'   String
-- '_recSelError' :: 'Prism'' 'SomeException' String
-- @
_recSelError :: (AsRecSelError p f t, Profunctor p, Functor f) => Overloaded' p f t String
_recSelError = recSelError . unwrapped
{-# INLINE _recSelError #-}

----------------------------------------------------------------------------
-- RecUpdError
----------------------------------------------------------------------------

-- | A record update was performed on a constructor without the
-- appropriate field. This can only happen with a datatype with multiple
-- constructors, where some fields are in one constructor but not another.
class AsRecUpdError p f t where
  -- |
  -- @
  -- 'recUpdError' :: 'Equality'' 'RecUpdError' 'RecUpdError'
  -- 'recUpdError' :: 'Prism'' 'SomeException' 'RecUpdError'
  -- @
  recUpdError :: Overloaded' p f t RecUpdError

-- | @'recUpdError' :: 'Equality'' 'RecUpdError' 'RecUpdError'@
instance AsRecUpdError p f RecUpdError where
  recUpdError = id
  {-# INLINE recUpdError #-}

-- | @'recUpdError' :: 'Prism'' 'SomeException' 'RecUpdError'@
instance (Prismatic p, Applicative f) => AsRecUpdError p f SomeException where
  recUpdError = exception
  {-# INLINE recUpdError #-}

-- | Information about the source location where the record was updated
--
-- 'RecUpdError' is isomorphic to a 'String'
--
-- @
-- '_recUpdError' :: 'Iso''   'RecUpdError'   String
-- '_recUpdError' :: 'Prism'' 'SomeException' String
-- @
_recUpdError :: (AsRecUpdError p f t, Profunctor p, Functor f) => Overloaded' p f t String
_recUpdError = recUpdError . unwrapped
{-# INLINE _recUpdError #-}

----------------------------------------------------------------------------
-- ErrorCall
----------------------------------------------------------------------------

-- | This is thrown when the user calls 'error'.
class AsErrorCall p f t where
  -- |
  -- @
  -- 'errorCall' :: 'Equality'' 'ErrorCall'     'ErrorCall'
  -- 'errorCall' :: 'Prism''    'SomeException' 'ErrorCall'
  -- @
  errorCall :: Overloaded' p f t ErrorCall

-- | @'errorCall' :: 'Equality'' 'ErrorCall' 'ErrorCall'@
instance AsErrorCall p f ErrorCall where
  errorCall = id
  {-# INLINE errorCall #-}

-- | @'errorCall' :: 'Prism'' 'SomeException' 'ErrorCall'@
instance (Prismatic p, Applicative f) => AsErrorCall p f SomeException where
  errorCall = exception
  {-# INLINE errorCall #-}

-- | Retrieve the argument given to 'error'.
--
-- 'ErrorCall' is isomorphic to a 'String'
--
-- >>> catching _errorCall (error "touch down!") return
-- "touch down!"
--
-- @
-- '_errorCall' :: 'Iso''   'ErrorCall'     'String'
-- '_errorCall' :: 'Prism'' 'SomeException' 'String'
-- @
_errorCall :: (AsErrorCall p f t, Profunctor p, Functor f) => Overloaded' p f t String
_errorCall = errorCall . unwrapped
{-# INLINE _errorCall #-}
