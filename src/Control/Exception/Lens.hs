{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  ( exception
  , catching, catching_
  , handling, handling_
  , throwing, throwingIO, throwingTo
  -- * Prismatic Exception
  , AsIOException(..)
  , AsArithException(..)
  , AsArrayException(..)
  , AsAssertionFailed(..)
  , AsAsyncException(..)
  , AsNonTermination(..)
  , AsNestedAtomically(..)
  , AsBlockedIndefinitelyOnMVar(..)
  , AsBlockedIndefinitelyOnSTM(..)
  , AsDeadlock(..)
  , AsNoMethodError(..)
  , AsPatternMatchFail(..)
  , AsRecConError(..)
  , AsRecSelError(..)
  , AsRecUpdError(..)
  , AsErrorCall(..)
  ) where

import Control.Exception
import Control.Lens
import Data.Monoid
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
-- >>> throwing nonTermination ()
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

class AsIOException t where
  ioException :: Prism' t IOException

instance AsIOException IOException where
  ioException = id
  {-# INLINE ioException #-}

instance AsIOException SomeException where
  ioException = exception
  {-# INLINE ioException #-}

----------------------------------------------------------------------------
-- ArithException
----------------------------------------------------------------------------

class AsArithException t where
  arithException :: Prism' t ArithException

  overflow :: Prism' t ()
  overflow = case runPrism arithException of
    (bt, seta) | bto <- bt Overflow -> prism (const bto) $ \s -> case seta s of
      Left t -> Left t
      Right Overflow -> Right ()
      Right a -> Left (bt a)
  {-# INLINE overflow #-}

  underflow :: Prism' t ()
  underflow = case runPrism arithException of
    (bt, seta) | btu <- bt Underflow -> prism (const btu) $ \s -> case seta s of
      Left t -> Left t
      Right Underflow -> Right ()
      Right a -> Left (bt a)
  {-# INLINE underflow #-}

  lossOfPrecision :: Prism' t ()
  lossOfPrecision = case runPrism arithException of
    (bt, seta) | btu <- bt LossOfPrecision -> prism (const btu) $ \s -> case seta s of
      Left t -> Left t
      Right LossOfPrecision -> Right ()
      Right a -> Left (bt a)
  {-# INLINE lossOfPrecision #-}

  divideByZero :: Prism' t ()
  divideByZero = case runPrism arithException of
    (bt, seta) | btu <- bt DivideByZero -> prism (const btu) $ \s -> case seta s of
      Left t -> Left t
      Right DivideByZero -> Right ()
      Right a -> Left (bt a)
  {-# INLINE divideByZero #-}

  denormal :: Prism' t ()
  denormal = case runPrism arithException of
    (bt, seta) | btu <- bt Denormal -> prism (const btu) $ \s -> case seta s of
      Left t -> Left t
      Right Denormal -> Right ()
      Right a -> Left (bt a)
  {-# INLINE denormal #-}

#if MIN_VERSION_base(4,6,0)
  ratioZeroDenominator :: Prism' t ()
  ratioZeroDenominator = case runPrism arithException of
    (bt, seta) | btu <- bt RatioZeroDenominator -> prism (const btu) $ \s -> case seta s of
      Left t -> Left t
      Right RatioZeroDenominator -> Right ()
      Right a -> Left (bt a)
  {-# INLINE ratioZeroDenominator #-}
#endif

instance AsArithException ArithException where
  arithException = id
  {-# INLINE arithException #-}

instance AsArithException SomeException where
  arithException = exception
  {-# INLINE arithException #-}

----------------------------------------------------------------------------
-- ArrayException
----------------------------------------------------------------------------

class AsArrayException t where
  arrayException :: Prism' t ArrayException

  indexOutOfBounds :: Prism' t String
  indexOutOfBounds = case runPrism arrayException of
    (bt, seta) -> prism (bt . IndexOutOfBounds) $ \s -> case seta s of
      Left t -> Left t
      Right (IndexOutOfBounds r) -> Right r
      Right a -> Left (bt a)
  {-# INLINE indexOutOfBounds #-}

  undefinedElement :: Prism' t String
  undefinedElement = case runPrism arrayException of
    (bt, seta) -> prism (bt . UndefinedElement) $ \s -> case seta s of
      Left t -> Left t
      Right (UndefinedElement r) -> Right r
      Right a -> Left (bt a)
  {-# INLINE undefinedElement #-}

instance AsArrayException ArrayException where
  arrayException = id
  {-# INLINE arrayException #-}

instance AsArrayException SomeException where
  arrayException = exception
  {-# INLINE arrayException #-}

----------------------------------------------------------------------------
-- AssertionFailed
----------------------------------------------------------------------------

class AsAssertionFailed t where
  assertionFailed :: Prism' t AssertionFailed

  _assertionFailed :: Prism' t String
  _assertionFailed = assertionFailed . unwrapped
  {-# INLINE _assertionFailed #-}

instance AsAssertionFailed AssertionFailed where
  assertionFailed = id
  {-# INLINE assertionFailed #-}

instance AsAssertionFailed SomeException where
  assertionFailed = exception
  {-# INLINE assertionFailed #-}

----------------------------------------------------------------------------
-- AsyncException
----------------------------------------------------------------------------

class AsAsyncException t where
  asyncException :: Prism' t AsyncException

  stackOverflow :: Prism' t ()
  stackOverflow = case runPrism asyncException of
    (bt, seta) | btu <- bt StackOverflow -> prism (const btu) $ \s -> case seta s of
      Left t -> Left t
      Right StackOverflow -> Right ()
      Right a -> Left (bt a)
  {-# INLINE stackOverflow #-}


  heapOverflow :: Prism' t ()
  heapOverflow = case runPrism asyncException of
    (bt, seta) | btu <- bt HeapOverflow -> prism (const btu) $ \s -> case seta s of
      Left t -> Left t
      Right HeapOverflow -> Right ()
      Right a -> Left (bt a)
  {-# INLINE heapOverflow #-}


  threadKilled :: Prism' t ()
  threadKilled = case runPrism asyncException of
    (bt, seta) | btu <- bt ThreadKilled -> prism (const btu) $ \s -> case seta s of
      Left t -> Left t
      Right ThreadKilled -> Right ()
      Right a -> Left (bt a)
  {-# INLINE threadKilled #-}

  userInterrupt :: Prism' t ()
  userInterrupt = case runPrism asyncException of
    (bt, seta) | btu <- bt UserInterrupt -> prism (const btu) $ \s -> case seta s of
      Left t -> Left t
      Right UserInterrupt -> Right ()
      Right a -> Left (bt a)
  {-# INLINE userInterrupt #-}

instance AsAsyncException AsyncException where
  asyncException = id
  {-# INLINE asyncException #-}

instance AsAsyncException SomeException where
  asyncException = exception
  {-# INLINE asyncException #-}

----------------------------------------------------------------------------
-- AsyncException
----------------------------------------------------------------------------

class AsNonTermination t where
  nonTermination :: Prism' t NonTermination

  _nonTermination :: Prism' t ()
  _nonTermination = nonTermination . iso (const ()) (const NonTermination)
  {-# INLINE _nonTermination #-}

instance AsNonTermination NonTermination where
  nonTermination = id
  {-# INLINE nonTermination #-}

instance AsNonTermination SomeException where
  nonTermination = exception
  {-# INLINE nonTermination #-}

----------------------------------------------------------------------------
-- NestedAtomically
----------------------------------------------------------------------------

class AsNestedAtomically t where
  nestedAtomically :: Prism' t NestedAtomically

  _nestedAtomically :: Prism' t ()
  _nestedAtomically = nestedAtomically . iso (const ()) (const NestedAtomically)
  {-# INLINE _nestedAtomically #-}

instance AsNestedAtomically NestedAtomically where
  nestedAtomically = id
  {-# INLINE nestedAtomically #-}

instance AsNestedAtomically SomeException where
  nestedAtomically = exception
  {-# INLINE nestedAtomically #-}

----------------------------------------------------------------------------
-- BlockedIndefinitelyOnMVar
----------------------------------------------------------------------------

class AsBlockedIndefinitelyOnMVar t where
  blockedIndefinitelyOnMVar :: Prism' t BlockedIndefinitelyOnMVar

  _blockedIndefinitelyOnMVar :: Prism' t ()
  _blockedIndefinitelyOnMVar = blockedIndefinitelyOnMVar . iso (const ()) (const BlockedIndefinitelyOnMVar)
  {-# INLINE _blockedIndefinitelyOnMVar #-}

instance AsBlockedIndefinitelyOnMVar BlockedIndefinitelyOnMVar where
  blockedIndefinitelyOnMVar = id
  {-# INLINE blockedIndefinitelyOnMVar #-}

instance AsBlockedIndefinitelyOnMVar SomeException where
  blockedIndefinitelyOnMVar = exception
  {-# INLINE blockedIndefinitelyOnMVar #-}

----------------------------------------------------------------------------
-- BlockedIndefinitelyOnSTM
----------------------------------------------------------------------------

class AsBlockedIndefinitelyOnSTM t where
  blockedIndefinitelyOnSTM :: Prism' t BlockedIndefinitelyOnSTM

  _blockedIndefinitelyOnSTM :: Prism' t ()
  _blockedIndefinitelyOnSTM = blockedIndefinitelyOnSTM . iso (const ()) (const BlockedIndefinitelyOnSTM)
  {-# INLINE _blockedIndefinitelyOnSTM #-}

instance AsBlockedIndefinitelyOnSTM BlockedIndefinitelyOnSTM where
  blockedIndefinitelyOnSTM = id
  {-# INLINE blockedIndefinitelyOnSTM #-}

instance AsBlockedIndefinitelyOnSTM SomeException where
  blockedIndefinitelyOnSTM = exception
  {-# INLINE blockedIndefinitelyOnSTM #-}

----------------------------------------------------------------------------
-- Deadlock
----------------------------------------------------------------------------

class AsDeadlock t where
  deadlock :: Prism' t Deadlock

  _deadlock :: Prism' t ()
  _deadlock = deadlock . iso (const ()) (const Deadlock)
  {-# INLINE _deadlock #-}

instance AsDeadlock Deadlock where
  deadlock = id
  {-# INLINE deadlock #-}

instance AsDeadlock SomeException where
  deadlock = exception
  {-# INLINE deadlock #-}

----------------------------------------------------------------------------
-- NoMethodError
----------------------------------------------------------------------------

class AsNoMethodError t where
  noMethodError :: Prism' t NoMethodError

  _noMethodError :: Prism' t String
  _noMethodError = noMethodError . unwrapped
  {-# INLINE _noMethodError #-}

instance AsNoMethodError NoMethodError where
  noMethodError = id
  {-# INLINE noMethodError #-}

instance AsNoMethodError SomeException where
  noMethodError = exception
  {-# INLINE noMethodError #-}

----------------------------------------------------------------------------
-- PatternMatchFail
----------------------------------------------------------------------------

class AsPatternMatchFail t where
  patternMatchFail :: Prism' t PatternMatchFail

  _patternMatchFail :: Prism' t String
  _patternMatchFail = patternMatchFail . unwrapped
  {-# INLINE _patternMatchFail #-}

instance AsPatternMatchFail PatternMatchFail where
  patternMatchFail = id
  {-# INLINE patternMatchFail #-}

instance AsPatternMatchFail SomeException where
  patternMatchFail = exception
  {-# INLINE patternMatchFail #-}

----------------------------------------------------------------------------
-- RecConError
----------------------------------------------------------------------------

class AsRecConError t where
  recConError :: Prism' t RecConError

  _recConError :: Prism' t String
  _recConError = recConError . unwrapped
  {-# INLINE _recConError #-}

instance AsRecConError RecConError where
  recConError = id
  {-# INLINE recConError #-}

instance AsRecConError SomeException where
  recConError = exception
  {-# INLINE recConError #-}

----------------------------------------------------------------------------
-- RecSelError
----------------------------------------------------------------------------

class AsRecSelError t where
  recSelError :: Prism' t RecSelError

  _recSelError :: Prism' t String
  _recSelError = recSelError . unwrapped
  {-# INLINE _recSelError #-}

instance AsRecSelError RecSelError where
  recSelError = id
  {-# INLINE recSelError #-}

instance AsRecSelError SomeException where
  recSelError = exception
  {-# INLINE recSelError #-}

----------------------------------------------------------------------------
-- RecUpdError
----------------------------------------------------------------------------

class AsRecUpdError t where
  recUpdError :: Prism' t RecUpdError

  _recUpdError :: Prism' t String
  _recUpdError = recUpdError . unwrapped
  {-# INLINE _recUpdError #-}

instance AsRecUpdError RecUpdError where
  recUpdError = id
  {-# INLINE recUpdError #-}

instance AsRecUpdError SomeException where
  recUpdError = exception
  {-# INLINE recUpdError #-}

----------------------------------------------------------------------------
-- ErrorCall
----------------------------------------------------------------------------

class AsErrorCall t where
  errorCall :: Prism' t ErrorCall

  _errorCall :: Prism' t String
  _errorCall = errorCall . unwrapped
  {-# INLINE _errorCall #-}

instance AsErrorCall ErrorCall where
  errorCall = id
  {-# INLINE errorCall #-}

instance AsErrorCall SomeException where
  errorCall = exception
  {-# INLINE errorCall #-}
