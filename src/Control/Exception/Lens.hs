{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
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
-- Copyright   :  (C) 2012-13 Edward Kmett
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
-- 'MonadCatchIO' instead of just 'IO'. This enables them to be used
-- more easily in 'Monad' transformer stacks.
----------------------------------------------------------------------------
module Control.Exception.Lens
  (
  -- * Handling
    catching, catching_
  , handling, handling_
  -- * Trying
  , trying
  -- * Throwing
  , throwing
  , throwingM
  , throwingTo
  -- * Exceptions
  , exception
  -- * Exception Handlers
  , Handleable(..)
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
  -- * Handling Exceptions
  , AsHandlingException(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.CatchIO as CatchIO hiding (try, tryJust)
import Control.Exception as Exception hiding (try, tryJust, catchJust)
import Control.Lens
import Control.Lens.Internal.Exception
import Data.Monoid
import GHC.Conc (ThreadId)
import Prelude
  ( asTypeOf, const, either, flip, id, maybe, undefined
  , ($), (.)
  ,  Maybe(..), Either(..), Functor(..), String, IO
  )

{-# ANN module "HLint: ignore Use Control.Exception.catch" #-}

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

------------------------------------------------------------------------------
-- Catching
------------------------------------------------------------------------------

-- | Catch exceptions that match a given 'Prism' (or any 'Getter', really).
--
-- >>> catching _AssertionFailed (assert False (return "uncaught")) $ \ _ -> return "caught"
-- "caught"
--
-- @
-- 'catching' :: 'MonadCatchIO' m => 'Prism'' 'SomeException' a     -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadCatchIO' m => 'Lens'' 'SomeException' a      -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadCatchIO' m => 'Traversal'' 'SomeException' a -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadCatchIO' m => 'Iso'' 'SomeException' a       -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadCatchIO' m => 'Getter' 'SomeException' a     -> m r -> (a -> m r) -> m r
-- 'catching' :: 'MonadCatchIO' m => 'Fold' 'SomeException' a       -> m r -> (a -> m r) -> m r
-- @
catching :: MonadCatchIO m => Getting (First a) SomeException a -> m r -> (a -> m r) -> m r
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
-- 'catching_' :: 'MonadCatchIO' m => 'Prism'' 'SomeException' a     -> m r -> m r -> m r
-- 'catching_' :: 'MonadCatchIO' m => 'Lens'' 'SomeException' a      -> m r -> m r -> m r
-- 'catching_' :: 'MonadCatchIO' m => 'Traversal'' 'SomeException' a -> m r -> m r -> m r
-- 'catching_' :: 'MonadCatchIO' m => 'Iso'' 'SomeException' a       -> m r -> m r -> m r
-- 'catching_' :: 'MonadCatchIO' m => 'Getter' 'SomeException' a     -> m r -> m r -> m r
-- 'catching_' :: 'MonadCatchIO' m => 'Fold' 'SomeException' a       -> m r -> m r -> m r
-- @
catching_ :: MonadCatchIO m => Getting (First a) SomeException a -> m r -> m r -> m r
catching_ l a b = catchJust (preview l) a (const b)
{-# INLINE catching_ #-}

-- | A helper function to provide conditional catch behavior.
catchJust :: (MonadCatchIO m, Exception e) => (e -> Maybe t) -> m a -> (t -> m a) -> m a
catchJust f m k = CatchIO.catch m $ \ e -> case f e of
  Nothing -> liftIO (throwIO e)
  Just x  -> k x
{-# INLINE catchJust #-}

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
-- 'handling' :: 'MonadCatchIO' m => 'Prism'' 'SomeException' a     -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadCatchIO' m => 'Lens'' 'SomeException' a      -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadCatchIO' m => 'Traversal'' 'SomeException' a -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadCatchIO' m => 'Iso'' 'SomeException' a       -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadCatchIO' m => 'Fold' 'SomeException' a       -> (a -> m r) -> m r -> m r
-- 'handling' :: 'MonadCatchIO' m => 'Getter' 'SomeException' a     -> (a -> m r) -> m r -> m r
-- @
handling :: MonadCatchIO m => Getting (First a) SomeException a -> (a -> m r) -> m r -> m r
handling l = flip (catching l)
{-# INLINE handling #-}

-- | A version of 'catching_' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.
--
-- >>> handling_ _NonTermination (return "caught") $ throwIO NonTermination
-- "caught"
--
-- @
-- 'handling_' :: 'MonadCatchIO' m => 'Prism'' 'SomeException' a     -> m r -> m r -> m r
-- 'handling_' :: 'MonadCatchIO' m => 'Lens'' 'SomeException' a      -> m r -> m r -> m r
-- 'handling_' :: 'MonadCatchIO' m => 'Traversal'' 'SomeException' a -> m r -> m r -> m r
-- 'handling_' :: 'MonadCatchIO' m => 'Iso'' 'SomeException' a       -> m r -> m r -> m r
-- 'handling_' :: 'MonadCatchIO' m => 'Getter' 'SomeException' a     -> m r -> m r -> m r
-- 'handling_' :: 'MonadCatchIO' m => 'Fold' 'SomeException' a       -> m r -> m r -> m r
-- @
handling_ :: MonadCatchIO m => Getting (First a) SomeException a -> m r -> m r -> m r
handling_ l = flip (catching_ l)
{-# INLINE handling_ #-}

------------------------------------------------------------------------------
-- Trying
------------------------------------------------------------------------------

-- | A variant of 'Control.Exception.try' that takes a 'Prism' (or any 'Getter') to select which
-- exceptions are caught (c.f. 'Control.Exception.tryJust', 'Control.Exception.catchJust'). If the
-- 'Exception' does not match the predicate, it is re-thrown.
--
-- @
-- 'trying' :: 'MonadCatchIO' m => 'Prism''     'SomeException' a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadCatchIO' m => 'Lens''      'SomeException' a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadCatchIO' m => 'Traversal'' 'SomeException' a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadCatchIO' m => 'Iso''       'SomeException' a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadCatchIO' m => 'Getter'     'SomeException' a -> m r -> m ('Either' a r)
-- 'trying' :: 'MonadCatchIO' m => 'Fold'       'SomeException' a -> m r -> m ('Either' a r)
-- @
trying :: MonadCatchIO m => Getting (First a) SomeException a -> m r -> m (Either a r)
trying l = tryJust (preview l)

-- | A helper version of 'Control.Exception.try' that doesn't needlessly require 'Functor'.
try :: (MonadCatchIO m, Exception e) => m a -> m (Either e a)
try a = CatchIO.catch (liftM Right a) (return . Left)

-- | A helper version of 'Control.Exception.tryJust' that doesn't needlessly require 'Functor'.
tryJust :: (MonadCatchIO m, Exception e) => (e -> Maybe b) -> m a -> m (Either b a)
tryJust p a = do
  r <- try a
  case r of
    Right v -> return (Right v)
    Left  e -> case p e of
      Nothing -> CatchIO.throw e `asTypeOf` return (Left undefined)
      Just b  -> return (Left b)

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
-- 'throwing' :: 'Prism'' 'SomeException' t -> t -> a
-- 'throwing' :: 'Iso'' 'SomeException' t   -> t -> a
-- @
throwing :: AReview s SomeException a b -> b -> a
throwing l = reviews l Exception.throw
{-# INLINE throwing #-}

-- | A variant of 'throwing' that can only be used within the 'IO' 'Monad'
-- (or any other 'MonadCatchIO' instance) to throw an 'Exception' described 
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
-- be raised when it is used within the 'MonadCatchIO' instance. The 'throwingM'
-- variant should be used in preference to 'throwing' to raise an 'Exception'
-- within the 'Monad' because it guarantees ordering with respect to other
-- monadic operations, whereas 'throwing' does not.
--
-- @
-- 'throwingM' l ≡ 'reviews' l 'CatchIO.throw'
-- @
--
-- @
-- 'throwingM' :: 'MonadCatchIO' m => 'Prism'' 'SomeException' t -> t -> m a
-- 'throwingM' :: 'MonadCatchIO' m => 'Iso'' 'SomeException' t   -> t -> m a
-- @
throwingM :: MonadCatchIO m => AReview s SomeException a b -> b -> m a
throwingM l = reviews l (liftIO . throwIO)
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
throwingTo :: MonadIO m => ThreadId -> AReview s SomeException a b -> b -> m ()
throwingTo tid l = reviews l (liftIO . throwTo tid)
{-# INLINE throwingTo #-}

----------------------------------------------------------------------------
-- IOException
----------------------------------------------------------------------------

-- | Exceptions that occur in the 'IO' 'Monad'. An 'IOException' records a
-- more specific error type, a descriptive string and maybe the handle that was
-- used when the error was flagged.
--
-- Due to their richer structure relative to other exceptions, these have
-- a more carefully overloaded signature.
class AsIOException p f t where
  -- | Unfortunately the name 'ioException' is taken by @base@ for
  -- throwing IOExceptions.
  --
  -- @
  -- '_IOException' :: 'Equality'' 'IOException' 'IOException'
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
-- @
-- '_Overflow' ≡ '_ArithException' '.' '_Overflow'
-- @
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
-- @
-- '_Underflow' ≡ '_ArithException' '.' '_Underflow'
-- @
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
-- @
-- '_LossOfPrecision' ≡ '_ArithException' '.' '_LossOfPrecision'
-- @
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
-- @
-- '_DivideByZero' ≡ '_ArithException' '.' '_DivideByZero'
-- @
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
-- @
-- '_Denormal' ≡ '_ArithException' '.' '_Denormal'
-- @
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
-- @
-- '_RatioZeroDenominator' ≡ '_ArithException' '.' '_RatioZeroDenominator'
-- @
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

-- | Exceptions generated by array operations.
class AsArrayException p f t where
  -- | Extract information about an 'ArrayException'.
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
-- @
-- '_IndexOutOfBounds' ≡ '_ArrayException' '.' '_IndexOutOfBounds'
-- @
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
-- @
-- '_UndefinedElement' ≡ '_ArrayException' '.' '_UndefinedElement'
-- @
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

-- | 'assert' was applied to 'Prelude.False'.
class AsAssertionFailed p f t where
  -- | This 'Exception' contains provides information about what assertion failed in the 'String'.
  --
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
  -- | There are several types of 'AsyncException'.
  --
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

-- | The current thread's stack exceeded its limit. Since an 'Exception' has
-- been raised, the thread's stack will certainly be below its limit again,
-- but the programmer should take remedial action immediately.
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
-- * It is undefined which thread receives this 'Exception'.
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

-- | This 'Exception' is raised by another thread calling
-- 'Control.Concurrent.killThread', or by the system if it needs to terminate
-- the thread for some reason.
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

-- | This 'Exception' is raised by default in the main thread of the program when
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

-- | Thrown when the runtime system detects that the computation is guaranteed
-- not to terminate. Note that there is no guarantee that the runtime system
-- will notice whether any given computation is guaranteed to terminate or not.
class (Profunctor p, Functor f) => AsNonTermination p f t where
  -- | There is no additional information carried in a 'NonTermination' 'Exception'.
  --
  -- @
  -- '_NonTermination' :: 'Iso''   'NonTermination' ()
  -- '_NonTermination' :: 'Prism'' 'SomeException'  ()
  -- @
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

-- | Thrown when the program attempts to call atomically, from the
-- 'Control.Monad.STM' package, inside another call to atomically.
class (Profunctor p, Functor f) => AsNestedAtomically p f t where
  -- | There is no additional information carried in a 'NestedAtomically' 'Exception'.
  --
  -- @
  -- '_NestedAtomically' :: 'Iso''   'NestedAtomically' ()
  -- '_NestedAtomically' :: 'Prism'' 'SomeException'    ()
  -- @
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

-- | The thread is blocked on an 'Control.Concurrent.MVar.MVar', but there
-- are no other references to the 'Control.Concurrent.MVar.MVar' so it can't
-- ever continue.
class (Profunctor p, Functor f) => AsBlockedIndefinitelyOnMVar p f t where
  -- | There is no additional information carried in a 'BlockedIndefinitelyOnMVar' 'Exception'.
  --
  -- @
  -- '_BlockedIndefinitelyOnMVar' :: 'Iso''   'BlockedIndefinitelyOnMVar' ()
  -- '_BlockedIndefinitelyOnMVar' :: 'Prism'' 'SomeException'             ()
  -- @
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

-- | The thread is waiting to retry an 'Control.Monad.STM.STM' transaction,
-- but there are no other references to any TVars involved, so it can't ever
-- continue.
class (Profunctor p, Functor f) => AsBlockedIndefinitelyOnSTM p f t where
  -- | There is no additional information carried in a 'BlockedIndefinitelyOnSTM' 'Exception'.
  --
  -- @
  -- '_BlockedIndefinitelyOnSTM' :: 'Iso''   'BlockedIndefinitelyOnSTM' ()
  -- '_BlockedIndefinitelyOnSTM' :: 'Prism'' 'SomeException'            ()
  -- @
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

-- | There are no runnable threads, so the program is deadlocked. The
-- 'Deadlock' 'Exception' is raised in the main thread only.
class (Profunctor p, Functor f) => AsDeadlock p f t where
  -- | There is no information carried in a 'Deadlock' 'Exception'.
  --
  -- @
  -- '_Deadlock' :: 'Iso''   'Deadlock'      ()
  -- '_Deadlock' :: 'Prism'' 'SomeException' ()
  -- @
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

-- | A class method without a definition (neither a default definition,
-- nor a definition in the appropriate instance) was called.
class (Profunctor p, Functor f) => AsNoMethodError p f t where
  -- | Extract a description of the missing method.
  --
  -- @
  -- '_NoMethodError' :: 'Iso''   'NoMethodError' 'String'
  -- '_NoMethodError' :: 'Prism'' 'SomeException' 'String'
  -- @
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

-- | A pattern match failed.
class (Profunctor p, Functor f) => AsPatternMatchFail p f t where
  -- | Information about the source location of the pattern.
  --
  -- @
  -- '_PatternMatchFail' :: 'Iso''   'PatternMatchFail' 'String'
  -- '_PatternMatchFail' :: 'Prism'' 'SomeException'    'String'
  -- @
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

-- | An uninitialised record field was used.
class (Profunctor p, Functor f) => AsRecConError p f t where
  -- | Information about the source location where the record was
  -- constructed.
  --
  -- @
  -- '_RecConError' :: 'Iso''   'RecConError'   'String'
  -- '_RecConError' :: 'Prism'' 'SomeException' 'String'
  -- @
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
  -- | Information about the source location where the record selection occurred.
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
  -- | Information about the source location where the record was updated.
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

-- | This is thrown when the user calls 'Prelude.error'.
class (Profunctor p, Functor f) => AsErrorCall p f t where
  -- | Retrieve the argument given to 'Prelude.error'.
  --
  -- 'ErrorCall' is isomorphic to a 'String'.
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
-- HandlingException
------------------------------------------------------------------------------

-- | This 'Exception' is thrown by @lens@ when the user somehow manages to rethrow
-- an internal 'HandlingException'.
class (Profunctor p, Functor f) => AsHandlingException p f t where
  -- | There is no information carried in a 'HandlingException'.
  --
  -- @
  -- '_HandlingException' :: 'Iso''   'HandlingException' ()
  -- '_HandlingException' :: 'Prism'' 'SomeException'     ()
  -- @
  _HandlingException :: Overloaded' p f t ()

instance (Profunctor p, Functor f) => AsHandlingException p f HandlingException where
  _HandlingException = trivial HandlingException
  {-# INLINE _HandlingException #-}

instance (Choice p, Applicative f) => AsHandlingException p f SomeException where
  _HandlingException = exception.trivial HandlingException
  {-# INLINE _HandlingException #-}

------------------------------------------------------------------------------
-- Helper Functions
------------------------------------------------------------------------------

trivial :: t -> Iso' t ()
trivial t = const () `iso` const t

