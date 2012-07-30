module Control.Exception.Lens
  ( traverseException
  ) where

import Control.Applicative
import Control.Exception
import Control.Lens

-- |
-- Traverse the strongly typed 'Exception' contained in 'SomeException' where the type of your function matches
-- the desired 'Exception'.
--
-- > traverseException :: (Applicative f, Exception a, Exception b) => (a -> f b) -> SomeException -> f SomeException
traverseException :: (Exception a, Exception b) => Traversal SomeException SomeException a b
traverseException f e = case fromException e of
  Just a -> toException <$> f a
  Nothing -> pure e
{-# INLINE traverseException #-}
