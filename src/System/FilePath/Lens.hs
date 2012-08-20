-----------------------------------------------------------------------------
-- |
-- Module      :  System.FilePath.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module System.FilePath.Lens
  ( (</>~), (<</>~), (<.>~), (<<.>~)
  , (</>=), (<</>=), (<.>=), (<<.>=)
  , _basename, _directory, _extension, _filename
  ) where

import Control.Applicative ((<$>))

import Control.Monad.State.Class as State
import System.FilePath
  ( (</>), (<.>), splitExtension
  , takeBaseName, takeDirectory
  , takeExtension, takeFileName
  )

import Control.Lens hiding ((<.>))

infixr 4 </>~, <</>~, <.>~, <<.>~
infix 4 </>=, <</>=, <.>=, <<.>=

-- | Modify the path by adding another path.
--
-- >>> :m + Control.Lens Data.Pair.Lens
-- >>> both </>~ "!!!" $ ("hello","world")
-- ("hello/!!!","world/!!!")
--
-- @
-- (\</\>~) :: 'Setter' a b 'FilePath' 'FilePath' -> 'FilePath' -> a -> b
-- (\</\>~) :: 'Iso' a b 'FilePath' 'FilePath' -> 'FilePath' -> a -> b
-- (\</\>~) :: 'Lens' a b 'FilePath' 'FilePath' -> 'FilePath' -> a -> b
-- (\</\>~) :: 'Traversal' a b 'FilePath' 'FilePath' -> 'FilePath' -> a -> b
-- @
(</>~) :: Setting a b FilePath FilePath -> FilePath -> a -> b
l </>~ n = over l (</> n)
{-# INLINE (</>~) #-}


-- | Modify the target(s) of a 'Simple' 'Lens', 'Iso', 'Setter' or 'Traversal' by adding a path.
--
-- @
-- (\</\>=) :: 'MonadState' a m => 'Simple' 'Setter' a 'FilePath' -> 'FilePath' -> m ()
-- (\</\>=) :: 'MonadState' a m => 'Simple' 'Iso' a 'FilePath' -> 'FilePath' -> m ()
-- (\</\>=) :: 'MonadState' a m => 'Simple' 'Lens' a 'FilePath' -> 'FilePath' -> m ()
-- (\</\>=) :: 'MonadState' a m => 'Simple' 'Traversal' a 'FilePath' -> 'FilePath' -> m ()
-- @
(</>=) :: MonadState a m => SimpleSetting a FilePath -> FilePath -> m ()
l </>= b = State.modify (l </>~ b)
{-# INLINE (</>=) #-}


-- | Add a path onto the end of the target of a 'Lens' and return the result
--
-- When you do not need the result of the operation, ('</>~') is more flexible.
(<</>~) :: LensLike ((,)FilePath) a b FilePath FilePath -> FilePath -> a -> (FilePath, b)
l <</>~ m = l <%~ (</> m)
{-# INLINE (<</>~) #-}


-- | Add a path onto the end of the target of a 'Lens' into
-- your monad's state and return the result.
--
-- When you do not need the result of the operation, ('</>=') is more flexible.
(<</>=) :: MonadState a m => SimpleLensLike ((,)FilePath) a FilePath -> FilePath -> m FilePath
l <</>= r = l <%= (</> r)
{-# INLINE (<</>=) #-}


-- | Modify the path by adding extension.
--
-- >>> :m + Control.Lens Data.Pair.Lens
-- >>> both <.>~ "!!!" $ ("hello","world")
-- ("hello.!!!","world.!!!")
--
-- @
-- (\<.\>~) :: 'Setter' a b 'FilePath' 'FilePath' -> 'String' -> a -> b
-- (\<.\>~) :: 'Iso' a b 'FilePath' 'FilePath' -> 'String' -> a -> b
-- (\<.\>~) :: 'Lens' a b 'FilePath' 'FilePath' -> 'String' -> a -> b
-- (\<.\>~) :: 'Traversal' a b 'FilePath' 'FilePath' -> 'String' -> a -> b
-- @
(<.>~) :: Setting a b FilePath FilePath -> String -> a -> b
l <.>~ n = over l (<.> n)
{-# INLINE (<.>~) #-}


-- | Modify the target(s) of a 'Simple' 'Lens', 'Iso', 'Setter' or 'Traversal' by adding an extension.
--
-- @
-- (\<.\>=) :: 'MonadState' a m => 'Simple' 'Setter' a 'FilePath' -> 'String' -> m ()
-- (\<.\>=) :: 'MonadState' a m => 'Simple' 'Iso' a 'FilePath' -> 'String' -> m ()
-- (\<.\>=) :: 'MonadState' a m => 'Simple' 'Lens' a 'FilePath' -> 'String' -> m ()
-- (\<.\>=) :: 'MonadState' a m => 'Simple' 'Traversal' a 'FilePath' -> 'String' -> m ()
-- @
(<.>=) :: MonadState a m => SimpleSetting a FilePath -> String -> m ()
l <.>= b = State.modify (l <.>~ b)
{-# INLINE (<.>=) #-}


-- | Add an extension onto the end of the target of a 'Lens' and return the result
--
-- When you do not need the result of the operation, ('<.>~') is more flexible.
(<<.>~) :: LensLike ((,)FilePath) a b FilePath FilePath -> String -> a -> (FilePath, b)
l <<.>~ m = l <%~ (<.> m)
{-# INLINE (<<.>~) #-}


-- | Add an extension onto the end of the target of a 'Lens' into
-- your monad's state and return the result.
--
-- When you do not need the result of the operation, ('<.>=') is more flexible.
(<<.>=) :: MonadState a m => SimpleLensLike ((,)FilePath) a FilePath -> String -> m FilePath
l <<.>= r = l <%= (<.> r)
{-# INLINE (<<.>=) #-}


-- | A lens reading and writing to the basename.
--
-- >>> _basename .~ "filename" $ "path/name.png"
-- "path/filename.png"
_basename :: Simple Lens FilePath FilePath
_basename f p = (<.> takeExtension p) . (takeDirectory p </>) <$> f (takeBaseName p)
{-# INLINE _basename #-}


-- | A lens reading and writing to the directory.
--
-- >>> "long/path/name.txt" ^. _directory
-- "long/path"
_directory :: Simple Lens FilePath FilePath
_directory f p = (</> takeFileName p) <$> f (takeDirectory p)
{-# INLINE _directory #-}


-- | A lens reading and writing to the extension.
--
-- >>> _extension .~ ".png" $ "path/name.txt"
-- "path/name.png"
_extension :: Simple Lens FilePath FilePath
_extension f p = (n <.>) <$> f e
 where
  (n, e) = splitExtension p
{-# INLINE _extension #-}


-- | A lens reading and writing to the full filename.
--
-- >>> _filename .~ "name.txt" $ "path/name.png"
-- "path/name.txt"
_filename :: Simple Lens FilePath FilePath
_filename f p = (takeDirectory p </>) <$> f (takeFileName p)
{-# INLINE _filename #-}
