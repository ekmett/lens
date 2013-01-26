-----------------------------------------------------------------------------
-- |
-- Module      :  System.FilePath.Lens
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module System.FilePath.Lens
  (
  -- * Operators
    (</>~), (<</>~), (<.>~), (<<.>~)
  , (</>=), (<</>=), (<.>=), (<<.>=)
  -- * Lenses
  , basename, directory, extension, filename
  ) where

import Control.Applicative ((<$>))

import Control.Monad.State as State
import System.FilePath
  ( (</>), (<.>), splitExtension
  , takeBaseName, takeDirectory
  , takeExtension, takeFileName
  )

import Control.Lens hiding ((<.>))

-- $setup
-- >>> :set -XNoOverloadedStrings

infixr 4 </>~, <</>~, <.>~, <<.>~
infix 4 </>=, <</>=, <.>=, <<.>=

-- | Modify the path by adding another path.
--
-- >>> both </>~ "bin" $ ("hello","world")
-- ("hello/bin","world/bin")
--
-- @
-- ('</>~') :: 'Setter' s a 'FilePath' 'FilePath' -> 'FilePath' -> s -> a
-- ('</>~') :: 'Iso' s a 'FilePath' 'FilePath' -> 'FilePath' -> s -> a
-- ('</>~') :: 'Lens' s a 'FilePath' 'FilePath' -> 'FilePath' -> s -> a
-- ('</>~') :: 'Traversal' s a 'FilePath' 'FilePath' -> 'FilePath' -> s -> a
-- @
(</>~) :: ASetter s t FilePath FilePath -> FilePath -> s -> t
l </>~ n = over l (</> n)
{-# INLINE (</>~) #-}


-- | Modify the target(s) of a 'Simple' 'Lens', 'Iso', 'Setter' or 'Traversal' by adding a path.
--
-- >>> execState (both </>= "bin") ("hello","world")
-- ("hello/bin","world/bin")
--
-- @
-- ('</>=') :: 'MonadState' s m => 'Setter'' s 'FilePath' -> 'FilePath' -> m ()
-- ('</>=') :: 'MonadState' s m => 'Iso'' s 'FilePath' -> 'FilePath' -> m ()
-- ('</>=') :: 'MonadState' s m => 'Lens'' s 'FilePath' -> 'FilePath' -> m ()
-- ('</>=') :: 'MonadState' s m => 'Traversal'' s 'FilePath' -> 'FilePath' -> m ()
-- @
(</>=) :: MonadState s m => ASetter' s FilePath -> FilePath -> m ()
l </>= b = State.modify (l </>~ b)
{-# INLINE (</>=) #-}


-- | Add a path onto the end of the target of a 'Lens' and return the result
--
-- When you do not need the result of the operation, ('</>~') is more flexible.
(<</>~) :: LensLike ((,)FilePath) s a FilePath FilePath -> FilePath -> s -> (FilePath, a)
l <</>~ m = l <%~ (</> m)
{-# INLINE (<</>~) #-}


-- | Add a path onto the end of the target of a 'Lens' into
-- your monad's state and return the result.
--
-- When you do not need the result of the operation, ('</>=') is more flexible.
(<</>=) :: MonadState s m => LensLike' ((,)FilePath) s FilePath -> FilePath -> m FilePath
l <</>= r = l <%= (</> r)
{-# INLINE (<</>=) #-}


-- | Modify the path by adding extension.
--
-- >>> both <.>~ "txt" $ ("hello","world")
-- ("hello.txt","world.txt")
--
-- @
-- ('<.>~') :: 'Setter' s a 'FilePath' 'FilePath' -> 'String' -> s -> a
-- ('<.>~') :: 'Iso' s a 'FilePath' 'FilePath' -> 'String' -> s -> a
-- ('<.>~') :: 'Lens' s a 'FilePath' 'FilePath' -> 'String' -> s -> a
-- ('<.>~') :: 'Traversal' s a 'FilePath' 'FilePath' -> 'String' -> s -> a
-- @
(<.>~) :: ASetter s a FilePath FilePath -> String -> s -> a
l <.>~ n = over l (<.> n)
{-# INLINE (<.>~) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Iso', 'Setter' or 'Traversal' by adding an extension.
--
-- >>> execState (both <.>= "txt") ("hello","world")
-- ("hello.txt","world.txt")
--
-- @
-- ('<.>=') :: 'MonadState' s m => 'Setter'' s 'FilePath' -> 'String' -> m ()
-- ('<.>=') :: 'MonadState' s m => 'Iso'' s 'FilePath' -> 'String' -> m ()
-- ('<.>=') :: 'MonadState' s m => 'Lens'' s 'FilePath' -> 'String' -> m ()
-- ('<.>=') :: 'MonadState' s m => 'Traversal'' s 'FilePath' -> 'String' -> m ()
-- @
(<.>=) :: MonadState s m => ASetter' s FilePath -> String -> m ()
l <.>= b = State.modify (l <.>~ b)
{-# INLINE (<.>=) #-}

-- | Add an extension onto the end of the target of a 'Lens' and return the result
--
-- >>> _1 <<.>~ "txt" $ ("hello","world")
-- ("hello.txt",("hello.txt","world"))
--
-- When you do not need the result of the operation, ('<.>~') is more flexible.
(<<.>~) :: LensLike ((,)FilePath) s a FilePath FilePath -> String -> s -> (FilePath, a)
l <<.>~ m = l <%~ (<.> m)
{-# INLINE (<<.>~) #-}


-- | Add an extension onto the end of the target of a 'Lens' into
-- your monad's state and return the result.
--
-- >>> evalState (_1 <<.>= "txt") $("hello","world")
-- "hello.txt"
--
-- When you do not need the result of the operation, ('<.>=') is more flexible.
(<<.>=) :: MonadState s m => LensLike' ((,)FilePath) s FilePath -> String -> m FilePath
l <<.>= r = l <%= (<.> r)
{-# INLINE (<<.>=) #-}


-- | A 'Lens' for reading and writing to the basename
--
-- >>> basename .~ "filename" $ "path/name.png"
-- "path/filename.png"
basename :: Lens' FilePath FilePath
basename f p = (<.> takeExtension p) . (takeDirectory p </>) <$> f (takeBaseName p)
{-# INLINE basename #-}


-- | A 'Lens' for reading and writing to the directory
--
-- >>> "long/path/name.txt" ^. directory
-- "long/path"
directory :: Lens' FilePath FilePath
directory f p = (</> takeFileName p) <$> f (takeDirectory p)
{-# INLINE directory #-}


-- | A 'Lens' for reading and writing to the extension
--
-- >>> extension .~ ".png" $ "path/name.txt"
-- "path/name.png"
extension :: Lens' FilePath FilePath
extension f p = (n <.>) <$> f e
 where
  (n, e) = splitExtension p
{-# INLINE extension #-}


-- | A 'Lens' for reading and writing to the full filename
--
-- >>> filename .~ "name.txt" $ "path/name.png"
-- "path/name.txt"
filename :: Lens' FilePath FilePath
filename f p = (takeDirectory p </>) <$> f (takeFileName p)
{-# INLINE filename #-}
