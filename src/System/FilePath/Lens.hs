{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  System.FilePath.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module System.FilePath.Lens
  (
  -- * Operators
    (</>~), (<</>~), (<<</>~), (<.>~), (<<.>~), (<<<.>~)
  , (</>=), (<</>=), (<<</>=), (<.>=), (<<.>=), (<<<.>=)
  -- * Lenses
  , basename, directory, extension, filename
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Control.Monad.State as State
import System.FilePath
  ( (</>), (<.>), splitExtension
  , takeBaseName, takeDirectory
  , takeExtension, takeFileName
  )

import Control.Lens hiding ((<.>))

-- $setup
-- >>> :set -XNoOverloadedStrings

{- NB: Be very careful if you are planning to modify the doctest output in
this module! Path separators are OS-dependent (\\ with Windows, / with Posix),
so we take great care to avoid using separators in doctest output so that they
will be valid on all operating systems.

If you find yourself wanting to test a function that uses path separators in
the output, it would be wise to:

1. Compare the tested expression and the expected results explicitly using (==).
2. Always use the </> function (and derived combinators) to construct path
   separators instead of typing them manually. That is, don't type out
   "foo/bar", but rather "foo" </> "bar".

This way we can avoid leaking path separators into the output. See the doctest
example for (</>~) for an example of how to do this.
-}

infixr 4 </>~, <</>~, <<</>~, <.>~, <<.>~, <<<.>~
infix 4 </>=, <</>=, <<</>=, <.>=, <<.>=, <<<.>=

-- | Modify the path by adding another path.
--
-- >>> (both </>~ "bin" $ ("hello","world")) == ("hello" </> "bin", "world" </> "bin")
-- True
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


-- | Modify the target(s) of a 'Lens'', 'Iso'', 'Setter'' or 'Traversal'' by adding a path.
--
-- >>> execState (both </>= "bin") ("hello","world") == ("hello" </> "bin", "world" </> "bin")
-- True
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


(<<</>~) :: Optical' (->) q ((,)FilePath) s FilePath -> FilePath -> q s (FilePath, s)
l <<</>~ b = l $ \a -> (a, a </> b)
{-# INLINE (<<</>~) #-}

(<<</>=) :: MonadState s m => LensLike' ((,)FilePath) s FilePath -> FilePath -> m FilePath
l <<</>= b = l %%= \a -> (a, a </> b)
{-# INLINE (<<</>=) #-}

-- | Modify the path by adding an extension.
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

-- | Modify the target(s) of a 'Lens'', 'Iso'', 'Setter'' or 'Traversal'' by adding an extension.
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
-- >>> evalState (_1 <<.>= "txt") ("hello","world")
-- "hello.txt"
--
-- When you do not need the result of the operation, ('<.>=') is more flexible.
(<<.>=) :: MonadState s m => LensLike' ((,)FilePath) s FilePath -> String -> m FilePath
l <<.>= r = l <%= (<.> r)
{-# INLINE (<<.>=) #-}

-- | Add an extension onto the end of the target of a 'Lens' but
-- return the old value
--
-- >>> _1 <<<.>~ "txt" $ ("hello","world")
-- ("hello",("hello.txt","world"))
--
-- When you do not need the old value, ('<.>~') is more flexible.
(<<<.>~) :: Optical' (->) q ((,)FilePath) s FilePath -> String -> q s (FilePath, s)
l <<<.>~ b = l $ \a -> (a, a <.> b)
{-# INLINE (<<<.>~) #-}

(<<<.>=) :: MonadState s m => LensLike' ((,)FilePath) s FilePath -> String -> m FilePath
l <<<.>= b = l %%= \a -> (a, a <.> b)
{-# INLINE (<<<.>=) #-}

-- | A 'Lens' for reading and writing to the basename
--
-- Note: This is 'not' a legal 'Lens' unless the outer 'FilePath' has both a directory
-- and filename component and the generated basenames are not null and contain no directory
-- separators.
--
-- >>> (basename .~ "filename" $ "path" </> "name.png") == "path" </> "filename.png"
-- True
basename :: Lens' FilePath FilePath
basename f p = (<.> takeExtension p) . (takeDirectory p </>) <$> f (takeBaseName p)
{-# INLINE basename #-}


-- | A 'Lens' for reading and writing to the directory
--
-- Note: this is /not/ a legal 'Lens' unless the outer 'FilePath' already has a directory component,
-- and generated directories are not null.
--
-- >>> (("long" </> "path" </> "name.txt") ^. directory) == "long" </> "path"
-- True
directory :: Lens' FilePath FilePath
directory f p = (</> takeFileName p) <$> f (takeDirectory p)
{-# INLINE directory #-}


-- | A 'Lens' for reading and writing to the extension
--
-- Note: This is /not/ a legal 'Lens', unless you are careful to ensure that generated
-- extension 'FilePath' components are either null or start with 'System.FilePath.extSeparator'
-- and do not contain any internal 'System.FilePath.extSeparator's.
--
-- >>> (extension .~ ".png" $ "path" </> "name.txt") == "path" </> "name.png"
-- True
extension :: Lens' FilePath FilePath
extension f p = (n <.>) <$> f e
 where
  (n, e) = splitExtension p
{-# INLINE extension #-}


-- | A 'Lens' for reading and writing to the full filename
--
-- Note: This is /not/ a legal 'Lens', unless you are careful to ensure that generated
-- filename 'FilePath' components are not null and do not contain any
-- elements of 'System.FilePath.pathSeparators's.
--
-- >>> (filename .~ "name.txt" $ "path" </> "name.png") == "path" </> "name.txt"
-- True
filename :: Lens' FilePath FilePath
filename f p = (takeDirectory p </>) <$> f (takeFileName p)
{-# INLINE filename #-}
