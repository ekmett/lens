{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.IO.Error.Lens
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module System.IO.Error.Lens where

import Control.Lens
import Control.Exception.Lens
import GHC.IO.Exception
import System.IO
import Foreign.C.Types

-- * IOException Lenses

-- | Where the error happened.
--
-- @
-- 'location' :: 'Lens''      'IOException'   'String'
-- 'location' :: 'Traversal'' 'SomeException' 'String'
-- @
location :: (AsIOException (->) f t, Functor f) => LensLike' f t String
location f = _IOException $ \s -> f (ioe_location s) <&> \e -> s { ioe_location = e }
{-# INLINE location #-}

-- | Error type specific information.
--
-- @
-- 'description' :: 'Lens''      'IOException'   'String'
-- 'description' :: 'Traversal'' 'SomeException' 'String'
-- @
description :: (AsIOException (->) f t, Functor f) => LensLike' f t String
description f = _IOException $ \s -> f (ioe_description s) <&> \e -> s { ioe_description = e }
{-# INLINE description #-}

-- | The handle used by the action flagging this error.
--
-- @
-- 'handle' :: 'Lens''      'IOException'   ('Maybe' 'Handle')
-- 'handle' :: 'Traversal'' 'SomeException' ('Maybe' 'Handle')
-- @
handle :: (AsIOException (->) f t, Functor f) => LensLike' f t (Maybe Handle)
handle f = _IOException $ \s -> f (ioe_handle s) <&> \e -> s { ioe_handle = e }
{-# INLINE handle #-}

-- | 'fileName' the error is related to.
--
-- @
-- 'fileName' :: 'Lens''      'IOException'   ('Maybe' 'FilePath')
-- 'fileName' :: 'Traversal'' 'SomeException' ('Maybe' 'FilePath')
-- @
fileName :: (AsIOException (->) f t, Functor f) => LensLike' f t (Maybe FilePath)
fileName f = _IOException $ \s -> f (ioe_filename s) <&> \e -> s { ioe_filename = e }
{-# INLINE fileName #-}

-- | 'errno' leading to this error, if any.
--
-- @
-- 'errno' :: 'Lens''      'IOException'   ('Maybe' 'FilePath')
-- 'errno' :: 'Traversal'' 'SomeException' ('Maybe' 'FilePath')
-- @
errno :: (AsIOException (->) f t, Functor f) => LensLike' f t (Maybe CInt)
errno f = _IOException $ \s -> f (ioe_errno s) <&> \e -> s { ioe_errno = e }
{-# INLINE errno #-}

------------------------------------------------------------------------------
-- Error Types
------------------------------------------------------------------------------

-- | What type of error it is
--
-- @
-- 'errorType' :: 'Lens''      'IOException'   'IOErrorType'
-- 'errorType' :: 'Traversal'' 'SomeException' 'IOErrorType'
-- @
errorType :: (AsIOException (->) f t, Functor f) => LensLike' f t IOErrorType
errorType f = _IOException $ \s -> f (ioe_type s) <&> \e -> s { ioe_type = e }
{-# INLINE errorType #-}

-- * IOErrorType Prisms
--
-- (These prisms are generated automatically)

makePrisms ''IOErrorType
