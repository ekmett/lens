{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.IO.Error.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module System.IO.Error.Lens where

import Control.Lens
import Control.Exception.Lens as Lens
import GHC.IO.Exception
import System.IO
import Foreign.C.Types

-- | where the error happened.
--
-- @
-- 'location' :: 'Lens''      'IOException'   'String'
-- 'location' :: 'Traversal'' 'SomeException' 'String'
-- @
location :: (AsIOException (->) f t, Functor f) => LensLike' f t String
location f = ioErr . \ f s -> f (ioe_location s) <&> \e -> s { ioe_location = e }

-- | Error type specific information.
--
-- @
-- 'description' :: 'Lens''      'IOException'   'String'
-- 'description' :: 'Traversal'' 'SomeException' 'String'
-- @
description :: (AsIOException (->) f t, Functor f) => LensLike' f t String
description = ioErr . \f s -> f (ioe_description s) <&> \e -> s { ioe_description = e }

-- | the handle used by the action flagging this error.
--
-- @
-- 'handle' :: 'Lens''      'IOException'   ('Maybe' 'Handle')
-- 'handle' :: 'Traversal'' 'SomeException' ('Maybe' 'Handle')
-- @
handle :: (AsIOException (->) f t, Functor f) => LensLike' f t (Maybe Handle)
handle = ioErr . \f s -> f (ioe_handle s) <&> \e -> s { ioe_handle = e }

-- | 'fileName' the error is related to.
--
-- @
-- 'fileName' :: 'Lens''      'IOException'   ('Maybe' 'FilePath')
-- 'fileName' :: 'Traversal'' 'SomeException' ('Maybe' 'FilePath')
-- @
fileName :: (AsIOException (->) f t, Functor f) => LensLike' f t (Maybe FilePath)
fileName = ioErr . \f s -> f (ioe_filename s) <&> \e -> s { ioe_filename = e }

-- | 'errno' leading to this error, if any.
--
-- @
-- 'errno' :: 'Lens''      'IOException'   ('Maybe' 'FilePath')
-- 'errno' :: 'Traversal'' 'SomeException' ('Maybe' 'FilePath')
-- @
errno :: (AsIOException (->) f t, Functor f) => LensLike' f t (Maybe CInt)
errno = ioErr . \f s -> f (ioe_errno s) <&> \e -> s { ioe_errno = e }

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
errorType = ioErr . \f s -> f (ioe_type s) <&> \e -> s { ioe_type = e }

makePrisms ''IOErrorType
