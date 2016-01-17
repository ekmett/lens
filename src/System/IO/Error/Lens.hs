{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.IO.Error.Lens
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module System.IO.Error.Lens where

import Control.Lens
import GHC.IO.Exception
import System.IO
import Foreign.C.Types

-- * IOException Lenses

-- | Where the error happened.
location :: Lens' IOException String
location f s = f (ioe_location s) <&> \e -> s { ioe_location = e }
{-# INLINE location #-}

-- | Error type specific information.
description :: Lens' IOException String
description f s = f (ioe_description s) <&> \e -> s { ioe_description = e }
{-# INLINE description #-}

-- | The handle used by the action flagging this error.
handle :: Lens' IOException (Maybe Handle)
handle f s = f (ioe_handle s) <&> \e -> s { ioe_handle = e }
{-# INLINE handle #-}

-- | 'fileName' the error is related to.
--
fileName :: Lens' IOException (Maybe FilePath)
fileName f s = f (ioe_filename s) <&> \e -> s { ioe_filename = e }
{-# INLINE fileName #-}

-- | 'errno' leading to this error, if any.
--
errno :: Lens' IOException (Maybe CInt)
errno f s = f (ioe_errno s) <&> \e -> s { ioe_errno = e }
{-# INLINE errno #-}

------------------------------------------------------------------------------
-- Error Types
------------------------------------------------------------------------------

-- | What type of error it is

errorType :: Lens' IOException IOErrorType
errorType f s = f (ioe_type s) <&> \e -> s { ioe_type = e }
{-# INLINE errorType #-}

-- * IOErrorType Prisms
--

_AlreadyExists :: Prism' IOErrorType ()
_AlreadyExists = only AlreadyExists

_NoSuchThing :: Prism' IOErrorType ()
_NoSuchThing = only NoSuchThing

_ResourceBusy :: Prism' IOErrorType ()
_ResourceBusy = only ResourceBusy

_ResourceExhausted :: Prism' IOErrorType ()
_ResourceExhausted = only ResourceExhausted

_EOF :: Prism' IOErrorType ()
_EOF = only EOF

_IllegalOperation :: Prism' IOErrorType ()
_IllegalOperation = only IllegalOperation

_PermissionDenied :: Prism' IOErrorType ()
_PermissionDenied = only PermissionDenied

_UserError :: Prism' IOErrorType ()
_UserError = only UserError

_UnsatisfiedConstraints :: Prism' IOErrorType ()
_UnsatisfiedConstraints = only UnsatisfiedConstraints

_SystemError :: Prism' IOErrorType ()
_SystemError = only SystemError

_ProtocolError :: Prism' IOErrorType ()
_ProtocolError = only ProtocolError

_OtherError :: Prism' IOErrorType ()
_OtherError = only OtherError

_InvalidArgument :: Prism' IOErrorType ()
_InvalidArgument = only InvalidArgument

_InappropriateType :: Prism' IOErrorType ()
_InappropriateType = only InappropriateType

_HardwareFault :: Prism' IOErrorType ()
_HardwareFault = only HardwareFault

_UnsupportedOperation :: Prism' IOErrorType ()
_UnsupportedOperation = only UnsupportedOperation

_TimeExpired :: Prism' IOErrorType ()
_TimeExpired = only TimeExpired

_ResourceVanished :: Prism' IOErrorType ()
_ResourceVanished = only ResourceVanished

_Interrupted :: Prism' IOErrorType ()
_Interrupted = only Interrupted
