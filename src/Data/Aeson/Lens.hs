{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013-2014, (c) Paul Wilson 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Aeson.Lens
  (
  -- * Numbers
    AsNumber(..)
  , _Integral
  , nonNull
  -- * Primitive
  , Primitive(..)
  , AsPrimitive(..)
  -- * Objects and Arrays
  , AsValue(..)
  , key, members
  , nth, values
  -- * Decoding
  , AsJSON(..)
  ) where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Scientific
import Data.ByteString.Lazy.Char8 as Lazy hiding (putStrLn)
import Data.ByteString.Lazy.UTF8 as UTF8 hiding (decode)
import Data.Data
import Data.HashMap.Strict (HashMap)
import Data.Text
import Data.Vector (Vector)
import Prelude hiding (null)

-- $setup
-- >>> :set -XOverloadedStrings

------------------------------------------------------------------------------
-- Scientific prisms
------------------------------------------------------------------------------

class AsNumber t where
  -- |
  -- >>> "[1, \"x\"]" ^? nth 0 . _Number
  -- Just 1
  --
  -- >>> "[1, \"x\"]" ^? nth 1 . _Number
  -- Nothing
  _Number :: Prism' t Scientific
#ifndef HLINT
  default _Number :: AsPrimitive t => Prism' t Scientific
  _Number = _Primitive._Number
#endif

  -- |
  -- Prism into an 'Double' over a 'Value', 'Primitive' or 'Scientific'
  --
  -- >>> "[10.2]" ^? nth 0 . _Double
  -- Just 10.2
  _Double :: Prism' t Double
  _Double = _Number.iso realToFrac realToFrac

  -- |
  -- Prism into an 'Integer' over a 'Value', 'Primitive' or 'Scientific'
  --
  -- >>> "[10]" ^? nth 0 . _Integer
  -- Just 10
  --
  -- >>> "[10.5]" ^? nth 0 . _Integer
  -- Nothing
  _Integer :: Prism' t Integer
  _Integer = _Number.iso floor fromIntegral

instance AsNumber Value where
  _Number = prism Number $ \v -> case v of Number n -> Right n; _ -> Left v

instance AsNumber Scientific where
  _Number = id

instance AsNumber ByteString
instance AsNumber String

------------------------------------------------------------------------------
-- Conversion Prisms
------------------------------------------------------------------------------

-- | Access Integer 'Value's as Integrals.
--
-- >>> "[10]" ^? nth 0 . _Integral
-- Just 10
--
-- >>> "[10.5]" ^? nth 0 . _Integral
-- Nothing
_Integral :: (AsNumber t, Integral a) => Prism' t a
_Integral = _Number . iso floor fromIntegral

------------------------------------------------------------------------------
-- Null values and primitives
------------------------------------------------------------------------------

-- | Primitives of 'Value'
data Primitive
  = StringPrim !Text
  | NumberPrim !Scientific
  | BoolPrim !Bool
  | NullPrim
  deriving (Eq,Ord,Show,Data,Typeable)

instance AsNumber Primitive where
  _Number = prism NumberPrim $ \v -> case v of NumberPrim s -> Right s; _ -> Left v

class AsNumber t => AsPrimitive t where
  -- |
  -- >>> "[1, \"x\", null, true, false]" ^? nth 0 . _Primitive
  -- Just (ScientificPrim 1)
  --
  -- >>> "[1, \"x\", null, true, false]" ^? nth 1 . _Primitive
  -- Just (StringPrim "x")
  --
  -- >>> "[1, \"x\", null, true, false]" ^? nth 2 . _Primitive
  -- Just NullPrim
  --
  -- >>> "[1, \"x\", null, true, false]" ^? nth 3 . _Primitive
  -- Just (BoolPrim True)
  --
  -- >>> "[1, \"x\", null, true, false]" ^? nth 4 . _Primitive
  -- Just (BoolPrim False)
  _Primitive :: Prism' t Primitive
#ifndef HLINT
  default _Primitive :: AsValue t => Prism' t Primitive
  _Primitive = _Value._Primitive
#endif

  -- "{\"a\": \"xyz\", \"b\": true}" ^? key "a" . _String
  -- Just "xyz"
  --
  -- >>> "{\"a\": \"xyz\", \"b\": true}" ^? key "b" . _String
  -- Nothing
  _String :: Prism' t Text
  _String = _Primitive.prism StringPrim (\v -> case v of StringPrim s -> Right s; _ -> Left v)

  -- >>> "{\"a\": \"xyz\", \"b\": true}" ^? key "b" . _Bool
  -- Just True
  --
  -- "{\"a\": \"xyz\", \"b\": true}" ^? key "a" . _Bool
  -- Nothing
  _Bool :: Prism' t Bool
  _Bool = _Primitive.prism BoolPrim (\v -> case v of BoolPrim b -> Right b; _ -> Left v)

  -- >>> "{\"a\": \"xyz\", \"b\": null}" ^? key "b" . _Null
  -- Just ()
  --
  -- >>> "{\"a\": \"xyz\", \"b\": null}" ^? key "a" . _Null
  -- Nothing
  _Null :: Prism' t ()
  _Null = _Primitive.prism (const NullPrim) (\v -> case v of NullPrim -> Right (); _ -> Left v)

instance AsPrimitive Value where
  _Primitive = prism fromPrim toPrim
    where
      toPrim (String s) = Right $ StringPrim s
      toPrim (Number n) = Right $ NumberPrim n
      toPrim (Bool b)   = Right $ BoolPrim b
      toPrim Null       = Right NullPrim
      toPrim v          = Left v
      fromPrim (StringPrim s) = String s
      fromPrim (NumberPrim n) = Number n
      fromPrim (BoolPrim b)   = Bool b
      fromPrim NullPrim       = Null

  _String = prism String $ \v -> case v of String s -> Right s; _ -> Left v
  _Bool = prism Bool (\v -> case v of Bool b -> Right b; _ -> Left v)
  _Null = prism (const Null) (\v -> case v of Null -> Right (); _ -> Left v)

instance AsPrimitive ByteString
instance AsPrimitive String

instance AsPrimitive Primitive where
  _Primitive = id

-- | Prism into non-'Null' values
--
-- >>> "{\"a\": \"xyz\", \"b\": null}" ^? key "a" . nonNull
-- Just (String "xyz")
--
-- >>> "{\"a\": {}, \"b\": null}" ^? key "a" . nonNull
-- Just (Object fromList [])
--
-- >>> "{\"a\": \"xyz\", \"b\": null}" ^? key "b" . nonNull
-- Nothing
nonNull :: Prism' Value Value
nonNull = prism id (\v -> if isn't _Null v then Right v else Left v)

------------------------------------------------------------------------------
-- Non-primitive traversals
------------------------------------------------------------------------------

class AsPrimitive t => AsValue t where
  -- |
  -- >>>"[1,2,3]" ^? _Value
  -- Just (Array (fromList [Number 1,Number 2,Number 3]))
  _Value :: Prism' t Value

  -- |
  -- >>> "{\"a\": {}, \"b\": null}" ^? key "a" . _Object
  -- Just fromList []
  --
  -- >>> "{\"a\": {}, \"b\": null}" ^? key "b" . _Object
  -- Nothing
  _Object :: Prism' t (HashMap Text Value)
  _Object = _Value.prism Object (\v -> case v of Object o -> Right o; _ -> Left v)

  -- |
  -- >>> "[1,2,3]" ^? _Array
  -- Just (fromList [Number 1,Number 2,Number 3])
  _Array :: Prism' t (Vector Value)
  _Array = _Value.prism Array (\v -> case v of Array a -> Right a; _ -> Left v)

instance AsValue Value where
  _Value = id

instance AsValue ByteString where
  _Value = _JSON

instance AsValue String where
  _Value = iso UTF8.fromString UTF8.toString._Value

-- |
-- Like 'ix', but for 'Object' with Text indices. This often has better
-- inference than 'ix' when used with OverloadedStrings.
--
-- >>> "{\"a\": 100, \"b\": 200}" ^? key "a"
-- Just (Number 100)
--
-- >>> "[1,2,3]" ^? key "a"
-- Nothing
key :: AsValue t => Text -> Traversal' t Value
key i = _Object . ix i

members :: AsValue t => IndexedTraversal' Text t Value
members = _Object . itraversed

-- | Like 'ix', but for Arrays with Int indexes
--
-- >>> "[1,2,3]" ^? nth 1
-- Just (Number 2)
--
-- >>> "\"a\": 100, \"b\": 200}" ^? nth 1
-- Nothing
--
-- >>> "[1,2,3]" & nth 1 .~ Number 20
-- "[1,20,3]"
nth :: AsValue t => Int -> Traversal' t Value
nth i = _Array . ix i

values :: AsValue t => IndexedTraversal' Int t Value
values = _Array . traversed

class AsJSON t where
  -- | A Prism into 'Value' on lazy 'ByteString's.
  _JSON :: (FromJSON a, ToJSON a) => Prism' t a

instance AsJSON Lazy.ByteString where
  _JSON = prism' encode decode

instance AsJSON String where
  _JSON = iso UTF8.fromString UTF8.toString._JSON

------------------------------------------------------------------------------
-- Orphan instances
------------------------------------------------------------------------------

type instance Index Value = Text
type instance IxValue Value = Value

instance Ixed Value where
  ix i = _Object.ix i

instance Contains Value where
  contains i f (Object o) = coerce (contains i f o)
  contains i f _ = coerce (indexed f i False)

instance Plated Value where
  plate f (Object o) = Object <$> traverse f o
  plate f (Array a) = Array <$> traverse f a
  plate _ xs = pure xs
