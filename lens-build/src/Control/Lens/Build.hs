{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Build
-- Copyright   :  (C) 2026 Edward A. Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Stability   :  experimental
--
-- Construct a single-constructor record from lens-style field assignments,
-- where /omitting a field is a compile-time type error/. This is the
-- "track partial construction in the type system" idea.
--
-- The set of fields assigned so far is carried in a phantom type-level list,
-- and 'build' only type-checks once that list covers every field of the record.
--
-- > data Foo = Foo { _a :: Int, _b :: Bool } deriving (Eq, Show)
-- > makeBuild ''Foo
-- >
-- > foo :: Foo
-- > foo = record & field @"a" 1 & field @"b" True & build
--
-- Leaving out @field \@\"b\"@ is rejected at compile time with
-- @build: missing field "b"@.
--
-- @makeBuild@ is self-contained — it does /not/ require @makeLenses@. The names
-- @field@ accepts are derived with the same @_field -> field@ convention, so
-- they line up with the lenses if you generate those too.
-----------------------------------------------------------------------------
module Control.Lens.Build
  ( -- * Building a record
    Builder
  , record
  , field
  , build
    -- * Per-record machinery
    -- $generated
  , RecordBuilder(..)
  , SetField(..)
    -- * Completeness checking
  , Complete
  , AllPresent
  , Elem
  ) where

import Data.Kind (Constraint)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))

-- | A partially-built record @s@, tagged with the type-level list @set@ of the
-- field names assigned so far.
--
-- It is a @newtype@ (a zero-cost wrapper, erased by the compiler to a
-- coercion) over the record itself: the underlying value starts with every
-- field bound to @undefined@ and gets overwritten field by field. The @set@
-- index is a /phantom/ parameter — it never appears in a constructor field, so
-- it carries compile-time information at no runtime cost.
--
-- The constructor is deliberately not exported: the only way to extract the
-- record is 'build', which refuses to compile until every field is set, so no
-- @undefined@ can escape.
newtype Builder s (set :: [Symbol]) = Builder s

-- | The supporting instances 'RecordBuilder' and 'SetField' are tedious to
-- write by hand; @Control.Lens.Build.TH.'Control.Lens.Build.TH.makeBuild'@
-- generates them from a record's declaration.

-- | What 'build' needs to know about a record type: its full field list and a
-- seed value with every field bound to @undefined@.
class RecordBuilder s where
  -- | All of @s@'s field names (lens-style, e.g. @\"a\"@ for @_a@), in
  -- declaration order. An /associated type family/: a type-level function whose
  -- result is fixed per instance, so @'AllFields' Foo@ reduces to @'[\"a\", \"b\"]@.
  type AllFields s :: [Symbol]
  -- | A value of @s@ with every field set to @undefined@. Safe to expose only
  -- because 'build' guarantees each field is overwritten before extraction.
  emptyRecord :: s

-- | How to set one named field of @s@.
class SetField s (name :: Symbol) where
  -- | The type stored in field @name@, e.g. @'FieldVal' Foo \"a\"@ reduces to @Int@.
  type FieldVal s name
  -- | Overwrite field @name@ with a new value.
  setField :: FieldVal s name -> s -> s

-- | Start building: an empty 'Builder' with no fields assigned yet.
record :: RecordBuilder s => Builder s '[]
record = Builder emptyRecord

-- | Assign one field, recording its name in the type-level @set@.
--
-- The field name is supplied by visible type application, e.g. @field \@\"a\" 1@.
-- Because @name@ only appears in the constraints and result, the signature is
-- ambiguous without that application (hence @AllowAmbiguousTypes@).
field
  :: forall name s set
   . SetField s name
  => FieldVal s name              -- ^ the new value for field @name@
  -> Builder s set                -- ^ the record so far
  -> Builder s (name ': set)      -- ^ same record, now with @name@ recorded as set
field x (Builder r) = Builder (setField @s @name x r)

-- | Finish building. Only type-checks when @set@ covers every field of @s@;
-- otherwise GHC reports the first missing field by name.
build :: Complete s set => Builder s set -> s
build (Builder r) = r

-- | The constraint "@set@ contains every field of @s@". Discharged when every
-- required field is present; otherwise reduces to a 'TypeError'.
type Complete s set = AllPresent (AllFields s) set

-- | @'AllPresent' required set@ holds iff every name in @required@ also appears
-- in @set@. A /closed type family/ returning a 'Constraint': it recurses over
-- @required@, demanding each element be present.
type family AllPresent (required :: [Symbol]) (set :: [Symbol]) :: Constraint where
  AllPresent '[]       _   = ()
  AllPresent (r ': rs) set = (Require r (Elem r set), AllPresent rs set)

-- | Is @x@ an element of the type-level list @xs@? The second clause matches a
-- repeated variable (@x@ in both positions), which a closed type family reads
-- as a type-equality test.
type family Elem (x :: Symbol) (xs :: [Symbol]) :: Bool where
  Elem _ '[]       = 'False
  Elem x (x ': _)  = 'True
  Elem x (_ ': ys) = Elem x ys

-- | Turn "field present?" into either a satisfied constraint or a friendly,
-- hand-written 'TypeError' naming the missing field.
type family Require (name :: Symbol) (present :: Bool) :: Constraint where
  Require _    'True  = ()
  Require name 'False =
    TypeError ('Text "build: missing field " ':<>: 'ShowType name)
