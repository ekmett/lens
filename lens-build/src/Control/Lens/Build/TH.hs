{-# LANGUAGE TemplateHaskellQuotes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Build.TH
-- Copyright   :  (C) 2026 Edward A. Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Stability   :  experimental
--
-- Template Haskell that generates the 'RecordBuilder' and 'SetField' instances
-- consumed by "Control.Lens.Build". Run it once per record:
--
-- > data Foo = Foo { _a :: Int, _b :: Bool } deriving (Eq, Show)
-- > makeBuild ''Foo
--
-- This is independent of @makeLenses@: the generated 'setField' uses record
-- update, not lenses. The field names it accepts are derived with lens's own
-- @_field -> field@ convention, so they coincide with the lenses if you also
-- run @makeLenses@ — but that is alignment, not a dependency.
--
-- Limitations (kept deliberately small for a first cut): single-constructor
-- /record/ types only, and /no type parameters/ — the field-seed trick also
-- assumes lazy fields (a strict or unboxed field would force the @undefined@
-- seed). 'makeBuild' fails with a clear message outside that scope.
-----------------------------------------------------------------------------
module Control.Lens.Build.TH
  ( makeBuild
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Datatype
  ( reifyDatatype
  , DatatypeInfo(..)
  , ConstructorInfo(..)
  , ConstructorVariant(..)
  )

import Control.Lens.TH (underscoreNoPrefixNamer, DefName(..))
import Control.Lens.Build
  ( RecordBuilder(AllFields, emptyRecord)
  , SetField(FieldVal, setField)
  )

-- | Generate the 'RecordBuilder' and per-field 'SetField' instances for a
-- single-constructor, monomorphic record type.
makeBuild :: Name -> DecsQ
makeBuild tyName = do
  di <- reifyDatatype tyName
  case datatypeInstTypes di of
    (_:_) ->
      fail "makeBuild: only monomorphic records (no type parameters) are supported"
    [] -> case datatypeCons di of
      [con] -> case constructorVariant con of
        RecordConstructor fieldNames ->
          buildInstances (datatypeName di) con fieldNames
        _ ->
          fail "makeBuild: a single record constructor with named fields is required"
      _ ->
        fail "makeBuild: single-constructor records only"

-- | Emit @instance RecordBuilder T@ and one @instance SetField T \"f\"@ per field.
buildInstances :: Name -> ConstructorInfo -> [Name] -> DecsQ
buildInstances tyName con fieldNames = do
  let conName    = constructorName con
      fieldTypes = constructorFields con
      -- lens-style names ("_a" -> "a"), matching what makeLenses produces.
      lensNames  = [ lensStr tyName fieldNames f | f <- fieldNames ]
  recDec  <- recordBuilderInstance tyName conName fieldNames lensNames
  setDecs <- sequence
    [ setFieldInstance tyName fname lname fty
    | (fname, lname, fty) <- zip3 fieldNames lensNames fieldTypes
    ]
  return (recDec : setDecs)

-- | The lens-style 'Symbol' for a field, via lens's own 'underscoreNoPrefixNamer'
-- (so @field \@\"a\"@ lines up with the lens @a@). Falls back to the raw field
-- name if the field doesn't fit the underscore convention.
lensStr :: Name -> [Name] -> Name -> String
lensStr tyName fieldNames fld =
  case underscoreNoPrefixNamer tyName fieldNames fld of
    (TopName n : _) -> nameBase n
    _               -> nameBase fld

-- | @instance RecordBuilder T where type AllFields T = '[...]; emptyRecord = T undefined ...@
recordBuilderInstance :: Name -> Name -> [Name] -> [String] -> Q Dec
recordBuilderInstance tyName conName fieldNames lensNames =
  instanceD (cxt []) (conT ''RecordBuilder `appT` conT tyName)
    [ tySynInstD $ tySynEqn Nothing
        (conT ''AllFields `appT` conT tyName)
        (promotedList [ litT (strTyLit s) | s <- lensNames ])
    , funD 'emptyRecord
        [ clause []
            (normalB (recConE conName [ fieldExp f (varE 'undefined) | f <- fieldNames ]))
            []
        ]
    ]

-- | @instance SetField T \"f\" where type FieldVal T \"f\" = Ty; setField x r = r { _f = x }@
setFieldInstance :: Name -> Name -> String -> Type -> Q Dec
setFieldInstance tyName fname lname fty = do
  x <- newName "x"
  r <- newName "r"
  instanceD (cxt [])
    (conT ''SetField `appT` conT tyName `appT` litT (strTyLit lname))
    [ tySynInstD $ tySynEqn Nothing
        (conT ''FieldVal `appT` conT tyName `appT` litT (strTyLit lname))
        (return fty)
    , funD 'setField
        [ clause [varP x, varP r]
            (normalB (recUpdE (varE r) [ fieldExp fname (varE x) ]))
            []
        ]
    ]

-- | Build a promoted type-level list @'[t1, t2, ...]@ from a list of types.
promotedList :: [TypeQ] -> TypeQ
promotedList = foldr (\t ts -> promotedConsT `appT` t `appT` ts) promotedNilT
