{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
#ifdef TRUSTWORTHY
# if MIN_VERSION_template_haskell(2,12,0)
{-# LANGUAGE Safe #-}
# else
{-# LANGUAGE Trustworthy #-}
# endif
#endif

#include "lens-common.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.TH
-- Copyright   :  (C) 2013-2016 Edward Kmett and Eric Mertens
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.TH where

import Control.Lens.Iso
import Control.Lens.Prism
import Control.Lens.Review
import Control.Lens.Type
import Control.Lens.Wrapped
import Data.Functor.Contravariant
import qualified Data.Set as Set
import Data.Set (Set)
import Language.Haskell.TH
import qualified Language.Haskell.TH.Datatype as D
import qualified Language.Haskell.TH.Datatype.TyVarBndr as D

-- | Apply arguments to a type constructor
appsT :: TypeQ -> [TypeQ] -> TypeQ
appsT = foldl appT

-- | Apply arguments to a function
appsE1 :: ExpQ -> [ExpQ] -> ExpQ
appsE1 = foldl appE

-- | Construct a tuple type given a list of types.
toTupleT :: [TypeQ] -> TypeQ
toTupleT [x] = x
toTupleT xs = appsT (tupleT (length xs)) xs

-- | Construct a tuple value given a list of expressions.
toTupleE :: [ExpQ] -> ExpQ
toTupleE [x] = x
toTupleE xs = tupE xs

-- | Construct a tuple pattern given a list of patterns.
toTupleP :: [PatQ] -> PatQ
toTupleP [x] = x
toTupleP xs = tupP xs

-- | Apply arguments to a type constructor.
conAppsT :: Name -> [Type] -> Type
conAppsT conName = foldl AppT (ConT conName)

-- | Generate many new names from a given base name.
newNames :: String {- ^ base name -} -> Int {- ^ count -} -> Q [Name]
newNames base n = sequence [ newName (base++show i) | i <- [1..n] ]

-- | Decompose an applied type into its individual components. For example, this:
--
-- @
-- Either Int Char
-- @
--
-- would be unfolded to this:
--
-- @
-- ('ConT' ''Either, ['ConT' ''Int, 'ConT' ''Char])
-- @
--
-- This function ignores explicit parentheses and visible kind applications.
unfoldType :: Type -> (Type, [Type])
unfoldType = go []
  where
    go :: [Type] -> Type -> (Type, [Type])
    go acc (ForallT _ _ ty) = go acc ty
    go acc (AppT ty1 ty2)   = go (ty2:acc) ty1
    go acc (SigT ty _)      = go acc ty
    go acc (ParensT ty)     = go acc ty
#if MIN_VERSION_template_haskell(2,15,0)
    go acc (AppKindT ty _)  = go acc ty
#endif
    go acc ty               = (ty, acc)

-- Construct a 'Type' using the datatype's type constructor and type
-- parameters. Unlike 'D.datatypeType', kind signatures are preserved to
-- some extent. (See the comments for 'dropSigsIfNonDataFam' below for more
-- details on this.)
datatypeTypeKinded :: D.DatatypeInfo -> Type
datatypeTypeKinded di
  = foldl AppT (ConT (D.datatypeName di))
  $ dropSigsIfNonDataFam
  $ D.datatypeInstTypes di
  where
    {-
    In an effort to prevent users from having to enable KindSignatures every
    time that they use lens' TH functionality, we strip off reified kind
    annotations from when:

    1. The kind of a type does not contain any kind variables. If it *does*
       contain kind variables, we want to preserve them so that we can generate
       type signatures that preserve the dependency order of kind and type
       variables. (The data types in test/T917.hs contain examples where this
       is important.) This will require enabling `PolyKinds`, but since
       `PolyKinds` implies `KindSignatures`, we can at least accomplish two
       things at once.
    2. The data type is not an instance of a data family. We make an exception
       for data family instances, since the presence or absence of a kind
       annotation can be the difference between typechecking or not.
       (See T917DataFam in tests/T917.hs for an example.) Moreover, the
       `TypeFamilies` extension implies `KindSignatures`.
    -}
    dropSigsIfNonDataFam :: [Type] -> [Type]
    dropSigsIfNonDataFam
      | isDataFamily (D.datatypeVariant di) = id
      | otherwise                           = map dropSig

    dropSig :: Type -> Type
    dropSig (SigT t k) | null (D.freeVariables k) = t
    dropSig t                                     = t

-- | Template Haskell wants type variables declared in a forall, so
-- we find all free type variables in a given type and declare them.
quantifyType :: Cxt -> Type -> Type
quantifyType = quantifyType' Set.empty

-- | This function works like 'quantifyType' except that it takes
-- a list of variables to exclude from quantification.
quantifyType' :: Set Name -> Cxt -> Type -> Type
quantifyType' exclude c t = ForallT vs c t
  where
  vs = filter (\tvb -> D.tvName tvb `Set.notMember` exclude)
     $ D.changeTVFlags D.SpecifiedSpec
     $ D.freeVariablesWellScoped (t:c) -- stable order

-- | Convert a 'TyVarBndr' into its corresponding 'Type'.
tvbToType :: D.TyVarBndr_ flag -> Type
tvbToType = D.elimTV VarT (SigT . VarT)

-- | Peel off a kind signature from a Type (if it has one).
unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

isDataFamily :: D.DatatypeVariant -> Bool
isDataFamily D.Datatype        = False
isDataFamily D.Newtype         = False
isDataFamily D.DataInstance    = True
isDataFamily D.NewtypeInstance = True

------------------------------------------------------------------------
-- TH-quoted names
------------------------------------------------------------------------
-- Note that this module only TemplateHaskellQuotes, not TemplateHaskell,
-- which makes lens able to be used in stage1 cross-compilers.

traversalTypeName      :: Name
traversalTypeName       = ''Traversal

traversal'TypeName     :: Name
traversal'TypeName      = ''Traversal'

lensTypeName           :: Name
lensTypeName            = ''Lens

lens'TypeName          :: Name
lens'TypeName           = ''Lens'

isoTypeName            :: Name
isoTypeName             = ''Iso

iso'TypeName           :: Name
iso'TypeName            = ''Iso'

getterTypeName         :: Name
getterTypeName          = ''Getter

foldTypeName           :: Name
foldTypeName            = ''Fold

prismTypeName          :: Name
prismTypeName           = ''Prism

prism'TypeName         :: Name
prism'TypeName          = ''Prism'

reviewTypeName          :: Name
reviewTypeName           = ''Review

wrappedTypeName         :: Name
wrappedTypeName          = ''Wrapped

unwrappedTypeName       :: Name
unwrappedTypeName        = ''Unwrapped

rewrappedTypeName       :: Name
rewrappedTypeName        = ''Rewrapped

_wrapped'ValName        :: Name
_wrapped'ValName         = '_Wrapped'

isoValName              :: Name
isoValName               = 'iso

prismValName            :: Name
prismValName             = 'prism

untoValName             :: Name
untoValName              = 'unto

phantomValName          :: Name
phantomValName           = 'phantom2

phantom2 :: (Functor f, Contravariant f) => f a -> f b
phantom2 = phantom
{-# INLINE phantom2 #-}

composeValName          :: Name
composeValName           = '(.)

idValName               :: Name
idValName                = 'id

fmapValName             :: Name
fmapValName              = 'fmap

pureValName             :: Name
pureValName              = 'pure

apValName               :: Name
apValName                = '(<*>)

rightDataName           :: Name
rightDataName            = 'Right

leftDataName            :: Name
leftDataName             = 'Left


------------------------------------------------------------------------
-- Support for generating inline pragmas
------------------------------------------------------------------------

inlinePragma :: Name -> [DecQ]
inlinePragma methodName = [pragInlD methodName Inline FunLike AllPhases]
