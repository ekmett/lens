{-# LANGUAGE CPP #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#ifdef HLINT
{-# ANN module "HLint: ignore Use camelCase" #-}
#endif

#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) (defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706)
#endif

#ifndef MIN_VERSION_containers
#define MIN_VERSION_containers(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.TH
-- Copyright   :  (C) 2013-2014 Edward Kmett, 2013 Eric Mertens
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Version (showVersion)
import Paths_lens (version)

-- | Compatibility shim for recent changes to template haskell's 'tySynInstD'
tySynInstD' :: Name -> [TypeQ] -> TypeQ -> DecQ
#if MIN_VERSION_template_haskell(2,9,0)
tySynInstD' fam ts r = tySynInstD fam (tySynEqn ts r)
#else
tySynInstD' = tySynInstD
#endif

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


-- | Return 'Name' contained in a 'TyVarBndr'.
bndrName :: TyVarBndr -> Name
bndrName (PlainTV  n  ) = n
bndrName (KindedTV n _) = n

fromSet :: Ord k => (k -> v) -> Set.Set k -> Map.Map k v
#if MIN_VERSION_containers(0,5,0)
fromSet = Map.fromSet
#else
fromSet f x = Map.fromList [ (k,f k) | k <- Set.toList x ]
#endif

------------------------------------------------------------------------
-- Manually quoted names
------------------------------------------------------------------------
-- By manually generating these names we avoid needing to use the
-- TemplateHaskell language extension when compiling the lens library.
-- This allows the library to be used in stage1 cross-compilers.

mkBaseName :: NameSpace -> String -> String -> Name
mkBaseName ns = mkNameG ns "base"

mkLensName :: NameSpace -> String -> String -> Name
mkLensName ns = mkNameG ns ("lens-" ++ showVersion version)

traversalTypeName      :: Name
traversalTypeName       = mkLensName TcClsName "Control.Lens.Type" "Traversal"

traversal'TypeName     :: Name
traversal'TypeName      = mkLensName TcClsName "Control.Lens.Type" "Traversal'"

lensTypeName           :: Name
lensTypeName            = mkLensName TcClsName "Control.Lens.Type" "Lens"

lens'TypeName          :: Name
lens'TypeName           = mkLensName TcClsName "Control.Lens.Type" "Lens'"

isoTypeName            :: Name
isoTypeName             = mkLensName TcClsName "Control.Lens.Type" "Iso"

iso'TypeName           :: Name
iso'TypeName            = mkLensName TcClsName "Control.Lens.Type" "Iso'"

getterTypeName         :: Name
getterTypeName          = mkLensName TcClsName "Control.Lens.Type" "Getter"

foldTypeName           :: Name
foldTypeName            = mkLensName TcClsName "Control.Lens.Type" "Fold"

prismTypeName          :: Name
prismTypeName           = mkLensName TcClsName "Control.Lens.Type" "Prism"

prism'TypeName         :: Name
prism'TypeName          = mkLensName TcClsName "Control.Lens.Type" "Prism'"

reviewTypeName          :: Name
reviewTypeName           = mkLensName TcClsName "Control.Lens.Type" "Review"

review'TypeName         :: Name
review'TypeName          = mkLensName TcClsName "Control.Lens.Type" "Review'"

wrappedTypeName         :: Name
wrappedTypeName          = mkLensName TcClsName "Control.Lens.Wrapped" "Wrapped"

unwrappedTypeName       :: Name
unwrappedTypeName        = mkLensName TcClsName "Control.Lens.Wrapped" "Unwrapped"

rewrappedTypeName       :: Name
rewrappedTypeName        = mkLensName TcClsName "Control.Lens.Wrapped" "Rewrapped"

_wrapped'ValName        :: Name
_wrapped'ValName         = mkLensName VarName "Control.Lens.Wrapped" "_Wrapped'"

isoValName              :: Name
isoValName               = mkLensName VarName "Control.Lens.Iso" "iso"

prismValName            :: Name
prismValName             = mkLensName VarName "Control.Lens.Prism" "prism"

untoValName             :: Name
untoValName              = mkLensName VarName "Control.Lens.Review" "unto"

coerceValName           :: Name
coerceValName            = mkLensName VarName "Control.Lens.Internal.Getter" "coerce"

composeValName          :: Name
composeValName           = mkBaseName VarName "GHC.Base" "."

idValName               :: Name
idValName                = mkBaseName VarName "GHC.Base" "id"

fmapValName             :: Name
fmapValName              = mkBaseName VarName "GHC.Base" "fmap"

pureValName             :: Name
pureValName              = mkBaseName VarName "Control.Applicative" "pure"

apValName               :: Name
apValName                = mkBaseName VarName "Control.Applicative" "<*>"

rightDataName           :: Name
rightDataName            = mkBaseName DataName "Data.Either" "Right"

leftDataName            :: Name
leftDataName             = mkBaseName DataName "Data.Either" "Left"
