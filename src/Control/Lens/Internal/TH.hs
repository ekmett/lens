{-# LANGUAGE CPP #-}
#ifdef TRUSTWORTHY
# if MIN_VERSION_template_haskell(2,12,0)
{-# LANGUAGE Safe #-}
# else
{-# LANGUAGE Trustworthy #-}
# endif
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

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
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

import Data.Functor.Contravariant
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Map as Map
import qualified Data.Set as Set
#ifndef CURRENT_PACKAGE_KEY
import Data.Version (showVersion)
import Paths_lens (version)
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

fromSet :: (k -> v) -> Set.Set k -> Map.Map k v
#if MIN_VERSION_containers(0,5,0)
fromSet = Map.fromSet
#else
fromSet f x = Map.fromDistinctAscList [ (k,f k) | k <- Set.toAscList x ]
#endif

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
#if MIN_VERSION_template_haskell(2,11,0)
    go acc (ParensT ty)     = go acc ty
#endif
#if MIN_VERSION_template_haskell(2,15,0)
    go acc (AppKindT ty _)  = go acc ty
#endif
    go acc ty               = (ty, acc)

------------------------------------------------------------------------
-- Manually quoted names
------------------------------------------------------------------------
-- By manually generating these names we avoid needing to use the
-- TemplateHaskell language extension when compiling the lens library.
-- This allows the library to be used in stage1 cross-compilers.

lensPackageKey         :: String
#ifdef CURRENT_PACKAGE_KEY
lensPackageKey          = CURRENT_PACKAGE_KEY
#else
lensPackageKey          = "lens-" ++ showVersion version
#endif

mkLensName_tc          :: String -> String -> Name
mkLensName_tc           = mkNameG_tc lensPackageKey

mkLensName_v           :: String -> String -> Name
mkLensName_v            = mkNameG_v lensPackageKey

traversalTypeName      :: Name
traversalTypeName       = mkLensName_tc "Control.Lens.Type" "Traversal"

traversal'TypeName     :: Name
traversal'TypeName      = mkLensName_tc "Control.Lens.Type" "Traversal'"

lensTypeName           :: Name
lensTypeName            = mkLensName_tc "Control.Lens.Type" "Lens"

lens'TypeName          :: Name
lens'TypeName           = mkLensName_tc "Control.Lens.Type" "Lens'"

isoTypeName            :: Name
isoTypeName             = mkLensName_tc "Control.Lens.Type" "Iso"

iso'TypeName           :: Name
iso'TypeName            = mkLensName_tc "Control.Lens.Type" "Iso'"

getterTypeName         :: Name
getterTypeName          = mkLensName_tc "Control.Lens.Type" "Getter"

foldTypeName           :: Name
foldTypeName            = mkLensName_tc "Control.Lens.Type" "Fold"

prismTypeName          :: Name
prismTypeName           = mkLensName_tc "Control.Lens.Type" "Prism"

prism'TypeName         :: Name
prism'TypeName          = mkLensName_tc "Control.Lens.Type" "Prism'"

reviewTypeName          :: Name
reviewTypeName           = mkLensName_tc "Control.Lens.Type" "Review"

wrappedTypeName         :: Name
wrappedTypeName          = mkLensName_tc "Control.Lens.Wrapped" "Wrapped"

unwrappedTypeName       :: Name
unwrappedTypeName        = mkLensName_tc "Control.Lens.Wrapped" "Unwrapped"

rewrappedTypeName       :: Name
rewrappedTypeName        = mkLensName_tc "Control.Lens.Wrapped" "Rewrapped"

_wrapped'ValName        :: Name
_wrapped'ValName         = mkLensName_v "Control.Lens.Wrapped" "_Wrapped'"

isoValName              :: Name
isoValName               = mkLensName_v "Control.Lens.Iso" "iso"

prismValName            :: Name
prismValName             = mkLensName_v "Control.Lens.Prism" "prism"

untoValName             :: Name
untoValName              = mkLensName_v "Control.Lens.Review" "unto"

phantomValName          :: Name
phantomValName           = mkLensName_v "Control.Lens.Internal.TH" "phantom2"

phantom2 :: (Functor f, Contravariant f) => f a -> f b
phantom2 = phantom
{-# INLINE phantom2 #-}

composeValName          :: Name
composeValName           = mkNameG_v "base" "GHC.Base" "."

idValName               :: Name
idValName                = mkNameG_v "base" "GHC.Base" "id"

fmapValName             :: Name
fmapValName              = mkNameG_v "base" "GHC.Base" "fmap"

#if MIN_VERSION_base(4,8,0)
pureValName             :: Name
pureValName              = mkNameG_v "base" "GHC.Base" "pure"

apValName               :: Name
apValName                = mkNameG_v "base" "GHC.Base" "<*>"
#else
pureValName             :: Name
pureValName              = mkNameG_v "base" "Control.Applicative" "pure"

apValName               :: Name
apValName                = mkNameG_v "base" "Control.Applicative" "<*>"
#endif

rightDataName           :: Name
rightDataName            = mkNameG_d "base" "Data.Either" "Right"

leftDataName            :: Name
leftDataName             = mkNameG_d "base" "Data.Either" "Left"


------------------------------------------------------------------------
-- Support for generating inline pragmas
------------------------------------------------------------------------

inlinePragma :: Name -> [DecQ]

#ifdef INLINING

#if MIN_VERSION_template_haskell(2,8,0)

# ifdef OLD_INLINE_PRAGMAS
-- 7.6rc1?
inlinePragma methodName = [pragInlD methodName (inlineSpecNoPhase Inline False)]
# else
-- 7.7.20120830
inlinePragma methodName = [pragInlD methodName Inline FunLike AllPhases]
# endif

#else
-- GHC <7.6, TH <2.8.0
inlinePragma methodName = [pragInlD methodName (inlineSpecNoPhase True False)]
#endif

#else

inlinePragma _ = []

#endif
