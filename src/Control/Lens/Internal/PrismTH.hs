{-# LANGUAGE CPP #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#include "lens-common.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.PrismTH
-- Copyright   :  (C) 2014-2016 Edward Kmett and Eric Mertens
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Control.Lens.Internal.PrismTH
  ( makePrisms
  , makeClassyPrisms
  , makeDecPrisms
  ) where

import Control.Applicative
import Control.Lens.Getter
import Control.Lens.Internal.TH
import Control.Lens.Lens
import Control.Lens.Setter
import Control.Monad
import Data.Char (isUpper)
import qualified Data.List as List
import Data.Set.Lens
import Data.Traversable
import Language.Haskell.TH
import qualified Language.Haskell.TH.Datatype as D
import qualified Language.Haskell.TH.Datatype.TyVarBndr as D
import Language.Haskell.TH.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Prelude

-- | Generate a 'Prism' for each constructor of a data type.
-- Isos generated when possible.
-- Reviews are created for constructors with existentially
-- quantified constructors and GADTs.
--
-- /e.g./
--
-- @
-- data FooBarBaz a
--   = Foo Int
--   | Bar a
--   | Baz Int Char
-- makePrisms ''FooBarBaz
-- @
--
-- will create
--
-- @
-- _Foo :: Prism' (FooBarBaz a) Int
-- _Bar :: Prism (FooBarBaz a) (FooBarBaz b) a b
-- _Baz :: Prism' (FooBarBaz a) (Int, Char)
-- @
makePrisms :: Name {- ^ Type constructor name -} -> DecsQ
makePrisms = makePrisms' True


-- | Generate a 'Prism' for each constructor of a data type
-- and combine them into a single class. No Isos are created.
-- Reviews are created for constructors with existentially
-- quantified constructors and GADTs.
--
-- /e.g./
--
-- @
-- data FooBarBaz a
--   = Foo Int
--   | Bar a
--   | Baz Int Char
-- makeClassyPrisms ''FooBarBaz
-- @
--
-- will create
--
-- @
-- class AsFooBarBaz s a | s -> a where
--   _FooBarBaz :: Prism' s (FooBarBaz a)
--   _Foo :: Prism' s Int
--   _Bar :: Prism' s a
--   _Baz :: Prism' s (Int,Char)
--
--   _Foo = _FooBarBaz . _Foo
--   _Bar = _FooBarBaz . _Bar
--   _Baz = _FooBarBaz . _Baz
--
-- instance AsFooBarBaz (FooBarBaz a) a
-- @
--
-- Generate an "As" class of prisms. Names are selected by prefixing the constructor
-- name with an underscore.  Constructors with multiple fields will
-- construct Prisms to tuples of those fields.
--
-- In the event that the name of a data type is also the name of one of its
-- constructors, the name of the 'Prism' generated for the data type will be
-- prefixed with an extra @_@ (if the data type name is prefix) or @.@ (if the
-- name is infix) to disambiguate it from the 'Prism' for the corresponding
-- constructor. For example, this code:
--
-- @
-- data Quux = Quux Int | Fred Bool
-- makeClassyPrisms ''Quux
-- @
--
-- will create:
--
-- @
-- class AsQuux s where
--   __Quux :: Prism' s Quux -- Data type prism
--   _Quux :: Prism' s Int   -- Constructor prism
--   _Fred :: Prism' s Bool
--
--   _Quux = __Quux . _Quux
--   _Fred = __Quux . _Fred
--
-- instance AsQuux Quux
-- @
makeClassyPrisms :: Name {- ^ Type constructor name -} -> DecsQ
makeClassyPrisms = makePrisms' False


-- | Main entry point into Prism generation for a given type constructor name.
makePrisms' :: Bool -> Name -> DecsQ
makePrisms' normal typeName =
  do info <- D.reifyDatatype typeName
     let cls | normal    = Nothing
             | otherwise = Just (D.datatypeName info)
         cons = D.datatypeCons info
     makeConsPrisms (datatypeTypeKinded info) (map normalizeCon cons) cls


-- | Generate prisms for the given 'Dec'
makeDecPrisms :: Bool {- ^ generate top-level definitions -} -> Dec -> DecsQ
makeDecPrisms normal dec =
  do info <- D.normalizeDec dec
     let cls | normal    = Nothing
             | otherwise = Just (D.datatypeName info)
         cons = D.datatypeCons info
     makeConsPrisms (datatypeTypeKinded info) (map normalizeCon cons) cls


-- | Generate prisms for the given type, normalized constructors, and
-- an optional name to be used for generating a prism class.
-- This function dispatches between Iso generation, normal top-level
-- prisms, and classy prisms.
makeConsPrisms :: Type -> [NCon] -> Maybe Name -> DecsQ

-- special case: single constructor, not classy -> make iso
makeConsPrisms t [con@(NCon _ [] [] _)] Nothing = makeConIso t con

-- top-level definitions
makeConsPrisms t cons Nothing =
  fmap concat $ for cons $ \con ->
    do let conName = view nconName con
       stab <- computeOpticType t cons con
       let n = prismName conName
       sequenceA
         ( [ sigD n (return (quantifyType [] (stabToType Set.empty stab)))
           , valD (varP n) (normalB (makeConOpticExp stab cons con)) []
           ]
           ++ inlinePragma n
         )


-- classy prism class and instance
makeConsPrisms t cons (Just typeName) =
  sequenceA
    [ makeClassyPrismClass t className methodName cons
    , makeClassyPrismInstance t className methodName cons
    ]
  where
  typeNameBase = nameBase typeName
  className = mkName ("As" ++ typeNameBase)
  sameNameAsCon = any (\con -> nameBase (view nconName con) == typeNameBase) cons
  methodName = prismName' sameNameAsCon typeName


data OpticType = PrismType | ReviewType
data Stab  = Stab Cxt OpticType Type Type Type Type

simplifyStab :: Stab -> Stab
simplifyStab (Stab cx ty _ t _ b) = Stab cx ty t t b b
  -- simplification uses t and b because those types
  -- are interesting in the Review case

stabSimple :: Stab -> Bool
stabSimple (Stab _ _ s t a b) = s == t && a == b

stabToType :: Set Name -> Stab -> Type
stabToType clsTVBNames stab@(Stab cx ty s t a b) =
  quantifyType' clsTVBNames cx stabTy
  where
  stabTy =
    case ty of
      PrismType  | stabSimple stab -> prism'TypeName  `conAppsT` [t,b]
                 | otherwise       -> prismTypeName   `conAppsT` [s,t,a,b]
      ReviewType                   -> reviewTypeName  `conAppsT` [t,b]

stabType :: Stab -> OpticType
stabType (Stab _ o _ _ _ _) = o

computeOpticType :: Type -> [NCon] -> NCon -> Q Stab
computeOpticType t cons con =
  do let cons' = List.delete con cons
     if null (_nconVars con)
         then computePrismType t (view nconCxt con) cons' con
         else computeReviewType t (view nconCxt con) (view nconTypes con)


computeReviewType :: Type -> Cxt -> [Type] -> Q Stab
computeReviewType s' cx tys =
  do let t = s'
     s <- fmap VarT (newName "s")
     a <- fmap VarT (newName "a")
     b <- toTupleT (map return tys)
     return (Stab cx ReviewType s t a b)


-- | Compute the full type-changing Prism type given an outer type,
-- list of constructors, and target constructor name. Additionally
-- return 'True' if the resulting type is a "simple" prism.
computePrismType :: Type -> Cxt -> [NCon] -> NCon -> Q Stab
computePrismType t cx cons con =
  do let ts      = view nconTypes con
         unbound = setOf typeVars t Set.\\ setOf typeVars cons
     sub <- sequenceA (Map.fromSet (newName . nameBase) unbound)
     b   <- toTupleT (map return ts)
     a   <- toTupleT (map return (substTypeVars sub ts))
     let s = substTypeVars sub t
     return (Stab cx PrismType s t a b)


computeIsoType :: Type -> [Type] -> TypeQ
computeIsoType t' fields =
  do sub <- sequenceA (Map.fromSet (newName . nameBase) (setOf typeVars t'))
     let t = return                    t'
         s = return (substTypeVars sub t')
         b = toTupleT (map return                    fields)
         a = toTupleT (map return (substTypeVars sub fields))

         ty | Map.null sub = appsT (conT iso'TypeName) [t,b]
            | otherwise    = appsT (conT isoTypeName) [s,t,a,b]

     quantifyType [] <$> ty



-- | Construct either a Review or Prism as appropriate
makeConOpticExp :: Stab -> [NCon] -> NCon -> ExpQ
makeConOpticExp stab cons con =
  case stabType stab of
    PrismType  -> makeConPrismExp stab cons con
    ReviewType -> makeConReviewExp con


-- | Construct an iso declaration
makeConIso :: Type -> NCon -> DecsQ
makeConIso s con =
  do let ty      = computeIsoType s (view nconTypes con)
         defName = prismName (view nconName con)
     sequenceA
       ( [ sigD       defName  ty
         , valD (varP defName) (normalB (makeConIsoExp con)) []
         ] ++
         inlinePragma defName
       )


-- | Construct prism expression
--
-- prism <<reviewer>> <<remitter>>
makeConPrismExp ::
  Stab ->
  [NCon] {- ^ constructors       -} ->
  NCon   {- ^ target constructor -} ->
  ExpQ
makeConPrismExp stab cons con = appsE [varE prismValName, reviewer, remitter]
  where
  ts = view nconTypes con
  fields  = length ts
  conName = view nconName con

  reviewer                   = makeReviewer       conName fields
  remitter | stabSimple stab = makeSimpleRemitter conName (length cons) fields
           | otherwise       = makeFullRemitter cons conName


-- | Construct an Iso expression
--
-- iso <<reviewer>> <<remitter>>
makeConIsoExp :: NCon -> ExpQ
makeConIsoExp con = appsE [varE isoValName, remitter, reviewer]
  where
  conName = view nconName con
  fields  = length (view nconTypes con)

  reviewer = makeReviewer    conName fields
  remitter = makeIsoRemitter conName fields


-- | Construct a Review expression
--
-- unto (\(x,y,z) -> Con x y z)
makeConReviewExp :: NCon -> ExpQ
makeConReviewExp con = appE (varE untoValName) reviewer
  where
  conName = view nconName con
  fields  = length (view nconTypes con)

  reviewer = makeReviewer conName fields


------------------------------------------------------------------------
-- Prism and Iso component builders
------------------------------------------------------------------------


-- | Construct the review portion of a prism.
--
-- (\(x,y,z) -> Con x y z) :: b -> t
makeReviewer :: Name -> Int -> ExpQ
makeReviewer conName fields =
  do xs <- newNames "x" fields
     lam1E (toTupleP (map varP xs))
           (conE conName `appsE1` map varE xs)


-- | Construct the remit portion of a prism.
-- Pattern match only target constructor, no type changing
--
-- (\x -> case s of
--          Con x y z -> Right (x,y,z)
--          _         -> Left x
-- ) :: s -> Either s a
makeSimpleRemitter ::
  Name {- The name of the constructor on which this prism focuses -} ->
  Int  {- The number of constructors the parent data type has     -} ->
  Int  {- The number of fields the constructor has                -} ->
  ExpQ
makeSimpleRemitter conName numCons fields =
  do x  <- newName "x"
     xs <- newNames "y" fields
     let matches =
           [ match (conP conName (map varP xs))
                   (normalB (appE (conE rightDataName) (toTupleE (map varE xs))))
                   []
           ] ++
           [ match wildP (normalB (appE (conE leftDataName) (varE x))) []
           | numCons > 1 -- Only generate a catch-all case if there is at least
                         -- one constructor besides the one being focused on.
           ]
     lam1E (varP x) (caseE (varE x) matches)


-- | Pattern match all constructors to enable type-changing
--
-- (\x -> case s of
--          Con x y z -> Right (x,y,z)
--          Other_n w   -> Left (Other_n w)
-- ) :: s -> Either t a
makeFullRemitter :: [NCon] -> Name -> ExpQ
makeFullRemitter cons target =
  do x <- newName "x"
     lam1E (varP x) (caseE (varE x) (map mkMatch cons))
  where
  mkMatch (NCon conName _ _ n) =
    do xs <- newNames "y" (length n)
       match (conP conName (map varP xs))
             (normalB
               (if conName == target
                  then appE (conE rightDataName) (toTupleE (map varE xs))
                  else appE (conE leftDataName) (conE conName `appsE1` map varE xs)))
             []


-- | Construct the remitter suitable for use in an 'Iso'
--
-- (\(Con x y z) -> (x,y,z)) :: s -> a
makeIsoRemitter :: Name -> Int -> ExpQ
makeIsoRemitter conName fields =
  do xs <- newNames "x" fields
     lam1E (conP conName (map varP xs))
           (toTupleE (map varE xs))


------------------------------------------------------------------------
-- Classy prisms
------------------------------------------------------------------------


-- | Construct the classy prisms class for a given type and constructors.
--
-- class ClassName r <<vars in type>> | r -> <<vars in Type>> where
--   topMethodName   :: Prism' r Type
--   conMethodName_n :: Prism' r conTypes_n
--   conMethodName_n = topMethodName . conMethodName_n
makeClassyPrismClass ::
  Type   {- Outer type      -} ->
  Name   {- Class name      -} ->
  Name   {- Top method name -} ->
  [NCon] {- Constructors    -} ->
  DecQ
makeClassyPrismClass t className methodName cons =
  do r <- newName "r"
     let methodType = appsT (conT prism'TypeName) [varT r,return t]
     methodss <- traverse (mkMethod r) cons'
     classD (cxt[]) className (D.plainTV r : vs) (fds r)
       ( sigD methodName methodType
       : map return (concat methodss)
       )

  where
  mkMethod r con =
    do Stab cx o _ _ _ b <- computeOpticType t cons con
       let rTy   = VarT r
           stab' = Stab cx o rTy rTy b b
           defName = view nconName con
           body    = appsE [varE composeValName, varE methodName, varE defName]
       sequenceA
         [ sigD defName        (return (stabToType (Set.fromList (r:vNames)) stab'))
         , valD (varP defName) (normalB body) []
         ]

  cons'         = map (over nconName prismName) cons
  vs            = D.freeVariablesWellScoped [t]
  vNames        = map D.tvName vs
  fds r
    | null vs   = []
    | otherwise = [FunDep [r] vNames]



-- | Construct the classy prisms instance for a given type and constructors.
--
-- instance Classname OuterType where
--   topMethodName = id
--   conMethodName_n = <<prism>>
makeClassyPrismInstance ::
  Type ->
  Name     {- Class name      -} ->
  Name     {- Top method name -} ->
  [NCon] {- Constructors    -} ->
  DecQ
makeClassyPrismInstance s className methodName cons =
  do let vs = D.freeVariablesWellScoped [s]
         cls = className `conAppsT` (s : map tvbToType vs)

     instanceD (cxt[]) (return cls)
       (   valD (varP methodName)
                (normalB (varE idValName)) []
       : [ do stab <- computeOpticType s cons con
              let stab' = simplifyStab stab
              valD (varP (prismName conName))
                (normalB (makeConOpticExp stab' cons con)) []
           | con <- cons
           , let conName = view nconName con
           ]
       )


------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------


-- | Normalized constructor
data NCon = NCon
  { _nconName :: Name
  , _nconVars :: [Name]
  , _nconCxt  :: Cxt
  , _nconTypes :: [Type]
  }
  deriving (Eq)

instance HasTypeVars NCon where
  typeVarsEx s f (NCon x vars y z) = NCon x vars <$> typeVarsEx s' f y <*> typeVarsEx s' f z
    where s' = List.foldl' (flip Set.insert) s vars

nconName :: Lens' NCon Name
nconName f x = fmap (\y -> x {_nconName = y}) (f (_nconName x))

nconCxt :: Lens' NCon Cxt
nconCxt f x = fmap (\y -> x {_nconCxt = y}) (f (_nconCxt x))

nconTypes :: Lens' NCon [Type]
nconTypes f x = fmap (\y -> x {_nconTypes = y}) (f (_nconTypes x))


-- | Normalize a single 'Con' to its constructor name and field types.
normalizeCon :: D.ConstructorInfo -> NCon
normalizeCon info = NCon (D.constructorName info)
                         (D.tvName <$> D.constructorVars info)
                         (D.constructorContext info)
                         (D.constructorFields info)


-- | Compute a prism's name by prefixing an underscore for normal
-- constructors and period for operators.
prismName :: Name -> Name
prismName = prismName' False

-- | Compute a prism's name with a special case for when the type
-- constructor matches one of the value constructors.
--
-- The overlapping flag will be 'True' in the event that:
--
-- 1. We are generating the name of a classy prism for a
--    data type, and
-- 2. The data type shares a name with one of its
--    constructors (e.g., @data A = A@).
--
-- In such a scenario, we take care not to generate the same
-- prism name that the constructor receives (e.g., @_A@).
-- For prefix names, we accomplish this by adding an extra
-- underscore; for infix names, an extra dot.
prismName' ::
  Bool {- ^ overlapping constructor -} ->
  Name {- ^ type constructor        -} ->
  Name {- ^ prism name              -}
prismName' sameNameAsCon n =
  case nameBase n of
    [] -> error "prismName: empty name base?"
    nb@(x:_) | isUpper x -> mkName (prefix '_' nb)
             | otherwise -> mkName (prefix '.' nb) -- operator
  where
    prefix :: Char -> String -> String
    prefix char str | sameNameAsCon = char:char:str
                    | otherwise     =      char:str
