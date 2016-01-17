{-# LANGUAGE CPP #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) 1
#endif
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
import Control.Lens.Tuple
import Control.Monad
import Data.Char (isUpper)
import Data.List
import Data.Monoid
import Data.Set.Lens
import Data.Traversable
import Language.Haskell.TH
import Language.Haskell.TH.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
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
makeClassyPrisms :: Name {- ^ Type constructor name -} -> DecsQ
makeClassyPrisms = makePrisms' False


-- | Main entry point into Prism generation for a given type constructor name.
makePrisms' :: Bool -> Name -> DecsQ
makePrisms' normal typeName =
  do info <- reify typeName
     case info of
       TyConI dec -> makeDecPrisms normal dec
       _          -> fail "makePrisms: expected type constructor name"


-- | Generate prisms for the given 'Dec'
makeDecPrisms :: Bool {- ^ generate top-level definitions -} -> Dec -> DecsQ
makeDecPrisms normal dec = case dec of
#if MIN_VERSION_template_haskell(2,11,0)
  DataD        _ ty vars _ cons _ -> next ty (convertTVBs vars) cons
  NewtypeD     _ ty vars _ con  _ -> next ty (convertTVBs vars) [con]
  DataInstD    _ ty tys  _ cons _ -> next ty tys                cons
  NewtypeInstD _ ty tys  _ con  _ -> next ty tys                [con]
#else
  DataD        _ ty vars   cons _ -> next ty (convertTVBs vars) cons
  NewtypeD     _ ty vars   con  _ -> next ty (convertTVBs vars) [con]
  DataInstD    _ ty tys    cons _ -> next ty tys                cons
  NewtypeInstD _ ty tys    con  _ -> next ty tys                [con]
#endif
  _                             -> fail "makePrisms: expected type constructor dec"
  where
  convertTVBs = map (VarT . bndrName)

  next ty args cons =
    makeConsPrisms (conAppsT ty args) (map normalizeCon cons) cls
    where
    cls | normal    = Nothing
        | otherwise = Just ty


-- | Generate prisms for the given type, normalized constructors, and
-- an optional name to be used for generating a prism class.
-- This function dispatches between Iso generation, normal top-level
-- prisms, and classy prisms.
makeConsPrisms :: Type -> [NCon] -> Maybe Name -> DecsQ

-- special case: single constructor, not classy -> make iso
makeConsPrisms t [con@(NCon _ Nothing _)] Nothing = makeConIso t con

-- top-level definitions
makeConsPrisms t cons Nothing =
  fmap concat $ for cons $ \con ->
    do let conName = view nconName con
       stab <- computeOpticType t cons con
       let n = prismName conName
       sequenceA
         [ sigD n (close (stabToType stab))
         , valD (varP n) (normalB (makeConOpticExp stab cons con)) []
         ]


-- classy prism class and instance
makeConsPrisms t cons (Just typeName) =
  sequenceA
    [ makeClassyPrismClass t className methodName cons
    , makeClassyPrismInstance t className methodName cons
    ]
  where
  className = mkName ("As" ++ nameBase typeName)
  methodName = prismName typeName


data OpticType = PrismType | ReviewType
data Stab  = Stab Cxt OpticType Type Type Type Type

simplifyStab :: Stab -> Stab
simplifyStab (Stab cx ty _ t _ b) = Stab cx ty t t b b
  -- simplification uses t and b because those types
  -- are interesting in the Review case

stabSimple :: Stab -> Bool
stabSimple (Stab _ _ s t a b) = s == t && a == b

stabToType :: Stab -> Type
stabToType stab@(Stab cx ty s t a b) = ForallT vs cx $
  case ty of
    PrismType  | stabSimple stab -> prism'TypeName  `conAppsT` [t,b]
               | otherwise       -> prismTypeName   `conAppsT` [s,t,a,b]
    ReviewType                   -> reviewTypeName  `conAppsT` [t,b]

  where
  vs = map PlainTV (Set.toList (setOf typeVars cx))

stabType :: Stab -> OpticType
stabType (Stab _ o _ _ _ _) = o

computeOpticType :: Type -> [NCon] -> NCon -> Q Stab
computeOpticType t cons con =
  do let cons' = delete con cons
     case view nconCxt con of
       Just xs -> computeReviewType t xs (view nconTypes con)
       Nothing -> computePrismType t cons' con


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
computePrismType :: Type -> [NCon] -> NCon -> Q Stab
computePrismType t cons con =
  do let ts      = view nconTypes con
         unbound = setOf typeVars t Set.\\ setOf typeVars cons
     sub <- sequenceA (fromSet (newName . nameBase) unbound)
     b   <- toTupleT (map return ts)
     a   <- toTupleT (map return (substTypeVars sub ts))
     let s = substTypeVars sub t
     return (Stab [] PrismType s t a b)


computeIsoType :: Type -> [Type] -> TypeQ
computeIsoType t' fields =
  do sub <- sequenceA (fromSet (newName . nameBase) (setOf typeVars t'))
     let t = return                    t'
         s = return (substTypeVars sub t')
         b = toTupleT (map return                    fields)
         a = toTupleT (map return (substTypeVars sub fields))

#ifndef HLINT
         ty | Map.null sub = appsT (conT iso'TypeName) [t,b]
            | otherwise    = appsT (conT isoTypeName) [s,t,a,b]
#endif

     close =<< ty



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
       [ sigD       defName  ty
       , valD (varP defName) (normalB (makeConIsoExp con)) []
       ]


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
  remitter | stabSimple stab = makeSimpleRemitter conName fields
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
  do xs <- replicateM fields (newName "x")
     lam1E (toTupleP (map varP xs))
           (conE conName `appsE1` map varE xs)


-- | Construct the remit portion of a prism.
-- Pattern match only target constructor, no type changing
--
-- (\x -> case s of
--          Con x y z -> Right (x,y,z)
--          _         -> Left x
-- ) :: s -> Either s a
makeSimpleRemitter :: Name -> Int -> ExpQ
makeSimpleRemitter conName fields =
  do x  <- newName "x"
     xs <- newNames "y" fields
     let matches =
           [ match (conP conName (map varP xs))
                   (normalB (appE (conE rightDataName) (toTupleE (map varE xs))))
                   []
           , match wildP (normalB (appE (conE leftDataName) (varE x))) []
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
  mkMatch (NCon conName _ n) =
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
#ifndef HLINT
     let methodType = appsT (conT prism'TypeName) [varT r,return t]
#endif
     methodss <- traverse (mkMethod (VarT r)) cons'
     classD (cxt[]) className (map PlainTV (r : vs)) (fds r)
       ( sigD methodName methodType
       : map return (concat methodss)
       )

  where
  mkMethod r con =
    do Stab cx o _ _ _ b <- computeOpticType t cons con
       let stab' = Stab cx o r r b b
           defName = view nconName con
           body    = appsE [varE composeValName, varE methodName, varE defName]
       sequenceA
         [ sigD defName        (return (stabToType stab'))
         , valD (varP defName) (normalB body) []
         ]

  cons'         = map (over nconName prismName) cons
  vs            = Set.toList (setOf typeVars t)
  fds r
    | null vs   = []
    | otherwise = [FunDep [r] vs]



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
  do let vs = Set.toList (setOf typeVars s)
         cls = className `conAppsT` (s : map VarT vs)

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
  , _nconCxt  :: Maybe Cxt
  , _nconTypes :: [Type]
  }
  deriving (Eq)

instance HasTypeVars NCon where
  typeVarsEx s f (NCon x y z) = NCon x <$> typeVarsEx s f y <*> typeVarsEx s f z

nconName :: Lens' NCon Name
nconName f x = fmap (\y -> x {_nconName = y}) (f (_nconName x))

nconCxt :: Lens' NCon (Maybe Cxt)
nconCxt f x = fmap (\y -> x {_nconCxt = y}) (f (_nconCxt x))

nconTypes :: Lens' NCon [Type]
nconTypes f x = fmap (\y -> x {_nconTypes = y}) (f (_nconTypes x))


-- | Normalize 'Con' to its constructor name and field types. For 'GadtC' and
-- 'RecGadtC', it takes the leftmost name.
normalizeCon :: Con -> NCon
normalizeCon (RecC    conName xs) = NCon conName Nothing (map (view _3) xs)
normalizeCon (NormalC conName xs) = NCon conName Nothing (map (view _2) xs)
normalizeCon (InfixC (_,x) conName (_,y)) = NCon conName Nothing [x,y]
normalizeCon (ForallC [] [] con) = normalizeCon con -- happens in GADTs
normalizeCon (ForallC _ cx con) = NCon n (cx1 <> cx2) tys
  where
  cx1 = Just cx
  NCon n cx2 tys = normalizeCon con
#if MIN_VERSION_template_haskell(2,11,0)
normalizeCon (GadtC conNames xs _)    = NCon (head conNames) Nothing (map (view _2) xs)
normalizeCon (RecGadtC conNames xs _) = NCon (head conNames) Nothing (map (view _3) xs)
#endif

-- | Compute a prism's name by prefixing an underscore for normal
-- constructors and period for operators.
prismName :: Name -> Name
prismName n = case nameBase n of
                [] -> error "prismName: empty name base?"
                x:xs | isUpper x -> mkName ('_':x:xs)
                     | otherwise -> mkName ('.':x:xs) -- operator


-- | Quantify all the free variables in a type.
close :: Type -> TypeQ
close t = forallT (map PlainTV (Set.toList vs)) (cxt[]) (return t)
  where
  vs = setOf typeVars t
