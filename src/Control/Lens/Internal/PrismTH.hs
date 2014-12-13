{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.PrismTH
-- Copyright   :  (C) 2014 Edward Kmett, (C) 2014 Eric Mertens
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Control.Lens.Internal.PrismTH
  ( makePrisms
  , makePrismsWith
  , makeClassyPrisms
  , makeDecPrisms
  , defaultPrismRules
  , tupleCtors
  , pairListCtors
  , PrismRules(..)
  , FieldCtors
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
import Data.Traversable (for,sequenceA,traverse)
import Language.Haskell.TH
import Language.Haskell.TH.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set

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
--   | Qux Int Char Bool
-- makePrisms ''FooBarBaz
-- @
--
-- will create
--
-- @
-- _Foo :: Prism' (FooBarBaz a) Int
-- _Bar :: Prism (FooBarBaz a) (FooBarBaz b) a b
-- _Baz :: Prism' (FooBarBaz a) (Int, Char)
-- _Qux :: Prism' (FooBarBaz a) (Int, Char, Bool)
-- @
makePrisms :: Name {- ^ Type constructor name -} -> DecsQ
makePrisms = makePrismsWith defaultPrismRules


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
makeClassyPrisms = makePrismsWith classyRules
  where
  classyRules = defaultPrismRules { _generateTopLevel = False }


-- | Main entry point into Prism generation for a given type constructor name.
makePrismsWith :: PrismRules -> Name -> DecsQ
makePrismsWith rules typeName =
  do info <- reify typeName
     case info of
       TyConI dec -> makeDecPrisms' rules dec
       _          -> fail "makePrisms: expected type constructor name"


-- | Generate prisms for the given 'Dec'
makeDecPrisms :: Bool -> Dec -> DecsQ
makeDecPrisms topLevel =
  makeDecPrisms' defaultPrismRules { _generateTopLevel = topLevel }

-- | Generate prisms for the given 'Dec'
makeDecPrisms' :: PrismRules -> Dec -> DecsQ
makeDecPrisms' rules dec = case dec of
  DataD        _ ty vars cons _ -> next ty (convertTVBs vars) cons
  NewtypeD     _ ty vars con  _ -> next ty (convertTVBs vars) [con]
  DataInstD    _ ty tys  cons _ -> next ty tys                cons
  NewtypeInstD _ ty tys  con  _ -> next ty tys                [con]
  _                             -> fail "makePrisms: expected type constructor dec"
  where
  convertTVBs = map (VarT . bndrName)

  next ty args cons =
    makeConsPrisms rules (conAppsT ty args) (map normalizeCon cons) cls
    where
    cls | _generateTopLevel rules    = Nothing
        | otherwise                  = Just ty


-- | Generate prisms for the given type, normalized constructors, and
-- an optional name to be used for generating a prism class.
-- This function dispatches between Iso generation, normal top-level
-- prisms, and classy prisms.
makeConsPrisms :: PrismRules -> Type -> [NCon] -> Maybe Name -> DecsQ

-- special case: single constructor, not classy -> make iso
makeConsPrisms rules t [con@(NCon _ Nothing _)] Nothing =
    makeConIso rules t con

-- top-level definitions
makeConsPrisms rules t cons Nothing =
  fmap concat $ for cons $ \con ->
    do let conName = view nconName con
       stab <- computeOpticType rules t cons con
       let n = prismName conName
       sequence
         [ sigD n (close (stabToType stab))
         , valD (varP n) (normalB (makeConOpticExp rules stab cons con)) []
         ]


-- classy prism class and instance
makeConsPrisms rules t cons (Just typeName) =
  sequence
    [ makeClassyPrismClass    rules t className methodName cons
    , makeClassyPrismInstance rules t className methodName cons
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

computeOpticType :: PrismRules -> Type -> [NCon] -> NCon -> Q Stab
computeOpticType rules t cons con =
  do let cons' = delete con cons
     case view nconCxt con of
       Just xs -> computeReviewType rules t xs (view nconTypes con)
       Nothing -> computePrismType rules t cons' con


computeReviewType :: PrismRules -> Type -> Cxt -> [Type] -> Q Stab
computeReviewType rules t cx tys =
  do
     let ctor = _typeCtor $ _fieldCtors rules
     s <- fmap VarT (newName "s")
     a <- fmap VarT (newName "a")
     b <- ctor (map return tys)
     return (Stab cx ReviewType s t a b)


-- | Compute the full type-changing Prism type given an outer type,
-- list of constructors, and target constructor name. Additionally
-- return 'True' if the resulting type is a "simple" prism.
computePrismType :: PrismRules -> Type -> [NCon] -> NCon -> Q Stab
computePrismType rules t cons con =
  do let ts      = view nconTypes con
         unbound = setOf typeVars t Set.\\ setOf typeVars cons
         ctor    = _typeCtor $ _fieldCtors rules
     sub <- sequenceA (fromSet (newName . nameBase) unbound)
     b   <- ctor (map return ts)
     a   <- ctor (map return (substTypeVars sub ts))
     let s = substTypeVars sub t
     return (Stab [] PrismType s t a b)


computeIsoType :: PrismRules -> Type -> [Type] -> TypeQ
computeIsoType rules t' fields =
  do sub <- sequenceA (fromSet (newName . nameBase) (setOf typeVars t'))
     let t = return                    t'
         s = return (substTypeVars sub t')
         ctor = _typeCtor $ _fieldCtors rules
         b = ctor (map return                    fields)
         a = ctor (map return (substTypeVars sub fields))

#ifndef HLINT
         ty | Map.null sub = appsT (conT iso'TypeName) [t,b]
            | otherwise    = appsT (conT isoTypeName) [s,t,a,b]
#endif

     close =<< ty



-- | Construct either a Review or Prism as appropriate
makeConOpticExp :: PrismRules -> Stab -> [NCon] -> NCon -> ExpQ
makeConOpticExp rules stab cons con =
  case stabType stab of
    PrismType  -> makeConPrismExp rules stab cons con
    ReviewType -> makeConReviewExp rules con


-- | Construct an iso declaration
makeConIso :: PrismRules -> Type -> NCon -> DecsQ
makeConIso rules s con =
  do let ty      = computeIsoType rules s (view nconTypes con)
         defName = prismName (view nconName con)
     sequence
       [ sigD       defName  ty
       , valD (varP defName) (normalB (makeConIsoExp rules con)) []
       ]


-- | Construct prism expression
--
-- prism <<reviewer>> <<remitter>>
makeConPrismExp ::
  PrismRules                        ->
  Stab                              ->
  [NCon] {- ^ constructors       -} ->
  NCon   {- ^ target constructor -} ->
  ExpQ
makeConPrismExp rules stab cons con = appsE [varE prismValName, reviewer, remitter]
  where
  ts = view nconTypes con
  fields  = length ts
  conName = view nconName con

  reviewer                   = makeReviewer rules conName fields
  remitter | stabSimple stab = makeSimpleRemitter rules conName fields
           | otherwise       = makeFullRemitter rules cons conName


-- | Construct an Iso expression
--
-- iso <<reviewer>> <<remitter>>
makeConIsoExp :: PrismRules -> NCon -> ExpQ
makeConIsoExp rules con = appsE [varE isoValName, remitter, reviewer]
  where
  conName = view nconName con
  fields  = length (view nconTypes con)

  reviewer = makeReviewer    rules conName fields
  remitter = makeIsoRemitter rules conName fields


-- | Construct a Review expression
--
-- unto (\(x,y,z) -> Con x y z)
makeConReviewExp :: PrismRules -> NCon -> ExpQ
makeConReviewExp rules con = appE (varE untoValName) reviewer
  where
  conName = view nconName con
  fields  = length (view nconTypes con)

  reviewer = makeReviewer rules conName fields


------------------------------------------------------------------------
-- Prism and Iso component builders
------------------------------------------------------------------------


-- | Construct the review portion of a prism.
--
-- (\(x,y,z) -> Con x y z) :: b -> t
makeReviewer :: PrismRules -> Name -> Int -> ExpQ
makeReviewer rules conName fields =
  do xs <- replicateM fields (newName "x")
     let ctor = _patCtor $ _fieldCtors rules
     lam1E (ctor (map varP xs))
           (conE conName `appsE1` map varE xs)


-- | Construct the remit portion of a prism.
-- Pattern match only target constructor, no type changing
--
-- (\x -> case s of
--          Con x y z -> Right (x,y,z)
--          _         -> Left x
-- ) :: s -> Either s a
makeSimpleRemitter :: PrismRules -> Name -> Int -> ExpQ
makeSimpleRemitter rules conName fields =
  do x  <- newName "x"
     xs <- replicateM fields (newName "y")
     let ctor    = _expCtor $ _fieldCtors rules
         matches =
           [ match (conP conName (map varP xs))
                   (normalB (appE (conE rightDataName) (ctor (map varE xs))))
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
makeFullRemitter :: PrismRules -> [NCon] -> Name -> ExpQ
makeFullRemitter rules cons target =
  do x <- newName "x"
     lam1E (varP x) (caseE (varE x) (map mkMatch cons))
  where
  mkMatch (NCon conName _ n) =
    do xs <- replicateM (length n) (newName "y")
       let ctor = _expCtor $ _fieldCtors rules
       match (conP conName (map varP xs))
             (normalB
               (if conName == target
                  then appE (conE rightDataName) (ctor (map varE xs))
                  else appE (conE leftDataName) (conE conName `appsE1` map varE xs)))
             []


-- | Construct the remitter suitable for use in an 'Iso'
--
-- (\(Con x y z) -> (x,y,z)) :: s -> a
makeIsoRemitter :: PrismRules -> Name -> Int -> ExpQ
makeIsoRemitter rules conName fields =
  do xs <- replicateM fields (newName "x")
     let ctor = _expCtor $ _fieldCtors rules
     lam1E (conP conName (map varP xs))
           (ctor (map varE xs))


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
  PrismRules                   ->
  Type   {- Outer type      -} ->
  Name   {- Class name      -} ->
  Name   {- Top method name -} ->
  [NCon] {- Constructors    -} ->
  DecQ
makeClassyPrismClass rules t className methodName cons =
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
    do Stab cx o _ _ _ b <- computeOpticType rules t cons con
       let stab' = Stab cx o r r b b
           defName = view nconName con
           body    = appsE [varE composeValName, varE methodName, varE defName]
       sequence
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
  PrismRules                     ->
  Type                           ->
  Name     {- Class name      -} ->
  Name     {- Top method name -} ->
  [NCon]   {- Constructors    -} ->
  DecQ
makeClassyPrismInstance rules s className methodName cons =
  do let vs = Set.toList (setOf typeVars s)
         cls = className `conAppsT` (s : map VarT vs)

     instanceD (cxt[]) (return cls)
       (   valD (varP methodName)
                (normalB (varE idValName)) []
       : [ do stab <- computeOpticType rules s cons con
              let stab' = simplifyStab stab
              valD (varP (prismName conName))
                (normalB (makeConOpticExp rules stab' cons con)) []
           | con <- cons
           , let conName = view nconName con
           ]
       )

------------------------------------------------------------------------
-- Prism generation parameters
------------------------------------------------------------------------

-- | Configuration for Prism generation.
data PrismRules = PrismRules
  { _generateTopLevel :: Bool
       -- ^ Generate top level definitions?
  , _fieldCtors       :: FieldCtors
  }

-- | Constructors for prism field generation.
data FieldCtors = FieldCtors
  { _typeCtor :: [TypeQ] -> TypeQ
  , _expCtor  :: [ExpQ] -> ExpQ
  , _patCtor  :: [PatQ] -> PatQ
  }

-- | Field constructors to generate a tuple of the length of the fields
-- requested.
--
-- _Qux :: Prism' (FooBarBaz a) (Int, Char, Bool)
tupleCtors :: FieldCtors
tupleCtors = FieldCtors
  { _typeCtor = toTupleT
  , _expCtor  = toTupleE
  , _patCtor  = toTupleP
  }

-- | Field constructors to generate a church pair list encoding of the of
-- fields requested.
--
-- _Qux :: Prism' (FooBarBaz a) (Int, (Char, Bool))
pairListCtors :: FieldCtors
pairListCtors = FieldCtors
  { _typeCtor = toPairListT
  , _expCtor  = toPairListE
  , _patCtor  = toPairListP
  }

defaultPrismRules :: PrismRules
defaultPrismRules = PrismRules
  { _generateTopLevel = True
  , _fieldCtors       = tupleCtors
  }

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


-- | Normalize 'Con' to its constructor name and field types.
normalizeCon :: Con -> NCon
normalizeCon (RecC    conName xs) = NCon conName Nothing (map (view _3) xs)
normalizeCon (NormalC conName xs) = NCon conName Nothing (map (view _2) xs)
normalizeCon (InfixC (_,x) conName (_,y)) = NCon conName Nothing [x,y]
normalizeCon (ForallC [] [] con) = normalizeCon con -- happens in GADTs
normalizeCon (ForallC _ cx con) = NCon n (cx1 <> cx2) tys
  where
  cx1 = Just cx
  NCon n cx2 tys = normalizeCon con


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
