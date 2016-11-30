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
  , makeArgTypePrisms
  , makeClassyPrisms
  , makeDecPrisms
  , makeArgTypeDecPrisms
  , PrismRulesSelector
  ) where

import Control.Applicative
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Internal.FieldTH
import Control.Lens.Internal.TH
import Control.Lens.Lens
import Control.Lens.Setter
import Control.Lens.Traversal
import Control.Lens.Tuple
import Control.Monad
import Data.Char (isUpper)
import Data.List
import Data.Maybe
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
makePrisms = makePrisms' (const Nothing) True


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
makeClassyPrisms = makePrisms' (const Nothing) False

type PrismRulesSelector = Name -> Maybe (Name, LensRules)

-- | Generate prisms for the specified type, where the prism for each
-- constructor with more than one argument targets either a tuple or a
-- generated data type. This behavior is controlled by the 'PrismRulesSelector'.
-- The function is passed the name of a constructor.
-- A result of 'Nothing' will generate a prism targetting a tuple.
-- A result of @Just (n, lr)@ will generate a data type named @n@, a prism to
-- that data type and lenses for that data type using @lr@.
makeArgTypePrisms :: PrismRulesSelector
  -> Bool {- ^ generate top-level definitions -}
  -> Name {- ^ Type constructor name -} -> DecsQ
makeArgTypePrisms = makePrisms'

-- | Main entry point into Prism generation for a given type constructor name.
makePrisms' :: PrismRulesSelector -> Bool -> Name -> DecsQ
makePrisms' prs normal typeName =
  do info <- reify typeName
     case info of
       TyConI dec -> makeArgTypeDecPrisms prs normal dec
       _          -> fail "makePrisms: expected type constructor name"

-- | Generate prisms for the given 'Dec'
makeDecPrisms :: Bool {- ^ generate top-level definitions -} -> Dec -> DecsQ
makeDecPrisms = makeArgTypeDecPrisms (const Nothing)

makeArgTypeDecPrisms :: PrismRulesSelector -> Bool {- ^ generate top-level definitions -} -> Dec -> DecsQ
makeArgTypeDecPrisms prs normal dec = case dec of
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
    makeConsPrisms prs (conAppsT ty args) (concatMap normalizeCon cons) cls
    where
    cls | normal    = Nothing
        | otherwise = Just ty


-- | Generate prisms for the given type, normalized constructors, and
-- an optional name to be used for generating a prism class.
-- This function dispatches between Iso generation, normal top-level
-- prisms, and classy prisms.
makeConsPrisms :: PrismRulesSelector -> Type -> [NCon] -> Maybe Name -> DecsQ

-- special case: single constructor, not classy -> make iso
makeConsPrisms prs t [con@(NCon _ Nothing _)] Nothing =
  makeConIso (computePrismTarget prs t con) t con

-- top-level definitions
makeConsPrisms prs t cons Nothing =
  fmap concat $ for cons $ \con ->
    do let conName = view nconName con
       stab <- computeOpticStab prs t cons con
       let n = prismName conName
       targetDs <- targetDecs (stabTarget stab)
       prismSigD <- sigD n (close (stabToType stab))
       prismValD <- valD (varP n) (normalB (makeConOpticExp stab cons con)) []
       return $ targetDs ++ [prismSigD, prismValD]



-- classy prism class and instance
makeConsPrisms prs t cons (Just typeName) =
  sequenceA
    [ makeClassyPrismClass prs t className methodName cons
    , makeClassyPrismInstance prs t className methodName cons
    ]
  where
  className = mkName ("As" ++ nameBase typeName)
  methodName = prismName typeName


data OpticType = PrismType | ReviewType
data PrismTarget = PrismTargetTuple [Type] -- the types of each tuple elements
  | PrismTargetDataType
      Name -- the name of the generate data type and of it's single constructor
      [TyVarBndr] -- the type variables for the generated data type
      [(Name, Type)] -- the name and type of each element
      LensRules -- the lens rules to apply in generating lenses on the generated
                -- data type

instance HasTypeVars PrismTarget where
  typeVarsEx s f (PrismTargetTuple ts) = PrismTargetTuple <$> typeVarsEx s f ts
  typeVarsEx s f (PrismTargetDataType n tvs fs lr) =
    (\x -> PrismTargetDataType n x fs lr) <$> typeVarsEx s f tvs

instance SubstType PrismTarget where
  substType m (PrismTargetTuple ts) = PrismTargetTuple (substType m ts)
  substType m (PrismTargetDataType n tvs fs lr) =
    PrismTargetDataType n
      (substType m tvs)
      (fs & traverse . traverse %~ substType m)
      lr

targetNumFields :: PrismTarget -> Int
targetNumFields (PrismTargetTuple ts) = length ts
targetNumFields (PrismTargetDataType _ _ ts _) = length ts

targetType :: PrismTarget -> TypeQ
targetType (PrismTargetTuple ts) = toTupleT (map return ts)
targetType (PrismTargetDataType n tvs _ _) =
  return $ conAppsT n (tvs ^.. typeVars . to VarT)

targetPattern :: [Name] -> PrismTarget  -> PatQ
targetPattern xs (PrismTargetTuple _) = toTupleP (map varP xs)
targetPattern xs (PrismTargetDataType n _ _ _) = conP n (map varP xs)

targetConExp :: [Name] -> PrismTarget ->  ExpQ
targetConExp xs (PrismTargetTuple _) = toTupleE (map varE xs)
targetConExp xs (PrismTargetDataType n _ _ _) = appsE1 (conE n) (map varE xs)

targetDecs :: PrismTarget -> DecsQ
targetDecs (PrismTargetTuple _) = return []
targetDecs (PrismTargetDataType n tvs fs rules) = do
  lds <- lensDecs
  return $ strippedTypeDec : lds
  where
    typeDec =
#if MIN_VERSION_template_haskell(2,11,0)
      DataD [] n tvs Nothing
        [RecC n [(n', Bang NoSourceUnpackedness NoSourceStrictness, t)
          | (n', t) <- fs]]
      []
#else
      DataD [] n tvs
        [RecC n [(n', NotStrict, t) | (n', t) <- fs]]
        []
#endif
    strippedTypeDec = stripFields typeDec
    lensDecs = makeFieldOpticsForDec rules typeDec

makeTargetNames :: PrismTarget -> Q [Name]
makeTargetNames t = replicateM (targetNumFields t) (newName "y")

data Stab  = Stab Cxt OpticType PrismTarget Type Type Type Type

simplifyStab :: Stab -> Stab
simplifyStab (Stab cx ty pt _ t _ b) = Stab cx ty pt t t b b
  -- simplification uses t and b because those types
  -- are interesting in the Review case

stabSimple :: Stab -> Bool
stabSimple (Stab _ _ _ s t a b) = s == t && a == b

stabToType :: Stab -> Type
stabToType stab@(Stab cx ty _ s t a b) = ForallT vs cx $
  case ty of
    PrismType  | stabSimple stab -> prism'TypeName  `conAppsT` [t,b]
               | otherwise       -> prismTypeName   `conAppsT` [s,t,a,b]
    ReviewType                   -> reviewTypeName  `conAppsT` [t,b]

  where
  vs = map PlainTV
     $ nub -- stable order
     $ toListOf typeVars cx

stabType :: Stab -> OpticType
stabType (Stab _ o _ _ _ _ _) = o

stabTarget :: Stab -> PrismTarget
stabTarget (Stab _ _ ta _ _ _ _) = ta

computePrismTarget :: PrismRulesSelector -> Type -> NCon -> PrismTarget
computePrismTarget prs t con =
  let maybeFields = sequenceA [do { n <- mn; return (n, t');}
        | (mn, t') <- view nconFields con]
      conTys = view nconTypes con
      conTyVars = toListOf typeVars conTys
      sortedConTyVars = filter (`elem` conTyVars) (toListOf typeVars t)
  in fromMaybe (PrismTargetTuple conTys) $ do
      (n, lr) <- prs (view nconName con)
      fields <- maybeFields
      return $ PrismTargetDataType n (fmap PlainTV sortedConTyVars) fields lr

computeOpticStab :: PrismRulesSelector -> Type -> [NCon] -> NCon -> Q Stab
computeOpticStab prs t cons con =
  do let cons' = delete con cons
         target = computePrismTarget prs t con
     case view nconCxt con of
       Just xs -> computeReviewStab target t xs
       Nothing -> computePrismStab target t cons'

computeReviewStab :: PrismTarget -> Type -> Cxt -> Q Stab
computeReviewStab target s' cx =
  do let t = s'
     s <- fmap VarT (newName "s")
     a <- fmap VarT (newName "a")
     b <- targetType target
     return (Stab cx ReviewType target s t a b)

-- | Compute the full type-changing Prism type given an outer type,
-- list of constructors, and target constructor name. Additionally
-- return 'True' if the resulting type is a "simple" prism.
computePrismStab :: PrismTarget -> Type -> [NCon] -> Q Stab
computePrismStab target t cons =
  do let unbound = setOf typeVars t Set.\\ setOf typeVars cons
     sub <- sequenceA (fromSet (newName . nameBase) unbound)
     b <- targetType target
     let a = substTypeVars sub b
         s = substTypeVars sub t
     return (Stab [] PrismType target s t a b)

computeIsoType :: PrismTarget -> Type -> TypeQ
computeIsoType target t'=
  do sub <- sequenceA (fromSet (newName . nameBase) (setOf typeVars t'))
     let t = return                    t'
         s = return (substTypeVars sub t')
         b = targetType target
         a = substTypeVars sub <$> b
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
    ReviewType -> makeConReviewExp (stabTarget stab) con


-- | Construct an iso declaration
makeConIso :: PrismTarget -> Type -> NCon -> DecsQ
makeConIso target s con =
  do let ty      = computeIsoType target s
         defName = prismName (view nconName con)
     sequenceA
       [ sigD       defName  ty
       , valD (varP defName) (normalB (makeConIsoExp target con)) []
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
  conName = view nconName con
  target = stabTarget stab
  reviewer                   = makeReviewer       target conName
  remitter | stabSimple stab = makeSimpleRemitter target conName
           | otherwise       = makeFullRemitter target cons conName


-- | Construct an Iso expression
--
-- iso <<reviewer>> <<remitter>>
makeConIsoExp :: PrismTarget -> NCon -> ExpQ
makeConIsoExp target con = appsE [varE isoValName, remitter, reviewer]
  where
  conName = view nconName con
  reviewer = makeReviewer target conName
  remitter = makeIsoRemitter target conName


-- | Construct a Review expression
--
-- unto (\(x,y,z) -> Con x y z)
makeConReviewExp :: PrismTarget -> NCon -> ExpQ
makeConReviewExp target con = appE (varE untoValName) reviewer
  where
  conName = view nconName con
  reviewer = makeReviewer target conName


------------------------------------------------------------------------
-- Prism and Iso component builders
------------------------------------------------------------------------


-- | Construct the review portion of a prism.
--
-- (\(x,y,z) -> Con x y z) :: b -> t
--
-- (\(Foo x y z)) -> Con x y z) :: b -> t
makeReviewer :: PrismTarget -> Name -> ExpQ
makeReviewer target conName =
  do xs <- makeTargetNames target
     lam1E (targetPattern xs target)
           (conE conName `appsE1` map varE xs)


-- | Construct the remit portion of a prism.
-- Pattern match only target constructor, no type changing
--
-- (\x -> case s of
--          Con x y z -> Right (x,y,z)
--          _         -> Left x
-- ) :: s -> Either s a
--
-- (\x -> case s of
--          Con x y z -> Right (Foo (x,y,z))
--          _         -> Left x
-- ) :: s -> Either s a
makeSimpleRemitter :: PrismTarget -> Name -> ExpQ
makeSimpleRemitter target conName =
  do x  <- newName "x"
     xs <- makeTargetNames target
     let matches =
           [ match (conP conName (map varP xs))
                   (normalB
                     (appE (conE rightDataName)
                     (targetConExp xs target))) []
           , match wildP (normalB (appE (conE leftDataName) (varE x))) []
           ]
     lam1E (varP x) (caseE (varE x) matches)


-- | Pattern match all constructors to enable type-changing
--
-- (\x -> case s of
--          Con x y z -> Right (x,y,z)
--          Other_n w   -> Left (Other_n w)
-- ) :: s -> Either t a
--
-- (\x -> case s of
--          Con x y z -> Right (Foo x y z)
--          Other_n w   -> Left (Other_n w)
-- ) :: s -> Either t a

makeFullRemitter :: PrismTarget -> [NCon] -> Name -> ExpQ
makeFullRemitter target cons con =
  do x <- newName "x"
     lam1E (varP x) (caseE (varE x) (map mkMatch cons))
  where
  mkMatch (NCon conName _ n) =
    do xs <- newNames "y" (length n)
       match (conP conName (map varP xs))
             (normalB
               (if conName == con
                  then appE (conE rightDataName) (targetConExp xs target)
                  else appE (conE leftDataName) (conE conName `appsE1` map varE xs)))
             []


-- | Construct the remitter suitable for use in an 'Iso'
--
-- (\(Con x y z) -> (x,y,z)) :: s -> a
--
-- (\(Con x y z) -> Foo x yz) :: s -> a
makeIsoRemitter :: PrismTarget -> Name -> ExpQ
makeIsoRemitter target conName =
  do xs <- makeTargetNames target
     lam1E (conP conName (map varP xs))
           (targetConExp xs target)


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
  PrismRulesSelector           ->
  Type   {- Outer type      -} ->
  Name   {- Class name      -} ->
  Name   {- Top method name -} ->
  [NCon] {- Constructors    -} ->
  DecQ
makeClassyPrismClass prs t className methodName cons =
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
    do Stab cx o target _ _ _ b <- computeOpticStab prs t cons con
       let stab' = Stab cx o target r r b b
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
  PrismRulesSelector ->
  Type ->
  Name     {- Class name      -} ->
  Name     {- Top method name -} ->
  [NCon] {- Constructors    -} ->
  DecQ
makeClassyPrismInstance prs s className methodName cons =
  do let vs = Set.toList (setOf typeVars s)
         cls = className `conAppsT` (s : map VarT vs)

     instanceD (cxt[]) (return cls)
       (   valD (varP methodName)
                (normalB (varE idValName)) []
       : [ do stab <- computeOpticStab prs s cons con
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
  , _nconFields :: [(Maybe Name, Type)]
  }
  deriving (Eq)

instance HasTypeVars NCon where
  typeVarsEx s f (NCon x y z) =
    NCon x <$> typeVarsEx s f y
           <*> (z & traverse . _2 %%~  typeVarsEx s f)

nconName :: Lens' NCon Name
nconName f x = fmap (\y -> x {_nconName = y}) (f (_nconName x))

nconCxt :: Lens' NCon (Maybe Cxt)
nconCxt f x = fmap (\y -> x {_nconCxt = y}) (f (_nconCxt x))

nconFields :: Lens' NCon [(Maybe Name, Type)]
nconFields f x = fmap (\y -> x {_nconFields = y}) (f (_nconFields x))

nconTypes :: Lens' NCon [Type]
nconTypes = partsOf (nconFields . traverse . _2)

-- | Normalize a single 'Con' to its constructor name and field types.
-- Because GADT syntax allows multiple constructors to be defined at
-- the same time, this function can return multiple normalized results.
normalizeCon :: Con -> [NCon]
normalizeCon (RecC    conName xs) =
  [NCon conName Nothing (map (\(n, _, t) -> (Just n, t)) xs)]
normalizeCon (NormalC conName xs) =
  [NCon conName Nothing (map (\(_, x) -> (Nothing, x)) xs)]
normalizeCon (InfixC (_,x) conName (_,y)) =
  [NCon conName Nothing [(Nothing, x), (Nothing, y)]]
normalizeCon (ForallC [] [] con) = normalizeCon con -- happens in GADTs
normalizeCon (ForallC _ cx1 con) =
  [NCon n (Just cx1 <> cx2) fs
    | NCon n cx2 fs <- normalizeCon con ]
#if MIN_VERSION_template_haskell(2,11,0)
normalizeCon (GadtC conNames xs _)    =
  [ NCon conName Nothing (map (\(_, x) -> (Nothing, x)) xs)
    | conName <- conNames ]
normalizeCon (RecGadtC conNames xs _) =
  [ NCon conName Nothing (map (\(n, _, t) -> (Just n, t)) xs)
    | conName <- conNames ]
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
