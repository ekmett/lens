{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
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
  , makeConstructors
  , makeHListPrisms
  , makeHListClassyPrisms
  , makeDecPrisms
  , makePrism
  ) where

import Control.Applicative
import Control.Lens.Getter
import Control.Lens.HList (HList(..))
import Control.Lens.Internal.FieldTH (makeClassInstance)
import Control.Lens.Internal.TH
import Control.Lens.Lens
import Control.Monad
import Data.Char (isUpper)
import qualified Data.List as List
import Data.Maybe (fromMaybe, isNothing)
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
--
-- On GHC 9.2 and later, with @-haddock@, each generated prism inherits its
-- constructor's Haddock documentation, as 'Control.Lens.TH.makeLenses'
-- does for fields.
makePrisms :: Name {- ^ Type constructor name -} -> DecsQ
makePrisms = makePrisms' tupleBuilders True


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
--
-- The class methods inherit their constructor's Haddock documentation, as
-- with 'makePrisms'.
makeClassyPrisms :: Name {- ^ Type constructor name -} -> DecsQ
makeClassyPrisms = makePrisms' tupleBuilders False


-- | Like 'makePrisms', but the fields of each constructor become a
-- 'Control.Lens.HList.HList' instead of a tuple.
--
-- A constructor with no field or one field also gets an 'HList': it focuses
-- @HList '[]@ or @HList '[a]@, not @()@ or @a@. To write these types, import
-- "Control.Lens.HList" and enable @DataKinds@.
--
-- /e.g./
--
-- @
-- data FooBarBaz a
--   = Foo Int
--   | Bar a
--   | Baz Int Char
-- makeHListPrisms ''FooBarBaz
-- @
--
-- will create
--
-- @
-- _Foo :: Prism' (FooBarBaz a) (HList '[Int])
-- _Bar :: Prism (FooBarBaz a) (FooBarBaz b) (HList '[a]) (HList '[b])
-- _Baz :: Prism' (FooBarBaz a) (HList '[Int, Char])
-- @
makeHListPrisms :: Name {- ^ Type constructor name -} -> DecsQ
makeHListPrisms = makePrisms' hlistBuilders True


-- | Like 'makeClassyPrisms', but the fields of each constructor become a
-- 'Control.Lens.HList.HList' instead of a tuple. See 'makeHListPrisms'.
makeHListClassyPrisms :: Name {- ^ Type constructor name -} -> DecsQ
makeHListClassyPrisms = makePrisms' hlistBuilders False


-- | How to bundle the fields of one constructor.
--
-- The comments below show tuples, such as @(x,y,z)@. With 'hlistBuilders' the
-- same place holds @x :# y :# z :# HNil@.
data FieldBuilders = FieldBuilders
  { fbType :: [TypeQ] -> TypeQ -- ^ the type of the fields (the optic's focus)
  , fbExp  :: [ExpQ]  -> ExpQ  -- ^ the fields as a value (the reviewer)
  , fbPat  :: [PatQ]  -> PatQ  -- ^ the fields as a pattern (the remitter)
  }

-- | Bundle the fields into a tuple, as 'makePrisms' does.
tupleBuilders :: FieldBuilders
tupleBuilders = FieldBuilders toTupleT toTupleE toTupleP

-- | Bundle the fields into an 'HList', as 'makeHListPrisms' does. Unlike a
-- tuple, 0 and 1 fields are not special cases.
hlistBuilders :: FieldBuilders
hlistBuilders = FieldBuilders toHListT toHListE toHListP

-- | @[a,b]@ becomes @HList '[a,b]@.
toHListT :: [TypeQ] -> TypeQ
toHListT ts = conT ''HList `appT` foldr cons promotedNilT ts
  where cons t acc = promotedConsT `appT` t `appT` acc

-- | @[x,y]@ becomes @x :# y :# HNil@.
toHListE :: [ExpQ] -> ExpQ
toHListE = foldr (\x acc -> appsE1 (conE '(:#)) [x, acc]) (conE 'HNil)

-- | @[x,y]@ becomes the pattern @x :# y :# HNil@.
toHListP :: [PatQ] -> PatQ
toHListP = foldr (\p acc -> conP '(:#) [p, acc]) (conP 'HNil [])


-- | Main entry point into Prism generation for a given type constructor name.
makePrisms' :: FieldBuilders -> Bool -> Name -> DecsQ
makePrisms' fb normal typeName =
  do info <- D.reifyDatatype typeName
     let cls | normal    = Nothing
             | otherwise = Just (D.datatypeName info)
         cons = D.datatypeCons info
     makeConsPrisms fb (datatypeTypeKinded info) (map normalizeCon cons) cls


-- | Generate prisms for the given 'Dec'
makeDecPrisms :: Bool {- ^ generate top-level definitions -} -> Dec -> DecsQ
makeDecPrisms normal dec =
  do info <- D.normalizeDec dec
     let cls | normal    = Nothing
             | otherwise = Just (D.datatypeName info)
         cons = D.datatypeCons info
     makeConsPrisms tupleBuilders (datatypeTypeKinded info) (map normalizeCon cons) cls


-- | Build a single optic for one data constructor, as an /expression/.
-- This is the one-off counterpart to 'makePrisms': rather than declaring a
-- @_Ctor@ for every constructor of a type, it splices in just the optic for
-- the named constructor, with its type inferred at the use site.
--
-- The kind of optic produced matches what 'makePrisms' would generate for
-- that constructor:
--
-- * a 'Control.Lens.Iso.Iso' when the constructor is the type's /only/
--   constructor (and is not existential\/GADT-like);
-- * a 'Control.Lens.Review.Review' for an existentially quantified or GADT
--   constructor; and
-- * a 'Control.Lens.Prism.Prism' otherwise.
makePrism :: Name {- ^ Data constructor name -} -> ExpQ
makePrism conName =
  do info <- D.reifyDatatype conName
     let t    = datatypeTypeKinded info
         cons = map normalizeCon (D.datatypeCons info)
     case cons of
       -- A type with a single, non-existential constructor yields an Iso,
       -- exactly as the special case in 'makeConsPrisms' does.
       [con@(NCon _ [] [] _)] -> makeConIsoExp tupleBuilders con
       _ -> case List.find (\con -> view nconName con == conName) cons of
              Just con -> do stab <- computeOpticType tupleBuilders t cons con
                             makeConOpticExp tupleBuilders stab cons con
              Nothing  -> fail $ "makePrism: " ++ nameBase conName
                              ++ " is not a data constructor of "
                              ++ nameBase (D.datatypeName info)


-- | Generate overloaded constructor prisms: the 'makePrisms' counterpart of
-- 'Control.Lens.TH.makeFields'. Each constructor @Con@ gets a class @AsCon@
-- whose one method, @_Con@, is a simple prism onto its fields, and this
-- type's instance of the class. The class is declared only when no class of
-- that name is in scope, so a later invocation on another type with a
-- same-named constructor just adds an instance, and both types share the
-- method.
--
-- /e.g./
--
-- @
-- data Type1 = One Int String | Two String
-- makeConstructors ''Type1
-- @
--
-- will create
--
-- @
-- class AsOne s a | s -> a where
--   _One :: Prism' s a
-- instance AsOne Type1 (Int, String) where
--   _One = prism ...
-- class AsTwo s a | s -> a where
--   _Two :: Prism' s a
-- instance AsTwo Type1 String where
--   _Two = prism ...
-- @
--
-- and then, anywhere @AsOne@ is in scope,
--
-- @
-- data Type2 = One Double | More Int
-- makeConstructors ''Type2
-- @
--
-- will create
--
-- @
-- instance AsOne Type2 Double where
--   _One = prism ...
-- class AsMore s a | s -> a where
--   _More :: Prism' s a
-- instance AsMore Type2 Int where
--   _More = prism ...
-- @
--
-- The methods are always simple prisms, even for a lone constructor or one
-- whose fields could change type, where 'makePrisms' would give an @Iso@ or
-- a type-changing @Prism@. They take the @_Con@ names 'makePrisms' would
-- use, so apply one generator or the other to a given type, not both.
--
-- Nothing is generated for an existentially quantified constructor, whose
-- payload type the functional dependency could not determine, or for an
-- operator-named constructor, which cannot form the @AsCon@ identifier.
--
-- Class sharing is by name: @AsCon@ and @_Con@ must be in scope unqualified,
-- and whatever is already named @AsCon@ gets the instance. A payload type
-- mentioning a type family is hidden behind an equality constraint in the
-- instance head, as 'Control.Lens.TH.makeFields' does; such an instance needs
-- @UndecidableInstances@ at the splice site.
--
-- The methods inherit no constructor documentation, being shared between
-- types.
makeConstructors :: Name {- ^ Type constructor name -} -> DecsQ
makeConstructors typeName =
  do info <- D.reifyDatatype typeName
     let t    = datatypeTypeKinded info
         cons = map normalizeCon (D.datatypeCons info)
     -- Constructor names are unique within a module, so unlike makeFields no
     -- bookkeeping is needed to avoid declaring a class twice per splice.
     fmap concat (for cons (makeConstructorDecs t cons))


-- | The class of one constructor, unless already in scope, and this type's
-- instance of it; nothing for a constructor that admits no overloaded
-- @Prism'@.
makeConstructorDecs :: Type -> [NCon] -> NCon -> DecsQ
makeConstructorDecs t cons con =
  do stab <- computeOpticType tupleBuilders t cons con
     let conName = view nconName con
     case stabType stab of
       PrismType | isPrefixName conName ->
         do let Stab cx _ _ _ _ b = stab -- b: the tuple of field types
                methodName = prismName conName
                clsBase    = "As" ++ nameBase conName
            mcls <- lookupTypeName clsBase
            let className = fromMaybe (mkName clsBase) mcls
            sequenceA
              ( [ makeConstructorClass className methodName | isNothing mcls ]
              ++ [ makeClassInstance cx className t b
                     ( valD (varP methodName)
                            (normalB (makeConOpticExp tupleBuilders stab cons con)) []
                     : inlinePragma methodName ) ]
              )
       _ -> return []


-- | @class AsCon s a | s -> a where _Con :: Prism' s a@
makeConstructorClass :: Name -> Name -> DecQ
makeConstructorClass className methodName =
  classD (cxt []) className [D.plainTV s, D.plainTV a] [FunDep [s] [a]]
    [sigD methodName (return (prism'TypeName `conAppsT` [VarT s, VarT a]))]
  where
  s = mkName "s"
  a = mkName "a"


-- | Generate prisms for the given type, normalized constructors, and
-- an optional name to be used for generating a prism class.
-- This function dispatches between Iso generation, normal top-level
-- prisms, and classy prisms.
makeConsPrisms :: FieldBuilders -> Type -> [NCon] -> Maybe Name -> DecsQ

-- special case: single constructor, not classy -> make iso
-- ('makePrism' has the corresponding single-constructor Iso case; keep the two
-- in sync.)
makeConsPrisms fb t [con@(NCon _ [] [] _)] Nothing = makeConIso fb t con

-- top-level definitions
makeConsPrisms fb t cons Nothing =
  fmap concat $ for cons $ \con ->
    do let conName = view nconName con
       stab <- computeOpticType fb t cons con
       let n = prismName conName
       copyDocs [conName] n
       sequenceA
         ( [ sigD n (return (quantifyType [] (stabToType Set.empty stab)))
           , valD (varP n) (normalB (makeConOpticExp fb stab cons con)) []
           ]
           ++ inlinePragma n
         )


-- classy prism class and instance
makeConsPrisms fb t cons (Just typeName) =
  sequenceA
    [ makeClassyPrismClass fb t className methodName cons
    , makeClassyPrismInstance fb t className methodName cons
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

computeOpticType :: FieldBuilders -> Type -> [NCon] -> NCon -> Q Stab
computeOpticType fb t cons con =
  do let cons' = List.delete con cons
     if null (_nconVars con)
         then computePrismType fb t (view nconCxt con) cons' con
         else computeReviewType fb t (view nconCxt con) (view nconTypes con)


computeReviewType :: FieldBuilders -> Type -> Cxt -> [Type] -> Q Stab
computeReviewType fb s' cx tys =
  do let t = s'
     s <- fmap VarT (newName "s")
     a <- fmap VarT (newName "a")
     b <- fbType fb (map return tys)
     return (Stab cx ReviewType s t a b)


-- | Compute the full type-changing Prism type given an outer type,
-- list of constructors, and target constructor name. Additionally
-- return 'True' if the resulting type is a "simple" prism.
computePrismType :: FieldBuilders -> Type -> Cxt -> [NCon] -> NCon -> Q Stab
computePrismType fb t cx cons con =
  do let ts      = view nconTypes con
         unbound = setOf typeVars t Set.\\ setOf typeVars cons
     sub <- sequenceA (Map.fromSet (newName . nameBase) unbound)
     b   <- fbType fb (map return ts)
     a   <- fbType fb (map return (substTypeVars sub ts))
     let s = substTypeVars sub t
     return (Stab cx PrismType s t a b)


computeIsoType :: FieldBuilders -> Type -> [Type] -> TypeQ
computeIsoType fb t' fields =
  do sub <- sequenceA (Map.fromSet (newName . nameBase) (setOf typeVars t'))
     let t = return                    t'
         s = return (substTypeVars sub t')
         b = fbType fb (map return                    fields)
         a = fbType fb (map return (substTypeVars sub fields))

         ty | Map.null sub = appsT (conT iso'TypeName) [t,b]
            | otherwise    = appsT (conT isoTypeName) [s,t,a,b]

     quantifyType [] <$> ty



-- | Construct either a Review or Prism as appropriate
makeConOpticExp :: FieldBuilders -> Stab -> [NCon] -> NCon -> ExpQ
makeConOpticExp fb stab cons con =
  case stabType stab of
    PrismType  -> makeConPrismExp fb stab cons con
    ReviewType -> makeConReviewExp fb con


-- | Construct an iso declaration
makeConIso :: FieldBuilders -> Type -> NCon -> DecsQ
makeConIso fb s con =
  do let ty      = computeIsoType fb s (view nconTypes con)
         defName = prismName (view nconName con)
     copyDocs [view nconName con] defName
     sequenceA
       ( [ sigD       defName  ty
         , valD (varP defName) (normalB (makeConIsoExp fb con)) []
         ] ++
         inlinePragma defName
       )


-- | Construct prism expression
--
-- prism <<reviewer>> <<remitter>>
makeConPrismExp ::
  FieldBuilders ->
  Stab ->
  [NCon] {- ^ constructors       -} ->
  NCon   {- ^ target constructor -} ->
  ExpQ
makeConPrismExp fb stab cons con = appsE [varE prismValName, reviewer, remitter]
  where
  ts = view nconTypes con
  fields  = length ts
  conName = view nconName con

  reviewer                   = makeReviewer       fb conName fields
  remitter | stabSimple stab = makeSimpleRemitter fb conName (length cons) fields
           | otherwise       = makeFullRemitter fb cons conName


-- | Construct an Iso expression
--
-- iso <<reviewer>> <<remitter>>
makeConIsoExp :: FieldBuilders -> NCon -> ExpQ
makeConIsoExp fb con = appsE [varE isoValName, remitter, reviewer]
  where
  conName = view nconName con
  fields  = length (view nconTypes con)

  reviewer = makeReviewer    fb conName fields
  remitter = makeIsoRemitter fb conName fields


-- | Construct a Review expression
--
-- unto (\(x,y,z) -> Con x y z)
makeConReviewExp :: FieldBuilders -> NCon -> ExpQ
makeConReviewExp fb con = appE (varE untoValName) reviewer
  where
  conName = view nconName con
  fields  = length (view nconTypes con)

  reviewer = makeReviewer fb conName fields


------------------------------------------------------------------------
-- Prism and Iso component builders
------------------------------------------------------------------------


-- | Construct the review portion of a prism.
--
-- (\(x,y,z) -> Con x y z) :: b -> t
makeReviewer :: FieldBuilders -> Name -> Int -> ExpQ
makeReviewer fb conName fields =
  do xs <- newNames "x" fields
     lam1E (fbPat fb (map varP xs))
           (conE conName `appsE1` map varE xs)


-- | Construct the remit portion of a prism.
-- Pattern match only target constructor, no type changing
--
-- (\x -> case s of
--          Con x y z -> Right (x,y,z)
--          _         -> Left x
-- ) :: s -> Either s a
makeSimpleRemitter ::
  FieldBuilders ->
  Name {- The name of the constructor on which this prism focuses -} ->
  Int  {- The number of constructors the parent data type has     -} ->
  Int  {- The number of fields the constructor has                -} ->
  ExpQ
makeSimpleRemitter fb conName numCons fields =
  do x  <- newName "x"
     xs <- newNames "y" fields
     let matches =
           [ match (conP conName (map varP xs))
                   (normalB (appE (conE rightDataName) (fbExp fb (map varE xs))))
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
makeFullRemitter :: FieldBuilders -> [NCon] -> Name -> ExpQ
makeFullRemitter fb cons target =
  do x <- newName "x"
     lam1E (varP x) (caseE (varE x) (map mkMatch cons))
  where
  mkMatch (NCon conName _ _ n) =
    do xs <- newNames "y" (length n)
       match (conP conName (map varP xs))
             (normalB
               (if conName == target
                  then appE (conE rightDataName) (fbExp fb (map varE xs))
                  else appE (conE leftDataName) (conE conName `appsE1` map varE xs)))
             []


-- | Construct the remitter suitable for use in an 'Iso'
--
-- (\(Con x y z) -> (x,y,z)) :: s -> a
makeIsoRemitter :: FieldBuilders -> Name -> Int -> ExpQ
makeIsoRemitter fb conName fields =
  do xs <- newNames "x" fields
     lam1E (conP conName (map varP xs))
           (fbExp fb (map varE xs))


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
  FieldBuilders ->
  Type   {- Outer type      -} ->
  Name   {- Class name      -} ->
  Name   {- Top method name -} ->
  [NCon] {- Constructors    -} ->
  DecQ
makeClassyPrismClass fb t className methodName cons =
  do r <- newName "r"
     let methodType = appsT (conT prism'TypeName) [varT r,return t]
     methodss <- traverse (mkMethod r) cons
     classD (cxt[]) className (D.plainTV r : vs) (fds r)
       ( sigD methodName methodType
       : map return (concat methodss)
       )

  where
  mkMethod r con =
    do Stab cx o _ _ _ b <- computeOpticType fb t cons con
       let rTy     = VarT r
           stab'   = Stab cx o rTy rTy b b
           conName = view nconName con
           defName = prismName conName
           body    = appsE [varE composeValName, varE methodName, varE defName]
       copyDocs [conName] defName
       sequenceA
         [ sigD defName        (return (stabToType (Set.fromList (r:vNames)) stab'))
         , valD (varP defName) (normalB body) []
         ]

  vs            = D.changeTVFlags bndrReq $ D.freeVariablesWellScoped [t]
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
  FieldBuilders ->
  Type ->
  Name     {- Class name      -} ->
  Name     {- Top method name -} ->
  [NCon] {- Constructors    -} ->
  DecQ
makeClassyPrismInstance fb s className methodName cons =
  do let vs = D.freeVariablesWellScoped [s]
         cls = className `conAppsT` (s : map tvbToType vs)

     instanceD (cxt[]) (return cls)
       (   valD (varP methodName)
                (normalB (varE idValName)) []
       : [ do stab <- computeOpticType fb s cons con
              let stab' = simplifyStab stab
              valD (varP (prismName conName))
                (normalB (makeConOpticExp fb stab' cons con)) []
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
prismName' sameNameAsCon n
  | null nb        = error "prismName: empty name base?"
  | isPrefixName n = mkName (prefix '_' nb)
  | otherwise      = mkName (prefix '.' nb) -- operator
  where
    nb = nameBase n
    prefix :: Char -> String -> String
    prefix char str | sameNameAsCon = char:char:str
                    | otherwise     =      char:str

-- | Whether a name is spelled as an identifier rather than an operator.
isPrefixName :: Name -> Bool
isPrefixName n = case nameBase n of
                   c:_ -> isUpper c
                   []  -> False
