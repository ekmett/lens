{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.FieldTH
-- Copyright   :  (C) 2014-2015 Edward Kmett, (C) 2014 Eric Mertens
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Control.Lens.Internal.FieldTH
  ( LensRules(..)
  , DefName(..)
  , makeFieldOptics
  , makeFieldOpticsForDec
  ) where

import Control.Lens.At
import Control.Lens.Fold
import Control.Lens.Internal.TH
import Control.Lens.Plated
import Control.Lens.Prism
import Control.Lens.Setter
import Control.Lens.Getter
import Control.Lens.Traversal
import Control.Lens.Tuple
import Control.Applicative
import Control.Monad
import Language.Haskell.TH.Lens
import Language.Haskell.TH
import Data.Foldable (toList)
import Data.Maybe (isJust,maybeToList)
import Data.List (nub, findIndices)
import Data.Either (partitionEithers)
import Data.Set.Lens
import           Data.Map ( Map )
import           Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Traversable as T
import Prelude


------------------------------------------------------------------------
-- Field generation entry point
------------------------------------------------------------------------


-- | Compute the field optics for the type identified by the given type name.
-- Lenses will be computed when possible, Traversals otherwise.
makeFieldOptics :: LensRules -> Name -> DecsQ
makeFieldOptics rules tyName =
  do info <- reify tyName
     case info of
       TyConI dec -> makeFieldOpticsForDec rules dec
       _          -> fail "makeFieldOptics: Expected type constructor name"


makeFieldOpticsForDec :: LensRules -> Dec -> DecsQ
makeFieldOpticsForDec rules dec = case dec of
  DataD    _ tyName vars cons _ ->
    makeFieldOpticsForDec' rules tyName (mkS tyName vars) cons
  NewtypeD _ tyName vars con  _ ->
    makeFieldOpticsForDec' rules tyName (mkS tyName vars) [con]
  DataInstD _ tyName args cons _ ->
    makeFieldOpticsForDec' rules tyName (tyName `conAppsT` args) cons
  NewtypeInstD _ tyName args con _ ->
    makeFieldOpticsForDec' rules tyName (tyName `conAppsT` args) [con]
  _ -> fail "makeFieldOptics: Expected data or newtype type-constructor"
  where
  mkS tyName vars = tyName `conAppsT` map VarT (toListOf typeVars vars)


-- | Compute the field optics for a deconstructed Dec
-- When possible build an Iso otherwise build one optic per field.
makeFieldOpticsForDec' :: LensRules -> Name -> Type -> [Con] -> DecsQ
makeFieldOpticsForDec' rules tyName s cons =
  do fieldCons <- traverse normalizeConstructor cons
     let allFields  = toListOf (folded . _2 . folded . _1 . folded) fieldCons
     let defCons    = over normFieldLabels (expandName allFields) fieldCons
         allDefs    = setOf (normFieldLabels . folded) defCons
     perDef <- T.sequenceA (fromSet (buildScaffold rules s defCons) allDefs)

     let defs = Map.toList perDef
     case _classyLenses rules tyName of
       Just (className, methodName) ->
         makeClassyDriver rules className methodName s defs
       Nothing -> do decss  <- traverse (makeFieldOptic rules) defs
                     return (concat decss)

  where

  -- Traverse the field labels of a normalized constructor
  normFieldLabels :: Traversal [(Name,[(a,Type)])] [(Name,[(b,Type)])] a b
  normFieldLabels = traverse . _2 . traverse . _1

  -- Map a (possibly missing) field's name to zero-to-many optic definitions
  expandName :: [Name] -> Maybe Name -> [DefName]
  expandName allFields = concatMap (_fieldToDef rules tyName allFields) . maybeToList

-- | Normalized the Con type into a uniform positional representation,
-- eliminating the variance between records, infix constructors, and normal
-- constructors.
normalizeConstructor ::
  Con ->
  Q (Name, [(Maybe Name, Type)]) -- ^ constructor name, field name, field type

normalizeConstructor (RecC n xs) =
  return (n, [ (Just fieldName, ty) | (fieldName,_,ty) <- xs])

normalizeConstructor (NormalC n xs) =
  return (n, [ (Nothing, ty) | (_,ty) <- xs])

normalizeConstructor (InfixC (_,ty1) n (_,ty2)) =
  return (n, [ (Nothing, ty1), (Nothing, ty2) ])

normalizeConstructor (ForallC _ _ con) =
  do con' <- normalizeConstructor con
     return (set (_2 . mapped . _1) Nothing con')



data OpticType = GetterType | LensType | IsoType

-- | Compute the positional location of the fields involved in
-- each constructor for a given optic definition as well as the
-- type of clauses to generate and the type to annotate the declaration
-- with.
buildScaffold ::
  LensRules                                                                  ->
  Type                              {- ^ outer type                       -} ->
  [(Name, [([DefName], Type)])]     {- ^ normalized constructors          -} ->
  DefName                           {- ^ target definition                -} ->
  Q (OpticType, OpticStab, [(Name, Int, [Int])])
              {- ^ optic type, definition type, field count, target fields -}
buildScaffold rules s cons defName =

  do (s',t,a,b) <- buildStab s (concatMap snd consForDef)

     let defType
           | Just (_,cx,a') <- preview _ForallT a =
               let optic | lensCase  = getterTypeName
                         | otherwise = foldTypeName
               in OpticSa cx optic s' a'

           -- Getter and Fold are always simple
           | not (_allowUpdates rules) =
               let optic | lensCase  = getterTypeName
                         | otherwise = foldTypeName
               in OpticSa [] optic s' a

           -- Generate simple Lens and Traversal where possible
           | _simpleLenses rules || s' == t && a == b =
               let optic | isoCase && _allowIsos rules = iso'TypeName
                         | lensCase                    = lens'TypeName
                         | otherwise                   = traversal'TypeName
               in OpticSa [] optic s' a

           -- Generate type-changing Lens and Traversal otherwise
           | otherwise =
               let optic | isoCase && _allowIsos rules = isoTypeName
                         | lensCase                    = lensTypeName
                         | otherwise                   = traversalTypeName
               in OpticStab optic s' t a b

         opticType | has _ForallT a            = GetterType
                   | not (_allowUpdates rules) = GetterType
                   | isoCase                   = IsoType
                   | otherwise                 = LensType

     return (opticType, defType, scaffolds)
  where
  consForDef :: [(Name, [Either Type Type])]
  consForDef = over (mapped . _2 . mapped) categorize cons

  scaffolds :: [(Name, Int, [Int])]
  scaffolds = [ (n, length ts, rightIndices ts) | (n,ts) <- consForDef ]

  rightIndices :: [Either Type Type] -> [Int]
  rightIndices = findIndices (has _Right)

  -- Right: types for this definition
  -- Left : other types
  categorize :: ([DefName], Type) -> Either Type Type
  categorize (defNames, t)
    | defName `elem` defNames = Right t
    | otherwise               = Left  t

  lensCase :: Bool
  lensCase = all (\x -> lengthOf (_2 . folded . _Right) x == 1) consForDef

  isoCase :: Bool
  isoCase = case scaffolds of
              [(_,1,[0])] -> True
              _           -> False


data OpticStab = OpticStab     Name Type Type Type Type
               | OpticSa   Cxt Name Type Type

stabToType :: OpticStab -> Type
stabToType (OpticStab  c s t a b) = quantifyType [] (c `conAppsT` [s,t,a,b])
stabToType (OpticSa cx c s   a  ) = quantifyType cx (c `conAppsT` [s,a])

stabToContext :: OpticStab -> Cxt
stabToContext OpticStab{}        = []
stabToContext (OpticSa cx _ _ _) = cx

stabToOptic :: OpticStab -> Name
stabToOptic (OpticStab c _ _ _ _) = c
stabToOptic (OpticSa _ c _ _) = c

stabToS :: OpticStab -> Type
stabToS (OpticStab _ s _ _ _) = s
stabToS (OpticSa _ _ s _) = s

stabToA :: OpticStab -> Type
stabToA (OpticStab _ _ _ a _) = a
stabToA (OpticSa _ _ _ a) = a

-- | Compute the s t a b types given the outer type 's' and the
-- categorized field types. Left for fixed and Right for visited.
-- These types are "raw" and will be packaged into an 'OpticStab'
-- shortly after creation.
buildStab :: Type -> [Either Type Type] -> Q (Type,Type,Type,Type)
buildStab s categorizedFields =
  do (subA,a) <- unifyTypes targetFields
     let s' = applyTypeSubst subA s

     -- compute possible type changes
     sub <- T.sequenceA (fromSet (newName . nameBase) unfixedTypeVars)
     let (t,b) = over both (substTypeVars sub) (s',a)

     return (s',t,a,b)

  where
  (fixedFields, targetFields) = partitionEithers categorizedFields
  fixedTypeVars               = setOf typeVars fixedFields
  unfixedTypeVars             = setOf typeVars s Set.\\ fixedTypeVars


-- | Build the signature and definition for a single field optic.
-- In the case of a singleton constructor irrefutable matches are
-- used to enable the resulting lenses to be used on a bottom value.
makeFieldOptic ::
  LensRules ->
  (DefName, (OpticType, OpticStab, [(Name, Int, [Int])])) ->
  DecsQ
makeFieldOptic rules (defName, (opticType, defType, cons)) =
  do cls <- mkCls
     T.sequenceA (cls ++ sig ++ def)
  where
  mkCls = case defName of
          MethodName c n | _generateClasses rules ->
            do classExists <- isJust <$> lookupTypeName (show c)
               return (if classExists then [] else [makeFieldClass defType c n])
          _ -> return []

  sig = case defName of
          _ | not (_generateSigs rules) -> []
          TopName n -> [sigD n (return (stabToType defType))]
          MethodName{} -> []

  fun n = funD n clauses : inlinePragma n

  def = case defName of
          TopName n      -> fun n
          MethodName c n -> [makeFieldInstance defType c (fun n)]

  clauses = makeFieldClauses rules opticType cons


------------------------------------------------------------------------
-- Classy class generator
------------------------------------------------------------------------


makeClassyDriver ::
  LensRules ->
  Name ->
  Name ->
  Type {- ^ Outer 's' type -} ->
  [(DefName, (OpticType, OpticStab, [(Name, Int, [Int])]))] ->
  DecsQ
makeClassyDriver rules className methodName s defs = T.sequenceA (cls ++ inst)

  where
  cls | _generateClasses rules = [makeClassyClass className methodName s defs]
      | otherwise = []

  inst = [makeClassyInstance rules className methodName s defs]


makeClassyClass ::
  Name ->
  Name ->
  Type {- ^ Outer 's' type -} ->
  [(DefName, (OpticType, OpticStab, [(Name, Int, [Int])]))] ->
  DecQ
makeClassyClass className methodName s defs = do
  let ss   = map (stabToS . view (_2 . _2)) defs
  (sub,s') <- unifyTypes (s : ss)
  c <- newName "c"
  let vars = toListOf typeVars s'
      fd   | null vars = []
           | otherwise = [FunDep [c] vars]


  classD (cxt[]) className (map PlainTV (c:vars)) fd
    $ sigD methodName (return (lens'TypeName `conAppsT` [VarT c, s']))
    : concat
      [ [sigD defName (return ty)
        ,valD (varP defName) (normalB body) []
        ] ++
        inlinePragma defName
      | (TopName defName, (_, stab, _)) <- defs
      , let body = appsE [varE composeValName, varE methodName, varE defName]
      , let ty   = quantifyType' (Set.fromList (c:vars))
                                 (stabToContext stab)
                 $ stabToOptic stab `conAppsT`
                       [VarT c, applyTypeSubst sub (stabToA stab)]
      ]


makeClassyInstance ::
  LensRules ->
  Name ->
  Name ->
  Type {- ^ Outer 's' type -} ->
  [(DefName, (OpticType, OpticStab, [(Name, Int, [Int])]))] ->
  DecQ
makeClassyInstance rules className methodName s defs = do
  methodss <- traverse (makeFieldOptic rules') defs

  instanceD (cxt[]) (return instanceHead)
    $ valD (varP methodName) (normalB (varE idValName)) []
    : map return (concat methodss)

  where
  instanceHead = className `conAppsT` (s : map VarT vars)
  vars         = toListOf typeVars s
  rules'       = rules { _generateSigs    = False
                       , _generateClasses = False
                       }

------------------------------------------------------------------------
-- Field class generation
------------------------------------------------------------------------

makeFieldClass :: OpticStab -> Name -> Name -> DecQ
makeFieldClass defType className methodName =
  classD (cxt []) className [PlainTV s, PlainTV a] [FunDep [s] [a]]
         [sigD methodName (return methodType)]
  where
  methodType = quantifyType' (Set.fromList [s,a])
                             (stabToContext defType)
             $ stabToOptic defType `conAppsT` [VarT s,VarT a]
  s = mkName "s"
  a = mkName "a"

makeFieldInstance :: OpticStab -> Name -> [DecQ] -> DecQ
makeFieldInstance defType className =
  instanceD (cxt [])
    (return (className `conAppsT` [stabToS defType, stabToA defType]))

------------------------------------------------------------------------
-- Optic clause generators
------------------------------------------------------------------------


makeFieldClauses :: LensRules -> OpticType -> [(Name, Int, [Int])] -> [ClauseQ]
makeFieldClauses rules opticType cons =
  case opticType of

    IsoType    -> [ makeIsoClause conName | (conName, _, _) <- cons ]

    GetterType -> [ makeGetterClause conName fieldCount fields
                    | (conName, fieldCount, fields) <- cons ]

    LensType   -> [ makeFieldOpticClause conName fieldCount fields irref
                    | (conName, fieldCount, fields) <- cons ]
      where
      irref = _lazyPatterns rules
           && length cons == 1



-- | Construct an optic clause that returns an unmodified value
-- given a constructor name and the number of fields on that
-- constructor.
makePureClause :: Name -> Int -> ClauseQ
makePureClause conName fieldCount =
  do xs <- replicateM fieldCount (newName "x")
     -- clause: _ (Con x1..xn) = pure (Con x1..xn)
     clause [wildP, conP conName (map varP xs)]
            (normalB (appE (varE pureValName) (appsE (conE conName : map varE xs))))
            []


-- | Construct an optic clause suitable for a Getter or Fold
-- by visited the fields identified by their 0 indexed positions
makeGetterClause :: Name -> Int -> [Int] -> ClauseQ
makeGetterClause conName fieldCount []     = makePureClause conName fieldCount
makeGetterClause conName fieldCount fields =
  do f  <- newName "f"
     xs <- replicateM (length fields) (newName "x")

     let pats (i:is) (y:ys)
           | i `elem` fields = varP y : pats is ys
           | otherwise = wildP : pats is (y:ys)
         pats is     _  = map (const wildP) is

         fxs   = [ appE (varE f) (varE x) | x <- xs ]
         body  = foldl (\a b -> appsE [varE apValName, a, b])
                       (appE (varE coerceValName) (head fxs))
                       (tail fxs)

     -- clause f (Con x1..xn) = coerce (f x1) <*> ... <*> f xn
     clause [varP f, conP conName (pats [0..fieldCount - 1] xs)]
            (normalB body)
            []

-- | Build a clause that updates the field at the given indexes
-- When irref is 'True' the value with me matched with an irrefutable
-- pattern. This is suitable for Lens and Traversal construction
makeFieldOpticClause :: Name -> Int -> [Int] -> Bool -> ClauseQ
makeFieldOpticClause conName fieldCount [] _ =
  makePureClause conName fieldCount
makeFieldOpticClause conName fieldCount (field:fields) irref =
  do f  <- newName "f"
     xs <- replicateM fieldCount          (newName "x")
     ys <- replicateM (1 + length fields) (newName "y")

     let xs' = foldr (\(i,x) -> set (ix i) x) xs (zip (field:fields) ys)

         mkFx i = appE (varE f) (varE (xs !! i))

         body0 = appsE [ varE fmapValName
                       , lamE (map varP ys) (appsE (conE conName : map varE xs'))
                       , mkFx field
                       ]

         body = foldl (\a b -> appsE [varE apValName, a, mkFx b]) body0 fields

     let wrap = if irref then tildeP else id

     clause [varP f, wrap (conP conName (map varP xs))]
            (normalB body)
            []


-- | Build a clause that constructs an Iso
makeIsoClause :: Name -> ClauseQ
makeIsoClause conName = clause [] (normalB (appsE [varE isoValName, destruct, construct])) []
  where
  destruct  = do x <- newName "x"
                 lam1E (conP conName [varP x]) (varE x)

  construct = conE conName


------------------------------------------------------------------------
-- Unification logic
------------------------------------------------------------------------

-- The field-oriented optic generation supports incorporating fields
-- with distinct but unifiable types into a single definition.



-- | Unify the given list of types, if possible, and return the
-- substitution used to unify the types for unifying the outer
-- type when building a definition's type signature.
unifyTypes :: [Type] -> Q (Map Name Type, Type)
unifyTypes (x:xs) = foldM (uncurry unify1) (Map.empty, x) xs
unifyTypes []     = fail "unifyTypes: Bug: Unexpected empty list"


-- | Attempt to unify two given types using a running substitution
unify1 :: Map Name Type -> Type -> Type -> Q (Map Name Type, Type)
unify1 sub (VarT x) y
  | Just r <- Map.lookup x sub = unify1 sub r y
unify1 sub x (VarT y)
  | Just r <- Map.lookup y sub = unify1 sub x r
unify1 sub x y
  | x == y = return (sub, x)
unify1 sub (AppT f1 x1) (AppT f2 x2) =
  do (sub1, f) <- unify1 sub  f1 f2
     (sub2, x) <- unify1 sub1 x1 x2
     return (sub2, AppT (applyTypeSubst sub2 f) x)
unify1 sub x (VarT y)
  | elemOf typeVars y (applyTypeSubst sub x) =
      fail "Failed to unify types: occurs check"
  | otherwise = return (Map.insert y x sub, x)
unify1 sub (VarT x) y = unify1 sub y (VarT x)

-- TODO: Unify contexts
unify1 sub (ForallT v1 [] t1) (ForallT v2 [] t2) =
     -- This approach works out because by the time this code runs
     -- all of the type variables have been renamed. No risk of shadowing.
  do (sub1,t) <- unify1 sub t1 t2
     v <- fmap nub (traverse (limitedSubst sub1) (v1++v2))
     return (sub1, ForallT v [] t)

unify1 _ x y = fail ("Failed to unify types: " ++ show (x,y))


-- | Perform a limited substitution on type variables. This is used
-- when unifying rank-2 fields when trying to achieve a Getter or Fold.
limitedSubst :: Map Name Type -> TyVarBndr -> Q TyVarBndr
limitedSubst sub (PlainTV n)
  | Just r <- Map.lookup n sub =
       case r of
         VarT m -> limitedSubst sub (PlainTV m)
         _ -> fail "Unable to unify exotic higher-rank type"
limitedSubst sub (KindedTV n k)
  | Just r <- Map.lookup n sub =
       case r of
         VarT m -> limitedSubst sub (KindedTV m k)
         _ -> fail "Unable to unify exotic higher-rank type"
limitedSubst _ tv = return tv


-- | Apply a substitution to a type. This is used after unifying
-- the types of the fields in unifyTypes.
applyTypeSubst :: Map Name Type -> Type -> Type
applyTypeSubst sub = rewrite aux
  where
  aux (VarT n) = Map.lookup n sub
  aux _        = Nothing


------------------------------------------------------------------------
-- Field generation parameters
------------------------------------------------------------------------


data LensRules = LensRules
  { _simpleLenses    :: Bool
  , _generateSigs    :: Bool
  , _generateClasses :: Bool
  , _allowIsos       :: Bool
  , _allowUpdates    :: Bool -- ^ Allow Lens/Traversal (otherwise Getter/Fold)
  , _lazyPatterns    :: Bool
  , _fieldToDef      :: Name -> [Name] -> Name -> [DefName]
       -- ^ Type Name -> Field Names -> Target Field Name -> Definition Names
  , _classyLenses    :: Name -> Maybe (Name,Name)
       -- type name to class name and top method
  }


-- | Name to give to generated field optics.
data DefName
  = TopName Name -- ^ Simple top-level definiton name
  | MethodName Name Name -- ^ makeFields-style class name and method name
  deriving (Show, Eq, Ord)

------------------------------------------------------------------------
-- Miscellaneous utility functions
------------------------------------------------------------------------


-- | Template Haskell wants type variables declared in a forall, so
-- we find all free type variables in a given type and declare them.
quantifyType :: Cxt -> Type -> Type
quantifyType c t = ForallT vs c t
  where
  vs = map PlainTV (toList (setOf typeVars t))

-- | This function works like 'quantifyType' except that it takes
-- a list of variables to exclude from quantification.
quantifyType' :: Set Name -> Cxt -> Type -> Type
quantifyType' exclude c t = ForallT vs c t
  where
  vs = map PlainTV (toList (setOf typeVars t Set.\\ exclude))


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
