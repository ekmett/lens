{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) (defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706)
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.TH
-- Copyright   :  (C) 2012-13 Edward Kmett, Michael Sloan
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Control.Lens.TH
  (
  -- * Constructing Lenses Automatically
    makeLenses, makeLensesFor
  , makeClassy, makeClassyFor
  , makeIso
  , makePrisms
  , makeWrapped
  , makeFields
  -- * Configuring Lenses
  , makeLensesWith
  , makeFieldsWith
  , defaultRules
  , defaultFieldRules
  , camelCaseFields
  , underscoreFields
  , LensRules(LensRules)
  , FieldRules(FieldRules)
  , lensRules
  , classyRules
  , isoRules
  , lensIso
  , lensField
  , lensClass
  , lensFlags
  , LensFlag(..)
  , simpleLenses
  , partialLenses
  , buildTraversals
  , handleSingletons
  , singletonIso
  , singletonRequired
  , createClass
  , createInstance
  , classRequired
  , singletonAndField
  , generateSignatures
  ) where

import Control.Applicative
#if !(MIN_VERSION_template_haskell(2,7,0))
import Control.Monad (ap)
#endif
import Control.Lens.At
import Control.Lens.Combinators
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Iso
import Control.Lens.Lens
import Control.Lens.Prism
import Control.Lens.Setter
import Control.Lens.Tuple
import Control.Lens.Traversal
import Control.Lens.Wrapped
import Data.Char (toLower, toUpper, isUpper)
import Data.Either (lefts)
import Data.Foldable hiding (concat)
import Data.Function (on)
import Data.List as List
import Data.Map as Map hiding (toList,map,filter)
import Data.Maybe as Maybe (isNothing,isJust,catMaybes,fromJust,mapMaybe)
import Data.Ord (comparing)
import Data.Set as Set hiding (toList,map,filter)
import Data.Set.Lens
import Data.Traversable hiding (mapM)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lens

{-# ANN module "HLint: ignore Use foldl" #-}

-- | Flags for 'Lens' construction
data LensFlag
  = SimpleLenses
  | PartialLenses
  | BuildTraversals
  | SingletonAndField
  | SingletonIso
  | HandleSingletons
  | SingletonRequired
  | CreateClass
  | CreateInstance
  | ClassRequired
  | GenerateSignatures
  deriving (Eq,Ord,Show,Read)

-- | Only Generate valid 'Control.Lens.Type.Simple' lenses.
simpleLenses      :: Lens' LensRules Bool
simpleLenses       = lensFlags.contains SimpleLenses

-- | Enables the generation of partial lenses, generating runtime errors for
-- every constructor that does not have a valid definition for the 'Lens'. This
-- occurs when the constructor lacks the field, or has multiple fields mapped
-- to the same 'Lens'.
partialLenses     :: Lens' LensRules Bool
partialLenses      = lensFlags.contains PartialLenses

-- | In the situations that a 'Lens' would be partial, when 'partialLenses' is
-- used, this flag instead causes traversals to be generated. Only one can be
-- used, and if neither are, then compile-time errors are generated.
buildTraversals   :: Lens' LensRules Bool
buildTraversals    = lensFlags.contains BuildTraversals

-- | Handle singleton constructors specially.
handleSingletons  :: Lens' LensRules Bool
handleSingletons   = lensFlags.contains HandleSingletons

-- | When building a singleton 'Iso' (or 'Lens') for a record constructor, build
-- both the 'Iso' (or 'Lens') for the record and the one for the field.
singletonAndField :: Lens' LensRules Bool
singletonAndField  = lensFlags.contains SingletonAndField

-- | Use 'Iso' for singleton constructors.
singletonIso      :: Lens' LensRules Bool
singletonIso       = lensFlags.contains SingletonIso

-- | Expect a single constructor, single field newtype or data type.
singletonRequired :: Lens' LensRules Bool
singletonRequired  = lensFlags.contains SingletonRequired

-- | Create the class if the constructor is 'Control.Lens.Type.Simple' and the 'lensClass' rule matches.
createClass       :: Lens' LensRules Bool
createClass        = lensFlags.contains CreateClass

-- | Create the instance if the constructor is 'Control.Lens.Type.Simple' and the 'lensClass' rule matches.
createInstance    :: Lens' LensRules Bool
createInstance     = lensFlags.contains CreateInstance

-- | Die if the 'lensClass' fails to match.
classRequired     :: Lens' LensRules Bool
classRequired      = lensFlags.contains ClassRequired

-- | Indicate whether or not to supply the signatures for the generated
-- lenses.
--
-- Disabling this can be useful if you want to provide a more restricted type
-- signature or if you want to supply hand-written haddocks.
generateSignatures :: Lens' LensRules Bool
generateSignatures = lensFlags.contains GenerateSignatures

-- | This configuration describes the options we'll be using to make
-- isomorphisms or lenses.
data LensRules = LensRules
  { _lensIso   :: String -> Maybe String
  , _lensField :: String -> Maybe String
  , _lensClass :: String -> Maybe (String, String)
  , _lensFlags :: Set LensFlag
  }

-- | 'Lens'' to access the convention for naming top level isomorphisms in our
-- 'LensRules'.
--
-- Defaults to lowercasing the first letter of the constructor.
lensIso :: Lens' LensRules (String -> Maybe String)
lensIso f (LensRules i n c o) = f i <&> \i' -> LensRules i' n c o

-- | 'Lens'' to access the convention for naming fields in our 'LensRules'.
--
-- Defaults to stripping the _ off of the field name, lowercasing the name, and
-- rejecting the field if it doesn't start with an '_'.
lensField :: Lens' LensRules (String -> Maybe String)
lensField f (LensRules i n c o) = f n <&> \n' -> LensRules i n' c o

-- | Retrieve options such as the name of the class and method to put in it to
-- build a class around monomorphic data types.
lensClass :: Lens' LensRules (String -> Maybe (String, String))
lensClass f (LensRules i n c o) = f c <&> \c' -> LensRules i n c' o

-- | Retrieve options such as the name of the class and method to put in it to
-- build a class around monomorphic data types.
lensFlags :: Lens' LensRules (Set LensFlag)
lensFlags f (LensRules i n c o) = f o <&> LensRules i n c

-- | Default 'LensRules'.
defaultRules :: LensRules
defaultRules = LensRules mLowerName fld (const Nothing) $
    Set.fromList [SingletonIso, SingletonAndField, CreateClass, CreateInstance, BuildTraversals, GenerateSignatures]
  where
    fld ('_':cs) = mLowerName cs
    fld _        = Nothing

mLowerName :: String -> Maybe String
mLowerName (c:cs) = Just (toLower c:cs)
mLowerName _ = Nothing

-- | Rules for making fairly simple partial lenses, ignoring the special cases
-- for isomorphisms and traversals, and not making any classes.
lensRules :: LensRules
lensRules = defaultRules
  & lensIso          .~ const Nothing
  & lensClass        .~ const Nothing
  & handleSingletons .~ True
  & partialLenses    .~ False
  & buildTraversals  .~ True

-- | Rules for making lenses and traversals that precompose another 'Lens'.
classyRules :: LensRules
classyRules = defaultRules
  & lensIso .~ const Nothing
  & handleSingletons .~ False
  & lensClass .~ classy
  & classRequired .~ True
  & partialLenses .~ False
  & buildTraversals .~ True
  where
    classy :: String -> Maybe (String, String)
    classy n@(a:as) = Just ("Has" ++ n, toLower a:as)
    classy _ = Nothing

-- | Rules for making an isomorphism from a data type.
isoRules :: LensRules
isoRules = defaultRules
  & handleSingletons  .~ True
  & singletonRequired .~ True
  & singletonAndField .~ True

-- | Build lenses (and traversals) with a sensible default configuration.
--
-- @
-- 'makeLenses' = 'makeLensesWith' 'lensRules'
-- @
makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesWith lensRules

-- | Make lenses and traversals for a type, and create a class when the
-- type has no arguments.
--
-- /e.g./
--
-- @
-- data Foo = Foo { _fooX, _fooY :: 'Int' }
-- 'makeClassy' ''Foo
-- @
--
-- will create
--
-- @
-- class HasFoo t where
--   foo :: 'Control.Lens.Type.Simple' 'Lens' t Foo
-- instance HasFoo Foo where foo = 'id'
-- fooX, fooY :: HasFoo t => 'Control.Lens.Type.Simple' 'Lens' t 'Int'
-- @
--
-- @
-- 'makeClassy' = 'makeLensesWith' 'classyRules'
-- @
makeClassy :: Name -> Q [Dec]
makeClassy = makeLensesWith classyRules

-- | Make a top level isomorphism injecting /into/ the type.
--
-- The supplied name is required to be for a type with a single constructor
-- that has a single argument.
--
-- /e.g./
--
-- @
-- newtype 'List' a = 'List' [a]
-- 'makeIso' ''List
-- @
--
-- will create
--
-- @
-- 'list' :: 'Iso' [a] [b] ('List' a) ('List' b)
-- @
--
-- @
-- 'makeIso' = 'makeLensesWith' 'isoRules'
-- @
makeIso :: Name -> Q [Dec]
makeIso = makeLensesWith isoRules

-- | Derive lenses and traversals, specifying explicit pairings
-- of @(fieldName, lensName)@.
--
-- If you map multiple names to the same label, and it is present in the same
-- constructor then this will generate a 'Traversal'.
--
-- /e.g./
--
-- @
-- 'makeLensesFor' [(\"_foo\", \"fooLens\"), (\"baz\", \"lbaz\")] ''Foo
-- 'makeLensesFor' [(\"_barX\", \"bar\"), (\"_barY\", \"bar\")] ''Bar
-- @
makeLensesFor :: [(String, String)] -> Name -> Q [Dec]
makeLensesFor fields = makeLensesWith $ lensRules & lensField .~ (`Prelude.lookup` fields)

-- | Derive lenses and traversals, using a named wrapper class, and
-- specifying explicit pairings of @(fieldName, traversalName)@.
--
-- Example usage:
--
-- @
-- 'makeClassyFor' \"HasFoo\" \"foo\" [(\"_foo\", \"fooLens\"), (\"bar\", \"lbar\")] ''Foo
-- @
makeClassyFor :: String -> String -> [(String, String)] -> Name -> Q [Dec]
makeClassyFor clsName funName fields = makeLensesWith $ classyRules
  & lensClass .~ const (Just (clsName,funName))
  & lensField .~ (`Prelude.lookup` fields)

-- | Build lenses with a custom configuration.
makeLensesWith :: LensRules -> Name -> Q [Dec]
makeLensesWith cfg nm = do
    inf <- reify nm
    case inf of
      TyConI decl -> case deNewtype decl of
        DataD ctx tyConName args cons _ -> case cons of
          [NormalC dataConName [(    _,ty)]]
            | cfg^.handleSingletons  -> makeIsoLenses cfg ctx tyConName args dataConName Nothing ty
          [RecC    dataConName [(fld,_,ty)]]
            | cfg^.handleSingletons  -> makeIsoLenses cfg ctx tyConName args dataConName (Just fld) ty
          _ | cfg^.singletonRequired -> fail "makeLensesWith: A single-constructor single-argument data type is required"
            | otherwise              -> makeFieldLenses cfg ctx tyConName args cons
        _ -> fail "makeLensesWith: Unsupported data type"
      _ -> fail "makeLensesWith: Expected the name of a data type or newtype"

-- | Generate a 'Prism' for each constructor of a data type.
makePrisms :: Name -> Q [Dec]
makePrisms nm = do
    inf <- reify nm
    case inf of
      TyConI decl -> case deNewtype decl of
        DataD ctx tyConName args cons _ ->
          makePrismsForCons ctx tyConName args cons
        _ -> fail "makePrisms: Unsupported data type"
      _ -> fail "makePrisms: Expected the name of a data type or newtype"

-----------------------------------------------------------------------------
-- Internal TH Implementation
-----------------------------------------------------------------------------

-- | Transform @NewtypeD@s declarations to @DataD@s.
deNewtype :: Dec -> Dec
deNewtype (NewtypeD ctx tyConName args c d) = DataD ctx tyConName args [c] d
deNewtype d = d

makePrismsForCons :: [Pred] -> Name -> [TyVarBndr] -> [Con] -> Q [Dec]
makePrismsForCons ctx tyConName args cons =
  concat <$> mapM (makePrismForCon ctx tyConName args canModifyTypeVar cons) cons
  where
    conTypeVars = map (Set.fromList . toListOf typeVars) cons
    canModifyTypeVar = (`Set.member` typeVarsOnlyInOneCon) . view name
    typeVarsOnlyInOneCon = Set.fromList . concat . filter (\xs -> length xs == 1) .  List.group . List.sort $ conTypeVars >>= toList

makePrismForCon :: [Pred] -> Name -> [TyVarBndr] -> (TyVarBndr -> Bool) -> [Con] -> Con -> Q [Dec]
makePrismForCon ctx tyConName args canModifyTypeVar allCons con = do
    remitterName <- newName "remitter"
    reviewerName <- newName "reviewer"
    xName <- newName "x"
    let resName = mkName $ '_': nameBase dataConName
    varNames <- for [0..length fieldTypes -1] $ \i -> newName ('x' : show i)
    altArgsList <- forM (view name <$> filter isAltArg args) $ \arg ->
      (,) arg <$> newName (nameBase arg)
    let altArgs = Map.fromList altArgsList
        hitClause =
          clause [conP dataConName (fmap varP varNames)]
          (normalB $ appE (conE 'Right) $ toTupleE $ varE <$> varNames) []
        otherCons = filter (/= con) allCons
        missClauses
          | List.null otherCons   = []
          | Map.null altArgs = [clause [varP xName] (normalB (appE (conE 'Left) (varE xName))) []]
          | otherwise        = reviewerIdClause <$> otherCons
    Prelude.sequence [
      sigD resName . forallT
        (args ++ (PlainTV <$> Map.elems altArgs))
        (return $ List.nub (ctx ++ substTypeVars altArgs ctx)) $
         if altArgsList == [] then
          conT ''Prism' `appsT`
            [ appsT (conT tyConName) $ varT . view name <$> args
            , toTupleT $ pure <$> fieldTypes
            ]
         else
          conT ''Prism `appsT`
            [ appsT (conT tyConName) $ varT . view name <$> args
            , appsT (conT tyConName) $ varT . view name <$> substTypeVars altArgs args
            , toTupleT $ pure <$> fieldTypes
            , toTupleT $ pure <$> substTypeVars altArgs fieldTypes
            ]
      , funD resName
        [ clause []
          (normalB (appsE [varE 'prism, varE remitterName, varE reviewerName]))
          [ funD remitterName
            [ clause [toTupleP (varP <$> varNames)] (normalB (appsE (conE dataConName : fmap varE varNames))) [] ]
          , funD reviewerName $ hitClause : missClauses
          ]
        ]
      ]
  where
    (dataConName, fieldTypes) = ctrNameAndFieldTypes con
    conArgs = setOf typeVars fieldTypes
    isAltArg arg = canModifyTypeVar arg && conArgs^.contains(arg^.name)

ctrNameAndFieldTypes :: Con -> (Name, [Type])
ctrNameAndFieldTypes (NormalC n ts) = (n, snd <$> ts)
ctrNameAndFieldTypes (RecC n ts) = (n, view _3 <$> ts)
ctrNameAndFieldTypes (InfixC l n r) = (n, [snd l, snd r])
ctrNameAndFieldTypes (ForallC _ _ c) = ctrNameAndFieldTypes c

-- When a 'Prism' can change type variables it needs to pattern match on all
-- other data constructors and rebuild the data so it will have the new type.
reviewerIdClause :: Con -> ClauseQ
reviewerIdClause con = do
  let (dataConName, fieldTypes) = ctrNameAndFieldTypes con
  varNames <- for [0 .. length fieldTypes - 1] $ \i ->
                newName ('x' : show i)
  clause [conP dataConName (fmap varP varNames)]
         (normalB $ appE (conE 'Left) $ appsE (conE dataConName : fmap varE varNames))
         []

toTupleT :: [TypeQ] -> TypeQ
toTupleT [x] = x
toTupleT xs = appsT (tupleT (length xs)) xs

toTupleE :: [ExpQ] -> ExpQ
toTupleE [x] = x
toTupleE xs = tupE xs

toTupleP :: [PatQ] -> PatQ
toTupleP [x] = x
toTupleP xs = tupP xs

-- | Given a set of names, build a map from those names to a set of fresh names
-- based on them.
freshMap :: Set Name -> Q (Map Name Name)
freshMap ns = Map.fromList <$> for (toList ns) (\ n -> (,) n <$> newName (nameBase n))

makeIsoTo :: Name -> ExpQ
makeIsoTo = conE

makeIsoFrom :: Name -> ExpQ
makeIsoFrom conName = do
  b <- newName "b"
  lamE [conP conName [varP b]] $ varE b

makeIsoBody :: Name -> Name -> (Name -> ExpQ) -> (Name -> ExpQ) -> DecQ
makeIsoBody lensName conName f g = funD lensName [clause [] (normalB body) []] where
  body = appsE [ varE 'iso
               , g conName
               , f conName
               ]

makeLensBody :: Name -> Name -> (Name -> ExpQ) -> (Name -> ExpQ) -> DecQ
makeLensBody lensName conName i o = do
  f <- newName "f"
  a <- newName "a"
  funD lensName [clause [] (normalB (
    lamE [varP f, varP a] $
      appsE [ varE 'fmap
            , o conName
            , varE f `appE` (i conName `appE` varE a)
            ])) []]

plain :: TyVarBndr -> TyVarBndr
plain (KindedTV t _) = PlainTV t
plain (PlainTV t) = PlainTV t

appArgs :: Type -> [TyVarBndr] -> Type
appArgs t [] = t
appArgs t (x:xs) = appArgs (AppT t (VarT (x^.name))) xs

apps :: Type -> [Type] -> Type
apps = Prelude.foldl AppT

appsT :: TypeQ -> [TypeQ] -> TypeQ
appsT = Prelude.foldl appT

makeIsoLenses :: LensRules
              -> Cxt
              -> Name
              -> [TyVarBndr]
              -> Name
              -> Maybe Name
              -> Type
              -> Q [Dec]
makeIsoLenses cfg ctx tyConName tyArgs0 dataConName maybeFieldName partTy = do
  let tyArgs = map plain tyArgs0
  m <- freshMap $ setOf typeVars tyArgs
  let aty = partTy
      bty = substTypeVars m aty
      cty = appArgs (ConT tyConName) tyArgs
      dty = substTypeVars m cty
      quantified = ForallT (tyArgs ++ substTypeVars m tyArgs) (ctx ++ substTypeVars m ctx)
      maybeIsoName = mkName <$> view lensIso cfg (nameBase dataConName)
      lensOnly = not $ cfg^.singletonIso
      isoCon   | lensOnly  = ConT ''Lens
               | otherwise = ConT ''Iso
      isoCon'  | lensOnly  = ConT ''Lens'
               | otherwise = ConT ''Iso'
      makeBody | lensOnly  = makeLensBody
               | otherwise = makeIsoBody
  isoDecls <- flip (maybe (return [])) maybeIsoName $ \isoName -> do
    let decl = SigD isoName $ quantified $
          if cfg^.simpleLenses || Map.null m
          then isoCon' `apps` [aty,cty]
          else isoCon `apps` [aty,bty,cty,dty]
    body <- makeBody isoName dataConName makeIsoFrom makeIsoTo
#ifndef INLINING
    return $ if cfg^.generateSignatures then [decl, body] else [body]
#else
    inlining <- inlinePragma isoName
    return $ if cfg^.generateSignatures then [decl, body, inlining] else [body, inlining]
#endif
  accessorDecls <- case mkName <$> (maybeFieldName >>= view lensField cfg . nameBase) of
    jfn@(Just lensName)
      | (jfn /= maybeIsoName) && (isNothing maybeIsoName || cfg^.singletonAndField) -> do
      let decl = SigD lensName $ quantified $
            if cfg^.simpleLenses || Map.null m
            then isoCon' `apps` [cty,aty]
            else isoCon `apps` [cty,dty,aty,bty]
      body <- makeBody lensName dataConName makeIsoTo makeIsoFrom
#ifndef INLINING
      return $ if cfg^.generateSignatures then [decl, body] else [body]
#else
      inlining <- inlinePragma lensName
      return $ if cfg^.generateSignatures then [decl, body, inlining] else [body, inlining]
#endif
    _ -> return []
  return $ isoDecls ++ accessorDecls

makeFieldLensBody :: Bool -> Name -> [(Con, [Name])] -> Maybe Name -> Q Dec
makeFieldLensBody isTraversal lensName conList maybeMethodName = case maybeMethodName of
    Just methodName -> do
       go <- newName "go"
       let expr = infixApp (varE methodName) (varE '(Prelude..)) (varE go)
       funD lensName [ clause [] (normalB expr) [funD go clauses] ]
    Nothing -> funD lensName clauses
  where
    clauses = map buildClause conList
    buildClause (con, fields) = do
      f <- newName "_f"
      vars <- for (con^..conNamedFields._1) $ \fld ->
          if fld `List.elem` fields
        then Left  <$> ((,) <$> newName ('_':(nameBase fld++"'")) <*> newName ('_':nameBase fld))
        else Right <$> newName ('_':nameBase fld)
      let cpats = map (varP . either fst id) vars               -- Deconstruction
          cvals = map (varE . either snd id) vars               -- Reconstruction
          fpats = map (varP . snd)                 $ lefts vars -- Lambda patterns
          fvals = map (appE (varE f) . varE . fst) $ lefts vars -- Functor applications
          conName = con^.name
          recon = appsE $ conE conName : cvals

          expr
            | not isTraversal && length fields /= 1
              = appE (varE 'error) . litE . stringL
              $ show lensName ++ ": expected a single matching field in " ++ show conName ++ ", found " ++ show (length fields)
            | List.null fields
              = appE (varE 'pure) recon
            | otherwise
              = let step Nothing r = Just $ infixE (Just $ lamE fpats recon) (varE '(<$>)) (Just r)
                    step (Just l) r = Just $ infixE (Just l) (varE '(<*>)) (Just r)
                in  fromJust $ List.foldl step Nothing fvals
              -- = infixE (Just $ lamE fpats recon) (varE '(<$>)) $ Just $ List.foldl1 (\l r -> infixE (Just l) (varE '(<*>)) (Just r)) fvals
      clause [varP f, conP conName cpats] (normalB expr) []

makeFieldLenses :: LensRules
                -> Cxt         -- ^ surrounding cxt driven by the data type context
                -> Name        -- ^ data/newtype constructor name
                -> [TyVarBndr] -- ^ args
                -> [Con]
                -> Q [Dec]
makeFieldLenses cfg ctx tyConName tyArgs0 cons = do
  let tyArgs = map plain tyArgs0
      maybeLensClass = view lensClass cfg $ nameBase tyConName
      maybeClassName = fmap (^._1.to mkName) maybeLensClass
  t <- newName "t"
  a <- newName "a"

  --TODO: there's probably a more efficient way to do this.
  lensFields <- map (\xs -> (fst $ head xs, map snd xs))
              . groupBy ((==) `on` fst) . sortBy (comparing fst)
              . concat
            <$> mapM (getLensFields $ view lensField cfg) cons

  -- varMultiSet knows how many usages of the type variables there are.
  let varMultiSet = List.concatMap (toListOf (conFields._2.typeVars)) cons
      varSet = Set.fromList $ map (view name) tyArgs

  bodies <- for lensFields $ \(lensName, fields) -> do
    let fieldTypes = map (view _3) fields
    -- All of the polymorphic variables not involved in these fields
        otherVars = varMultiSet List.\\ fieldTypes^..typeVars
    -- New type variable binders, and the type to represent the selected fields
    (tyArgs', cty) <- unifyTypes tyArgs fieldTypes
    -- Map for the polymorphic variables that are only involved in these fields, to new names for them.
    m <- freshMap . Set.difference varSet $ Set.fromList otherVars
    let aty | isJust maybeClassName = VarT t
            | otherwise             = appArgs (ConT tyConName) tyArgs'
        bty = substTypeVars m aty
        dty = substTypeVars m cty

        s = setOf folded m
        relevantBndr b = s^.contains (b^.name)
        relevantCtx = not . Set.null . Set.intersection s . setOf typeVars
        tvs = tyArgs' ++ filter relevantBndr (substTypeVars m tyArgs')
        ps = filter relevantCtx (substTypeVars m ctx)
        qs = case maybeClassName of
           Just n | not (cfg^.createClass) -> ClassP n [VarT t] : (ctx ++ ps)
                  | otherwise              -> ps
           _                               -> ctx ++ ps
        tvs' = case maybeClassName of
           Just _ | not (cfg^.createClass) -> PlainTV t : tvs
                  | otherwise              -> []
           _                               -> tvs

        --TODO: Better way to write this?
        fieldMap = fromListWith (++) $ map (\(cn,fn,_) -> (cn, [fn])) fields
        conList = map (\c -> (c, Map.findWithDefault [] (view name c) fieldMap)) cons
        maybeMethodName = fmap (mkName . view _2) maybeLensClass

    isTraversal <- do
      let notSingular = filter ((/= 1) . length . snd) conList
          showCon (c, fs) = pprint (c^.name) ++ " { " ++ intercalate ", " (map pprint fs) ++ " }"
      case (cfg^.buildTraversals, cfg^.partialLenses) of
        (True,  True) -> fail "Cannot makeLensesWith both of the flags buildTraversals and partialLenses."
        (False, True) -> return False
        (True,  False) | List.null notSingular -> return False
                       | otherwise -> return True
        (False, False) | List.null notSingular -> return False
                       | otherwise -> fail . unlines $
          [ "Cannot use 'makeLensesWith' with constructors that don't map just one field"
          , "to a lens, without using either the buildTraversals or partialLenses flags."
          , if length conList == 1
            then "The following constructor failed this criterion for the " ++ pprint lensName ++ " lens:"
            else "The following constructors failed this criterion for the " ++ pprint lensName ++ " lens:"
          ] ++ map showCon conList

    let decl = SigD lensName $ ForallT tvs' qs vars
          where
          vars
            | aty == bty && cty == dty || cfg^.simpleLenses || isJust maybeClassName
               = apps (ConT (if isTraversal then ''Traversal' else ''Lens')) [aty,cty]
            | otherwise
               = apps (ConT (if isTraversal then ''Traversal else ''Lens)) [aty,bty,cty,dty]

    body <- makeFieldLensBody isTraversal lensName conList maybeMethodName
#ifndef INLINING
    return $ if cfg^.generateSignatures then [decl, body] else [body]
#else
    inlining <- inlinePragma lensName
    return $ if cfg^.generateSignatures then [decl, body, inlining] else [body, inlining]
#endif
  let defs = Prelude.concat bodies
  case maybeLensClass of
    Nothing -> return defs
    Just (clsNameString, methodNameString) -> do
      let clsName    = mkName clsNameString
          methodName = mkName methodNameString
          varArgs    = varT . view name <$> tyArgs
          appliedCon = conT tyConName `appsT` varArgs
      Prelude.sequence $
        filter (\_ -> cfg^.createClass) [
          classD (return []) clsName (PlainTV t : tyArgs) (if List.null tyArgs then [] else [FunDep [t] (view name <$> tyArgs)]) (
            sigD methodName (appsT (conT ''Lens') [varT t, appliedCon]) :
            map return defs)]
        ++ filter (\_ -> cfg^.createInstance) [
          instanceD (return []) ((conT clsName `appT` appliedCon) `appsT` varArgs) [
            funD methodName [clause [varP a] (normalB (varE a)) []]
#ifdef INLINING
            , inlinePragma methodName
#endif
            ]]
        ++ filter (\_ -> not $ cfg^.createClass) (map return defs)

-- | Gets @[(lens name, (constructor name, field name, type))]@ from a record constructor.
getLensFields :: (String -> Maybe String) -> Con -> Q [(Name, (Name, Name, Type))]
getLensFields f (RecC cn fs)
  = return . catMaybes
  $ fs <&> \(fn,_,t) -> f (nameBase fn) <&> \ln -> (mkName ln, (cn,fn,t))
getLensFields _ _
  = return []

-- TODO: properly fill this out
--
-- Ideally this would unify the different field types, and figure out which polymorphic variables
-- need to be the same.  For now it just leaves them the same and yields the first type.
-- (This leaves us open to inscrutable compile errors in the generated code)
unifyTypes :: [TyVarBndr] -> [Type] -> Q ([TyVarBndr], Type)
unifyTypes tvs tys = return (tvs, head tys)

-- | Build 'Wrapped' instance for a given newtype
makeWrapped :: Name -> DecsQ
makeWrapped nm = do
  inf <- reify nm
  case inf of
    TyConI decl ->
      case deNewtype decl of
        DataD _ tyConName args [con] _ -> makeWrappedInstance tyConName args con
        _                              -> fail "makeWrapped: Unsupported data type"
    _ -> fail "makeWrapped: Expected the name of a newtype or datatype"

makeWrappedInstance :: Name -> [TyVarBndr] -> Con -> DecsQ
makeWrappedInstance tyConName tyArgs con = do
  let tyNames = view name <$> tyArgs

  tyNameRemap <- makeNameRemap tyNames

  (newtypeConName, fieldType) <- case ctrNameAndFieldTypes con of
    (a,[b]) -> return (a,b)
    _       -> fail "makeWrappedInstance: Constructor must have a single field"

  let outer1 = conT tyConName `appsT` fmap varT tyNames
      inner1 = return fieldType

      outer2 = conT tyConName `appsT` fmap (varT . snd) tyNameRemap
      inner2 = return $ substTypeVars (Map.fromList tyNameRemap) fieldType

  dec <- instanceD (cxt [])
             (conT ''Wrapped `appsT` [inner1, inner2, outer1, outer2])
             [makeIsoBody 'wrapped newtypeConName makeIsoFrom makeIsoTo]

  return [dec]
  where
  -- Return list to preserve order, convert to Map later
  makeNameRemap tyNames
    = for tyNames $ \ tyName -> do
        tyName1 <- newName (show tyName)
        return (tyName, tyName1)

#if !(MIN_VERSION_template_haskell(2,7,0))
-- | The orphan instance for old versions is bad, but programming without 'Applicative' is worse.
instance Applicative Q where
  pure = return
  (<*>) = ap
#endif

#ifdef INLINING

inlinePragma :: Name -> Q Dec
#if MIN_VERSION_template_haskell(2,8,0)

# ifdef OLD_INLINE_PRAGMAS
-- 7.6rc1?
inlinePragma methodName = pragInlD methodName $ inlineSpecNoPhase Inline False
# else
-- 7.7.20120830
inlinePragma methodName = pragInlD methodName Inline FunLike AllPhases
# endif

#else
-- GHC <7.6, TH <2.8.0
inlinePragma methodName = pragInlD methodName $ inlineSpecNoPhase True False
#endif

#endif

data FieldRules = FieldRules
    { _getPrefix          :: String -> Maybe String
    , _rawLensNaming      :: String -> String
    , _niceLensNaming     :: String -> Maybe String
    , _classNaming        :: String -> Maybe String
    }

data Field = Field
    { _fieldName          :: Name
    , _fieldLensPrefix    :: String
    , _fieldLensName      :: Name
    , _fieldClassName     :: Name
    , _fieldClassLensName :: Name
    }

overHead :: (a -> a) -> [a] -> [a]
overHead _ []     = []
overHead f (x:xs) = f x : xs

-- | Field rules for fields in the form @ _prefix_fieldname @
underscoreFields :: FieldRules
underscoreFields = FieldRules prefix rawLens niceLens classNaming
  where
    prefix ('_':xs) | '_' `List.elem` xs = Just (takeWhile (/= '_') xs)
    prefix _                             = Nothing
    rawLens     x = x ++ "_lens"
    niceLens    x = prefix   x <&> \n -> drop (length n + 2) x
    classNaming x = niceLens x <&> ("Has_" ++)

-- | Field rules for fields in the form @ prefixFieldname @
camelCaseFields :: FieldRules
camelCaseFields = FieldRules prefix rawLens niceLens classNaming
  where
    sep x = case break isUpper x of
        (p, s) | List.null p || List.null s -> Nothing
               | otherwise                  -> Just (p,s)
    prefix      x = do ('_':xs,_) <- sep x; return xs
    rawLens     x = x ++ "Lens"
    niceLens    x = overHead toLower . snd <$> sep x
    classNaming x = niceLens x <&> \ (n:ns) -> "Has" ++ toUpper n : ns

collectRecords :: [Con] -> [VarStrictType]
collectRecords cons = rs
  where
    recs = filter (\r -> case r of RecC{} -> True; _ -> False) cons
    rs' = List.concatMap (\(RecC _ _rs) -> _rs) recs
    rs = nubBy ((==) `on` (^._1)) rs'

verboseLenses :: FieldRules -> Name -> Q [Dec]
verboseLenses c src = do
    rs <- do
        inf <- reify src
        case inf of
          TyConI decl -> case deNewtype decl of
            DataD _ _ _ cons _ -> do
              let rs = collectRecords cons
              if List.null rs
                then fail "verboseLenses: Expected the name of a record type"
                else return rs
            _ -> fail "verboseLenses: Unsupported data type"
          _ -> fail "verboseLenses: Expected the name of a data type or newtype"
    flip makeLenses' src
        $ mkFields c rs
        & map (\(Field n _ l _ _) -> (show n, show l))
  where
    makeLenses' fields' =
        makeLensesWith $ lensRules
            & lensField .~ (`Prelude.lookup` fields')
            & buildTraversals .~ False
            & partialLenses .~ True

mkFields :: FieldRules -> [VarStrictType] -> [Field]
mkFields (FieldRules prefix' raw' nice' clas') rs
    = Maybe.mapMaybe namer rs
    & List.groupBy (on (==) _fieldLensPrefix)
    & (\ gs -> case gs of 
        x:_ -> x
        _   -> [])
  where
    namer (n', _, _) = do
        let field   = nameBase n'
            rawlens = mkName (raw' field)
        prefix <- prefix' field
        nice   <- mkName <$> nice' field
        clas   <- mkName <$> clas' field
        return (Field (mkName field) prefix rawlens clas nice)

hasClassAndInstance :: FieldRules -> Name -> Q [Dec]
hasClassAndInstance cfg src = do
    c <- newName "c"
    e <- newName "e"
    (vs,rs) <- do
        inf <- reify src
        case inf of
          TyConI decl -> case deNewtype decl of
            DataD _ _ vs cons _ -> do
                let rs = collectRecords cons
                if List.null rs
                  then fail "hasClassAndInstance: Expected the name of a record type"
                  else return (vs,rs)
            _ -> fail "hasClassAndInstance: Unsupported data type"
          _ -> fail "hasClassAndInstance: Expected the name of a data type or newtype"
    fmap concat . forM (mkFields cfg rs) $ \(Field field _ fullLensName className lensName) -> do
        classHas <- classD
            (return [])
            className
            [ PlainTV c, PlainTV e ]
            [ FunDep [c] [e] ]
            [ sigD lensName (conT ''Lens' `appsT` [varT c, varT e])]
        fieldType <- do
            VarI _ t _ _ <- reify field
            case t of
                AppT    _    fieldType          -> return fieldType
                ForallT _ [] (AppT _ fieldType) -> return fieldType
                _                               -> error "Cannot get fieldType"
        instanceHas <- instanceD
            (return [])
            (conT className `appsT` [conT src `appsT` map (varT.view name) vs, return fieldType])
            [
#ifdef INLINING
              inlinePragma lensName,
#endif
              funD lensName [ clause [] (normalB (global fullLensName)) [] ]
            ]
        classAlreadyExists <- isJust `fmap` lookupTypeName (show className)
        return (if classAlreadyExists then [instanceHas] else [classHas, instanceHas])

-- | Make fields with the specified 'FieldRules'.
makeFieldsWith :: FieldRules -> Name -> Q [Dec]
makeFieldsWith c n = liftA2 (++) (verboseLenses c n) (hasClassAndInstance c n)

-- | @ makeFields = 'makeFieldsWith' 'defaultFieldRules' @
makeFields :: Name -> Q [Dec]
makeFields = makeFieldsWith defaultFieldRules

-- | @ defaultFieldRules = 'camelCaseFields' @
defaultFieldRules :: FieldRules
defaultFieldRules = camelCaseFields
