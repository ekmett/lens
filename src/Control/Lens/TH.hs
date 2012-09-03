{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

-- in case we're being loaded from ghci
#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) (defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706)
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.TH
-- Copyright   :  (C) 2012 Edward Kmett, Michael Sloan
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  TemplateHaskell
--
-----------------------------------------------------------------------------
module Control.Lens.TH
  (
  -- * Constructing Lenses Automatically
    makeLenses, makeLensesFor
  , makeClassy, makeClassyFor
  , makeIso
  -- * Configuring Lenses
  , makeLensesWith
  , defaultRules
  , LensRules(LensRules)
  , lensRules
  , classyRules
  , isoRules
  , lensIso
  , lensField
  , lensClass
  , lensFlags
  , LensFlag(..)
  , simpleLenses, partialLenses, handleSingletons, singletonIso, singletonRequired, createClass, createInstance, classRequired
  ) where

import Control.Applicative
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Iso
import Control.Lens.Setter
import Control.Lens.Tuple
import Control.Lens.Traversal
import Control.Lens.Type
import Control.Lens.IndexedLens
import Control.Monad
import Data.Char (toLower)
import Data.Either (lefts)
import Data.Foldable hiding (concat)
import Data.Function (on)
import Data.List as List
import Data.Map as Map hiding (toList,map,filter)
import Data.Maybe (isNothing,isJust, catMaybes)
import Data.Ord (comparing)
import Data.Set as Set hiding (toList,map,filter)
import Data.Set.Lens
import Data.Traversable hiding (mapM)
import Language.Haskell.TH
import Language.Haskell.TH.Lens

-- | Flags for lens construction
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
  deriving (Eq,Ord,Show,Read)

-- | Only Generate valid 'Simple' 'Lens' lenses.
simpleLenses      :: Simple Lens LensRules Bool
simpleLenses       = lensFlags.contains SimpleLenses

-- | Enables the generation of partial lenses, generating runtime errors for
-- every constructor that does not have a valid definition for the lens. This
-- occurs when the constructor lacks the field, or has multiple fields mapped
-- to the same lens.
partialLenses     :: Simple Lens LensRules Bool
partialLenses      = lensFlags.contains PartialLenses

-- | In the situations that a lens would be partial, when 'partialLenses' is
-- used, this flag instead causes traversals to be generated.  Only one can be
-- used, and if neither are, then compile-time errors are generated.
buildTraversals   :: Simple Lens LensRules Bool
buildTraversals    = lensFlags.contains BuildTraversals

-- | Handle singleton constructors specially.
handleSingletons  :: Simple Lens LensRules Bool
handleSingletons   = lensFlags.contains HandleSingletons

-- | When building a singleton 'Iso' (or 'Lens') for a record constructor, build both
-- the 'Iso' (or 'Lens') for the record and the one for the field.
singletonAndField :: Simple Lens LensRules Bool
singletonAndField  = lensFlags.contains SingletonAndField

-- | Use 'Iso' for singleton constructors.
singletonIso      :: Simple Lens LensRules Bool
singletonIso       = lensFlags.contains SingletonIso

-- | Expect a single constructor, single field newtype or data type.
singletonRequired :: Simple Lens LensRules Bool
singletonRequired  = lensFlags.contains SingletonRequired

-- | Create the class if the constructor is simple and the 'lensClass' rule matches.
createClass       :: Simple Lens LensRules Bool
createClass        = lensFlags.contains CreateClass

-- | Create the instance if the constructor is simple and the 'lensClass' rule matches.
createInstance    :: Simple Lens LensRules Bool
createInstance     = lensFlags.contains CreateInstance

-- | Die if the 'lensClass' fails to match.
classRequired     :: Simple Lens LensRules Bool
classRequired      = lensFlags.contains ClassRequired


-- | This configuration describes the options we'll be using to make isomorphisms or lenses.
data LensRules = LensRules
  { _lensIso   :: String -> Maybe String
  , _lensField :: String -> Maybe String
  , _lensClass :: String -> Maybe (String, String)
  , _lensFlags :: Set LensFlag
  }

-- | Lens to access the convention for naming top level isomorphisms in our lens rules.
--
-- Defaults to lowercasing the first letter of the constructor.
lensIso :: Simple Lens LensRules (String -> Maybe String)
lensIso f (LensRules i n c o) = (\i' -> LensRules i' n c o) <$> f i

-- | Lens to access the convention for naming fields in our lens rules.
--
-- Defaults to stripping the _ off of the field name, lowercasing the name, and
-- rejecting the field if it doesn't start with an '_'.
lensField :: Simple Lens LensRules (String -> Maybe String)
lensField f (LensRules i n c o) = (\n' -> LensRules i n' c o) <$> f n

-- | Retrieve options such as the name of the class and method to put in it to
-- build a class around monomorphic data types.
lensClass :: Simple Lens LensRules (String -> Maybe (String, String))
lensClass f (LensRules i n c o) = (\c' -> LensRules i n c' o) <$> f c

-- | Retrieve options such as the name of the class and method to put in it to
-- build a class around monomorphic data types.
lensFlags :: Simple Lens LensRules (Set LensFlag)
lensFlags f (LensRules i n c o) = LensRules i n c <$> f o

-- | Default lens rules
defaultRules :: LensRules
defaultRules = LensRules top field (const Nothing) $
    Set.fromList [SingletonIso, SingletonAndField, CreateClass, CreateInstance, BuildTraversals]
  where
    top (c:cs) = Just (toLower c:cs)
    top _      = Nothing
    field ('_':c:cs) = Just (toLower c:cs)
    field _          = Nothing

-- | Rules for making fairly simple partial lenses, ignoring the special cases
-- for isomorphisms and traversals, and not making any classes.
lensRules :: LensRules
lensRules
  = lensIso   .~ const Nothing
  $ lensClass .~ const Nothing
  $ handleSingletons .~ True
  $ partialLenses .~ False
  $ buildTraversals .~ True
  $ defaultRules

-- | Rules for making lenses and traversals that precompose another lens.
classyRules :: LensRules
classyRules
    = lensIso .~ const Nothing
    $ handleSingletons .~ False
    $ lensClass .~ classy
    $ classRequired .~ True
    $ partialLenses .~ False
    $ buildTraversals .~ True
    $ defaultRules
  where
    classy :: String -> Maybe (String, String)
    classy n@(a:as) = Just ("Has" ++ n, toLower a:as)
    classy _ = Nothing

-- | Rules for making an isomorphism from a data type
isoRules :: LensRules
isoRules
  = singletonRequired .~ True
  $ singletonAndField .~ True
  $ defaultRules

-- | Build lenses (and traversals) with a sensible default configuration.
--
-- > makeLenses = makeLensesWith lensRules
makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesWith lensRules

-- | Make lenses and traversals for a type, and create a class when the type has no arguments.
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
--   foo :: 'Simple' 'Lens' t Foo
-- instance HasFoo Foo where foo = 'id'
-- fooX, fooY :: HasFoo t => 'Simple' 'Lens' t 'Int'
-- @
--
-- > makeClassy = makeLensesWith classyRules
makeClassy :: Name -> Q [Dec]
makeClassy = makeLensesWith classyRules

-- | Make a top level isomorphism injecting /into/ the type.
--
-- The supplied name is required to be for a type with a single constructor that has a single argument
--
-- /e.g./
--
-- @
-- newtype List a = List [a]
-- makeIso ''List
-- @
--
-- will create
--
-- @
-- list :: Iso [a] [b] ('List' a) ('List' b)
-- @
--
-- > makeIso = makeLensesWith isoRules
makeIso :: Name -> Q [Dec]
makeIso = makeLensesWith isoRules

-- | Derive lenses and traversals, specifying explicit pairings of @(fieldName, lensName)@.
--
-- If you map multiple names to the same label, and it is present in the same constructor then this will generate a 'Traversal'.
--
-- /e.g./
--
-- > makeLensesFor [("_foo", "fooLens"), ("baz", "lbaz")] ''Foo
-- > makeLensesFor [("_barX", "bar"), ("_barY", "bar)] ''Bar
makeLensesFor :: [(String, String)] -> Name -> Q [Dec]
makeLensesFor fields = makeLensesWith
  $ lensField .~ (`Prelude.lookup` fields)
  $ lensRules

-- | Derive lenses and traversals, using a named wrapper class, and specifying
-- explicit pairings of @(fieldName, traversalName)@.
--
-- Example usage:
--
-- > makeClassyFor "HasFoo" "foo" [("_foo", "fooLens"), ("bar", "lbar")] ''Foo
makeClassyFor :: String -> String -> [(String, String)] -> Name -> Q [Dec]
makeClassyFor clsName funName fields = makeLensesWith
  $ lensClass .~ const (Just (clsName,funName))
  $ lensField .~ (`Prelude.lookup` fields)
  $ classyRules

-- | Build lenses with a custom configuration.
makeLensesWith :: LensRules -> Name -> Q [Dec]
makeLensesWith cfg nm = do
    inf <- reify nm
    case inf of
      (TyConI decl) -> case deNewtype decl of
        (DataD ctx tyConName args cons _) -> case cons of
          [NormalC dataConName [(    _,ty)]]
            | cfg^.handleSingletons
             -> makeIsoLenses cfg ctx tyConName args dataConName Nothing ty

          [RecC    dataConName [(fld,_,ty)]]
            | cfg^.handleSingletons
             -> makeIsoLenses cfg ctx tyConName args dataConName (Just fld) ty

          _ | cfg^.singletonRequired
             -> fail "makeLensesWith: A single-constructor single-argument data type is required"

            | otherwise
             -> makeFieldLenses cfg ctx tyConName args cons

        _ -> fail "makeLensesWith: Unsupported data type"
      _ -> fail "makeLensesWith: Expected the name of a data type or newtype"
  where
    deNewtype (NewtypeD ctx tyConName args c d) = DataD ctx tyConName args [c] d
    deNewtype d = d

-----------------------------------------------------------------------------
-- Internal TH Implementation
-----------------------------------------------------------------------------

-- | Given a set of names, build a map from those names to a set of fresh names based on them.
freshMap :: Set Name -> Q (Map Name Name)
freshMap ns = Map.fromList <$> for (toList ns) (\ n -> (,) n <$> newName (nameBase n))

makeIsoTo :: Name -> ExpQ
makeIsoTo conName = lamE [varP (mkName "f"), conP conName [varP (mkName "a")]] $
  appsE [ return (VarE 'fmap)
        , conE conName
        , varE (mkName "f") `appE` varE (mkName "a")
        ]

makeIsoFrom :: Name -> ExpQ
makeIsoFrom conName = lamE [varP (mkName "f"), varP (mkName "a")] $
  appsE [ return (VarE 'fmap)
        , lamE [conP conName [varP (mkName "b")]] $ varE (mkName "b")
        , varE (mkName "f") `appE` (conE conName `appE` varE (mkName "a"))
        ]

makeIsoBody :: Name -> Name -> (Name -> ExpQ) -> (Name -> ExpQ) -> DecQ
makeIsoBody lensName conName f g = funD lensName [clause [] (normalB body) []] where
  body = appsE [ return (VarE 'isomorphic)
               , f conName
               , g conName
               ]

makeLensBody :: Name -> Name -> (Name -> ExpQ) -> (Name -> ExpQ) -> DecQ
makeLensBody lensName conName f _ = funD lensName [clause [] (normalB (f conName)) []]

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

-- | Given
--
-- > newtype Cxt b => Foo a b c d = Foo { _baz :: Bar a b }
--
-- This will generate:
--
-- > foo :: (Cxt b, Cxt f) => Iso (Foo a b c d) (Foo e f g h) (Bar a b) (Bar e f)
-- > foo = isomorphic (\f a -> (\(Foo b) -> b) <$> f (Foo a))
-- >                  (\f (Foo a) -> fmap Foo (f a))
-- > {-# INLINE foo #-}

-- > baz :: (Cxt b, Cxt f) => Iso (Bar a b) (Bar e f) (Foo a b c d) (Foo e f g h)
-- > baz = isomorphic (\f (Foo a) -> fmap Foo (f a))
-- >                  (\f a -> fmap (\(Foo b) -> b) (f (Foo a)))
-- > {-# INLINE baz #-}
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
      makeBody | lensOnly  = makeLensBody
               | otherwise = makeIsoBody
  isoDecls <- flip (maybe (return [])) maybeIsoName $ \isoName -> do
    let decl = SigD isoName $ quantified $ isoCon `apps`
          if cfg^.simpleLenses then [aty,aty,cty,cty] else [aty,bty,cty,dty]
    body <- makeBody isoName dataConName makeIsoFrom makeIsoTo
#ifndef INLINING
    return [decl, body]
#else
    inlining <- inlinePragma isoName
    return [decl, body, inlining]
#endif
  accessorDecls <- case mkName <$> (maybeFieldName >>= view lensField cfg . nameBase) of
    jfn@(Just lensName)
      | (jfn /= maybeIsoName) && (isNothing maybeIsoName || cfg^.singletonAndField) -> do
      let decl = SigD lensName $ quantified $ isoCon `apps`
                   if cfg^.simpleLenses then [cty,cty,aty,aty]
                                        else [cty,dty,aty,bty]
      body <- makeBody lensName dataConName makeIsoTo makeIsoFrom
#ifndef INLINING
      return [decl, body]
#else
      inlining <- inlinePragma lensName
      return [decl, body, inlining]
#endif
    _ -> return []
  return $ isoDecls ++ accessorDecls

makeFieldLensBody :: Bool -> Name -> [(Con, [Name])] -> Maybe Name -> Q Dec
makeFieldLensBody isTraversal lensName conList maybeMethodName = case maybeMethodName of
    Just methodName -> do
       go <- newName "go"
       let expr = infixApp (varE methodName) (varE (mkName ".")) (varE go)
       funD lensName [ clause [] (normalB expr) [funD go clauses] ]
    Nothing -> funD lensName clauses
  where
    clauses = map buildClause conList
    plainClause ps d = clause ps d []
    buildClause (con, fields) = do
      let allFields :: [Name]
          allFields = con^..conNamedFields._1
          conName = con^.name
          conWild = conP conName (replicate (length allFields) wildP)

      if not isTraversal && length fields /= 1
        then plainClause [wildP, conWild] . normalB . appE (varE 'error) . litE . stringL
           $ show lensName ++ ": expected a single matching field in " ++ show conName ++ ", found " ++ show (length fields)
        else do
          vars <- for allFields $ \field ->
              if field `List.elem` fields
            then fmap Left $ (,) <$> newName (nameBase field) <*> newName (nameBase field ++ "'")
            else Right <$> newName (nameBase field)
          f <- newName "f"
          let cpats = map (varP . either fst id) vars               -- Deconstruction
              cvals = map (varE . either snd id) vars               -- Reconstruction
              fpats = map (varP . snd)                 $ lefts vars -- Lambda patterns
              fvals = map (appE (varE f) . varE . fst) $ lefts vars -- Functor applications
              recon = appsE $ conE conName : cvals

              expr = if List.null fields
                   then appE (varE 'pure) recon
                   else uInfixE (lamE fpats recon) (varE '(<$>)) $ List.foldl1 (\l r -> uInfixE l (varE '(<*>)) r) fvals
          plainClause [varP f, conP conName cpats] (normalB expr)

makeFieldLenses :: LensRules
                -> Cxt         -- ^ surrounding cxt driven by the data type context
                -> Name        -- ^ data/newtype constructor name
                -> [TyVarBndr] -- ^ args
                -> [Con]
                -> Q [Dec]
makeFieldLenses cfg ctx tyConName tyArgs0 cons = do
  let tyArgs = map plain tyArgs0
      maybeLensClass = do
        guard $ tyArgs == []
        view lensClass cfg $ nameBase tyConName
      maybeClassName = fmap (^._1.to mkName) maybeLensClass
  classDecls <- case maybeLensClass of
    Nothing -> return []
    Just (clsNameString, methodNameString) -> do
      let clsName    = mkName clsNameString
          methodName = mkName methodNameString
      t <- newName "t"
      a <- newName "a"
      Prelude.sequence $
        filter (\_ -> cfg^.createClass)
          [ classD (return []) clsName [PlainTV t] []
            [ sigD methodName $ appsT (return (ConT ''Lens)) [varT t, varT t, conT tyConName, conT tyConName] ]]
        ++ filter (\_ -> cfg^.createInstance)
          [ instanceD (return []) (conT clsName `appT` conT tyConName)
            [ funD methodName [clause [varP a] (normalB (varE a)) []]
#ifdef INLINING
            , inlinePragma methodName
#endif
            ]]

  --TODO: there's probably a more efficient way to do this.
  lensFields <- map (\xs -> (fst $ head xs, map snd xs))
              . groupBy ((==) `on` fst) . sortBy (comparing fst) . concat
            <$> mapM (getLensFields $ view lensField cfg) cons

  -- varMultiSet knows how many usages of the type variables there are.
  let varMultiSet = List.concatMap (toListOf (conFields._2.typeVars)) cons
      varSet = Set.fromList $ map (view name) tyArgs

  -- if not (cfg^.partialLenses) && not (cfg^.BuildTraversals)
  bodies <- for lensFields $ \(lensName, fields) -> do
    let fieldTypes = map (view _3) fields
    -- All of the polymorphic variables not involved in these fields
        otherVars = varMultiSet List.\\ fieldTypes^..typeVars
    -- New type variable binders, and the type to represent the selected fields
    (tyArgs', cty) <- unifyTypes tyArgs fieldTypes
    -- Map for the polymorphic variables that are only involved in these fields, to new names for them.
    m <- freshMap . Set.difference varSet $ Set.fromList otherVars
    x <- newName "x"
    let aty | isJust maybeClassName = VarT x
            | otherwise             = appArgs (ConT tyConName) tyArgs'
        bty = substTypeVars m aty
        dty = substTypeVars m cty

        s = setOf folded m
        relevantBndr b = s^.contains (b^.name)
        relevantCtx = not . Set.null . Set.intersection s . setOf typeVars
        tvs = tyArgs' ++ filter relevantBndr (substTypeVars m tyArgs')
        ps = ctx ++ filter relevantCtx (substTypeVars m ctx)
        qs = case maybeClassName of
           Just n -> ClassP n [VarT x] : ps
           _      -> ps
        tvs' | isJust maybeClassName = PlainTV x : tvs
             | otherwise             = tvs

        --TODO: Better way to write this?
        fieldMap = fromListWith (++) $ map (\(cn,fn,_) -> (cn, [fn])) fields
        conList = map (\c -> (c, Map.findWithDefault [] (view name c) fieldMap)) cons
        maybeMethodName = fmap (mkName . view _2) maybeLensClass

    isTraversal <- do
      let notSingular = filter ((/= 1) . length . snd) conList
          showCon (c, fs) = pprint (view name c) ++ " { " ++ concat (intersperse ", " $ map pprint fs) ++ " }"
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

    --TODO: consider detecting simpleLenses, and generating signatures involving "Simple"?
    let decl = SigD lensName
             . ForallT tvs' qs
             . apps (if isTraversal then ConT ''Traversal else ConT ''Lens)
             $ if cfg^.simpleLenses then [aty,aty,cty,cty] else [aty,bty,cty,dty]

    body <- makeFieldLensBody isTraversal lensName conList maybeMethodName
#ifndef INLINING
    return [decl, body]
#else
    inlining <- inlinePragma lensName
    return [decl, body, inlining]
#endif
  return $ classDecls ++ Prelude.concat bodies

-- | Gets @[(lens name, (constructor name, field name, type))]@ from a record constructor
getLensFields :: (String -> Maybe String) -> Con -> Q [(Name, (Name, Name, Type))]
getLensFields nameFunc (RecC cn fs)
  = return . catMaybes
  $ map (\(fn,_,t) -> (\ln -> (mkName ln, (cn,fn,t))) <$> nameFunc (nameBase fn)) fs
getLensFields _ _
  = warn "makeLensesWith: encountered a non-record constructor, for which no lenses will be generated."
  >> return []

-- TODO: properly fill this out
--
-- Ideally this would unify the different field types, and figure out which polymorphic variables
-- need to be the same.  For now it just leaves them the same and yields the first type.
-- (This leaves us open to inscrutable compile errors in the generated code)
unifyTypes :: [TyVarBndr] -> [Type] -> Q ([TyVarBndr], Type)
unifyTypes tvs tys = return (tvs, head tys)

{-
fieldDescs :: Set Name -> [(Name,Strict,Type)] -> [FieldDesc]
fieldDescs acc ((nm,_,ty):rest) =
  FieldDesc nm ty (acc `Set.union` setOf typeVars (map thd rest)) :
  fieldDescs (acc `Set.union` setOf typeVars ty) rest
fieldDescs _ [] = []
-}

warn :: String -> Q ()
#if MIN_VERSION_template_haskell(2,8,0)
warn = reportWarning
#else
warn = report False
#endif

#if !(MIN_VERSION_template_haskell(2,7,0))
-- | The orphan instance for old versions is bad, but programing without 'Applicative' is worse.
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
