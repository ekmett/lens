{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.TH
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  TemplateHaskell
--
----------------------------------------------------------------------------
module Control.Lens.TH
  (
  -- * Constructing Lenses Automatically
    makeLenses
  , makeLensesFor
  , makeLensesWith
  -- ** Common configurations
  , defaultLensRules
  , classyClasses
  , mixedClasses
  , noClasses
  -- * Lenses for configuring lenses
  , LensRules(LensRules)
  , isoLensRule
  , fieldLensRule
  , addBothLensRule
  , noIsoLensRule
  , simpleLensRule
  -- ** Manual class configuration
  , classLensRule
  , classyClassLensRule
  , mixedClassLensRule
  , noClassLensRule
  , LensClass(LensClass)
  , lensClassName
  , lensClassCreateClass
  , lensClassCreateInstance
  , lensClassMember
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad (guard)
import Data.Char (toLower)
import Data.Foldable
import Data.List as List
import Data.Map as Map hiding (toList,map,filter)
import Data.Map.Lens
import Data.Maybe (isNothing,isJust)
import Data.Monoid
import Data.Set as Set hiding (toList,map,filter)
import Data.Set.Lens
import Data.Traversable
import Language.Haskell.TH
import Language.Haskell.TH.Lens

data LensClass = LensClass
  { _lensClassName           :: String
  , _lensClassCreateClass    :: Bool
  , _lensClassCreateInstance :: Bool
  , _lensClassMember         :: String
  } deriving (Eq,Ord,Show,Read)

-- | What class name should we use?
--
-- (Typically something like HasFoo)
lensClassName :: Simple Lens LensClass String
lensClassName f (LensClass n cc ci m) = (\n' -> LensClass n' cc ci m) <$> f n

-- | Should we create the class?
--
-- > class HasFoo t where
-- >   foo :: Lens t t Foo Foo
lensClassCreateClass :: Simple Lens LensClass Bool
lensClassCreateClass f (LensClass n cc ci m) = (\cc' -> LensClass n cc' ci m) <$> f cc

-- | Should we create the self-instance?
-- > instance HasFoo Foo where
-- >   foo = id
lensClassCreateInstance :: Simple Lens LensClass Bool
lensClassCreateInstance f (LensClass n cc ci m) = (\ci' -> LensClass n cc ci' m) <$> f ci

-- | Choose the member of the class to precompose with
--
-- (Usually something like the name of the data type lowercased.)
lensClassMember :: Simple Lens LensClass String
lensClassMember f (LensClass n cc ci m) = (\m' -> LensClass n cc ci m') <$> f m

-- | This configuration describes the options we'll be using to make isomorphisms or lenses
data LensRules = LensRules
  { _isoLensRule        :: String -> Maybe String
  , _fieldLensRule      :: String -> Maybe String
  , _classLensRule      :: Bool -> String -> Maybe LensClass
  , _addBothLensRule    :: Bool
  , _useNoIsoLensRule   :: Bool
  , _useSimpleLensRule  :: Bool
--  , _traversalLensRule  :: Bool
  }

-- | Lens to access the convention for naming top level isomorphisms in our lens rules
--
-- Defaults to lowercasing the first letter of the constructor.
isoLensRule :: Simple Lens LensRules (String -> Maybe String)
isoLensRule f (LensRules i n c b lni sl) = (\i' -> LensRules i' n c b lni sl) <$> f i

-- | Lens to access the convention for naming fields in our lens rules
--
-- Defaults to stripping the _ off of the field name and lowercasing the name and
-- rejecting the field if it doesn't start with an '_'.
fieldLensRule :: Simple Lens LensRules (String -> Maybe String)
fieldLensRule f (LensRules i n c b lni sl) = (\n' -> LensRules i n' c b lni sl) <$> f n

-- | Retrieve options such as the name of the class and method to put in it to build a class around monomorphic data types.
classLensRule :: Simple Lens LensRules (Bool -> String -> Maybe LensClass)
classLensRule f (LensRules i n c b lni sl) = (\c' -> LensRules i n c' b lni sl) <$> f c

-- | This flag indicates whether or not we should attempt to add both an isomorphism lens and a top level accessor
--
-- Defaults to 'True'
addBothLensRule :: Simple Lens LensRules Bool
addBothLensRule f (LensRules i n c b lni sl) = (\b' -> LensRules i n c b' lni sl) <$> f b

-- | Sometimes you know that you'll be refactoring your code, so you may want to avoid generating isomorphisms
-- even when they are available.
--
-- Defaults to 'True'
noIsoLensRule :: Simple Lens LensRules Bool
noIsoLensRule f (LensRules i n c b lni sl) = (\lni' -> LensRules i n c b lni' sl) <$> f lni

-- | Never use lens families, always return a 'Simple' 'Lens' or 'Simple' 'Iso'
--
-- Defaults to 'False'
simpleLensRule :: Simple Lens LensRules Bool
simpleLensRule f (LensRules i n c b lni sl) = LensRules i n c b lni <$> f sl

-- | Always make a class when the data type is monomorphic.
--
-- No isomorphism lens will be created for monomorphic types that have a 
-- singular single-argument data constructor
--
-- > makeLensesWith $ classLensRule <~ classyClassLensRule $ defaultConfig
classyClassLensRule :: Bool -> String -> Maybe LensClass
classyClassLensRule _ n@(a:as) = Just $ LensClass ("Has" ++ n) True True (toLower a:as)
classyClassLensRule _ _ = Nothing

-- | Useful classLensRule when you want isomorphisms when possible
--   then classes wherever monomorphic and will accept normal
--   lenses otherwise. This is the default definition.
--
-- > makeLensesWith $ classLensRule <~ mixedClassLensRule $ defaultConfig
mixedClassLensRule :: Bool -> String -> Maybe LensClass
mixedClassLensRule True _ = Nothing
mixedClassLensRule _ n@(a:as) = Just $ LensClass ("Has" ++ n) True True (toLower a:as)
mixedClassLensRule _ _ = Nothing

-- | Use this if you want to have no class.
--
-- > makeLensesWith $ classLensRule <~ noClassLensRule $ defaultConfig
noClassLensRule :: Bool -> String -> Maybe LensClass
noClassLensRule _ _ = Nothing

-- | Default lens rules
defaultLensRules :: LensRules
defaultLensRules = LensRules top field mixedClassLensRule True False False where
  top (c:cs) = Just (toLower c:cs)
  top _      = Nothing
  field ('_':c:cs) = Just (toLower c:cs)
  field _          = Nothing

-- | Reconfigure to precompose classes wherever possible, rather than use isomorphisms
classyClasses :: LensRules -> LensRules
classyClasses = classLensRule <~ classyClassLensRule

-- | Prefer isomorphisms to classes.
mixedClasses :: LensRules -> LensRules
mixedClasses = classLensRule <~ mixedClassLensRule

-- | Never prefix our lenses with a class.
noClasses :: LensRules -> LensRules
noClasses = classLensRule <~ noClassLensRule

-- | Given a set of names, build a map from those names to a set of fresh names based on them.
freshMap :: Set Name -> Q (Map Name Name)
freshMap ns = Map.fromList <$> for (toList ns) (\ n -> (,) n <$> newName (nameBase n))

makeIsoTo :: Name -> ExpQ
makeIsoTo conName = lamE [varP (mkName "f"), conP conName [varP (mkName "a")]] $
  appsE [ varE (mkName "fmap")
        , conE conName
        , varE (mkName "f") `appE` varE (mkName "a")
        ]

makeIsoFrom :: Name -> ExpQ
makeIsoFrom conName = lamE [varP (mkName "f"), varP (mkName "a")] $
  appsE [ varE (mkName "fmap")
        , lamE [conP conName [varP (mkName "b")]] $ varE (mkName "b")
        , varE (mkName "f") `appE` (conE conName `appE` varE (mkName "a"))
        ]

makeIsoBody :: Name -> Name -> (Name -> ExpQ) -> (Name -> ExpQ) -> DecQ
makeIsoBody lensName conName f g = funD lensName [clause [] (normalB body) []] where
  body = appsE [ varE (mkName "isomorphic")
               , f conName
               , g conName
               ]

makeLensBody :: Name -> Name -> (Name -> ExpQ) -> (Name -> ExpQ) -> DecQ
makeLensBody lensName conName f _ = funD lensName [clause [] (normalB (f conName)) []]

appArgs :: Type -> [TyVarBndr] -> Type
appArgs t [] = t
appArgs t (x:xs) = appArgs (AppT t (VarT (x^.name))) xs

apps :: Type -> [Type] -> Type
apps t [] = t
apps t (x:xs) = apps (t `AppT` x) xs

appsT :: TypeQ -> [TypeQ] -> TypeQ
appsT t [] = t
appsT t (x:xs) = appsT (t `appT` x) xs

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
makeIsoLenses cfg ctx tyConName tyArgs dataConName maybeFieldName partTy = do
  m <- freshMap $ setOf typeVars tyArgs
  let aty = partTy
      bty = substTypeVars m aty
      cty = appArgs (ConT tyConName) tyArgs
      dty = substTypeVars m cty
      quantified = ForallT (tyArgs ++ substTypeVars m tyArgs) (ctx ++ substTypeVars m ctx)
      maybeIsoName = mkName <$> view isoLensRule cfg (nameBase dataConName)
      lensOnly = cfg^.noIsoLensRule
      isoCon   | lensOnly  = ConT (mkName "Control.Lens.Body")
               | otherwise = ConT (mkName "Control.Lens.Iso")
      makeBody | lensOnly  = makeLensBody
               | otherwise = makeIsoBody
  isoDecls <- flip (maybe (return [])) maybeIsoName $ \isoName -> do
    let decl = SigD isoName $ quantified $ isoCon `apps`
          if cfg^.simpleLensRule then [aty,aty,cty,cty] else [aty,bty,cty,dty]
    body <- makeBody isoName dataConName makeIsoFrom makeIsoTo
    inlining <- pragInlD isoName $ inlineSpecNoPhase True False
    return [decl, body, inlining]
  accessorDecls <- case mkName <$> (maybeFieldName >>= view fieldLensRule cfg . nameBase) of
    jfn@(Just lensName)
      | (jfn /= maybeIsoName) && (isNothing maybeIsoName || cfg^.addBothLensRule) -> do
      let decl = SigD lensName $ quantified $ isoCon `apps`
                   if cfg^.simpleLensRule then [cty,cty,aty,aty]
                                          else [cty,dty,aty,bty]
      body <- makeBody lensName dataConName makeIsoTo makeIsoFrom
      inlining <- pragInlD lensName $ inlineSpecNoPhase True False
      return [decl, body, inlining]
    _ -> return []
  return $ isoDecls ++ accessorDecls

data FieldDesc = FieldDesc
  { _fieldName                   :: Name
  , _fieldType                   :: Type
  , _fieldTypeVarsBoundElsewhere :: Set Name
  }

thd :: (a,b,c) -> c
thd (_,_,c) = c

fieldDescs :: Set Name -> [(Name,Strict,Type)] -> [FieldDesc]
fieldDescs acc ((nm,_,ty):rest) =
  FieldDesc nm ty (acc `Set.union` setOf typeVars (map thd rest)) :
  fieldDescs (acc `Set.union` setOf typeVars ty) rest
fieldDescs _ [] = []

conFieldDescs :: Con -> [FieldDesc]
conFieldDescs (RecC _ fields) = fieldDescs mempty fields
conFieldDescs _ = []

commonFieldDescs :: [Con] -> [FieldDesc]
commonFieldDescs = toList . Prelude.foldr walk mempty where
  walk con m = Prelude.foldr step m (conFieldDescs con)
  step d@(FieldDesc nm ty bds) m = case m^.at nm of
    Just (FieldDesc _ _ bds') -> at nm <~ Just (FieldDesc nm ty (bds `Set.union` bds')) $ m
    Nothing                   -> at nm <~ Just d                                        $ m

errorClause :: Name -> Name -> Name -> ClauseQ
errorClause lensName fieldName conName
  = clause [] (normalB (varE (mkName "error") `appE` litE (stringL err))) []
  where
    err = show lensName ++ ": no matching field "
       ++ show fieldName ++ " in constructor "
       ++ show conName

makeFieldLensBody :: Name -> Name -> [Con] -> Maybe Name -> Q Dec
makeFieldLensBody lensName fieldName cons maybeMethodName = case maybeMethodName of
    Just methodName -> do
       go <- newName "go"
       funD lensName [ clause [] (normalB (infixApp (varE methodName) (varE (mkName ".")) (varE go))) [funD go (map clauses cons)]]
    Nothing -> funD lensName (map clauses cons)
  where
    clauses (RecC conName fields) = case List.findIndex (\(n,_,_) -> n == fieldName) fields of
      Just i -> do
        names <- for fields $ \(n,_,_) -> newName (nameBase n)
        f     <- newName "f"
        x     <- newName "y"
        clause [varP f, conP conName $ map varP names] (normalB
               (appsE [ varE (mkName "fmap")
                      , lamE [varP x] $ appsE $ conE conName : map varE (element i <~ x $ names)
                      , varE (mkName "f") `appE` varE (names^.element i)
                      ])) []
      Nothing -> errorClause lensName fieldName conName
    clauses con = errorClause lensName fieldName (con^.name)

-- TODO: When there are constructors with missing fields, turn that field into a _traversal_ not a lens.
-- TODO: When the supplied mapping function maps multiple different fields to the same name, try to unify them into a Traversal.
-- TODO: Add support for precomposing a lens from a class onto all constructed lenses
makeFieldLenses :: LensRules
                -> Cxt         -- ^ surrounding cxt driven by the data type context
                -> Name        -- ^ data/newtype constructor name
                -> [TyVarBndr] -- ^ args
                -> [Con]
                -> Q [Dec]
makeFieldLenses cfg ctx tyConName tyArgs cons = do
  x <- newName "x"
  let maybeLensClass = do
        guard $ tyArgs == []
        view classLensRule cfg False (nameBase tyConName)
      maybeClassName = fmap (^.lensClassName.to mkName) maybeLensClass
      aty | isJust maybeClassName = VarT x
          | otherwise             = appArgs (ConT tyConName) tyArgs
      vs = setOf typeVars tyArgs
      fieldMap = commonFieldDescs cons
  classDecls <- case maybeLensClass of
    Nothing -> return []
    Just (LensClass clsNameString mkCls mkIns methodNameString) -> do
      let clsName    = mkName clsNameString
          methodName = mkName methodNameString
      t <- newName "t"
      a <- newName "a"
      Prelude.sequence $
        filter (const mkCls)
          [ classD (return []) clsName [PlainTV t] []
            [ sigD methodName $ conT (mkName "Control.Lens.Lens") `appsT` [varT t,varT t, conT tyConName, conT tyConName]]]
        ++ filter (const mkIns)
          [ instanceD (return []) (conT clsName `appT` conT tyConName)
            [ funD methodName [clause [varP a] (normalB (varE a)) []]
            , pragInlD methodName $ inlineSpecNoPhase True False ]]
  bodies <- for (toList fieldMap) $ \ (FieldDesc nm cty bds) ->
     case mkName <$> view fieldLensRule cfg (nameBase nm) of
       Nothing -> return []
       Just lensName -> do
         m <- freshMap $ Set.difference vs bds
         let bty = substTypeVars m aty
             dty = substTypeVars m cty
             s = setOf folded m
             relevantBndr b = s^.contains (b^.name)
             relevantCtx = not . Set.null . Set.intersection s . setOf typeVars
             tvs = tyArgs ++ filter relevantBndr (substTypeVars m tyArgs)
             ps = ctx ++ filter relevantCtx (substTypeVars m ctx)
             qs = case maybeClassName of
                Just n -> ClassP n [VarT x] : ps
                _      -> ps
             tvs' | isJust maybeClassName = PlainTV x : tvs
                  | otherwise             = tvs
         let decl = SigD lensName $ ForallT tvs' qs $ ConT (mkName "Control.Lens.Lens") `apps`
                      if cfg^.simpleLensRule then [aty,aty,cty,cty]
                                             else [aty,bty,cty,dty]
         body <- makeFieldLensBody lensName nm cons $ fmap (mkName . view lensClassMember) maybeLensClass
         inlining <- pragInlD lensName $ inlineSpecNoPhase True False
         return [decl, body, inlining]
  return $ classDecls ++ Prelude.concat bodies

-- | Build lenses with a custom configuration
makeLensesWith :: LensRules -> Name -> Q [Dec]
makeLensesWith cfg nm = reify nm >>= \inf -> case inf of
  TyConI dt -> case dt of
    NewtypeD ctx tyConName args (NormalC dataConName [(_,ty)])  _ | isoWanted tyConName ->
      makeIsoLenses cfg ctx tyConName args dataConName Nothing ty
    DataD ctx tyConName args [NormalC dataConName [(_,ty)]]  _    | isoWanted tyConName ->
      makeIsoLenses cfg ctx tyConName args dataConName Nothing ty
    NewtypeD ctx tyConName args (RecC dataConName [(fld,_,ty)]) _ | isoWanted tyConName ->
      makeIsoLenses cfg ctx tyConName args dataConName (Just fld) ty
    DataD ctx tyConName args [RecC dataConName [(fld,_,ty)]] _    | isoWanted tyConName ->
      makeIsoLenses cfg ctx tyConName args dataConName (Just fld) ty
    DataD ctx tyConName args dataCons _ ->
      makeFieldLenses cfg ctx tyConName args dataCons
    _ -> error "Unsupported data type"
  _ -> error "Expected the name of a data type or newtype"
  where isoWanted = isNothing . view classLensRule cfg True . nameBase

-- | Build lenses with a sensible default configuration
makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesWith defaultLensRules

-- | Derive lenses, specifying explicit pairings of @(fieldName, lensName)@.
--
-- Example usage:
--
-- > makeLensesFor [("_foo", "fooLens"), ("bar", "lbar")] ''Foo
makeLensesFor :: [(String, String)] -> Name -> Q [Dec]
makeLensesFor fields = makeLensesWith $ fieldLensRule <~ (`Prelude.lookup` fields)
                                      $ isoLensRule <~ const Nothing
                                      $ defaultLensRules
