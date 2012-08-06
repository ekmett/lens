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
  ( LensRules(LensRules)
  , lensIso
  , lensField
  , lensClass
  , lensFlags
  , LensFlag(..)
  , simpleLenses, handleSingletons, singletonIso, singletonRequired, createClass, createInstance, classRequired
  -- * Constructing Lenses Automatically
  , makeClassy, makeClassyFor
  , makeIso
  , makeLenses, makeLensesFor
  , makeLensesWith
  , defaultRules
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
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

-- | Flags for lens construction
data LensFlag
  = SimpleLenses

  | SingletonAndField
  | SingletonIso
  | HandleSingletons
  | SingletonRequired

  | CreateClass
  | CreateInstance
  | ClassRequired
  deriving (Eq,Ord,Show,Read)

-- | Only Generate valid 'Simple' 'Lens' lenses
simpleLenses      :: Simple Lens LensRules Bool
simpleLenses       = lensFlags.contains SimpleLenses

-- | Handle singleton constructors specially
handleSingletons :: Simple Lens LensRules Bool
handleSingletons = lensFlags.contains HandleSingletons

-- | When building an singleton iso (or lens) for a record constructor, build both
singletonAndField :: Simple Lens LensRules Bool
singletonAndField  = lensFlags.contains SingletonAndField

-- | Use Iso for singleton constructors
singletonIso :: Simple Lens LensRules Bool
singletonIso = lensFlags.contains SingletonIso

-- | Expect a single constructor, single field newtype or data type.
singletonRequired  :: Simple Lens LensRules Bool
singletonRequired   = lensFlags.contains SingletonRequired

-- | Create the class if the constructor is simple and the 'lensClass' rule matches
createClass       :: Simple Lens LensRules Bool
createClass        = lensFlags.contains CreateClass

-- | Create the instance if the constructor is simple and the 'lensClass' rule matches
createInstance    :: Simple Lens LensRules Bool
createInstance     = lensFlags.contains CreateInstance

-- | Die if the 'lensClass' fails to match
classRequired     :: Simple Lens LensRules Bool
classRequired      = lensFlags.contains ClassRequired

-- | This configuration describes the options we'll be using to make isomorphisms or lenses
data LensRules = LensRules
  { _lensIso   :: String -> Maybe String
  , _lensField :: String -> Maybe String
  , _lensClass :: String -> Maybe (String, String)
  , _lensFlags :: Set LensFlag
  }

-- | Lens to access the convention for naming top level isomorphisms in our lens rules
--
-- Defaults to lowercasing the first letter of the constructor.
lensIso :: Simple Lens LensRules (String -> Maybe String)
lensIso f (LensRules i n c o) = (\i' -> LensRules i' n c o) <$> f i

-- | Lens to access the convention for naming fields in our lens rules
--
-- Defaults to stripping the _ off of the field name and lowercasing the name and
-- rejecting the field if it doesn't start with an '_'.
lensField :: Simple Lens LensRules (String -> Maybe String)
lensField f (LensRules i n c o) = (\n' -> LensRules i n' c o) <$> f n

-- | Retrieve options such as the name of the class and method to put in it to build a class around monomorphic data types.
lensClass :: Simple Lens LensRules (String -> Maybe (String, String))
lensClass f (LensRules i n c o) = (\c' -> LensRules i n c' o) <$> f c

-- | Retrieve options such as the name of the class and method to put in it to build a class around monomorphic data types.
lensFlags :: Simple Lens LensRules (Set LensFlag)
lensFlags f (LensRules i n c o) = LensRules i n c <$> f o

-- | Default lens rules
defaultRules :: LensRules
defaultRules = LensRules top field (const Nothing) $
    Set.fromList [SingletonIso, SingletonAndField, CreateClass, CreateInstance]
  where
    top (c:cs) = Just (toLower c:cs)
    top _      = Nothing
    field ('_':c:cs) = Just (toLower c:cs)
    field _          = Nothing

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
makeIsoLenses cfg ctx tyConName tyArgs dataConName maybeFieldName partTy = do
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
    inlining <- pragInlD isoName $ inlineSpecNoPhase True False
    return [decl, body, inlining]
  accessorDecls <- case mkName <$> (maybeFieldName >>= view lensField cfg . nameBase) of
    jfn@(Just lensName)
      | (jfn /= maybeIsoName) && (isNothing maybeIsoName || cfg^.singletonAndField) -> do
      let decl = SigD lensName $ quantified $ isoCon `apps`
                   if cfg^.simpleLenses then [cty,cty,aty,aty]
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
    Just (FieldDesc _ _ bds') -> at nm .~ Just (FieldDesc nm ty (bds `Set.union` bds')) $ m
    Nothing                   -> at nm .~ Just d                                        $ m

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
                      , lamE [varP x] $ appsE $ conE conName : map varE (element i .~ x $ names)
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
        view lensClass cfg (nameBase tyConName)
      maybeClassName = fmap (^._1.to mkName) maybeLensClass
      aty | isJust maybeClassName = VarT x
          | otherwise             = appArgs (ConT tyConName) tyArgs
      vs = setOf typeVars tyArgs
      fieldMap = commonFieldDescs cons
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
            , pragInlD methodName $ inlineSpecNoPhase True False ]]
  bodies <- for (toList fieldMap) $ \ (FieldDesc nm cty bds) ->
     case mkName <$> view lensField cfg (nameBase nm) of
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

         let decl = SigD lensName $ ForallT tvs' qs $
                    apps (ConT ''Lens) $
                    if cfg^.simpleLenses
                    then [aty,aty,cty,cty]
                    else [aty,bty,cty,dty]
         body <- makeFieldLensBody lensName nm cons $ fmap (mkName . view _2) maybeLensClass
         inlining <- pragInlD lensName $ inlineSpecNoPhase True False
         return [decl, body, inlining]
  return $ classDecls ++ Prelude.concat bodies

-- | Build lenses with a custom configuration
makeLensesWith :: LensRules -> Name -> Q [Dec]
makeLensesWith cfg nm = reify nm >>= \inf -> case inf of
  TyConI dt -> case dt of
    NewtypeD ctx tyConName args (NormalC dataConName [(_,ty)])  _ | cfg^.handleSingletons ->
      makeIsoLenses cfg ctx tyConName args dataConName Nothing ty
    DataD ctx tyConName args [NormalC dataConName [(_,ty)]]  _    | cfg^.handleSingletons ->
      makeIsoLenses cfg ctx tyConName args dataConName Nothing ty
    NewtypeD ctx tyConName args (RecC dataConName [(fld,_,ty)]) _ | cfg^.handleSingletons ->
      makeIsoLenses cfg ctx tyConName args dataConName (Just fld) ty
    DataD ctx tyConName args [RecC dataConName [(fld,_,ty)]] _    | cfg^.handleSingletons ->
      makeIsoLenses cfg ctx tyConName args dataConName (Just fld) ty
    _ | cfg^.singletonRequired -> fail "makeLensesWith: A single-constructor single-argument data type is required"
    DataD ctx tyConName args dataCons _ ->
      makeFieldLenses cfg ctx tyConName args dataCons
    _ -> fail "Unsupported data type"
  _ -> fail "Expected the name of a data type or newtype"

-- | Build lenses with a sensible default configuration
makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesWith
  $ lensIso   .~ const Nothing
  $ lensClass .~ const Nothing
  $ handleSingletons .~ True    -- generate an Iso for the field if its the only one
  $ defaultRules

-- | Make a top level isomorphism injecting _into_ the type
--
-- The supplied name is required to be for a type with a single constructor that has a single argument
makeIso :: Name -> Q [Dec]
makeIso = makeLensesWith
  $ singletonRequired .~ True
  $ singletonAndField .~ True
  $ defaultRules

-- | Make 'classy lenses' for a type
makeClassy :: Name -> Q [Dec]
makeClassy = makeLensesWith
  $ lensIso .~ const Nothing
  $ handleSingletons .~ False
  $ lensClass .~ classy
  $ classRequired .~ True
  $ defaultRules

classy :: String -> Maybe (String, String)
classy n@(a:as) = Just ("Has" ++ n, toLower a:as)
classy _ = Nothing

-- | Derive lenses, specifying explicit pairings of @(fieldName, lensName)@.
--
-- Example usage:
--
-- > makeLensesFor [("_foo", "fooLens"), ("bar", "lbar")] ''Foo
makeLensesFor :: [(String, String)] -> Name -> Q [Dec]
makeLensesFor fields = makeLensesWith
  $ lensField .~ (`Prelude.lookup` fields)
  $ lensIso   .~ const Nothing
  $ lensClass .~ const Nothing
  $ handleSingletons .~ True
  $ defaultRules

-- | Derive lenses, specifying explicit pairings of @(fieldName, lensName)@
-- using a wrapper class.
--
-- Example usage:
--
-- > makeClassyFor "HasFoo" "foo" [("_foo", "fooLens"), ("bar", "lbar")] ''Foo
makeClassyFor :: String -> String -> [(String, String)] -> Name -> Q [Dec]
makeClassyFor clsName funName fields = makeLensesWith
  $ lensField .~ (`Prelude.lookup` fields)
  $ lensIso .~ const Nothing
  $ lensClass .~ const (Just (clsName,funName))
  $ handleSingletons .~ False
  $ defaultRules

-- The orphan instance for old versions is bad, but programing without Applicative is worse.
#if !(MIN_VERSION_template_haskell(2,7,0))
instance Applicative Q where
  pure = return
  (<*>) = ap
#endif
