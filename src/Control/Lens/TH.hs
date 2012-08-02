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
  , isoLensRule
  , fieldLensRule
  , defaultLensRules
  -- ** Constructing Lenses Automatically
  , makeLenses
  , makeLensesWith
  , makeLensesFor
  ) where

import Control.Applicative
import Control.Lens
import Data.Char (toLower)
import Data.Foldable
import Data.List as List
import Data.Map as Map hiding (toList,map,filter)
import Data.Map.Lens
import Data.Maybe (isNothing)
import Data.Monoid
import Data.Set as Set hiding (toList,map,filter)
import Data.Set.Lens
import Data.Traversable
import Language.Haskell.TH
import Language.Haskell.TH.Lens

-- | This configuration describes the options we'll be using to make isomorphisms or lenses
data LensRules = LensRules
  { _isoLensRule   :: String -> Maybe String -- ^ used to name the top level isomorphism for single constructor, single field data types and newtypes
  , _fieldLensRule :: String -> Maybe String -- ^ used to name the lens, given the name of the basic field
  , _addBothLensRule :: Bool
  }

-- | Lens to access the convention for naming top level isomorphisms in our lens rules
isoLensRule :: Simple Lens LensRules (String -> Maybe String)
isoLensRule f (LensRules i n b) = (\i' -> LensRules i' n b) <$> f i

-- | Lens to access the convention for naming fields in our lens rules
fieldLensRule :: Simple Lens LensRules (String -> Maybe String)
fieldLensRule f (LensRules i n b) = (\n' -> LensRules i n' b) <$> f n

-- | This flag indicates whether or not we should attempt to add both an isomorphism lens and a top level accessor
addBothLensRule :: Simple Lens LensRules Bool
addBothLensRule f (LensRules i n b) = LensRules i n <$> f b

-- | Default lens rules
defaultLensRules :: LensRules
defaultLensRules = LensRules top field True where
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

appArgs :: Type -> [TyVarBndr] -> Type
appArgs t [] = t
appArgs t (x:xs) = appArgs (AppT t (VarT (x^.name))) xs

apps :: Type -> [Type] -> Type
apps t [] = t
apps t (x:xs) = apps (t `AppT` x) xs

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
makeIso :: LensRules
        -> Cxt
        -> Name
        -> [TyVarBndr]
        -> Name
        -> Maybe Name
        -> Type
        -> Q [Dec]
makeIso cfg ctx tyConName tyArgs dataConName maybeFieldName partTy = do
  m <- freshMap $ setOf typeVars tyArgs
  let aty = partTy
      bty = substTypeVars m aty
      cty = appArgs (ConT tyConName) tyArgs
      dty = substTypeVars m cty
      quantified = ForallT (tyArgs ++ substTypeVars m tyArgs) (ctx ++ substTypeVars m ctx)
      maybeIsoName = mkName <$> view isoLensRule cfg (nameBase dataConName)
  isoDecls <- flip (maybe (return [])) maybeIsoName $ \isoName -> do
    let decl = SigD isoName $ quantified $
                 ConT (mkName "Control.Lens.Iso") `apps` [aty,bty,cty,dty]
    body <- makeIsoBody isoName dataConName makeIsoFrom makeIsoTo
    inlining <- pragInlD isoName (inlineSpecNoPhase True False)
    return [decl, body, inlining]
  accessorDecls <- case mkName <$> (maybeFieldName >>= view fieldLensRule cfg . nameBase) of
    jfn@(Just lensName)
      | (jfn /= maybeIsoName) && (isNothing maybeIsoName || view addBothLensRule cfg) -> do
      let decl = SigD lensName $ quantified $
                   ConT (mkName "Control.Lens.Iso") `apps` [cty,dty,aty,bty]
      body <- makeIsoBody lensName dataConName makeIsoTo makeIsoFrom
      inlining <- pragInlD lensName (inlineSpecNoPhase True False)
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
fieldDescs acc ((nm,_,ty):rest) = FieldDesc nm ty (acc <> setOf typeVars (map thd rest)) : fieldDescs (acc <> setOf typeVars ty) rest
fieldDescs _ [] = []

conFieldDescs :: Con -> [FieldDesc]
conFieldDescs (RecC _ fields) = fieldDescs mempty fields
conFieldDescs _ = []

commonFieldDescs :: [Con] -> [FieldDesc]
commonFieldDescs = toList . Prelude.foldr walk mempty where
  walk con m = Prelude.foldr step m (conFieldDescs con)
  step d@(FieldDesc nm ty bds) m = case m^.at nm of
    Just (FieldDesc _ _ bds') -> at nm <~ Just (FieldDesc nm ty (bds <> bds')) $ m
    Nothing                   -> at nm <~ Just d                               $ m

errorClause :: Name -> Name -> Name -> ClauseQ
errorClause lensName fieldName conName = clause [] (normalB (varE (mkName "error") `appE` litE (stringL err))) [] where
  err = show lensName ++ ": no matching field " ++ show fieldName ++ " in constructor " ++ show conName

makeFieldLensBody :: Name -> Name -> [Con] -> Q Dec
makeFieldLensBody lensName fieldName = funD lensName . map clauses where
  clauses (RecC conName fields) = case List.findIndex (\(n,_,_) -> n == fieldName) fields of
    Just i -> do
      names  <- for fields $ \(n,_,_) -> newName (nameBase n)
      f      <- newName "f"
      nm     <- newName "x"
      clause [varP f, conP conName $ map varP names] (normalB
             (appsE [ varE (mkName "fmap")
                    , lamE [varP nm] $ appsE (conE conName : map varE (element i <~ nm $ names))
                    , varE (mkName "f") `appE` varE (names^.element i)
                    ])) []
    Nothing -> errorClause lensName fieldName conName
  clauses con = errorClause lensName fieldName (con^.name)

-- TODO: When there are constructors with missing fields, turn that field into a _traversal_ not a lens.
-- TODO: When the supplied mapping function maps multiple different fields to the same name, try to unify them into a Traversal.
makeFieldLenses :: LensRules
                -> Cxt         -- ^ surrounding cxt driven by the data type context
                -> Name        -- ^ data/newtype constructor name
                -> [TyVarBndr] -- ^ args
                -> [Con]
                -> Q [Dec]
makeFieldLenses cfg ctx tyConName tyArgs cons = do
  let aty = appArgs (ConT tyConName) tyArgs
      vs = setOf typeVars tyArgs
      fieldMap = commonFieldDescs cons
  fmap Prelude.concat . for (toList fieldMap) $ \ (FieldDesc nm cty bds) ->
     case mkName <$> view fieldLensRule cfg (nameBase nm) of
       Nothing -> return []
       Just lensName -> do
         m <- freshMap $ Set.difference vs bds
         let bty = substTypeVars m aty
             dty = substTypeVars m cty
             s = setOf folded m -- get the target values
             relevantBndr b = s^.contains (b^.name)
             relevantCtx = not . Set.null . Set.intersection s . setOf typeVars
             tvs = tyArgs ++ filter relevantBndr (substTypeVars m tyArgs)
             ps = ctx ++ filter relevantCtx (substTypeVars m ctx)
         let decl = SigD lensName $ ForallT tvs ps $ ConT (mkName "Control.Lens.Lens") `apps` [aty,bty,cty,dty]
         body <- makeFieldLensBody lensName nm cons
         inlining <- pragInlD lensName (inlineSpecNoPhase True False)
         return [decl, body, inlining]

-- | Build lenses with a custom configuration
makeLensesWith :: LensRules -> Name -> Q [Dec]
makeLensesWith cfg nm = reify nm >>= \inf -> case inf of
  TyConI dt -> case dt of
    NewtypeD ctx tyConName args (NormalC dataConName [(_,ty)])  _ -> makeIso cfg ctx tyConName args dataConName Nothing ty
    DataD    ctx tyConName args [NormalC dataConName [(_,ty)]]  _ -> makeIso cfg ctx tyConName args dataConName Nothing ty
    NewtypeD ctx tyConName args (RecC dataConName [(fld,_,ty)]) _ -> makeIso cfg ctx tyConName args dataConName (Just fld) ty
    DataD    ctx tyConName args [RecC dataConName [(fld,_,ty)]] _ -> makeIso cfg ctx tyConName args dataConName (Just fld) ty
    DataD    ctx tyConName args dataCons _                        -> makeFieldLenses cfg ctx tyConName args dataCons
    _ -> error "Unsupported data type"
  _ -> error "Expected the name of a data type or newtype"

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
