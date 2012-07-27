{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 704
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.TH
-- Copyright   :  (C) 2012 Edward Kmett, Dan Burton
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  TemplateHaskell
--
----------------------------------------------------------------------------
module Control.Lens.TH
  (
  -- ** Constructing Lenses Automatically
    makeLenses
  , makeLensesBy
  , makeLensesFor
  ) where

import           Data.Char (toLower)
import           Control.Applicative
import           Language.Haskell.TH

-------------------------------------
-- Constructing Lenses Automatically
-------------------------------------

-- | Derive lenses for the record selectors in
-- a single-constructor data declaration,
-- or for the record selector in a newtype declaration.
-- Lenses will only be generated for record fields which
-- are prefixed with an underscore.
--
-- Example usage:
--
-- > makeLenses ''Foo
makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesBy defaultNameTransform

-- | Derive lenses, specifying explicit pairings of @(fieldName, lensName)@.
--
-- Example usage:
--
-- > makeLensesFor [("_foo", "fooLens"), ("bar", "lbar")] ''Foo
makeLensesFor :: [(String, String)] -> Name -> Q [Dec]
makeLensesFor fields = makeLensesBy (`Prelude.lookup` fields)

-- | Derive lenses with the provided name transformation
-- and filtering function. Produce @Just lensName@ to generate a lens
-- of the resultant name, or @Nothing@ to not generate a lens
-- for the input record name.
--
-- Example usage:
--
-- > makeLensesBy (\n -> Just (n ++ "L")) ''Foo
makeLensesBy ::
     (String -> Maybe String) -- ^ the name transformer
  -> Name -> Q [Dec]
makeLensesBy nameTransform datatype = do
  typeInfo          <- extractLensTypeInfo datatype
  let derive1 = deriveLens nameTransform typeInfo
  constructorFields <- extractConstructorFields datatype
  Prelude.concat <$> Prelude.mapM derive1 constructorFields

------------------------------------------------------------------------------
-- Template Haskell Implementation Details
------------------------------------------------------------------------------

-- | By default, if the field name begins with an underscore,
-- then the underscore will simply be removed (and the new first character
-- lowercased if necessary).
defaultNameTransform :: String -> Maybe String
defaultNameTransform ('_':c:rest) = Just $ toLower c : rest
defaultNameTransform _ = Nothing

-- | Information about the larger type the lens will operate on.
type LensTypeInfo = (Name, [TyVarBndr])

-- | Information about the smaller type the lens will operate on.
type ConstructorFieldInfo = (Name, Strict, Type)

extractLensTypeInfo :: Name -> Q LensTypeInfo
extractLensTypeInfo datatype = do
  let datatypeStr = nameBase datatype
  i <- reify datatype
  return $ case i of
    TyConI (DataD    _ n ts _ _) -> (n, ts)
    TyConI (NewtypeD _ n ts _ _) -> (n, ts)
    _ -> error $ "Can't derive Lens for: "  ++ datatypeStr ++ ", type name required."

extractConstructorFields :: Name -> Q [ConstructorFieldInfo]
extractConstructorFields datatype = do
  let datatypeStr = nameBase datatype
  i <- reify datatype
  return $ case i of
    TyConI (DataD    _ _ _ [RecC _ fs] _) -> fs
    TyConI (NewtypeD _ _ _ (RecC _ fs) _) -> fs
    TyConI (DataD    _ _ _ [_]         _) -> error $ "Can't derive Lens without record selectors: " ++ datatypeStr
    TyConI NewtypeD{} -> error $ "Can't derive Lens without record selectors: " ++ datatypeStr
    TyConI TySynD{}   -> error $ "Can't derive Lens for type synonym: " ++ datatypeStr
    TyConI DataD{}    -> error $ "Can't derive Lens for tagged union: " ++ datatypeStr
    _                 -> error $ "Can't derive Lens for: "  ++ datatypeStr ++ ", type name required."

-- Derive a lens for the given record selector
-- using the given name transformation function.
deriveLens :: (String -> Maybe String)
           -> LensTypeInfo
           -> ConstructorFieldInfo
           -> Q [Dec]
deriveLens nameTransform ty field = case nameTransform (nameBase fieldName) of
  Nothing          -> return []
  Just lensNameStr -> do
    body <- deriveLensBody (mkName lensNameStr) fieldName
    return [body]
  where
    (fieldName, _fieldStrict, _fieldType) = field
    (_tyName, _tyVars) = ty  -- just to clarify what's here

-- Given a record field name,
-- produces a single function declaration:
-- lensName f a = (\x -> a { field = x }) `fmap` f (field a)
deriveLensBody :: Name -> Name -> Q Dec
deriveLensBody lensName fieldName = funD lensName [defLine]
  where
    a = mkName "a"
    f = mkName "f"
    defLine = clause pats (normalB body) []
    pats = [varP f, varP a]
    body = [| (\x -> $(record a fieldName [|x|]))
              `fmap` $(appE (varE f) (appE (varE fieldName) (varE a)))
            |]
    record rec fld val = val >>= \v -> recUpdE (varE rec) [return (fld, v)]

