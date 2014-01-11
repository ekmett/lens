{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
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
-- Copyright   :  (C) 2012-13 Edward Kmett, Michael Sloan, Maxwell Swadling
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
  , makeClassy, makeClassyFor, makeClassy_
  , makeIso
  , makePrisms
  , makeWrapped
  , makeFields
  , makeInferableLenses, (???)
  -- * Constructing Lenses Given a Declaretion Quote
  , declareLenses, declareLensesFor
  , declareClassy, declareClassyFor
  , declareIso
  , declarePrisms
  , declareWrapped
  , declareFields
  -- * Configuring Lenses
  , makeLensesWith
  , makeFieldsWith
  , declareLensesWith
  , declareFieldsWith
  , defaultRules
  , defaultFieldRules
  , camelCaseFields
  , underscoreFields
  , LensRules(LensRules)
  , FieldRules(FieldRules)
  , lensRules
  , classyRules
  , classyRules_
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
import Control.Monad ((<=<), when, replicateM)
#if !(MIN_VERSION_template_haskell(2,7,0))
import Control.Monad (ap)
#endif
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans.Writer
import Control.Lens.At
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Iso
import Control.Lens.Lens
import Control.Lens.Prism
import Control.Lens.Review
import Control.Lens.Setter
import Control.Lens.Tuple
import Control.Lens.Traversal
import Control.Lens.Wrapped
import Control.Lens.Internal.TH
import Data.Char (toLower, toUpper, isUpper)
import Data.Either (lefts)
import Data.Foldable hiding (concat, any)
import Data.Function (on)
import Data.List as List
import Data.Map as Map hiding (toList,map,filter)
import Data.Maybe as Maybe (isNothing,isJust,catMaybes,fromJust,mapMaybe)
import Data.Monoid
import Data.Ord (comparing)
import Data.Set as Set hiding (toList,map,filter)
import Data.Set.Lens
import Data.Traversable hiding (mapM)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lens

#ifdef HLINT
{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Use fewer imports" #-}
{-# ANN module "HLint: ignore Use foldl" #-}
#endif

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

lensRulesFor :: [(String, String)] -> LensRules
lensRulesFor fields = lensRules & lensField .~ (`Prelude.lookup` fields)

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

classyRulesFor
  :: (String -> Maybe (String, String)) -> [(String, String)] -> LensRules
classyRulesFor classFun fields = classyRules
  & lensClass .~ classFun
  & lensField .~ (`Prelude.lookup` fields)

underscorePrefixRules :: LensRules
underscorePrefixRules = LensRules mLowerName fld (const Nothing) $
    Set.fromList [SingletonIso, SingletonAndField, CreateClass,
                  CreateInstance, BuildTraversals, GenerateSignatures]
  where
    fld cs = Just ('_':cs)

classyRules_ :: LensRules
classyRules_ = underscorePrefixRules
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
-- /e.g./
--
-- @
-- data FooBar
--   = Foo { _x, _y :: 'Int' }
--   | Bar { _x :: 'Int' }
-- 'makeLenses' ''FooBar
-- @
--
-- will create
--
-- @
-- x :: 'Lens'' FooBar 'Int'
-- x f (Foo a b) = (\a\' -> Foo a\' b) \<$\> f a
-- x f (Bar a)   = Bar \<$\> f a
-- y :: 'Traversal'' FooBar 'Int'
-- y f (Foo a b) = (\b\' -> Foo a  b\') \<$\> f b
-- y _ c\@(Bar _) = pure c
-- @
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

-- | Make lenses and traversals for a type, and create a class when the type
-- has no arguments.  Works the same as 'makeClassy' except that (a) it
-- expects that record field names do not begin with an underscore, (b) all
-- record fields are made into lenses, and (c) the resulting lens is prefixed
-- with an underscore.
makeClassy_ :: Name -> Q [Dec]
makeClassy_ = makeLensesWith classyRules_

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
makeLensesFor fields = makeLensesWith $ lensRulesFor fields

-- | Derive lenses and traversals, using a named wrapper class, and
-- specifying explicit pairings of @(fieldName, traversalName)@.
--
-- Example usage:
--
-- @
-- 'makeClassyFor' \"HasFoo\" \"foo\" [(\"_foo\", \"fooLens\"), (\"bar\", \"lbar\")] ''Foo
-- @
makeClassyFor :: String -> String -> [(String, String)] -> Name -> Q [Dec]
makeClassyFor clsName funName fields = makeLensesWith $
  classyRulesFor (const $ Just (clsName, funName)) fields

-- | Build lenses with a custom configuration.
makeLensesWith :: LensRules -> Name -> Q [Dec]
makeLensesWith cfg nm = do
    inf <- reify nm
    case inf of
      TyConI decl -> makeLensesForDec cfg decl
      _ -> fail "makeLensesWith: Expected the name of a data type or newtype"

-- | Generate a 'Prism' for each constructor of a data type.
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
makePrisms :: Name -> Q [Dec]
makePrisms nm = do
    inf <- reify nm
    case inf of
      TyConI decl -> makePrismsForDec decl
      _ -> fail "makePrisms: Expected the name of a data type or newtype"

-- | Make lenses for all records in the given declaration quote. All record
-- syntax in the input will be stripped off.
--
-- /e.g./
--
-- @
-- declareLenses [d|
--   data Foo = Foo { fooX, fooY :: 'Int' }
--     deriving 'Show'
--   |]
-- @
--
-- will create
--
-- @
-- data Foo = Foo 'Int' 'Int' deriving 'Show'
-- fooX, fooY :: 'Lens'' Foo Int
-- @
--
-- @ declareLenses = 'declareLensesWith' ('lensRules' '&' 'lensField' '.~' 'Just') @
declareLenses :: Q [Dec] -> Q [Dec]
declareLenses = declareLensesWith (lensRules & lensField .~ Just)

-- | Similar to 'makeLensesFor', but takes a declaration quote.
declareLensesFor :: [(String, String)] -> Q [Dec] -> Q [Dec]
declareLensesFor fields = declareLensesWith $
  lensRulesFor fields & lensField .~ Just

-- | For each record in the declaration quote, make lenses and traversals for
-- it, and create a class when the type has no arguments. All record syntax
-- in the input will be stripped off.
--
-- /e.g./
--
-- @
-- declareClassy [d|
--   data Foo = Foo { fooX, fooY :: 'Int' }
--     deriving 'Show'
--   |]
-- @
--
-- will create
--
-- @
-- data Foo = Foo 'Int' 'Int' deriving 'Show'
-- class HasFoo t where
--   foo :: 'Lens'' t Foo
-- instance HasFoo Foo where foo = 'id'
-- fooX, fooY :: HasFoo t => 'Lens'' t 'Int'
-- @
--
-- @ declareClassy = 'declareLensesWith' ('classyRules' '&' 'lensField' '.~' 'Just') @
declareClassy :: Q [Dec] -> Q [Dec]
declareClassy = declareLensesWith (classyRules & lensField .~ Just)

-- | Similar to 'makeClassyFor', but takes a declaration quote.
declareClassyFor :: [(String, (String, String))] -> [(String, String)] -> Q [Dec] -> Q [Dec]
declareClassyFor classes fields = declareLensesWith $
  classyRulesFor (`Prelude.lookup`classes) fields & lensField .~ Just

-- | For each datatype declaration, make a top level isomorphism injecting
-- /into/ the type. The types are required to be for a type with a single
-- constructor that has a single argument.
--
-- All record syntax in the input will be stripped off.
--
-- /e.g./
--
-- @
-- declareIso [d|
--   newtype WrappedInt = Wrap { unrwap :: 'Int' }
--   newtype 'List' a = 'List' [a]
--   |]
-- @
--
-- will create
--
-- @
-- newtype WrappedList = Wrap 'Int'
-- newtype List a = List [a]
-- 'wrap' :: 'Iso'' Int WrappedInt
-- 'unwrap' :: 'Iso'' WrappedInt Int
-- 'list' :: 'Iso' [a] [b] ('List' a) ('List' b)
-- @
--
-- @ declareIso = 'declareLensesWith' ('isoRules' '&' 'lensField' '.~' 'Just') @
declareIso :: Q [Dec] -> Q [Dec]
declareIso = declareLensesWith $ isoRules & lensField .~ Just

-- | Generate a 'Prism' for each constructor of each data type.
--
-- /e.g./
--
-- @
-- declarePrisms [d|
--   data Exp = Lit Int | Var String | Lambda{ bound::String, body::Exp }
--   |]
-- @
--
-- will create
--
-- @
-- data Exp = Lit Int | Var String | Lambda { bound::String, body::Exp }
-- _Lit :: 'Prism'' Exp Int
-- _Var :: 'Prism'' Exp String
-- _Lambda :: 'Prism'' Exp (String, Exp)
-- @
declarePrisms :: Q [Dec] -> Q [Dec]
declarePrisms = declareWith $ \dec -> do
  emit =<< Trans.lift (makePrismsForDec dec)
  return dec

-- | Build 'Wrapped' instance for each newtype.
declareWrapped :: Q [Dec] -> Q [Dec]
declareWrapped = declareWith $ \dec -> do
  maybeDecs <- Trans.lift (makeWrappedForDec dec)
  forM_ maybeDecs emit
  return dec

-- | @ declareFields = 'declareFieldsWith' 'defaultFieldRules' @
declareFields :: Q [Dec] -> Q [Dec]
declareFields = declareFieldsWith defaultFieldRules

-- | Declare lenses for each records in the given declarations, using the
-- specified 'LensRules'. Any record syntax in the input will be stripped
-- off.
declareLensesWith :: LensRules -> Q [Dec] -> Q [Dec]
declareLensesWith rules = declareWith $ \dec -> do
  emit =<< Trans.lift (makeLensesForDec rules dec)
  return $ stripFields dec

-- | Declare fields for each records in the given declarations, using the
-- specified 'FieldRules'. Any record syntax in the input will be stripped
-- off.
declareFieldsWith :: FieldRules -> Q [Dec] -> Q [Dec]
declareFieldsWith rules = declareWith $ \dec -> do
  emit =<< Trans.lift (makeFieldsForDec rules dec)
  return $ stripFields dec

-----------------------------------------------------------------------------
-- Internal TH Implementation
-----------------------------------------------------------------------------

-- | Transform @NewtypeD@s declarations to @DataD@s and @NewtypeInstD@s to
-- @DataInstD@s.
deNewtype :: Dec -> Dec
deNewtype (NewtypeD ctx tyName args c d) = DataD ctx tyName args [c] d
deNewtype (NewtypeInstD ctx tyName args c d) = DataInstD ctx tyName args [c] d
deNewtype d = d

makePrismsForDec :: Dec -> Q [Dec]
makePrismsForDec decl = case makeDataDecl decl of
  Just dataDecl -> makePrismsForCons dataDecl
  _ -> fail "makePrisms: Unsupported data type"

makePrismsForCons :: DataDecl -> Q [Dec]
makePrismsForCons dataDecl@(DataDecl _ _ _ _ [_]) = case constructors dataDecl of
  -- Iso promotion via tuples
  [NormalC dataConName xs] ->
    makeIsoLenses isoRules dataDecl dataConName Nothing $ map (view _2) xs
  [RecC    dataConName xs] ->
    makeIsoLenses isoRules dataDecl dataConName Nothing $ map (view _3) xs
  _                        ->
    fail "makePrismsForCons: A single-constructor data type is required"
makePrismsForCons dataDecl =
  concat <$> mapM (makePrismOrReviewForCon dataDecl canModifyTypeVar ) (constructors dataDecl)
  where
    conTypeVars = map (Set.fromList . toListOf typeVars) (constructors dataDecl)
    canModifyTypeVar = (`Set.member` typeVarsOnlyInOneCon) . view name
    typeVarsOnlyInOneCon = Set.fromList . concat . filter (\xs -> length xs == 1) .  List.group . List.sort $ conTypeVars >>= toList

onlyBuildReview :: Con -> Bool
onlyBuildReview ForallC{} = True
onlyBuildReview _         = False

makePrismOrReviewForCon :: DataDecl -> (TyVarBndr -> Bool) -> Con -> Q [Dec]
makePrismOrReviewForCon dataDecl canModifyTypeVar con
  | onlyBuildReview con = makeReviewForCon dataDecl con
  | otherwise           = makePrismForCon dataDecl canModifyTypeVar con

makeReviewForCon :: DataDecl -> Con -> Q [Dec]
makeReviewForCon dataDecl con = do
    let functionName                    = mkName ('_': nameBase dataConName)
        (dataConName, fieldTypes)       = ctrNameAndFieldTypes con

    sName       <- newName "s"
    aName       <- newName "a"
    fieldNames  <- replicateM (length fieldTypes) (newName "x")

    -- Compute the type: Constructor Constraints => Review s (Type x y z) a fieldTypes
    let s                = varT sName
        t                = return (fullType dataDecl (map (VarT . view name) (dataParameters dataDecl)))
        a                = varT aName
        b                = toTupleT (map return fieldTypes)

        (conTyVars, conCxt) = case con of ForallC x y _ -> (x,y)
                                          _             -> ([],[])

        functionType     = forallT (map PlainTV [sName, aName] ++ conTyVars ++ dataParameters dataDecl)
                                   (return conCxt)
                                   (conT ''Review `appsT` [s,t,a,b])

    -- Compute expression: unto (\(fields) -> Con fields)
    let pat  = toTupleP (map varP fieldNames)
        lam  = lam1E pat (conE dataConName `appsE1` map varE fieldNames)
        body = varE 'unto `appE` lam

    Prelude.sequence
      [ sigD functionName functionType
      , funD functionName [clause [] (normalB body) []]
      ]

makePrismForCon :: DataDecl -> (TyVarBndr -> Bool) -> Con -> Q [Dec]
makePrismForCon dataDecl canModifyTypeVar con = do
    remitterName <- newName "remitter"
    reviewerName <- newName "reviewer"
    xName <- newName "x"
    let resName = mkName $ '_': nameBase dataConName
    varNames <- for [0..length fieldTypes -1] $ \i -> newName ('x' : show i)
    let args = dataParameters dataDecl
    altArgsList <- forM (view name <$> filter isAltArg args) $ \arg ->
      (,) arg <$> newName (nameBase arg)
    let altArgs = Map.fromList altArgsList
        hitClause =
          clause [conP dataConName (fmap varP varNames)]
          (normalB $ appE (conE 'Right) $ toTupleE $ varE <$> varNames) []
        otherCons = filter (/= con) (constructors dataDecl)
        missClauses
          | List.null otherCons   = []
          | Map.null altArgs = [clause [varP xName] (normalB (appE (conE 'Left) (varE xName))) []]
          | otherwise        = reviewerIdClause <$> otherCons
    Prelude.sequence [
      sigD resName . forallT
        (args ++ (PlainTV <$> Map.elems altArgs))
        (return $ List.nub (dataContext dataDecl ++ substTypeVars altArgs (dataContext dataDecl))) $
         if List.null altArgsList then
          conT ''Prism' `appsT`
            [ return $ fullType dataDecl $ VarT . view name <$> args
            , toTupleT $ pure <$> fieldTypes
            ]
         else
          conT ''Prism `appsT`
            [ return $ fullType dataDecl $ VarT . view name <$> args
            , return $ fullType dataDecl $ VarT . view name <$> substTypeVars altArgs args
            , toTupleT $ pure <$> fieldTypes
            , toTupleT $ pure <$> substTypeVars altArgs fieldTypes
            ]
      , funD resName
        [ clause []
          (normalB (appsE [varE 'prism, varE remitterName, varE reviewerName]))
          [ funD remitterName
            [ clause [toTupleP (varP <$> varNames)] (normalB (conE dataConName `appsE1` fmap varE varNames)) [] ]
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
         (normalB (appE (conE 'Left) (conE dataConName `appsE1` fmap varE varNames)))
         []

-- | Given a set of names, build a map from those names to a set of fresh names
-- based on them.
freshMap :: Set Name -> Q (Map Name Name)
freshMap ns = Map.fromList <$> for (toList ns) (\ n -> (,) n <$> newName (nameBase n))

-- i.e. AppT (AppT (TupleT 2) (ConT GHC.Types.Int)) (ConT GHC.Base.String)
-- --> (\(x, y) -> Rect x y)
makeIsoFrom :: Type -> Name -> Q ([Name], Exp)
makeIsoFrom ty conName = lam <$> deCom ty
  where
    lam (ns, e) = (ns, LamE [TupP (map VarP ns)] e)
    deCom (TupleT _) = return ([], ConE conName)
    deCom (AppT l _) = do
      (ln, l') <- deCom l
      x <- newName "x"
      return (ln ++ [x], AppE l' (VarE x))
    deCom t = fail $ "unable to create isomorphism for: " ++ show t

-- i.e. AppT (AppT (TupleT 2) (ConT GHC.Types.Int)) (ConT GHC.Base.String)
-- --> (\(Rect x y) -> (x, y))
makeIsoTo :: [Name] -> Name -> ExpQ
makeIsoTo ns conName = lamE [conP conName (map varP ns)]
                          $ tupE $ map varE ns

makeIsoBody :: Name -> Exp -> Exp -> DecQ
makeIsoBody lensName f t = funD lensName [clause [] (normalB body) []] where
  body = appsE [ varE 'iso
               , return f
               , return t
               ]

makeLensBody :: Name -> Exp -> Exp -> DecQ
makeLensBody lensName i o = do
  f <- newName "f"
  a <- newName "a"
  funD lensName [clause [] (normalB (
    lamE [varP f, varP a] $
      appsE [ varE 'fmap
            , return o
            , varE f `appE` (return i `appE` varE a)
            ])) []]

plain :: TyVarBndr -> TyVarBndr
plain (KindedTV t _) = PlainTV t
plain (PlainTV t) = PlainTV t

apps :: Type -> [Type] -> Type
apps = Prelude.foldl AppT

makeLensesForDec :: LensRules -> Dec -> Q [Dec]
makeLensesForDec cfg decl = case makeDataDecl decl of
  Just dataDecl -> makeLensesForCons cfg dataDecl
  Nothing -> fail "makeLensesWith: Unsupported data type"

makeLensesForCons :: LensRules -> DataDecl -> Q [Dec]
makeLensesForCons cfg dataDecl = case constructors dataDecl of
  [NormalC dataConName [(    _,ty)]]
    | cfg^.handleSingletons  ->
      makeIsoLenses cfg dataDecl dataConName Nothing [ty]
  [RecC    dataConName [(fld,_,ty)]]
    | cfg^.handleSingletons  ->
      makeIsoLenses cfg dataDecl dataConName (Just fld) [ty]
  _ | cfg^.singletonRequired ->
      fail "makeLensesWith: A single-constructor single-argument data type is required"
    | otherwise              ->
      makeFieldLenses cfg dataDecl

makeDataDecl :: Dec -> Maybe DataDecl
makeDataDecl dec = case deNewtype dec of
  DataD ctx tyName args cons _ -> Just DataDecl
    { dataContext = ctx
    , tyConName = Just tyName
    , dataParameters = args
    , fullType = apps $ ConT tyName
    , constructors = cons
    }
  DataInstD ctx familyName args cons _ -> Just DataDecl
    { dataContext = ctx
    , tyConName = Nothing
    , dataParameters = map PlainTV vars
    , fullType = \tys -> apps (ConT familyName) $
        substType (Map.fromList $ zip vars tys) args
    , constructors = cons
    }
    where
      -- The list of "type parameters" to a data family instance is not
      -- explicitly specified in the source. Here we define it to be
      -- the set of distinct type variables that appear in the LHS. e.g.
      --
      -- data instance F a Int (Maybe (a, b)) = G
      --
      -- has 2 type parameters: a and b.
      vars = toList $ setOf typeVars args
  _ -> Nothing

-- | A data, newtype, data instance or newtype instance declaration.
data DataDecl = DataDecl
  { dataContext :: Cxt -- ^ Datatype context.
  , tyConName :: Maybe Name
    -- ^ Type constructor name, or Nothing for a data family instance.
  , dataParameters :: [TyVarBndr] -- ^ List of type parameters
  , fullType :: [Type] -> Type
    -- ^ Create a concrete record type given a substitution to
    -- 'detaParameters'.
  , constructors :: [Con] -- ^ Constructors
  -- , derivings :: [Name] -- currently not needed
  }

makeIsoLenses :: LensRules
              -> DataDecl
              -> Name
              -> Maybe Name
              -> [Type]
              -> Q [Dec]
makeIsoLenses cfg dataDecl dataConName maybeFieldName partTy = do
  let tyArgs = map plain (dataParameters dataDecl)
  m <- freshMap $ setOf typeVars tyArgs
  let aty = List.foldl' AppT (TupleT $ length partTy) partTy
      bty = substTypeVars m aty
      cty = fullType dataDecl $ map (VarT . view name) tyArgs
      dty = substTypeVars m cty
      quantified = ForallT (tyArgs ++ substTypeVars m tyArgs)
        (dataContext dataDecl ++ substTypeVars m (dataContext dataDecl))
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
    (ns, f) <- makeIsoFrom aty dataConName
    t <- makeIsoTo ns dataConName
    body <- makeBody isoName f t
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
      (ns, f) <- makeIsoFrom aty dataConName
      t <- makeIsoTo ns dataConName
      body <- makeBody lensName t f
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
    buildClause (con@RecC{}, fields) = do
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
          recon = conE conName `appsE1` cvals

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

    -- Non-record are never the target of a generated field lens body
    buildClause (con, _fields) = do
      let fieldCount = lengthOf conFields con
      vars <- replicateM fieldCount (newName "x")
      let conName = con^.name
          expr
            | isTraversal       = [| pure $(conE conName `appsE1` map varE vars) |] -- We must rebuild the value to support type changing
            | otherwise         = [| error errorMsg |]
            where errorMsg = show lensName ++ ": non-record constructors require traversals to be generated"

      -- clause:  _ c@Con{} = expr
      -- expr:    pure c
      clause [wildP, conP conName (map varP vars)] (normalB expr) []

makeFieldLenses :: LensRules
                -> DataDecl
                -> Q [Dec]
makeFieldLenses cfg dataDecl = do
  let tyArgs = map plain $ dataParameters dataDecl
      maybeLensClass = view lensClass cfg . nameBase =<< tyConName dataDecl
      maybeClassName = fmap (^._1.to mkName) maybeLensClass
      cons = constructors dataDecl
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
            | otherwise             = fullType dataDecl $ map (VarT . view name) tyArgs'
        bty = substTypeVars m aty
        dty = substTypeVars m cty

        s = setOf folded m
        relevantBndr b = s^.contains (b^.name)
        relevantCtx = not . Set.null . Set.intersection s . setOf typeVars
        tvs = tyArgs' ++ filter relevantBndr (substTypeVars m tyArgs')
        ctx = dataContext dataDecl
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
          appliedCon = fullType dataDecl <$> sequenceA varArgs
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
    TyConI decl -> do
      maybeDecs <- makeWrappedForDec decl
      maybe (fail "makeWrapped: Unsupported data type") return maybeDecs
    _ -> fail "makeWrapped: Expected the name of a newtype or datatype"

makeWrappedForDec :: Dec -> Q (Maybe [Dec])
makeWrappedForDec decl = case makeDataDecl decl of
  Just dataDecl | [con]   <- constructors dataDecl
                , [field] <- toListOf (conFields._2) con
    -> do wrapped   <- makeWrappedInstance dataDecl con field
          rewrapped <- makeRewrappedInstance dataDecl
          return (Just [rewrapped, wrapped])
  _ -> return Nothing

makeRewrappedInstance :: DataDecl -> DecQ
makeRewrappedInstance dataDecl = do

   t <- varT <$> newName "t"

   let typeArgs = map (view name) (dataParameters dataDecl)

   typeArgs' <- do
     m <- freshMap (Set.fromList typeArgs)
     return (substTypeVars m typeArgs)

       -- Con a b c...
   let appliedType  = return (fullType dataDecl (map VarT typeArgs))

       -- Con a' b' c'...
       appliedType' = return (fullType dataDecl (map VarT typeArgs'))

       -- Con a' b' c'... ~ t
       eq = equalP appliedType' t

       -- Rewrapped (Con a b c...) t
       klass = conT ''Rewrapped `appsT` [appliedType, t]

   -- instance (Con a' b' c'... ~ t) => Rewrapped (Con a b c...) t
   instanceD (cxt [eq]) klass []

makeWrappedInstance :: DataDecl-> Con -> Type -> DecQ
makeWrappedInstance dataDecl con fieldType = do

  let conName = view name con
  let typeArgs = toListOf typeVars (dataParameters dataDecl)

  -- Con a b c...
  let appliedType  = fullType dataDecl (map VarT typeArgs)

  -- type Unwrapped (Con a b c...) = $fieldType
  let unwrappedATF = tySynInstD' ''Unwrapped [return appliedType] (return fieldType)

  -- Wrapped (Con a b c...)
  let klass        = conT ''Wrapped `appT` return appliedType

  -- _Wrapped' = iso (\(Con x) -> x) Con
  let wrapFun      = conE conName
  let unwrapFun    = newName "x" >>= \x -> lam1E (conP conName [varP x]) (varE x)
  let isoMethod    = funD '_Wrapped' [clause [] (normalB [|iso $unwrapFun $wrapFun|]) []]

  -- instance Wrapped (Con a b c...) where
  --   type Unwrapped (Con a b c...) = fieldType
  --   _Wrapped' = iso (\(Con x) -> x) Con
  instanceD (cxt []) klass [unwrappedATF, isoMethod]

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
    { _getPrefix          :: [String] -> String -> Maybe String
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
    prefix _ ('_':xs) | '_' `List.elem` xs = Just (takeWhile (/= '_') xs)
    prefix _ _                             = Nothing
    rawLens     x = x ++ "_lens"
    niceLens    x = prefix [] x <&> \n -> drop (length n + 2) x
    classNaming x = niceLens x <&> ("Has_" ++)

-- | Field rules for fields in the form @ prefixFieldname or _prefixFieldname @
-- If you want all fields to be lensed, then there is no reason to use an @_@ before the prefix.
-- If any of the record fields leads with an @_@ then it is assume a field without an @_@ should not have a lens created.
camelCaseFields :: FieldRules
camelCaseFields = FieldRules prefix rawLens niceLens classNaming
  where
    sepUpper x = case break isUpper x of
        (p, s) | List.null p || List.null s -> Nothing
               | otherwise                  -> Just (p,s)

    prefix fields = fmap fst . sepUpper <=< dealWith_ fields

    rawLens     x = x ++ "Lens"
    niceLens    x = overHead toLower . snd <$> sepUpper x
    classNaming x = niceLens x <&> \ (n:ns) -> "Has" ++ toUpper n : ns

    dealWith_ :: [String] -> String -> Maybe String
    dealWith_ fields field | not $ any (fst . leading_) fields = Just field
                           | otherwise = if leading then Just trailing else Nothing
      where
        leading_ ('_':xs) = (True, xs)
        leading_      xs  = (False, xs)
        (leading, trailing) = leading_ field



collectRecords :: [Con] -> [VarStrictType]
collectRecords cons = rs
  where
    recs = filter (\r -> case r of RecC{} -> True; _ -> False) cons
    rs' = List.concatMap (\(RecC _ _rs) -> _rs) recs
    rs = nubBy ((==) `on` (^._1)) rs'

verboseLenses :: FieldRules -> Dec -> Q [Dec]
verboseLenses c decl = do
  cons <- case deNewtype decl of
    DataD _ _ _ cons _ -> return cons
    DataInstD _ _ _ cons _ -> return cons
    _ -> fail "verboseLenses: Unsupported data type"
  let rs = collectRecords cons
  if List.null rs
    then fail "verboseLenses: Expected the name of a record type"
    else flip makeLenses' decl
            $ mkFields c rs
            & map (\(Field n _ l _ _) -> (show n, show l))
  where
    makeLenses' fields' =
        makeLensesForDec $ lensRules
            & lensField .~ (`Prelude.lookup` fields')
            & buildTraversals .~ False
            & partialLenses .~ True

mkFields :: FieldRules -> [VarStrictType] -> [Field]
mkFields (FieldRules prefix' raw' nice' clas') rs
    = Maybe.mapMaybe namer fields
    & List.groupBy (on (==) _fieldLensPrefix)
    & (\ gs -> case gs of 
        x:_ -> x
        _   -> [])
  where
    fields = map (nameBase . fst3) rs
    fst3 (x,_,_) = x
    namer field = do
        let rawlens = mkName (raw' field)
        prefix <- prefix' fields field
        nice   <- mkName <$> nice' field
        clas   <- mkName <$> clas' field
        return (Field (mkName field) prefix rawlens clas nice)

hasClassAndInstance :: FieldRules -> Dec -> Q [Dec]
hasClassAndInstance cfg decl = do
    c <- newName "c"
    e <- newName "e"
    dataDecl <- case makeDataDecl decl of
        Just dataDecl -> return dataDecl
        _ -> fail "hasClassAndInstance: Unsupported data type"
    let rs = collectRecords $ constructors dataDecl
    when (List.null rs) $
      fail "hasClassAndInstance: Expected the name of a record type"
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
            (return $ ConT className `apps`
              [fullType dataDecl $ map (VarT . view name) (dataParameters dataDecl)
              , fieldType])
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
makeFieldsWith c n = do
  inf <- reify n
  case inf of
    TyConI decl -> makeFieldsForDec c decl
    _ -> fail "makeFieldsWith: Expected the name of a data type or newtype"

makeFieldsForDec :: FieldRules -> Dec -> Q [Dec]
makeFieldsForDec cfg decl = liftA2 (++)
  (verboseLenses cfg decl)
  (hasClassAndInstance cfg decl)

-- | Generate lenses which can be inferred.
--
-- If the same type is used twice in a constructor, then it will produce
-- duplicate instances, which can not be compiled.
--
-- For example: data Foo = Foo { _bar :: String, _baz :: Int }
--
-- instance Functor f => IsLens Foo String f where
--   (???) = bar
-- 
-- instance Functor f => IsLens Foo Int f where
--   (???) = baz
makeInferableLenses :: Name -> Q [Dec]
makeInferableLenses nm = do
  ls <- makeLenses nm
  r <- reify nm
  case r of
    (TyConI (DataD _ n _ cs _)) -> do
      ts <- tysOf cs
      inst <- forM ts $ \t -> do
        f <- newName "f"
        instanceD (cxt [classP ''Functor [varT f]])
                  (appT (appT (appT (conT ''IsInferable) (conT n)) (return t)) (varT f))
                  [fromN t ls]
      return (ls ++ inst)
    _ -> fail "only inferable on constructors"

  where
    tysOf :: [Con] -> Q [Type]
    tysOf [NormalC _ xs] = return $ map snd xs
    tysOf [RecC _ xs] = return $ map (\(_, _, x) -> x) xs
    tysOf _ = fail "only supports types with a single constructor"
    
    fromN :: Type -> [Dec] -> Q Dec
    fromN a ls = case d of
        Just (SigD n _) -> funD '(???) [clause [] (normalB (varE n)) []]
        _ -> fail "makeLenses' derived something I can't use"
      where
        d = flip List.find ls $ \x -> case x of
          (SigD _ (ForallT [] [] t)) -> farRight t == a
          _ -> False
    
    farRight :: Type -> Type
    farRight (AppT _ r) = farRight r
    farRight t = t

-- Helper class for makeInferableLenses
class IsInferable a b f | a b -> f where
  (???) :: Functor f => (b -> f b) -> a -> f a

-- | Generate overloaded field accessors.
--
-- /e.g/
--
-- @
-- data Foo a = Foo { _fooX :: 'Int', _fooY : a }
-- newtype Bar = Bar { _barX :: 'Char' }
-- makeFields ''Foo
-- makeFields ''Bar
-- @
--
-- will create
--
-- @
-- _fooXLens :: Lens' (Foo a) Int
-- _fooYLens :: Lens (Foo a) (Foo b) a b
-- class HasX s a | s -> a where
--   x :: Lens' s a
-- instance HasX (Foo a) Int where
--   x = _fooXLens
-- class HasY s a | s -> a where
--   y :: Lens' s a
-- instance HasY (Foo a) a where
--   y = _fooYLens
-- _barXLens :: Iso' Bar Char
-- instance HasX Bar Char where
--   x = _barXLens
-- @
--
-- @
-- makeFields = 'makeFieldsWith' 'defaultFieldRules'
-- @
makeFields :: Name -> Q [Dec]
makeFields = makeFieldsWith defaultFieldRules

-- | @ defaultFieldRules = 'camelCaseFields' @
defaultFieldRules :: FieldRules
defaultFieldRules = camelCaseFields

-- Declaretion quote stuff

declareWith :: (Dec -> Declare Dec) -> Q [Dec] -> Q [Dec]
declareWith fun = (runDeclare . traverseDataAndNewtype fun =<<)

-- | Monad for emitting top-level declarations as a side effect.
type Declare = WriterT (Endo [Dec]) Q

runDeclare :: Declare [Dec] -> Q [Dec]
runDeclare dec = do
  (out, endo) <- runWriterT dec
  return $ out ++ appEndo endo []

emit :: [Dec] -> Declare ()
emit decs = tell $ Endo (decs++)

-- | Traverse each data, newtype, data instance or newtype instance
-- declaration.
traverseDataAndNewtype :: (Applicative f) => (Dec -> f Dec) -> [Dec] -> f [Dec]
traverseDataAndNewtype f decs = traverse go decs
  where
    go dec = case dec of
      DataD{} -> f dec
      NewtypeD{} -> f dec
      DataInstD{} -> f dec
      NewtypeInstD{} -> f dec

      -- Recurse into instance declarations because they main contain
      -- associated data family instances.
      InstanceD ctx inst body -> InstanceD ctx inst <$> traverse go body

      _ -> pure dec

stripFields :: Dec -> Dec
stripFields dec = case dec of
  DataD ctx tyName tyArgs cons derivings ->
    DataD ctx tyName tyArgs (map deRecord cons) derivings
  NewtypeD ctx tyName tyArgs con derivings ->
    NewtypeD ctx tyName tyArgs (deRecord con) derivings
  DataInstD ctx tyName tyArgs cons derivings ->
    DataInstD ctx tyName tyArgs (map deRecord cons) derivings
  NewtypeInstD ctx tyName tyArgs con derivings ->
    NewtypeInstD ctx tyName tyArgs (deRecord con) derivings
  _ -> dec

deRecord :: Con -> Con
deRecord con@NormalC{} = con
deRecord con@InfixC{} = con
deRecord (ForallC tyVars ctx con) = ForallC tyVars ctx $ deRecord con
deRecord (RecC conName fields) = NormalC conName (map dropFieldName fields)
  where dropFieldName (_, str, typ) = (str, typ)
