module Main where


import Text.Parsec
import Data.Char
import Control.Applicative ((<*))
import Control.Monad (guard)
import Control.Lens
import Data.Maybe
import Data.Traversable
import Data.Foldable
import Data.List (isInfixOf, dropWhileEnd)
import System.Environment
import Language.Haskell.Exts
import System.Process
import Prelude hiding (concatMap, mapM, mapM_, concat, elem, notElem)
import Debug.Trace

imports =
  [ "Control.Applicative"
  , "Control.Applicative.Backwards (Backwards)"
  , "Control.Exception"
  , "Data.Typeable (Typeable)"
  , "Data.Dynamic (Dynamic)"
  , "Data.Dynamic.Lens"
  , "Data.Hashable (Hashable)"
  , "Data.HashSet (HashSet)"
  , "Data.Typeable.Lens"
  , "Control.Lens"
  , "Control.Lens.Internal"
  , "Control.Monad.RWS"
  , "Control.Monad.Reader"
  , "Control.Monad.State"
  , "Control.Monad.Trans.Error (Error,ErrorT)"
  , "Control.Monad.Writer"
  , "Data.Foldable (Foldable)"
  , "Data.ByteString.Lens"
  , "Data.Sequence (Seq)"
  , "Data.Vector (Vector)"
  , "Data.Void (Void)"
  , "Data.Word (Word8)"
  , "Data.Bits (Bits)"
  , "Data.Bits.Lens"
  , "Data.ByteString (ByteString)"
  , "Data.Set (Set)"
  , "Data.IntSet (IntSet)"
  , "Data.List.Split"
  , "Data.List.Split.Lens"
  , "Data.Set.Lens"
  , "qualified Control.Lens.Cons"
  , "qualified Control.Lens.Fold"
  , "qualified Control.Lens.Getter"
  , "qualified Control.Lens.Internal.Indexed"
  , "qualified Control.Lens.Iso"
  , "qualified Control.Lens.Prism"
  , "qualified Control.Lens.Setter"
  , "qualified Control.Lens.Traversal"
  , "qualified Control.Lens.Tuple"
  , "qualified Data.ByteString as StrictB"
  , "qualified Data.ByteString.Lazy as LazyB"
  , "qualified Data.ByteString.Lazy"
  , "qualified Data.Complex"
  , "Data.Complex (Complex)"
  , "Data.Array (Array, Ix)"
  , "Data.Map (Map)"
  , "Data.Functor.Identity (Identity)"
  , "qualified Data.Vector as Vector"
  , "qualified Data.Vector.Unboxed as Unboxed"
  , "qualified Data.Vector.Storable as Storable"
  , "Data.Vector.Primitive (Prim)"
  , "qualified Data.Vector.Primitive as Prim"
  , "Data.HashMap.Lazy (HashMap)"
  , "Data.Tree (Tree)"
  , "Foreign.Storable (Storable)"
  , "Data.Int (Int64)"
  , "qualified Data.Complex.Lens"
  , "qualified Data.List.Lens"
  , "qualified Data.Monoid"
  , "qualified Data.Text as StrictT"
  , "qualified Data.Text.Lazy as LazyT"
  , "qualified Data.Text"
  , "qualified Data.Text.Internal"
  , "qualified Data.Traversable"
  , "qualified Numeric.Natural"
  , "Data.Vector.Unboxed (Unbox)"
  ]

usedExtensions =
  [ "Rank2Types"
  , "FlexibleContexts"
  ]

valueBlacklist =
  [ "nat"
  , "fresh"
  , "singular"
  , "unsafeSingular"
  , "dropping"
  , "droppingWhile"
  , "idroppingWhile"
  , "taking"
  , "takingWhile"
  , "itakingWhile"
  ]

fileBlacklist =
  [ "src/Control/Lens/TH.hs"
  ]

myParserMode = defaultParseMode
  { extensions = glasgowExts
  , fixities = Just
             $ preludeFixities
            ++ [Fixity AssocRight 9 (UnQual (Symbol "#."))
               ,Fixity AssocLeft 8 (UnQual (Symbol ".#"))
               ,Fixity AssocLeft 4 (UnQual (Symbol "<$"))
               ,Fixity AssocLeft 4 (UnQual (Symbol "<$>"))
               ,Fixity AssocLeft 4 (UnQual (Symbol "<*>"))
               ]
  }

processCpp = readProcess "cpp" ["-P","-include","dist/build/autogen/cabal_macros.h","-Iincludes","-DHLINT"]

main :: IO ()
main = do
  fns <- getArgs
  let fns' = filter (`notElem` fileBlacklist) fns
  ms <- for fns' $ \fn -> do
    txt <- readFile fn
    nocpp <- processCpp txt
    case parseModuleWithComments myParserMode {parseFilename = fn} nocpp of
      ParseFailed srcloc err -> fail (fn ++ ": " ++ show srcloc ++ ": " ++ err)
      ParseOk (_m,comments) -> return comments

  putStr $ unlines $
          map (\x -> "{-# LANGUAGE " ++ x ++ " #-}") usedExtensions
       ++ map ("import " ++) imports
       ++ iconcatMap (\i -> render i . asType) (concat ms)

asType (Comment _ _ str)
  | "::" `isInfixOf` str =
    case cleanQuotes True (filterAts str) of
      Nothing -> error ("!"++str)
      Just clean -> case parseExp clean of
            ParseFailed _ err -> Left (str ++ " : " ++ err)
            ParseOk     (ExpTypeSig _ e t) -> Right (e,t)
            ParseOk     _ -> Left str
  | otherwise = Left str

render _ (Left s) = []
render i (Right (l,r)) | prettyPrint l `elem` valueBlacklist = []
render i (Right (l,r)) =
  [ "check_" ++ show i ++ " :: " ++ prettyPrint r
  , "check_" ++ show i ++ args ++ " = " ++ prettyPrint (Paren l) ++ args
  ]
  where
  arity = typeArity r
  args = concatMap (\x -> [' ',x]) (take arity ['a'..'z'])

typeArity (TyForall _ _ x) = typeArity x
typeArity (TyFun _ x) = 1 + typeArity x
typeArity _ = 0

cleanQuotes True  ('\'':xs) = quotedPart xs
cleanQuotes False ('\'':xs) = fmap ('\'':) (cleanQuotes False xs)
cleanQuotes _ (x:xs) = fmap (x:) (cleanQuotes (isEligible x) xs)
cleanQuotes _ [] = Just []

isEligible '(' = True
isEligible '[' = True
isEligible ' ' = True
isEligible _   = False

quotedPart ('\'':'\'':xs) = fmap ('\'':) (cleanQuotes False xs)
quotedPart ('\'':xs) = cleanQuotes False xs
quotedPart (x:xs)
  | isEndEligible x = Nothing
  | otherwise = fmap (x:) (quotedPart xs)
quotedPart [] = Nothing

isEndEligible ')' = True
isEndEligible ']' = True
isEndEligible ' ' = True
isEndEligible _   = False

filterAts = dropWhileEnd (=='@') . dropWhileEnd isSpace . dropWhile (=='@') . dropWhile isSpace . dropWhile (=='|') . dropWhile isSpace
