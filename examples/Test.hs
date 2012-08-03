{-# LANGUAGE TemplateHaskell #-}
module Test where

import Control.Lens
import Control.Lens.TH

data Foo a = Foo a
makeLenses ''Foo

data Bar a b c = Bar { _baz :: (a, b) }
makeLenses ''Bar

data Quux a b = Quux { _quaffle :: Int, _quartz :: Double }
makeLenses ''Quux

data Quark a = Qualified  { _gaffer :: a }
             | Unqualified { _gaffer :: a, blockingGaffer :: a }
makeLenses ''Quark

data LensCrafted a = Still { _still :: a }
                   | Works { _still :: a }
makeLenses ''LensCrafted

data Mono = Mono { _monoFoo :: Int, _monoBar :: Int }
makeLenses ''Mono

--data Nucleosis = Nucleosis { _nuclear :: Mono }
--makeLenses ''Nucleosis
