module Laws where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Classes
import A.hs

_BarLaws :: Gen Bar -> [(String,[Laws])]
_BarLaws gen = [("Bar", tyLaws')]
  where
    tyLaws' =
      [ showLaws gen
      , eqLaws gen
      ]

_FooLaws :: Gen Foo -> [(String,[Laws])]
_FooLaws gen = [("Foo", tyLaws')]
  where
    tyLaws' =
      [ showLaws gen
      , eqLaws gen
      , semigroupLaws gen
      , monoidLaws gen
      ]
