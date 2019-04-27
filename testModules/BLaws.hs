module Laws where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Classes
import Hedgehog.Generic
import B.hs

main :: IO ()
main = lawsCheckMany allLaws

allLaws :: [(String,[Laws])]
allLaws = mconcat
  [ _BarLaws genBar
  , _FooLaws genFoo
  ]

genBar :: Gen (Bar)
genBar = error "genBar: not implemented"

_BarLaws :: Gen (Bar) -> [(String, [Laws])]
_BarLaws gen = [("Bar", tyLaws')]
  where
    tyLaws' =
      [ showLaws gen
      , eqLaws gen
      ]

genFoo :: Gen (Foo)
genFoo = error "genFoo: not implemented"

_FooLaws :: Gen (Foo) -> [(String, [Laws])]
_FooLaws gen = [("Foo", tyLaws')]
  where
    tyLaws' =
      [ showLaws gen
      , eqLaws gen
      , semigroupLaws gen
      ]


