module Laws where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Classes
import Hedgehog.Generic
import D.hs

main :: IO ()
main = lawsCheckMany allLaws

allLaws :: [(String,[Laws])]
allLaws = mconcat
  [ _BarLaws genBar
  , _FooLaws genFoo
  ]

genBar :: Gen (Bar
                                                                                                                                                         a)
genBar = hgen

_BarLaws :: Gen (Bar
                                                                                                                                                                                              a) -> [(String, [Laws])]
_BarLaws gen = [("Bar
                                                                                                                                                                                                                                           a", tyLaws')]
  where
    tyLaws' =
      [ genericLaws gen
      , showLaws gen
      , eqLaws gen
      ]

genFoo :: Gen (Foo
                                                                                                                                                                                                                                                                                                                                                                         a)
genFoo = error "genFoo: not implemented"

_FooLaws :: Gen (Foo
                                                                                                                                                                                                                                                                                                                                                                                                                                         a) -> [(String, [Laws])]
_FooLaws gen = [("Foo
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      a", tyLaws')]
  where
    tyLaws' =
      [ showLaws gen
      , eqLaws gen
      ]


