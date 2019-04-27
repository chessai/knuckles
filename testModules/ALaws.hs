module Laws where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Classes
import Hedgehog.Generic
import A.hs

main :: IO ()
main = lawsCheckMany allLaws

allLaws :: [(String,[Laws])]
allLaws = mconcat
  [ _BarLaws genBar
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
