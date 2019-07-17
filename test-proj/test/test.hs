module Main (
        main
    ) where
import Hedgehog
import Hedgehog.Classes
import Hedgehog.Generic
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Foo
type Laws' = [(String, [Laws])]
main :: IO Bool
main = lawsCheckMany allLaws
allLaws :: Laws'
allLaws = mconcat [_FooLaws, _BLaws]
_GenFoo3 :: Gen x0 -> Gen x1 -> Gen (Foo )
_GenFoo3 = error "Knuckles: Unable to supply generator."
_GenFoo2 :: Gen x0 -> Gen (Foo Integerx0)
_GenFoo2 = error "Knuckles: Unable to supply generator."
_GenFoo1 :: Gen (Foo Integer Integer)
_GenFoo1 = error "Knuckles: Unable to supply generator."
_GenB1 :: Gen (B Integer Integer)
_GenB1 = error "Knuckles: Unable to supply generator."
_GenB2 :: Gen x0 -> Gen (B Integerx0)
_GenB2 = error "Knuckles: Unable to supply generator."
_FooLaws :: Laws'
_FooLaws
  = pure $ ("Foo", tyLaws)
  where
      tyLaws
        = [bifunctorLaws _GenFoo3, functorLaws _GenFoo2, eqLaws _GenFoo1,
           showLaws _GenFoo1]
_BLaws :: Laws'
_BLaws
  = pure $ ("B", tyLaws)
  where
      tyLaws = [eqLaws _GenB1, showLaws _GenB1, functorLaws _GenB2]