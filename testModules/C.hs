{-# language DeriveGeneric #-}

module C where

import GHC.Generics

data Foo = Foo
  deriving (Eq, Show, Ord)

data Bar = Bar
  deriving (Eq, Show, Generic)
