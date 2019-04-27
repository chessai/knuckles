{-# language DeriveGeneric #-}

module D where

import GHC.Generics

data Foo a = Foo
  deriving (Eq, Show, Ord)

data Bar a = Bar
  deriving (Eq, Show, Generic)
