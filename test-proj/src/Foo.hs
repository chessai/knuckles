{-# language DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Foo
  ( Foo(..)
  ) where

import Data.Bifunctor

data Foo x y = Foo
  deriving (Ord, Show)

instance Eq (Foo x y) where
  _ == _ = True

instance Functor (Foo x) where
  fmap _ Foo = Foo

instance Bifunctor Foo where
  bimap _ _ Foo = Foo

newtype B m a = B (m a)
  deriving newtype (Eq, Show)
  deriving newtype (Functor,Applicative,Monad)
