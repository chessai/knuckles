module B where

data Foo = Foo
  deriving (Eq, Show, Ord)

instance Semigroup Foo where
  _ <> _ = Foo

data Bar = Bar
  deriving (Eq, Show)
