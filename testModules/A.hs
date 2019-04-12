module A where

data Foo = Foo
  deriving (Eq,Show,Ord)

instance Semigroup Foo where
  _ <> _ = Foo

instance Monoid Foo where
  mempty = Foo

data Bar = Bar
  deriving (Eq,Show,Ord)
