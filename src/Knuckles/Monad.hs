{-# language
        BangPatterns
      , DeriveAnyClass
      , DerivingStrategies
      , FlexibleContexts
      , GeneralizedNewtypeDeriving
      , InstanceSigs
      , LambdaCase
      , NoMonomorphismRestriction
      , OverloadedStrings
      , RankNTypes
      , ScopedTypeVariables
      , StandaloneDeriving
      , TypeApplications
      , ViewPatterns
      , UndecidableInstances
  #-}

module Knuckles.Monad
  ( Knuckles(..)
  , KnucklesError(..)
  , Env(..)
  , withEnv
  , runKnuckles
  , runKnuckles'
  ) where

import Control.Exception hiding (fromException)
import Control.Monad.Except
import Control.Monad.Reader
import DynFlags
import Exception (ExceptionMonad(..))
import GHC (runGhc)
import GHC.Paths (libdir)
import GhcMonad
import Outputable
import qualified Distribution.Verbosity as Cabal

newtype Knuckles a = Knuckles
  { getKnuckles :: ReaderT Env (ExceptT KnucklesError Ghc) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving newtype (MonadReader Env)
  deriving newtype (MonadError KnucklesError)
  deriving newtype (HasDynFlags)

instance GhcMonad Knuckles where
  getSession = liftGhc getSession
  setSession = liftGhc . setSession

instance ExceptionMonad Knuckles where
  gcatch :: Exception e
    => Knuckles a
    -> (e -> Knuckles a)
    -> Knuckles a
  gcatch act hndl = Knuckles
    $ ReaderT
    $ \env -> ExceptT
    $ gcatch (runKnuckles env act)
    $ \e -> runKnuckles env (hndl e)

  gmask :: forall a b. ()
    => ((Knuckles a -> Knuckles a) -> Knuckles b)
    -> Knuckles b
  gmask f = Knuckles
    $ ReaderT
    $ \env -> ExceptT
    $ Ghc
    $ \s -> gmask
    $ \io_restore ->
        let g_restore :: Ghc a -> Ghc a
            g_restore (Ghc m) = Ghc $ \s' -> io_restore (m s')
            k_restore :: Knuckles a -> Knuckles a
            k_restore = liftGhc' g_restore
        in unGhc (runKnuckles env (f k_restore)) s

liftGhc :: Ghc a -> Knuckles a
liftGhc ghc = Knuckles $ ReaderT $ const (ExceptT (fmap Right ghc))

liftGhc' :: (Ghc a -> Ghc a) -> (Knuckles a -> Knuckles a)
liftGhc' ghcf k = Knuckles
  $ ReaderT
  $ \env -> ExceptT $ runKnuckles env k >>= \case
      Left e -> pure (Left e)
      Right a -> Right <$> ghcf (pure a)

runKnuckles :: Env -> Knuckles a -> Ghc (Either KnucklesError a)
runKnuckles env = runExceptT . flip runReaderT env . getKnuckles

runKnuckles' :: Env -> Knuckles a -> IO (Either KnucklesError a)
runKnuckles' env = runGhc (Just libdir) . runKnuckles env

data Env = Env
  !(Cabal.Verbosity) -- ^ cabal verbosity
  FilePath -- ^ cabal file (tld </> self)
  FilePath -- ^ top-level dir
  FilePath -- ^ package environment file

withEnv :: MonadReader r m => (r -> m a) -> m a
withEnv f = ask >>= f

data KnucklesError
  = NoLibraryComponent
  | NotPassedACabalFile
  | FilePassedDoesNotExist
  | CabalThrewAnError IOException
  | MoreThanOneHsSourceDir
  | KnucklesErrorSome SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

instance Outputable KnucklesError where
  ppr k = ppr (show k)
