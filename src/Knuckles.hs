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
      , RecordWildCards
      , ScopedTypeVariables
      , StandaloneDeriving
      , TypeApplications
      , ViewPatterns
      , UndecidableInstances
  #-}

module Knuckles where

import Data.Foldable (fold)
import Control.Exception hiding (fromException)
import Control.Monad.Except
import Control.Monad.Reader
import CoreSyn (isOrphan)
import Data.Bifunctor
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Monoid
import Digraph
import DynFlags
import Exception (ExceptionMonad(..))
import GHC
import GHC.LanguageExtensions.Type
import GHC.Paths (libdir)
import GHC.SourceGen
import GhcMake
import GhcMonad
import HscMain (batchMsg)
import HscTypes
import InstEnv
import Knuckles.Monad
import Module
import Name
import Options.Applicative
import DriverPhases (isHaskellishTarget)
import Outputable hiding ((<>),text)
import System.Environment (getArgs)
import System.FilePath
import TyCon
import Type
import qualified Data.Map.Strict as M
import qualified Distribution.Compat.Exception as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Types.BuildInfo as Cabal
import qualified Distribution.Types.CondTree as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal
import qualified Distribution.Types.Library as Cabal
import qualified Distribution.Verbosity as Cabal
import qualified Outputable as O
import qualified System.Directory as Dir

constModule :: HsModule'
constModule = module'
  (Just "Const")
  (Just [var "const", var "id"])
  [
  ]
  [ typeSig "const" $ a --> b --> a
  , funBind "const" $ matchRhs [wildP, x] x

  , typeSig "id" $ a --> a
  , funBind "id" $ matchRhs [x] x
  ]
  where
    a = var "a"
    b = var "b"
    x = var "x"

envInfo :: ParserInfo Env
envInfo = info ((Env
  <$> ( pure Cabal.normal
      )
  <*> ( strOption $ fold [ long "cabal", help "cabal file" ]
      )
  <*> ( strOption $ fold [ long "tld", help "top level directory of cabal project" ]
      )
  <*> ( strOption $ fold [ long "env", help "ghc environment file" ]
      )) <**> helper) mempty

main :: IO ()
main = do
  env <- execParser envInfo
  (putStrLn . showSDocUnsafe . ppr) =<< runKnuckles'
    env
    (withEnv $ \(Env _ proj tld genv) -> do {
        dflags <- getSessionDynFlags
      ; void $ setSessionDynFlags $ dflags { packageEnv = Just genv }
      ; liftIO $ Dir.setCurrentDirectory tld
      ; (hsSrcDir, module_names) <- getModules proj
      ; liftIO $ Dir.setCurrentDirectory hsSrcDir
      ; loadProj module_names
      ; module_graph <- getModuleGraph
      ; tcs <- mapM typecheck (mgModSummaries module_graph)
      ; let cls_insts = concatMap gatherClsInsts tcs
      -- ; pure $ map clsInstOccName cls_insts
      ; let inst_heads = map instanceHead cls_insts
      ; let inst_sigs = map instanceSig cls_insts
      ; pure $ fst $ (inst_heads, inst_sigs)
      -- ; pure $ (map ClsInstPpr cls_insts, map instanceHead cls_insts)
    })

isCabal :: MonadError KnucklesError m
  => FilePath -> m ()
isCabal fp = unless (isExtensionOf ".cabal" fp)
  $ throwError NotPassedACabalFile

doesCabalExist :: (MonadIO m, MonadError KnucklesError m)
  => FilePath -> m ()
doesCabalExist fp = do
  h <- liftIO $ Dir.doesFileExist fp
  unless h $ throwError FilePassedDoesNotExist

type HsSrcDir = FilePath

getModules :: (MonadIO m, MonadError KnucklesError m, MonadReader Env m)
  => FilePath -> m (HsSrcDir, [ModuleName])
getModules fp = withEnv $ \(Env v _ _ _) -> do
  pkgDescr <- liftIO $ Cabal.catchIO
    (Right <$> Cabal.readGenericPackageDescription v fp)
    (pure . Left)
  case pkgDescr of
    Left e -> throwError (CabalThrewAnError e)
    Right g -> maybeToError (Cabal.condLibrary g) NoLibraryComponent
      $ \condTreeLib -> do
          let hsSrcDirs = id
                . Cabal.hsSourceDirs
                . Cabal.libBuildInfo
                . Cabal.condTreeData
                $ condTreeLib
          when (length hsSrcDirs /= 1)
            $ throwError MoreThanOneHsSourceDir
          let modules = id
                . fmap mkModuleName
                . fmap Cabal.toFilePath
                . Cabal.explicitLibModules
                . Cabal.condTreeData
                $ condTreeLib
          pure (head hsSrcDirs, modules)

maybeToError :: MonadError e m => Maybe a -> e -> (a -> m b) -> m b
maybeToError Nothing e _ = throwError e
maybeToError (Just a) _ f = f a

data Supported
  = Show
  | Eq
  | Semigroup
  | Monoid
  | Generic
  deriving (Eq,Show,Enum,Bounded)

instance HasOccName Supported where
  occName x = mkTcOcc $ case x of
    Show -> "Show"
    Eq -> "Eq"
    Semigroup -> "Semigroup"
    Monoid -> "Monoid"
    Generic -> "Generic"

{-
supportedToLaws :: Supported -> SDoc
supportedToLaws = \case
  Show -> "showLaws"
  Eq -> "eqLaws"
  Semigroup -> "semigroupLaws"
  Monoid -> "monoidLaws"
  Generic -> "genericLaws"

supportedOccNames :: [OccName]
supportedOccNames = map occName ([minBound .. maxBound] :: [Supported])

isSupported :: OccName -> Bool
isSupported o = any (== o) supportedOccNames

occNameToSupported :: OccName -> Maybe Supported
occNameToSupported o = if isSupported o
  then case occNameString o of
    "Show" -> Just Show
    "Eq" -> Just Eq
    "Semigroup" -> Just Semigroup
    "Monoid" -> Just Monoid
    "Generic" -> Just Generic
    _ -> Nothing
  else Nothing

build :: ()
  => String
  -> [HClass]
  -> [SDoc]
build targetMod hs = concat
  [ [buildHeader targetMod]
  , [buildBody hs]
  , ["\n"]
  ]

prebuild0 :: [HClass] -> [TyLaws]
prebuild0 = htoTyLaws . hfromList

prebuild :: [HClass] -> [Datum]
prebuild = map genDatum . prebuild0

buildBody :: [HClass] -> SDoc
buildBody = body_ . prebuild

buildHeader :: ()
  => String -- ^ target module
  -> SDoc
buildHeader targetMod = hcat
  [ "module Laws where\n\n"
  , "import Hedgehog\n"
  , "import qualified Hedgehog.Gen as Gen\n"
  , "import qualified Hedgehog.Range as Range\n"
  , "import Hedgehog.Classes\n"
  , "import Hedgehog.Generic\n"
  , hcat ["import ", O.text targetMod, "\n\n"]
  ]

data TyLaws = TyLaws
  { _tyLawsType :: Type
  , _tyLawsClsInsts :: [Supported]
  }

isValid :: TyLaws -> Bool
isValid (TyLaws _ os) = go . foldMap (bimap (All . (==Show)) (All . (==Eq)) . diag) $ os
  where
    diag x = (x,x)
    go (All x,All y) = x && y

isGeneric :: TyLaws -> Bool
isGeneric (TyLaws _ os) = any (== Generic) os

body_ :: [Datum] -> SDoc
body_ d = hcat
  [ main_
  , "\n"
  , allLaws d
  , "\n"
  , allCode d
  , "\n"
  ]

main_ :: SDoc
main_ = hcat
  [ "main :: IO ()\n"
  , "main = lawsCheckMany allLaws\n"
  ]

allLaws :: [Datum] -> SDoc
allLaws d = hcat
  [ "allLaws :: [(String,[Laws])]\n"
  , "allLaws = mconcat\n"
  , "  [ "
  , allLaws'
  , "  ]\n"
  ]
  where
    allLaws' = hcat
      $ punctuate "  , "
      $ map (\(Datum _ ident gen) -> hcat [ident, " ", gen, "\n"]) d

allCode :: [Datum] -> SDoc
allCode d = hcat
  $ punctuate "\n"
  $ map (\(Datum c _ _) -> c) d

genDatum :: TyLaws -> Datum
genDatum tyl@(TyLaws t os) =
  let ty = ppr t
      tyCon = ppr (tyConName (tyConAppTyCon t))
      genTy = hcat ["gen",tyCon]
      genV = if isGeneric tyl
        then "hgen"
        else hcat ["error \"", genTy, ": not implemented\""]
      tyLaws = hcat ["_", tyCon, "Laws"]
      tyLaws' = hcat
        $ punctuate "      , "
        $ map (\o -> supportedToLaws o O.<> " gen\n") os
      codeGen = hcat
        [ genTy, " :: Gen (", ty, ")\n"
        , genTy, " = ", genV, "\n"
        ]
      codeLaw = hcat
        [ tyLaws, " :: Gen (", ty, ") -> [(String, [Laws])]\n"
        , tyLaws, " gen = [(\"", ty, "\", tyLaws')]\n"
        , "  where\n"
        , "    tyLaws' =\n"
        , "      [ "
        , tyLaws'
        , "      ]\n"
        ]
      code = hcat [codeGen,"\n",codeLaw]
  in Datum code tyLaws genTy

data Datum = Datum
  { _datumCode :: SDoc
  , _datumIdentifier :: SDoc
  , _datumGenerator :: SDoc
  }

newtype UnsafeType = UnsafeType Type
  deriving (Outputable)

instance Eq UnsafeType where
  x == y = showSDocUnsafe (ppr x) == showSDocUnsafe (ppr y)

instance Ord UnsafeType where
  compare x y = compare (showSDocUnsafe (ppr x)) (showSDocUnsafe (ppr y))

newtype HMap = HMap { _getHMap :: Map UnsafeType [OccName] }

instance Semigroup HMap where
  HMap x <> HMap y = HMap (M.unionWith (<>) x y)

instance Monoid HMap where
  mempty = HMap mempty

singleton :: UnsafeType -> [OccName] -> HMap
singleton ty os = HMap (M.singleton ty os)

hsingleton :: HClass -> HMap
hsingleton (HClass o tys) = foldMap (\ty -> singleton (UnsafeType ty) [o]) tys

hfromList :: [HClass] -> HMap
hfromList = foldMap hsingleton

htoTyLaws :: HMap -> [TyLaws]
htoTyLaws (HMap m) = M.foldMapWithKey toTyLaws m

toTyLaws :: UnsafeType -> [OccName] -> [TyLaws]
toTyLaws (UnsafeType ty) os =
  let t = TyLaws ty (mapMaybe occNameToSupported os)
  in if isValid t
    then [t]
    else []

data HClass = HClass
  { _hclassOccName :: OccName
  , _hclassTypes :: [Type]
  }

instance Outputable HClass where
  ppr (HClass x y) = ppr (x,y)

generate :: ()
  => String -- ^ module name without ".hs"
  -> FilePath -- ^ output file, without ".hs"
  -> IO ()
generate targetMod outFile = do
  hclasses <- getHClasses targetMod
  p <- mapM safeShowSDoc (build targetMod hclasses)
  writeFile (outFile <> ".hs") (mconcat p)

getHClasses :: String -> IO [HClass]
getHClasses targetMod = simpleLoad targetMod $ do
  t <- typecheck targetMod
  pure $ gatherHClasses t

safeShowSDoc :: SDoc -> IO String
safeShowSDoc doc = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  pure $ showSDoc dflags doc

{-
ghcDynamic :: ()
  => String -- ^ module to compile
  -> [String] -- ^ modules to load
  -> Ghc a -- ^ ghc computation to run
  -> IO a -- ^ final result
ghcDynamic targetMod modules ghc = defaultGhc $ do
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags $ dflags
    { hscTarget = HscInterpreted, ghcLink = LinkInMemory }
  setTargets =<< sequence [ guessTarget (targetMod <> ".hs") Nothing ]
  _ <- load LoadAllTargets
  setContext
    $ map (IIDecl . simpleImportDecl . mkModuleName)
    $ targetMod : modules
  ghc
-}

defaultGhc :: Ghc a -> IO a
defaultGhc ghc = defaultErrorHandler
  defaultFatalMessager
  defaultFlushOut
  (runGhc (Just libdir) ghc)

simpleLoad :: String -> Ghc a -> IO a
simpleLoad targetMod ghc = defaultGhc $ do
  _ <- defaultSetExtensions []
  target <- guessTarget (targetMod <> ".hs") Nothing
  setTargets [target]
  _ <- load LoadAllTargets
  ghc

defaultSetExtensions :: [Extension] -> Ghc [InstalledUnitId]
defaultSetExtensions xs = do
  dflags <- getSessionDynFlags
  let dflags' = foldl xopt_set dflags xs
  setSessionDynFlags dflags'

gatherClsInsts :: TypecheckedModule -> [ClsInst]
gatherClsInsts t =
  let (_,moddets) = tm_internals_ t
      clsInsts = md_insts moddets
  in clsInsts

gatherHClasses :: TypecheckedModule -> [HClass]
gatherHClasses t =
  let clsInsts = gatherClsInsts t
      hclasses = map (\c -> HClass (clsInstOccName c) (is_tys c)) clsInsts
  in hclasses

clsInstOccName :: ClsInst -> OccName
clsInstOccName = occName . is_cls_nm

typecheck :: String -> Ghc TypecheckedModule
typecheck targetMod = do
  modSum <- getModSummary $ mkModuleName targetMod
  p <- parseModule modSum
  typecheckModule p
-}

typecheck :: GhcMonad m => ModSummary -> m TypecheckedModule
typecheck (ms_mod_name -> mod_name) = do
  p <- parseModule =<< getModSummary mod_name
  typecheckModule p

typecheckProj :: GhcMonad m => m [TypecheckedModule]
typecheckProj = do {
    module_graph <- getModuleGraph
  ; mapM typecheck (mgModSummaries module_graph)
  }

getProjClsInsts :: GhcMonad m => m [ClsInst]
getProjClsInsts = do {
    tcs <- typecheckProj
  ; pure $ concatMap gatherClsInsts tcs
  }

gatherClsInsts :: TypecheckedModule -> [ClsInst]
gatherClsInsts (tm_internals_ -> (_, moddets)) = md_insts moddets

clsInstOccName :: ClsInst -> OccName
clsInstOccName = occName . is_cls_nm

clsInstHead :: ClsInst -> InstanceHead
clsInstHead (instanceHead -> (tyvars, cls, types))
  = InstanceHead tyvars cls types

data InstanceHead = InstanceHead
  { inst_h_tyvars :: [TyVar] -- unbounded, sorted
  , inst_h_cls :: Class -- single class
  , inst_h_sat :: [Type]
  }

newtype ClsInstPpr = ClsInstPpr ClsInst
instance Outputable ClsInstPpr where
  ppr (ClsInstPpr c) = pprInst c

pprInst :: ClsInst -> SDoc
pprInst ClsInst{..} = O.hcat
  [ O.text "Class name: ", ppr is_cls_nm, O.text "\n"
  , O.text "Top of type args: ", ppr is_tcs, O.text "\n"
  , O.text "DFunName: ", ppr is_dfun_name, O.text "\n"
  , O.text "TyVars: ", ppr is_tvs, O.text "\n"
  , O.text "Class: ", ppr is_cls, O.text "\n"
  , O.text "Tys: ", ppr is_tys, O.text "\n"
  , O.text "DFunId: ", ppr is_dfun, O.text "\n"
  , O.text "Flag: ", ppr is_flag, O.text "\n"
  , O.text "IsOrphan: ", ppr (isOrphan is_orphan), O.text "\n"
  ]

-- TODO: stop ignoring default extensions
-- TODO: get fully qualified Class name, to ensure actual equality

-- now you can `getModuleGraph` appropriately
loadProj :: GhcMonad m => [ModuleName] -> m SuccessFlag
loadProj ms = do
  let targetIds = map TargetModule ms
  let targets = map (\tId -> Target tId False Nothing) targetIds
  setTargets targets
  mod_graph <- depanal [] False
  --liftIO $ print mod_graph
  success <- load' LoadAllTargets (Just batchMsg) mod_graph
--  warnUnusedPackages -- not exported
  pure success

-- Goals
--
