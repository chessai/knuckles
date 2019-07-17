{-# language
        DeriveAnyClass
      , DerivingStrategies
      , FlexibleContexts
      , GeneralizedNewtypeDeriving
      , LambdaCase
      , NoMonomorphismRestriction
      , OverloadedStrings
      , RecordWildCards
      , ViewPatterns
  #-}

module Knuckles
  ( main
  ) where

import Class (className)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except
import Control.Monad.Reader
import CoreSyn (isOrphan)
import Data.Bifunctor (first)
import Data.Bimap (Bimap)
import Data.Char (isSpace)
import Data.Foldable (fold)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (intercalate,groupBy)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Set (Set)
import Data.String (fromString)
import GHC
import GHC.SourceGen
import GhcMake
import HscMain (batchMsg)
import HscTypes
import InstEnv
import Knuckles.Monad
import Name
import Options.Applicative
import Outputable hiding ((<>),text)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import TyCon
import Type
import qualified Control.Applicative as A
import qualified Control.Monad.Catch as Catch
import qualified Data.Bimap as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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

envInfo :: ParserInfo Env
envInfo = info ((Env
  <$> ( pure Cabal.normal
      )
  <*> ( strOption $ fold [ long "cabal", help "cabal file" ]
      )
  <*> ( strOption $ fold [ long "tld", help "top level directory of cabal project" ]
      )
  <*> ( strOption $ fold [ long "env", help "ghc environment file" ]
      )
  <*> ( strOption $ fold [ long "test", help "test directory" ]
      )
  ) <**> helper) mempty

withCurrentDir :: (MonadIO m, MonadMask m)
  => FilePath
  -> m a
  -> m a
withCurrentDir dir act
  = Catch.bracket
      (liftIO Dir.getCurrentDirectory)
      (\ogDir -> liftIO (Dir.setCurrentDirectory ogDir))
      $ const (liftIO (Dir.setCurrentDirectory dir) >> act)

main :: IO ()
main = do
  env <- execParser envInfo
  (putStrLn . showSDocUnsafe . ppr) =<< runKnuckles'
    env
    (withEnv $ \(Env _ proj tld genv testDir) -> do {
        dflags <- getSessionDynFlags
      ; void $ setSessionDynFlags $ dflags { packageEnv = Just genv }
      ; liftIO $ Dir.setCurrentDirectory tld
      ; (hsSrcDir, module_names) <- getModules proj
      ; withCurrentDir hsSrcDir $ void $ loadProj module_names
      ; cls_insts <- getProjClsInsts
      ; let inst_heads = map clsInstHead cls_insts
      ; let gen_map = collectTyCons (mksGenMap inst_heads)
      ; let gen_module = genModule module_names gen_map
      ; dflags' <- getSessionDynFlags
      ; let module_text = showPpr dflags' gen_module
      ; withCurrentDir testDir
          $ liftIO
          $ writeFile "test.hs" module_text
      ; pure $ gen_map
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

getModules :: (MonadIO m, MonadError KnucklesError m, MonadReader Env m)
  => FilePath -> m (FilePath, [ModuleName])
getModules fp = withEnv $ \(Env v _ _ _ _) -> do
  doesCabalExist fp
  isCabal fp
  pkgDescr <- liftIO $ Cabal.catchIO
    (Right <$> Cabal.readGenericPackageDescription v fp)
    (pure . Left)
  case pkgDescr of
    Left e -> throwError (CabalThrewAnError e)
    Right g -> maybeToError (Cabal.condLibrary g) NoLibraryComponent
      $ \condTreeLib -> do
          let treeData = Cabal.condTreeData condTreeLib
          let hsSrcDirs = id
                . Cabal.hsSourceDirs
                . Cabal.libBuildInfo
                $ treeData
          when (length hsSrcDirs /= 1)
            $ throwError MoreThanOneHsSourceDir
          let hsSrcDir = head hsSrcDirs
          let modules = id
                . fmap mkModuleName
                . fmap Cabal.toFilePath
                . Cabal.explicitLibModules
                $ treeData
          pure (hsSrcDir, modules)

maybeToError :: MonadError e m => Maybe a -> e -> (a -> m b) -> m b
maybeToError Nothing e _ = throwError e
maybeToError (Just a) _ f = f a

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

--clsInstOccName :: ClsInst -> OccName
--clsInstOccName = occName . is_cls_nm

clsInstHead :: ClsInst -> InstanceHead
clsInstHead (instanceHead -> (tyvars, cls, types))
  = InstanceHead tyvars cls types

data InstanceHead = InstanceHead
  { inst_h_tyvars :: [TyVar] -- unbounded, sorted
  , inst_h_cls :: Class -- single class
  , inst_h_sat :: [Type]
  }

instance Outputable InstanceHead where
  ppr InstanceHead{..} = O.hcat
    [ O.text "InstanceHead {\n"
    , O.text "  , inst_h_tyvars = ", ppr inst_h_tyvars, O.text "\n"
    , O.text "  , inst_h_cls = ", ppr inst_h_cls, O.text "\n"
    , O.text "  , inst_h_sat = ", ppr inst_h_sat, O.text "\n"
    , O.text "}\n"
    ]

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

-- TODO: Only types with valid Eq/Show instances can be used
-- TODO: stop ignoring default extensions
-- TODO: get fully qualified Class name, to ensure actual equality
-- TODO: must be a vanilla algtycon

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

--
-- Plan for Haskell98 instances
--
-- 1) Take haskell project as input
-- 2) Typecheck the modules of that project
-- 3) For each 'ClsInst', get the 'InstanceHead'
-- 4) In the 'InstanceHead', we require that the list of 'Type'
--    is a singleton. If it is not, discard the current 'InstanceHead'.
-- 5) Is the 'Class' supported? If not, discard the current
--    'InstanceHead'.
-- 6) Code generation (based on hedgehog-classes typeclass arity)
--
-- Consider the following module
--   (assume a valid TypeEnv/single-module project):
--
-- ```
-- -- FooMod.hs
--
-- data Foo x y = Foo
--
-- instance Eq (Foo x y) where
--   _ == _ = True
--
-- instance Functor (Foo x) where
--   fmap _ Foo = Foo
--
-- instance Bifunctor Foo where
--   bimap _ _ _ = Foo
-- ```
--
-- Note that x and y are phantom (Don't worry about representation
-- right now).
--
-- The following 'InstanceHead's are generated:
--
-- ```
-- InstanceHead {
--   , inst_h_tyvars = [x,y]
--   , inst_h_cls = Eq
--   , inst_h_sat = [Foo x y]
-- }
--
-- InstanceHead {
--   , inst_h_tyvars = [x]
--   , inst_h_cls = Functor
--   , inst_h_sat = [Foo x]
-- }

-- InstanceHead {
--   , inst_h_tyvars = []
--   , inst_h_cls = Bifunctor
--   , inst_h_sat = [Foo]
-- }
-- ```
--
-- For each (instance,type) pair in some type isomorphic to
-- `(Type,[Instance])`, we want to generate code based on Class kind.
--
-- We will label class kinds like so:
--
-- T1 = Type -> Constraint
-- T2 = (Type -> Type) -> Constraint
-- T3 = (Type -> Type -> Type) -> Constraint
--
-- For our example we have one of each (T1 = Eq, T2 = Functor, T3 = Bifunctor).
--
-- For T_N, we need (length inst_h_tyvars - N + 1) tyvars in the
-- generated code.
--
-- Our generated code for our `Foo` example (note the boilerplate
-- at the beginning):
--
-- ```
-- {-# language PackageImports #-}
--
-- import Hedgehog
-- import Hedgehog.Classes
-- import Hedgehog.Generic
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
--
-- import "pkgName" qualified FooMod
--
-- main :: IO ()
-- main = lawsCheckMany allLaws
--
-- type Laws' = [(String, [Laws])]
--
-- allLaws :: Laws'
-- allLaws = mconcat
--   [ _FooLaws
--   ]
--
-- -- T1 gen for `Foo x y`
-- _FooGen1 :: Gen (Foo x y)
-- _FooGen1 = pure Foo
--
-- -- T2 gen for `Foo x y`
-- _FooGen2 :: forall y. Gen y -> Gen (Foo x y)
-- _FooGen2 _ = pure Foo
--
-- -- T3 gen for `Foo x y`
-- _FooGen3 :: forall x y. Gen x -> Gen y -> Gen (Foo x y)
-- _FooGen3 _ pure Foo
--
-- _FooLaws :: Laws'
-- _FooLaws = [("Foo", tyLaws)]
--   where
--     tyLaws =
--       [ eqLaws _FooGen1
--       , functorLaws _FooGen2
--       , bifunctorLaws _FooGen3
--       ]
-- ```
--
-- This becomes more complicated when the representation of the tyvars
-- becomes anything but phantom.
-- I think the best we can do is use hedgehog-generic, that is,
-- if the representation type has a Generic instance.
-- If it doesn't, then we might have `_TyconGenN = error "msg"`.
--
-- All tyvars must be kinded (TYPE 'LiftedRep).
--

supportedLaws :: Supported -> String
supportedLaws = \case
  Show -> "showLaws"
  Eq -> "eqLaws"
  Semigroup -> "monoidLaws"
  Monoid -> "monoidLaws"
  Generic -> "genericLaws"

  Functor -> "functorLaws"

  Bifunctor -> "bifunctorLaws"

data Supported
  = Show
  | Eq
  | Semigroup
  | Monoid
  | Generic

  | Functor

  | Bifunctor
  deriving (Eq,Ord,Show,Enum,Bounded)

instance Outputable Supported where
  ppr = O.text . show

instance HasOccName Supported where
  occName x = mkTcOcc $ case x of
    Show -> "Show"
    Eq -> "Eq"
    Semigroup -> "Semigroup"
    Monoid -> "Monoid"
    Generic -> "Generic"
    Functor -> "Functor"
    Bifunctor -> "Bifunctor"

data T = T1 | T2 | T3
  deriving (Show,Eq,Ord,Enum,Bounded)

instance Outputable T where
  ppr = O.text . show

data Instance = Instance
  { inst_t :: T
  , inst_supported :: Supported
  , inst_tyvars :: [TyVar]
  }

instance Outputable Instance where
  ppr (Instance t s ty) = ppr (t,s,ty)

type GenMap = Map UnsafeType [Instance]

newtype UnsafeType = UnsafeType { getUnsafeType :: Type }
  deriving newtype (Outputable)

instance Eq UnsafeType where
  (UnsafeType x) == (UnsafeType y) = eqType x y

instance Ord UnsafeType where
  compare (UnsafeType x) (UnsafeType y)
    = nonDetCmpType x y

instance Show UnsafeType where
  show (UnsafeType ty) = showSDocUnsafe (ppr ty)

--instances_ts :: [Instance] -> [T]
--instances_ts = map inst_t

t_rdr :: T -> String
t_rdr = \case { T1 -> "1"; T2 -> "2"; T3 -> "3" }

-- number of tyvars for us to fill in
--foralldTyVars :: Instance -> Int
--foralldTyVars Instance{..} = max 0
--  (length inst_tyvars + 1 - (case inst_t of {
--    T1 -> 1; T2 -> 2; T3 -> 3
--  }))

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

fresh :: Int -> String
fresh n = id
  . trim
  . intercalate " "
  . take n
  . map (\(x,nth) -> x ++ show nth)
  $ zip (cycle ["x"]) [(0::Int)..]

ints :: Int -> String
ints n = trim
  . intercalate " "
  $ replicate n "Integer"

{-| Gather kinds of TyVars
 -  Fill in fake based on kind
 -}
--kind0, kind1, kind2 :: String
--kind0 = "Integer"; kind1 = "Identity"; kind2 = "Either"

genModule :: [ModuleName] -> GenMap -> HsModule'
genModule (map (import' . ModuleNameStr) -> extraImports) gen_map = module'
  (Just "Main")
  (Just [var "main"])
  ( [ import' "Hedgehog"
    , import' "Hedgehog.Classes"
    , import' "Hedgehog.Generic"
    , qualified' (as' (import' "Hedgehog.Gen") "Gen")
    , qualified' (as' (import' "Hedgehog.Range") "Range")
    ]
    ++ extraImports
  )
  ( [type' laws [] (var "[(String, [Laws])]")]
  ++ [ typeSig "main" $ var "IO Bool"
     , funBind "main"
         $ matchRhs []
         $ var "lawsCheckMany" @@ var "allLaws"
     , typeSig "allLaws" $ var "Laws'"
     , funBind "allLaws"
         $ matchRhs []
         $ var "mconcat" @@ bode allLawsBody
     ]
  ++ genAll gen_map
  ++ allLawsDecls
  )
  where
    (allLawsBody, allLawsDecls) = genAllLaws gen_map

laws :: RdrNameStr
laws = "Laws'"

bode :: [String] -> HsExpr'
bode = list . fmap (var . fromString)

tyConRdr :: Type -> String
tyConRdr = occNameString . occName . tyConName . tyConAppTyCon

-- TODO: a lot of the generator code is not idiomatic
--       (i.e. does not use ast but instead uses fromString)

genAllLaws :: GenMap -> ([String],[HsDecl'])
genAllLaws = id
  . M.foldMapWithKey (\u insts -> first pure (genAllLaws' u insts))

--unsafeCompareByTyCon :: UnsafeType -> UnsafeType -> Ordering
--unsafeCompareByTyCon u u' = nonDetCmpTc (go u) (go u') where
--  go = tyConAppTyCon . getUnsafeType

unsafeEqByTyCon :: UnsafeType -> UnsafeType -> Bool
unsafeEqByTyCon u u' = case nonDetCmpTc (go u) (go u') of
  EQ -> True
  _ -> False
  where
    go = tyConAppTyCon . getUnsafeType

-- remove redundant tycons
collectTyCons :: GenMap -> GenMap
collectTyCons = id
  . M.fromList
  . map
      ( first
          ( fromMaybe (error "Knuckles: collectTyCon: internal error")
          . getFirst
          )
      . foldMap (\(u, is) -> (First (Just u), is))
      )
  . groupBy (\(u, _) (u', _) -> unsafeEqByTyCon u u')
  . M.toList

genAllLaws' :: UnsafeType -> [Instance] -> (String, [HsDecl'])
genAllLaws' u@(getUnsafeType -> ty) insts =
  let tyConRdr' = tyConRdr ty
      name = fromString $ "_" <> tyConRdr' <> "Laws"
      matches = match [] body
      body = where'
        (rhs $ var "pure $" @@ -- couldnt see immediately how to wrap
                               -- something in the list constructor
          (tuple
            [ var (fromString ("\"" ++ tyConRdr' ++ "\""))
            , var "tyLaws"
            ]
          )
        )
        [ funBind "tyLaws"
            $ matchRhs []
            $ list
            $ foldMap (pure . genLaws u) insts
        ]
      bind =
        [ typeSig name $ var "Laws'"
        , funBind name matches
        ]
  in (name, bind)

genLaws :: UnsafeType -> Instance -> HsExpr'
genLaws (getUnsafeType -> ty) Instance{..} =
  let tyConRdr' = tyConRdr ty
      n = t_rdr inst_t
  in var $ fromString $ mconcat
       [ supportedLaws inst_supported
       , " _Gen"
       , tyConRdr'
       , n
       ]

genAll :: GenMap -> [HsDecl']
genAll = M.foldMapWithKey (foldMap . gen)

t_vars :: T -> Int
t_vars = \case { T1 -> 0; T2 -> 1; T3 -> 2; }

globalGenState :: IORef (Map UnsafeType (Set T))
globalGenState = unsafePerformIO $ newIORef mempty
{-# noinline globalGenState #-}

hasGen :: UnsafeType -> T -> IO Bool
hasGen u t = do
  genState <- readIORef globalGenState
  case M.lookup u genState of
    Nothing -> pure False
    Just s -> pure $ S.member t s

putGen :: UnsafeType -> T -> IO ()
putGen u t = modifyIORef' globalGenState
  (\m -> M.alter
    (\case {
        Nothing -> Just (S.singleton t)
      ; Just s -> Just (S.insert t s)
    }) u m
  )

gen :: UnsafeType -> Instance -> [HsDecl']
gen u@(getUnsafeType -> ty) Instance{..} = unsafePerformIO $ do {
    has_gen <- hasGen u inst_t
  ; if has_gen then pure [] else do {
      let tyConRdr' = tyConRdr ty
    ; let name = fromString $ trim $ "_Gen" <> tyConRdr' <> t_rdr inst_t
    ; let n_tyvars = length inst_tyvars
    ; let n_foralld_tyvars = t_vars inst_t
    ; let n_solid_tyvars = n_tyvars
    ; let foralldsCrap = case inst_t of {
              T1 -> ""
            ; T2 -> "Gen x0 -> "
            ; T3 -> "Gen x0 -> Gen x1 -> "
          }
    ; let variant = var
            $ fromString
            $ foralldsCrap
            <> "Gen "
            <> wrap
              ( mconcat
                [ tyConRdr'
                , " "
                , ints n_solid_tyvars
                , " "
                , fresh n_foralld_tyvars
                ]
              )
    ; let defaultGen
            = var "error \"Knuckles: Unable to supply generator.\""
    ; let decls =
            [ typeSig name $ variant
            , funBind name $ matchRhs [] defaultGen
            ]
    ; putGen u inst_t
    ; pure decls
    }
  }
{-# noinline gen #-}

wrap :: String -> String
wrap s = "(" ++ s ++ ")"

mksGenMap :: [InstanceHead] -> GenMap
mksGenMap = M.unionsWith (++) . map mkGenMap

--plusGenMap :: GenMap -> GenMap -> GenMap
--plusGenMap = M.unionWith (++)

mkGenMap :: InstanceHead -> GenMap
mkGenMap = mkGenMap2 . mkGenMap1

mkGenMap2 :: Maybe (Type, Instance) -> GenMap
mkGenMap2 Nothing = mempty
mkGenMap2 (Just (ty,inst)) = M.singleton (UnsafeType ty) [inst]

mkGenMap1 :: InstanceHead -> Maybe (Type, Instance)
mkGenMap1 InstanceHead{..} = do
  ty <- whenA (length inst_h_sat == 1) $ pure (head inst_h_sat)
  s <- getSupported inst_h_cls
  pure (ty, Instance (supportedT s) s inst_h_tyvars)

whenA :: Alternative m => Bool -> m a -> m a
whenA b m = if b then m else A.empty

--unlessA :: Alternative m => Bool -> m a -> m a
--unlessA b m = if b then A.empty else m

supportedT :: Supported -> T
supportedT = \case
  Eq -> T1
  Show -> T1
  Semigroup -> T1
  Monoid -> T1
  Generic -> T1
  Functor -> T2
  Bifunctor -> T3

allSupported :: [Supported]
allSupported = [minBound .. maxBound]

supported :: Bimap Supported OccName
supported = B.fromList
  $ map (\s -> (s, occName s)) allSupported

getSupported :: Class -> Maybe Supported
getSupported = flip B.lookupR supported . occName . className

