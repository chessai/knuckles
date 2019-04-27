{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
{-# language StandaloneDeriving #-}

module Knuckles where

import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import DynFlags
import GHC
import GHC.LanguageExtensions.Type
import GHC.Paths (libdir)
import HscTypes
import InstEnv
import Module
import Name
import Outputable hiding ((<>),text)
import TyCon
import Type
import qualified Data.Map.Strict as M
import qualified Outputable as O

data Supported
  = Show
  | Eq
  | Semigroup
  | Monoid
  | Generic
  deriving (Eq,Show,Enum,Bounded)

instance HasOccName Supported where
  occName = \case
    Show -> mkTcOcc "Show"
    Eq -> mkTcOcc "Eq"
    Semigroup -> mkTcOcc "Semigroup"
    Monoid -> mkTcOcc "Monoid"
    Generic -> mkTcOcc "Generic"

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
isValid (TyLaws _ os) = any (== Show) os && any (== Eq) os

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
