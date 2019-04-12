{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language ViewPatterns #-}

module Knuckles (generate) where

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
import qualified Data.Map.Strict as M
import qualified Outputable as O

data Supported
  = Show
  | Eq
  | Semigroup
  | Monoid
  deriving (Eq,Show,Enum,Bounded)

instance HasOccName Supported where
  occName = \case
    Show -> mkTcOcc "Show"
    Eq -> mkTcOcc "Eq"
    Semigroup -> mkTcOcc "Semigroup"
    Monoid -> mkTcOcc "Monoid"

supportedToLaws :: Supported -> SDoc
supportedToLaws = \case
  Show -> "showLaws"
  Eq -> "eqLaws"
  Semigroup -> "semigroupLaws"
  Monoid -> "monoidLaws"

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
    _ -> Nothing
  else Nothing

build :: String -> [HClass] -> [SDoc]
build targetMod hs = header : punctuate "\n" (map outputTyLaws (htoTyLaws (hfromList hs)))
  where
    header = hcat
      [ "module Laws where\n\n"
      , "import Hedgehog\n"
      , "import qualified Hedgehog.Gen as Gen\n"
      , "import qualified Hedgehog.Range as Range\n"
      , "import Hedgehog.Classes\n"
      , "import " O.<> O.text (targetMod <> ".hs") O.<> "\n\n"
      ]

data TyLaws = TyLaws
  { _tyLawsType :: Type
  , _tyLawsClsInsts :: [Supported]
  }

isValid :: TyLaws -> Bool
isValid (TyLaws _ os) = any (== Show) os && any (== Eq) os

outputTyLaws :: TyLaws -> SDoc
outputTyLaws (TyLaws t os) = hcat
  [ tyLaws
  , " :: Gen "
  , ty
  , " -> [(String,[Laws])]\n"
  , tyLaws
  , " gen = [(\""
  , ty
  , "\", tyLaws')]\n"
  , "  where\n"
  , "    tyLaws' =\n"
  , "      [ "
  , tyLaws'
  , "      ]\n"
  ]
  where
    ty = ppr t
    tyLaws = hcat ["_", ty, "Laws"]
    tyLaws' = hcat $ fixup (map (\o -> supportedToLaws o O.<> " gen\n") os)

fixup :: [SDoc] -> [SDoc]
fixup [] = []
fixup (x:xs) = punctuate "      , " (x : xs)

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

{- 
printInterestingNames :: IO ()
printInterestingNames = getInterestingNames >>= myPrint

printClsInsts :: IO ()
printClsInsts = getClsInsts >>= myPrint

printClsInstsOccNames :: IO ()
printClsInstsOccNames = getClsInstsOccNames >>= myPrint

printClsInstsOccNamesWithTypes :: IO ()
printClsInstsOccNamesWithTypes = getClsInstsOccNamesWithTypes >>= myPrint

printFOO :: IO ()
printFOO = do
  hclasses <- getClsInstsOccNamesWithTypes
  myPrint' (build hclasses) 
-}

generate :: ()
  => String -- ^ module name without ".hs"
  -> FilePath -- ^ output file, without ".hs"
  -> IO ()
generate targetMod outFile = do
  hclasses <- getHClasses targetMod
  p <- mapM safeShowSDoc (build targetMod hclasses)
  writeFile (outFile <> ".hs") (mconcat p) 

getHClasses :: String -> IO [HClass]
getHClasses targetMod = defaultGhc targetMod $ do
  t <- typecheck targetMod
  pure $ gatherHClasses t
 
{-
getClsInstsOccNamesWithTypes :: IO [HClass]
getClsInstsOccNamesWithTypes = defaultGhc $ do
  t <- typecheck targetMod
  pure $ gatherClsInstsOccNamesWithTypes t

getClsInstsOccNames :: IO [OccName]
getClsInstsOccNames = defaultGhc $ do
  t <- typecheck targetMod
  pure $ gatherClsInstOccNames t
   
getInterestingNames :: IO [Name]
getInterestingNames = defaultGhc $ do
  targetModule <- mkModuleFromStr targetMod
  names <- getNames targetMod
  pure $ interestingNames names targetModule

getClsInsts :: IO [ClsInst]
getClsInsts = defaultGhc $ do
  t <- typecheck targetMod
  pure $ gatherClsInsts t

safeShow :: Outputable a => a -> IO String
safeShow res = safeShowSDoc (ppr res)
-}

safeShowSDoc :: SDoc -> IO String
safeShowSDoc doc = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  pure $ showSDoc dflags doc

{-
-- | Given a module and a list of names, return only those names
--   which are 'interesting' (See 'interested').
interestingNames :: ()
  => [Name]
  -> Module
  -> [Name]
interestingNames names m = filter (interested m) names

interested :: Module -> Name -> Bool
interested m = getPredicate $ foldMap Predicate
  [ isTyConName
  , moduleMatches m 
  ]

moduleMatches :: Module -> Name -> Bool
moduleMatches m n = case nameModule_maybe n of
  Nothing -> False
  Just mName -> m == mName

-- | Return the 'Name' only if it is defined in that module.
--   We are 'interested' in a name if it is in that module.
interestedName :: ()
  => Name
  -> Module
  -> Maybe Name
interestedName n m = do
  mName <- nameModule_maybe n
  if mName == m then pure n else Nothing

mkModuleFromStr :: String -> Ghc Module
mkModuleFromStr targetMod = do
  modSum <- getModSummary $ mkModuleName targetMod
  pure (ms_mod modSum)
-}

defaultGhc :: String -> Ghc a -> IO a
defaultGhc targetMod ghc = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
   runGhc (Just libdir) $ do
     _ <- entry targetMod
     ghc   

defaultSetExtensions :: [Extension] -> Ghc [InstalledUnitId]
defaultSetExtensions xs = do
  dflags <- getSessionDynFlags
  let dflags' = foldl xopt_set dflags xs
  setSessionDynFlags dflags'

entry :: String -> Ghc SuccessFlag
entry targetMod = do
  _ <- defaultSetExtensions []
  target <- guessTarget (targetMod <> ".hs") Nothing
  setTargets [target]
  load LoadAllTargets

{- 
getNames :: String -> Ghc [Name]
getNames targetMod = do
  t <- typecheck targetMod
  pure $ gatherNames t

gatherNames :: TypecheckedModule -> [Name]
gatherNames t =
  let (tcenv, _) = tm_internals_ t
      names = concatMap (map gre_name) $ occEnvElts $ tcg_rdr_env tcenv
  in names
-}

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

{-
gatherClsInstsOccNamesWithTypes :: TypecheckedModule -> [HClass]
gatherClsInstsOccNamesWithTypes t =
  let clsInsts = gatherClsInsts t
      clsInstsOccNamesWithTypes = map (\c -> HClass (clsInstOccName c) (is_tys c)) clsInsts
  in clsInstsOccNamesWithTypes

gatherClsInstsNames :: TypecheckedModule -> [Name]
gatherClsInstsNames t =
  let clsInsts = gatherClsInsts t
      clsInstsNames = map is_cls_nm clsInsts
  in clsInstsNames
-}
  
clsInstOccName :: ClsInst -> OccName
clsInstOccName = occName . is_cls_nm

{-
gatherClsInstOccNames :: TypecheckedModule -> [OccName]
gatherClsInstOccNames t =
  let clsInsts = gatherClsInsts t
      clsInstsOccNames = map clsInstOccName clsInsts
  in clsInstsOccNames
-}
 
typecheck :: String -> Ghc TypecheckedModule
typecheck targetMod = do
  modSum <- getModSummary $ mkModuleName targetMod
  p <- parseModule modSum
  typecheckModule p
