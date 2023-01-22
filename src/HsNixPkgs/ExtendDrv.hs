{-# LANGUAGE DeriveGeneric #-}

module HsNixPkgs.ExtendDrv
  ( module H,
    MonadDeriv (..),
    DrvStrBuilder,
    AsDrvStr (..),
    fromDrvStr,
    SimpleDeps (..),
    emptySimpleDeps,
    SetupHook (..),
    DrvOutput (..),
    storePath,
    storePathStr,
    ExtendDeriv (..),
    getOutputAt,
    getDefaultOut,
    FileDeriv (..),
  )
where

import Control.Arrow
import Data.Default
import Data.Foldable
import Data.Hashable
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHC.Generics (Generic)
import HsNix.Derivation hiding (storePath, storePathStr)
import HsNix.Hash as H
import HsNixPkgs.ConfigSet
import HsNixPkgs.Shell
import qualified HsNixPkgs.Types as Typ

data SimpleDeps a = SimpleDeps
  { depsBuildBuild,
    -- same as nixpkgs nativeBuildInputs
    depsBuildHost,
    depsBuildTarget,
    depsHostHost,
    -- same as nixpkgs buildInputs
    depsHostTarget,
    depsTargetTarget ::
      [a]
  }
  deriving (Eq, Show)

instance Semigroup (SimpleDeps a) where
  l <> r =
    SimpleDeps
      { depsBuildBuild = depsBuildBuild l <> depsBuildBuild r,
        depsBuildHost = depsBuildHost l <> depsBuildHost r,
        depsBuildTarget = depsBuildTarget l <> depsBuildTarget r,
        depsHostHost = depsHostHost l <> depsHostHost r,
        depsHostTarget = depsHostTarget l <> depsHostTarget r,
        depsTargetTarget = depsTargetTarget l <> depsTargetTarget r
      }

instance Monoid (SimpleDeps a) where
  mempty = emptySimpleDeps

emptySimpleDeps :: SimpleDeps a
emptySimpleDeps =
  SimpleDeps
    { depsBuildBuild = [],
      depsBuildHost = [],
      depsBuildTarget = [],
      depsHostHost = [],
      depsHostTarget = [],
      depsTargetTarget = []
    }

data SetupHook m = SetupHook
  { hookScript :: Script m (),
    hookEnv, hookPassAsFile :: Typ.EnvHook m,
    hookFun :: ConfigSet -> ConfigSet
  }

instance Default (SetupHook m) where
  def = mempty

instance Semigroup (SetupHook m) where
  l <> r =
    SetupHook
      { hookScript = hookScript l >> hookScript r,
        hookEnv = hookEnv l <> hookEnv r,
        hookPassAsFile = hookPassAsFile l <> hookPassAsFile r,
        hookFun = hookFun l >>> hookFun r
      }

instance Monoid (SetupHook m) where
  mempty =
    SetupHook
      { hookScript = pure (),
        hookEnv = mempty,
        hookPassAsFile = mempty,
        hookFun = id
      }

data DrvOutput m = DrvOutput
  { dOutParentDrv :: Derivation m,
    dOutName :: Maybe Text,
    dOutHasExecutable, dOutHasData :: Bool,
    dOutSetupHook :: Maybe (Int -> Int -> SetupHook m),
    dOutPropagatedDeps :: SimpleDeps (DrvOutput m)
  }
  deriving (Generic)

instance (MonadDeriv m) => Show (DrvOutput m) where
  show a = concat ["DrvOutput (", show (dOutParentDrv a), ".", show (dOutName a), ")"]

instance (MonadDeriv m) => Eq (DrvOutput m) where
  a == b = dOutParentDrv a == dOutParentDrv b && dOutName a == dOutName b

instance (MonadDeriv m) => Hashable (DrvOutput m) where
  hashWithSalt s a = hashWithSalt s (dOutParentDrv a, dOutName a)

storePath :: MonadDeriv m => DrvOutput m -> m (StorePath m)
storePath d = storePathOf (dOutParentDrv d) (dOutName d)

storePathStr :: MonadDeriv m => DrvOutput m -> m (DrvStr m)
storePathStr d = storePathStrOf (dOutParentDrv d) (dOutName d)

data ExtendDeriv m = ExtDrv
  { eDrvBaseDrv :: Derivation m,
    eDrvOutputs :: NEL.NonEmpty (DrvOutput m)
  }

deriving instance (MonadDeriv m) => Show (ExtendDeriv m)

deriving instance (MonadDeriv m) => Eq (ExtendDeriv m)

getOutputAt :: ExtendDeriv m -> Maybe Text -> DrvOutput m
getOutputAt d on@(Just _) = fromJust (find (\i -> dOutName i == on) (eDrvOutputs d))
getOutputAt d Nothing = NEL.head (eDrvOutputs d)

getDefaultOut :: ExtendDeriv m -> DrvOutput m
getDefaultOut d = getOutputAt d Nothing

data FileDeriv m = FileDrv
  { fDrvBase :: Derivation m,
    fDrvIsDirectory :: Bool,
    fDrvPath :: Text
  }