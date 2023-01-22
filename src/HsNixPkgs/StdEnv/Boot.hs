module HsNixPkgs.StdEnv.Boot
  ( StdEnvG (..),
    StdEnvBoot,
    stdEnvBoot,
  )
where

import HsNixPkgs.ExtendDrv
import HsNixPkgs.System

data BootStdEnv

data StdEnvG b m = StdEnv
  { stdEnvShell :: m (DrvStr m),
    stdEnvDeps :: SimpleDeps (System -> System -> ExtendDeriv m)
  }

type StdEnvBoot = StdEnvG BootStdEnv

stdEnvBoot :: System -> System -> System -> StdEnvG BootStdEnv m
stdEnvBoot = _