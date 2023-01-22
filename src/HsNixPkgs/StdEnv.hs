module HsNixPkgs.StdEnv
  ( StdEnvG (..),
    StdEnv,
    stdEnv,
  )
where

import HsNixPkgs.StdEnv.Boot
import HsNixPkgs.System

data StdEnvRegular

type StdEnv = StdEnvG StdEnvRegular

stdEnv :: System -> System -> System -> StdEnv m
stdEnv = _