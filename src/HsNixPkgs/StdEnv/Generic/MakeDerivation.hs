module HsNixPkgs.StdEnv.Generic.MakeDerivation
  ( BuilderArg (..),
    OutputArg (..),
    defOutputArg,
    DrvType (..),
    MkDerivationArg (..),
    defMkDerivationArg,
    outputOpts,
    mkDerivation,
  )
where

import Control.Monad
import Data.Foldable
import Data.Functor
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text (Text)
import qualified HsNix.Derivation as ND
import HsNix.Hash
import HsNixPkgs.ExtendDrv
import HsNixPkgs.Shell
import HsNixPkgs.Shell.Util
import HsNixPkgs.StdEnv.Generic.Dependent (getDependenciesDrv)
import HsNixPkgs.System
import qualified HsNixPkgs.Types as Typ
import HsNixPkgs.Util

data BuilderArg m = BuilderArg
  { buildEnv,
    buildPassAsFile ::
      Typ.EnvHook m,
    builder :: Script m (ScriptTerm m)
  }

data OutputArg m = OutputArg
  { oArgSetupHook :: Maybe (Int -> Int -> SetupHook m),
    oArgHasData :: Bool,
    oArgHasExecutable :: Bool,
    oArgPropagatedDeps :: SimpleDeps (System -> System -> DrvOutput m)
  }

defOutputArg :: Bool -> Bool -> OutputArg m
defOutputArg exec dat =
  OutputArg
    { oArgSetupHook = Nothing,
      oArgHasData = dat,
      oArgHasExecutable = exec,
      oArgPropagatedDeps = emptySimpleDeps
    }

data DrvType m a
  = InputAddressed (NEL.NonEmpty (Text, OutputArg m))
  | FixedOutput HashMode (Hash a) (OutputArg m)
  | ContentAddressed HashMode (NEL.NonEmpty (Text, OutputArg m))

data MkDerivationArg a m = MkDrvArg
  { pName :: Text,
    version :: Maybe Text,
    sourceDateEpoch :: Int,
    sslCertFile :: Maybe (DrvStr m),
    derivType :: DrvType m a,
    shell :: m (DrvStr m),
    extraEnv, extraPassAsFile :: Typ.EnvHook m
  }

defMkDerivationArg ::
  Text ->
  Maybe Text ->
  DrvType m a ->
  m (DrvStr m) ->
  MkDerivationArg a m
defMkDerivationArg n v t s =
  MkDrvArg
    { pName = n,
      version = v,
      sourceDateEpoch = 315532800, -- 1980-01-01 12:00:00
      sslCertFile = Nothing,
      derivType = t,
      shell = s,
      extraEnv = mempty,
      extraPassAsFile = mempty
    }

outputOpts :: MkDerivationArg a m -> NEL.NonEmpty (Text, OutputArg m)
outputOpts MkDrvArg {derivType = t} = case t of
  InputAddressed os -> os
  FixedOutput _ _ opt -> NEL.singleton ("out", opt)
  ContentAddressed _ os -> os

mkDerivation ::
  (NamedHashAlgo a, MonadDeriv m) =>
  MkDerivationArg a m ->
  m (BuilderArg m) ->
  -- | build
  System ->
  -- | host
  System ->
  -- | target
  System ->
  ExtendDeriv m
mkDerivation mda ba buildPlatform hostPlatform targetPlatform =
  let outOpts = outputOpts mda
      drv = ND.derivation $ do
        sh <- shell mda
        baS <- ba
        propS <-
          traverse
            ( \(n, OutputArg {oArgPropagatedDeps = dep}) ->
                let depDrv =
                      getDependenciesDrv
                        buildPlatform
                        hostPlatform
                        targetPlatform
                        dep
                 in if depDrv == emptySimpleDeps
                      then pure (pure ())
                      else
                        let dest = quoteTerms [var (Var (toDrvStr n)), "/nix-support"]
                         in traverse
                              ( \(name, d) ->
                                  traverse storePathStr d <&> \ds ->
                                    unless (null ds) $
                                      echoCmd [str (listToStr ds)]
                                        |> quoteTerms [dest, "/", name]
                              )
                              [ ("propagated-build-build-deps", depsBuildBuild depDrv),
                                ("propagated-native-build-inputs", depsBuildHost depDrv),
                                ("propagated-build-target-deps", depsBuildTarget depDrv),
                                ("propagated-host-host-deps", depsHostHost depDrv),
                                ("propagated-build-inputs", depsHostTarget depDrv),
                                ("propagated-target-target-deps", depsTargetTarget depDrv)
                              ]
                              <&> \s ->
                                runCmd_ "mkdir" ["-p", dest] >> sequenceA_ s
            )
            outOpts
        pure
          ( ( ND.defaultDrvArg
                ( mconcat
                    [ pName mda,
                      if isStatic hostPlatform then "-static" else mempty,
                      if buildPlatform /= hostPlatform
                        then "-" <> toConfigTriple hostPlatform
                        else mempty,
                      maybe mempty ("-" <>) (version mda)
                    ]
                )
                sh
                (toNixSys buildPlatform)
            )
              { ND.drvArgs = ["-c", "$buildScript"],
                ND.drvType = case derivType mda of
                  InputAddressed os -> ND.InputAddressed (fmap fst os)
                  FixedOutput m o _ -> ND.FixedOutput m o
                  ContentAddressed m os -> ND.ContentAddressed m (fmap fst os),
                ND.drvEnv =
                  let cert = fromMaybe "/no-cert-file.crt" (sslCertFile mda)
                   in ("SOURCE_DATE_EPOCH", fromString (show (sourceDateEpoch mda)))
                        : ("NIX_SSL_CERT_FILE", cert)
                        : ("SSL_CERT_FILE", cert)
                        : Typ.toDrvEnvs (buildEnv baS <> extraEnv mda),
                ND.drvPassAsFile =
                  ( "buildScript",
                    script (Just sh) $ do
                      b <- builder baS
                      runCmd_ b []
                      sequenceA_ propS
                  )
                    : Typ.toDrvEnvs (buildPassAsFile baS <> extraPassAsFile mda)
              }
          )
   in ExtDrv
        { eDrvBaseDrv = drv,
          eDrvOutputs =
            fmap
              ( \(n, a) ->
                  DrvOutput
                    { dOutParentDrv = drv,
                      dOutName = Just n,
                      dOutHasExecutable = oArgHasExecutable a,
                      dOutHasData = oArgHasData a,
                      dOutSetupHook = oArgSetupHook a,
                      dOutPropagatedDeps =
                        getDependenciesDrv
                          buildPlatform
                          hostPlatform
                          targetPlatform
                          (oArgPropagatedDeps a)
                    }
              )
              outOpts
        }