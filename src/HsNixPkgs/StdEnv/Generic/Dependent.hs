{-# LANGUAGE LambdaCase #-}

module HsNixPkgs.StdEnv.Generic.Dependent
  ( getDependenciesDrv,
    propagateDep,
    setupDeps,
  )
where

import Control.Monad.State
import Control.Monad.Writer
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Maybe
import GHC.Stack
  ( CallStack,
    HasCallStack,
    callStack,
    prettyCallStack,
  )
import HsNix.Derivation hiding (storePathStr)
import HsNixPkgs.ExtendDrv
import HsNixPkgs.System
import qualified HsNixPkgs.Types as Typ

getDependenciesDrv ::
  -- | build
  System ->
  -- | host
  System ->
  -- | target
  System ->
  SimpleDeps (System -> System -> DrvOutput m) ->
  SimpleDeps (DrvOutput m)
getDependenciesDrv buildP hostP targetP dep =
  SimpleDeps
    { depsBuildBuild = fmap (\f -> f buildP buildP) (depsBuildBuild dep),
      depsBuildHost = fmap (\f -> f buildP hostP) (depsBuildHost dep),
      depsBuildTarget = fmap (\f -> f buildP targetP) (depsBuildTarget dep),
      depsHostHost = fmap (\f -> f hostP hostP) (depsHostHost dep),
      depsHostTarget = fmap (\f -> f hostP targetP) (depsHostTarget dep),
      depsTargetTarget = fmap (\f -> f targetP targetP) (depsTargetTarget dep)
    }

type Offset = (Int, Int)

transitive :: Offset -> Offset -> Offset
transitive o1 (h1, t1) = (mapOffset o1 h1, mapOffset o1 t1)
  where
    mapOffset (h, t) i = i + (if i <= 0 then h else t - 1)

canPropagate :: Offset -> Offset -> Bool
canPropagate (h0, t0) (h1, t1) = inRange (h0 + h1) && inRange (t0 + t1)
  where
    inRange v = v == -1 || v == 0 || v == 1

newtype Result m = Res
  { getRes ::
      M.Map
        Offset
        ([DrvOutput m] -> [DrvOutput m])
  }

instance (MonadDeriv m) => Semigroup (Result m) where
  Res l <> Res r = Res (M.unionWith (.) l r)

instance (MonadDeriv m) => Monoid (Result m) where
  mempty = Res M.empty

type PropM m = WriterT (Result m) (State (HM.HashMap (Offset, DrvOutput m) Bool))

propagateVertex ::
  (MonadDeriv m) =>
  CallStack ->
  Offset ->
  DrvOutput m ->
  PropM m ()
propagateVertex root o d@DrvOutput {dOutPropagatedDeps = dep} =
  gets (HM.lookup (o, d)) >>= \case
    Just True -> return () -- visited and finished
    Just False ->
      -- visited but not finished
      error ("Cycle detected when propagating dependencies\n" ++ prettyCallStack root)
    Nothing -> do
      modify (HM.insert (o, d) False)

      propDep (-1, -1) (depsBuildBuild dep)
      propDep (-1, 0) (depsBuildHost dep)
      propDep (-1, 1) (depsBuildTarget dep)
      propDep (0, 0) (depsHostHost dep)
      propDep (0, 1) (depsHostTarget dep)
      propDep (1, 1) (depsTargetTarget dep)

      modify (HM.insert (o, d) True)
      tell (Res (M.singleton o (d :)))
  where
    propDep od ds =
      when (canPropagate o od) $
        propagateEdge root (transitive o od) ds

propagateEdge :: (MonadDeriv m) => CallStack -> Offset -> [DrvOutput m] -> PropM m ()
propagateEdge root o = traverse_ (propagateVertex root o)

propagateDep :: (HasCallStack, MonadDeriv m) => [SimpleDeps (DrvOutput m)] -> SimpleDeps (DrvOutput m)
propagateDep dep =
  SimpleDeps
    { depsBuildBuild = lookupDef (-1, -1),
      depsBuildHost = lookupDef (-1, 0),
      depsBuildTarget = lookupDef (-1, 1),
      depsHostHost = lookupDef (0, 0),
      depsHostTarget = lookupDef (0, 1),
      depsTargetTarget = lookupDef (1, 1)
    }
  where
    cs = callStack

    proDep =
      getRes
        ( evalState
            ( execWriterT
                ( traverse_
                    ( \d -> do
                        propagateEdge cs (-1, -1) (depsBuildBuild d)
                        propagateEdge cs (-1, 0) (depsBuildHost d)
                        propagateEdge cs (-1, 1) (depsBuildTarget d)
                        propagateEdge cs (0, 0) (depsHostHost d)
                        propagateEdge cs (0, 1) (depsBuildTarget d)
                        propagateEdge cs (1, 1) (depsTargetTarget d)
                    )
                    dep
                )
            )
            HM.empty
        )
    lookupDef k = maybe [] (\f -> reverse (f [])) (M.lookup k proDep)

runSetupHooks :: SimpleDeps (DrvOutput m) -> SetupHook m
runSetupHooks dep =
  foldMap
    mconcat
    ( let run ho to = mapMaybe (fmap (\f -> f ho to) . dOutSetupHook)
       in [ run (-1) (-1) (depsBuildBuild dep),
            run (-1) 0 (depsBuildHost dep),
            run (-1) 1 (depsBuildTarget dep),
            run 0 0 (depsHostHost dep),
            run 0 1 (depsHostTarget dep),
            run 1 1 (depsTargetTarget dep)
          ]
    )

setupDeps :: MonadDeriv m => SimpleDeps (DrvOutput m) -> m (SetupHook m)
setupDeps sd = do
  let db = concat [depsBuildBuild sd, depsBuildHost sd, depsBuildTarget sd]
  p <-
    traverse
      (fmap (\sp -> fromDrvStr sp <> "/bin") . storePathStr)
      (filter dOutHasExecutable db)
  sh <-
    traverse
      (fmap (\sp -> fromDrvStr sp <> "/share") . storePathStr)
      (filter dOutHasData db)
  hp <-
    traverse
      (fmap (\sp -> fromDrvStr sp <> "/bin") . storePathStr)
      (filter dOutHasExecutable (depsHostHost sd ++ depsHostTarget sd))
  pure
    ( def
        { hookEnv =
            Typ.fromList
              [ ("PATH", (<> mconcat p)),
                ("XDG_DATA_DIR", (<> mconcat sh)),
                ("HOST_PATH", (<> mconcat hp))
              ]
        }
        <> runSetupHooks sd
    )
