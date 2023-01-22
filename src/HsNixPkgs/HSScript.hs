{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module HsNixPkgs.HSScript
  ( CodeM,
    ModuleM,
    declFun,
    mainFunc,
    useModule,
    HsModule,
    stringModule,
    declModule,
    HsExecutable (..),
    hsExecutable,
  )
where

import Control.Monad.RWS.Strict
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Set as S
import Data.String
import HsNixPkgs.HSScript.Internal.BuildMod
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

newtype CodeM a = CM (State Int a)
  deriving (Functor, Applicative, Monad)

instance Quote CodeM where
  newName n = CM (state (\i -> (i, i + 1)) <&> mkNameU n . fromIntegral)

type PkgDep s = (HM.HashMap String s, HS.HashSet String)

type DepM s = WriterT (PkgDep s) (State (HM.HashMap String Bool))

data Mod s = Mod
  { depM :: DepM s (),
    modExt :: S.Set Extension,
    modDec :: [Dec] -> [Dec]
  }

instance Semigroup (Mod s) where
  l <> r =
    Mod
      { depM = depM l >> depM r,
        modExt = S.union (modExt l) (modExt r),
        modDec = modDec l . modDec r
      }

instance Monoid (Mod s) where
  mempty =
    Mod
      { depM = pure (),
        modExt = S.empty,
        modDec = id
      }

data MainModule

data NormalModule

newtype ModuleM s t a = M (RWS String (Mod s) Int a)
  deriving (Functor, Applicative, Monad)

runModuleM :: String -> ModuleM s t a -> (a, (DepM s (), [Extension]), [Dec])
runModuleM n (M m) =
  let (v, d) = evalRWS m n 0
   in (v, (depM d, S.toList (modExt d)), modDec d [])

declFun :: [Extension] -> CodeM [Dec] -> ModuleM s t Name
declFun e (CM md) =
  M
    ( do
        ds <- state (runState md)
        tell (Mod {depM = pure (), modExt = S.fromList e, modDec = (ds ++)})
        Name
          ( OccName
              ( showName
                  ( case last ds of
                      FunD n _ -> n
                      ValD (VarP n) _ _ -> n
                      TySynD n _ _ -> n
                      d -> error ("Unsupported decl: " ++ show d)
                  )
              )
          )
          . NameQ
          . ModName
          <$> ask
    )

mainFunc :: [Extension] -> CodeM Exp -> ModuleM s MainModule Exp
mainFunc e (CM c) =
  M
    ( tell (mempty {modExt = S.fromList e})
        >> state (runState c)
    )

useModule :: HsModule s NormalModule a -> ModuleM s t a
useModule m = M (modVal m <$ tell (mempty {depM = modDepM m}))

data HsModule s t a = HSModule
  { modDepM :: DepM s (),
    modVal :: a
  }

addDep :: String -> PkgDep s -> DepM s () -> DepM s ()
addDep name val dm =
  gets (HM.lookup name) >>= \case
    Just True -> pure ()
    Just False -> error "cycle detected in module dependency graph"
    Nothing -> dm >> tell val >> modify (HM.insert name True)

stringModule :: String -> s -> [String] -> a -> HsModule s NormalModule a
stringModule name text p v =
  HSModule
    { modDepM =
        addDep
          name
          (HM.singleton name text, HS.fromList p)
          (pure ()),
      modVal = v
    }

declModule ::
  IsString s =>
  String ->
  ModuleM s NormalModule a ->
  HsModule s NormalModule a
declModule name c =
  let (v, (dm, ext), decs) = runModuleM name c
      (dt, pd) = buildMod name ext decs
   in HSModule
        { modDepM =
            addDep name (HM.singleton name (fromString dt), pd) $
              modify (HM.insert name False) >> dm,
          modVal = v
        }

data HsExecutable s = HsExecutable
  { execMainModule :: s,
    execOtherModules :: HM.HashMap String s,
    execDependencies :: HS.HashSet String
  }
  deriving (Show)

hsExecutable :: IsString s => ModuleM s MainModule Exp -> HsExecutable s
hsExecutable m =
  let (e, (dm, ext), decs) = runModuleM "Main" m
      (om, opd) = evalState (execWriterT dm) HM.empty
      (dt, pd) =
        buildMod
          "Main"
          ext
          ( let main = mkName "main"
             in SigD main (ConT ''IO `AppT` TupleT 0)
                  : FunD main [Clause [] (NormalB e) []]
                  : decs
          )
   in HsExecutable
        { execMainModule = fromString dt,
          execOtherModules = om,
          execDependencies = HS.union opd pd
        }