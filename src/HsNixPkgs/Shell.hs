{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HsNixPkgs.Shell
  ( Script,
    ScriptTerm,
    module System.Shell,
    runCmd_,
    ifTrue,
    varQ,
    outputQ,
    orCC,
    andCC,
    Hook (..),
    runWithHook,
  )
where

import Data.Maybe
import Data.Text (Text)
import HsNix.Derivation as D
import System.Shell hiding (toStr)
import qualified System.Shell as S
import System.Shell.Bash as B

type Script m = BashScript (DrvStr m)

type ScriptTerm m = Term (DrvStr m) (BashScript (DrvStr m))

instance (MonadDeriv m) => Quotable (DrvStr m) where
  quote t =
    fmap
      ( \case
          D.QStr s -> B.QStr s
          D.QEscape c -> B.QEscape c
      )
      (D.quote B.quoteChar t)

instance (MonadDeriv m) => ShellStr (DrvStr m) where
  type Builder (DrvStr m) = DrvStrBuilder m
  fromStr = fromDrvStr
  toStr = toDrvStr

runCmd_ :: Shell t m => Term t m -> [Term t m] -> m ()
runCmd_ = runCmd []

ifTrue :: Shell t m => m () -> m () -> m ()
ifTrue c t = ifCmd c t (pure ())

varQ :: Var t -> Term t m
varQ = S.quote . var

outputQ :: m () -> Term t m
outputQ = S.quote . output

orCC :: Shell t m => m () -> m () -> m ()
l `orCC` r = andOrCmd (aoTerm l `orC` aoTerm r)

andCC :: Shell t m => m () -> m () -> m ()
l `andCC` r = andOrCmd (aoTerm l `andC` aoTerm r)

data Hook p m = Hook
  { preHook, postHook :: Maybe (Script m ())
  }

instance Semigroup (Hook p m) where
  hl <> hr =
    Hook
      { preHook = merge (preHook hl) (preHook hr),
        postHook = merge (postHook hl) (postHook hr)
      }
    where
      merge (Just l) (Just r) = Just (l >> r)
      merge l Nothing = l
      merge Nothing r = r

instance Monoid (Hook p m) where
  mempty = Hook {preHook = Nothing, postHook = Nothing}

runWithHook :: MonadDeriv m => Text -> Hook p m -> Script m () -> Script m ()
runWithHook name hook f = do
  pre <-
    funcDef
      (toDrvStr ("pre_" <> name))
      (fromMaybe (return ()) (preHook hook))
  post <-
    funcDef
      (toDrvStr ("post_" <> name))
      (fromMaybe (return ()) (postHook hook))
  runCmd_ pre [] >> f >> runCmd_ post []
