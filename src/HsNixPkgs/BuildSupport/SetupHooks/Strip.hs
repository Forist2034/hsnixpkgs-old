module HsNixPkgs.BuildSupport.SetupHooks.Strip
  ( StripList (..),
    StripArg (..),
    stripCmd,
  )
where

import Data.Default
import Data.Foldable (traverse_)
import HsNix.Derivation hiding (quote)
import HsNixPkgs.Shell
import HsNixPkgs.Shell.Util

data StripList m = StripList
  { stripList :: [ScriptTerm m],
    stripFlags :: [ScriptTerm m]
  }

data StripArg m = StripArg
  { stripDebugList :: StripList m,
    stripAllList :: StripList m
  }

instance (MonadDeriv m) => Default (StripArg m) where
  def =
    StripArg
      { stripDebugList =
          StripList
            { stripList = ["lib", "lib32", "lib64", "libexec", "bin", "sbin"],
              stripFlags = ["--strip-debug"]
            },
        stripAllList =
          StripList
            { stripList = [],
              stripFlags = ["--strip-all"]
            }
      }

stripCmd ::
  MonadDeriv m =>
  -- | strip arg for host
  Maybe (StripArg m) ->
  -- | strip arg for target
  Maybe (StripArg m) ->
  Script m (ScriptTerm m -> Script m ())
stripCmd Nothing Nothing = pure (const (pure ()))
stripCmd sah sat = do
  doStripF <- funcDef "_doStrip" $ do
    prefix <- localVar ("prefix" @= var "1")
    doStrip (var prefix) (var "STRIP") (var "RANLIB") sah
    doStrip (var prefix) (var "STRIP_FOR_TARGET") (var "STRIP_FOR_TARGET") sat
  return (\t -> runCmd_ doStripF [t])
  where
    stripDir pref strip ranlib slist =
      traverse_
        ( \p ->
            let path = quoteTerms [pref, "/", p]
             in ifTrue
                  (test (TFileExists path))
                  ( do
                      echoCmd
                        ( ["stripping (with command ", strip, " and flags "]
                            ++ stripFlags slist
                            ++ [") in ", path]
                        )
                      findCmdExec
                        path
                        ["-type", "f"]
                        (\it -> [strip : (stripFlags slist ++ [it])])
                      findCmdExec
                        path
                        ["-name", quote "*.a", "type", "-f"]
                        (\it -> [[ranlib, it]])
                  )
        )
        (stripList slist)
    doStrip pref strip ranlib sa =
      case sa of
        Just s -> do
          stripDir pref strip ranlib (stripDebugList s)
          stripDir pref strip ranlib (stripAllList s)
        Nothing -> return ()