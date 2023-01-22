module HsNixPkgs.BuildSupport.SetupHooks.Move
  ( moveLib64,
    moveSbin,
    moveSystemdUserUnit,
    ForceShare (..),
    moveToShare,
  )
where

import Data.Default
import Data.Foldable (traverse_)
import HsNixPkgs.ExtendDrv
import HsNixPkgs.Shell
import HsNixPkgs.Shell.Util

move :: (Shell t m, Applicative f) => Term t m -> Term t m -> f (Term t m -> m ())
move from to =
  pure
    ( \d ->
        let src = quoteTerms [d, from]
            dest = quoteTerms [d, to]
         in ifTrue
              (test (TNot (TDirExists src `TOr` TSymbolicLink src)))
              ( do
                  echoCmd ["moving ", src, "/* to ", dest]
                  runCmd_ "mkdir" ["-p", dest]
                  subShell $ do
                    runCmd_ "shopt" ["-s", "dotglob"]
                    forCmdVar
                      "i"
                      [src <> "/*"]
                      ( \i ->
                          runCmd_ "mv" [quote (var i), dest]
                      )
                  runCmd_ "rmdir" [src]
                  runCmd_ "ln" ["-s", dest, src]
              )
    )

moveLib64 :: MonadDeriv m => Script m (ScriptTerm m -> Script m ())
moveLib64 = move "/lib64" "/lib"

moveSbin :: MonadDeriv m => Script m (ScriptTerm m -> Script m ())
moveSbin = move "/sbin" "/bin"

moveSystemdUserUnit :: MonadDeriv m => Script m (ScriptTerm m -> Script m ())
moveSystemdUserUnit = move "/lib/systemd/user" "/share/systemd/user"

newtype ForceShare m = FS [ScriptTerm m]

instance (MonadDeriv m) => Default (ForceShare m) where
  def = FS ["man", "doc", "info"]

moveToShare :: MonadDeriv m => ForceShare m -> Script m (ScriptTerm m -> Script m ())
moveToShare (FS p) =
  pure
    ( \d ->
        traverse_
          ( \pa ->
              let src = quoteTerms [d, "/", pa]
                  dest = quoteTerms [d, "/share/", pa]
               in ifTrue
                    (test (TDirExists src))
                    ( ifCmd
                        (test (TDirExists dest))
                        (echoCmd ["both ", src, " and ", dest, " exist!"])
                        ( do
                            echoCmd ["moving ", src, " to ", dest]
                            runCmd_ "mkdir" ["-p", quoteTerms [pa, "/share"]]
                            runCmd_ "mv" [src, dest]
                        )
                    )
          )
          p
    )