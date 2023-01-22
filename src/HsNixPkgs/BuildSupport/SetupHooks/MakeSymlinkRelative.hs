module HsNixPkgs.BuildSupport.SetupHooks.MakeSymlinkRelative
  ( makeSymlinkRelative,
  )
where

import HsNixPkgs.ExtendDrv
import HsNixPkgs.Shell
import HsNixPkgs.Shell.Util

makeSymlinkRelative :: MonadDeriv m => Script m (ScriptTerm m -> Script m ())
makeSymlinkRelative =
  pure
    ( \d -> do
        target <- localVar ("symlinkTarget" @= "")
        i <- localVar ("i" @= "")
        whileCmd
          (readR [i])
          ( do
              setVar (target @= output (runCmd_ "readlink" [varQ i]))
              ifTrue
                (test ((quote (var target) <> "/") `TStrNotEqual` (quote d <> "/*")))
                continueCmd
              ifTrue
                (test (TNot (TFileExists (varQ target))))
                (echoCmd ["the symlink ", var i, " is broken, it points to ", var target, " (which is missing)"])
              echoCmd ["rewriting symlink ", var i, " to be relative to ", d]
              runCmd_ "ln" ["-snrf", varQ target, varQ i]
          )
          <| output (findCmd d ["-type", "l"])
    )