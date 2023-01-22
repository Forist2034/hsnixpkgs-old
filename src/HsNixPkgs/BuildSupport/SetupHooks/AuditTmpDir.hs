module HsNixPkgs.BuildSupport.SetupHooks.AuditTmpDir (auditTmpDir) where

import HsNixPkgs.ExtendDrv
import HsNixPkgs.Shell
import HsNixPkgs.Shell.Util

auditTmpDir :: MonadDeriv m => Script m (ScriptTerm m -> Script m ())
auditTmpDir =
  pure
    ( \d -> do
        let tmpDir = Var "TMPDIR"
        echoCmd ["checking for references to ", var tmpDir, "/ in ", d, "..."]
        forFind
          (quote d)
          ["-type", "f"]
          ( \i ->
              do
                ifTrue
                  (test (varQ i `TStrMatch` ".build-id"))
                  continueCmd
                ifTrue
                  (isELF (var i))
                  ( ifTrue
                      ( group (runCmd_ "printf" [":"] >> runCmd_ "patchelf" ["--print-rpath", quote (var i)])
                          -|- grepQ ["-F", quoteTerms [":", var tmpDir, "/"]]
                      )
                      ( echoCmd ["RPATH of binary ", var i, " contains a forbidden reference to ", var tmpDir, "/"]
                          >> exitCmd ["1"]
                      )
                  )
                ifTrue
                  (isScript (varQ i))
                  ( ifTrue
                      ( test
                          ( TFileExists
                              ( quoteTerms
                                  [ output (runCmd_ "dirname" [quote (var i)]),
                                    "/.",
                                    output (runCmd_ "basename" [quote (var i)]),
                                    "-wrapped"
                                  ]
                              )
                          )
                      )
                      ( ifTrue
                          (grepQ ["-F", quoteTerms [var tmpDir, "/"], quote (var i)])
                          ( echoCmd ["wrapper script ", var i, " contains a forbidden reference to ", var tmpDir, "/"]
                              >> exitCmd ["1"]
                          )
                      )
                  )
          )
    )