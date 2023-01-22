module HsNixPkgs.BuildSupport.SetupHooks.CompressManPages (compressManPages) where

import HsNixPkgs.ExtendDrv (MonadDeriv)
import HsNixPkgs.Shell
import HsNixPkgs.Shell.Util

compressManPages :: MonadDeriv m => Script m (ScriptTerm m -> Script m ())
compressManPages =
  pure
    ( \d ->
        do
          echoCmd ["gzipping man pages under ", d, "/share/man"]
          forFind
            (quoteTerms [d, "/share/man"])
            ["-type", "f", "-a", quote "!", "-regex", quote ".*\\.\\(bz2\\|gz\\|xz\\)$"]
            ( \f ->
                ifCmd
                  (runCmd_ "gzip" ["-c", "-n", quote (var f)] |> quoteTerms [var f, ".gz"])
                  (runCmd_ "rm" [var f])
                  (runCmd_ "rm" [quoteTerms [var f, ".gz"]])
            )
          forOutputLine
            ( findCmd (quoteTerms [d, "/share/man/"]) ["-type", "l", "-a", quote "!", "-regex", quote ".*\\.\\(bz2\\|gz\\|xz\\)$"]
                -|- runCmd_ "sort" ["-z"]
            )
            ( \f -> do
                target <-
                  localVar
                    ( "target"
                        @= quote
                          ( output
                              (runCmd_ "readlink" ["-f", quote (var f)])
                          )
                    )
                ifTrue
                  (test (TRegularFile (quoteTerms [var target, ".gz"])))
                  ( runCmd_ "ln" ["-sf", quoteTerms [var target, ".gz"], quoteTerms [var f, ".gz"]]
                      `andCC` runCmd_ "rm" [quote (var f)]
                  )
            )
    )