module HsNixPkgs.BuildSupport.SetupHooks.MultipleOutput
  ( MultiOutArg (..),
    defMultiOutArg,
    multiOutDocs,
    multiOutDevs,
  )
where

import Data.Text (Text)
import HsNix.Derivation hiding (quote)
import HsNixPkgs.Shell
import HsNixPkgs.Shell.Util

data MultiOutArg = MultiOutArg
  { outputDev,
    outputBin,
    outputInclude,
    outputLib,
    outputDoc,
    outputDevDoc,
    outputMan,
    outputDevMan,
    outputInfo ::
      Text,
    shareDocName :: Text
  }

defMultiOutArg :: [Text] -> Text -> MultiOutArg
defMultiOutArg os sdn =
  let dev = getOrDefault "dev" "out"
      bin = getOrDefault "bin" "out"
      man = getOrDefault "man" bin
   in MultiOutArg
        { outputDev = dev,
          outputBin = bin,
          outputInclude = getOrDefault "include" dev,
          outputLib = getOrDefault "lib" "out",
          outputDoc = getOrDefault "doc" "out",
          outputDevDoc = "devdoc",
          outputMan = man,
          outputDevMan = getOrDefault "devman" (getOrDefault "devdoc" man),
          outputInfo = getOrDefault "info" bin,
          shareDocName = sdn
        }
  where
    getOrDefault v d =
      if v `elem` os then v else d

move :: Shell t m => Term t m -> Term t m -> Term t m -> m ()
move srcOut srcPat destOut =
  forCmdVar
    "srcPath"
    [srcOut <> srcPat]
    ( \srcPath -> do
        ifTrue
          ( test
              ( TNot (TDirExists (varQ srcPath))
                  `TAnd` TNot
                    (TSymbolicLink (varQ srcPath))
              )
          )
          continueCmd
        destPath <-
          localVar
            ( "destPath"
                @= quoteTerms
                  [ destOut,
                    varTrimPrefix srcPath ShortestM srcOut
                  ]
            )
        echoCmd ["moving from ", var srcPath, " to ", var destPath]
        ifCmd
          (test (TDirExists (varQ srcPath) `TAnd` TDirExists (varQ destPath)))
          ( do
              runCmd_ "rmdir" [varQ srcPath, "--ignore-fail-on-non-empty"]
              ifTrue (test (TDirExists (varQ srcPath))) $ do
                runCmd_ "mv" ["-t", varQ destPath, var srcPath <> "/*"]
                runCmd_ "rmdir" [varQ srcPath]
          )
          ( do
              runCmd_
                "mkdir"
                [ "-p",
                  outputQ
                    ( runCmd_ "readlink" ["-m", quoteTerms [var destPath, "/.."]]
                    )
                ]
              runCmd_ "mv" [varQ srcPath, varQ destPath]
          )

        srcParent <-
          localVar
            ( "srcParent"
                @= outputQ
                  (runCmd_ "readlink" ["-m", quoteTerms [var srcPath, "/.."]])
            )
        ifTrue (runCmd_ "rmdir" [varQ srcParent]) $ do
          echoCmd ["Removing empty ", var srcParent, "/ and (possibly) its parents"]
          runCmd_
            "rmdir"
            [ "-p",
              "--ignore-fail-on-non-empty",
              outputQ (runCmd_ "readlink" ["-m", quoteTerms [var srcParent, "/.."]])
            ]
    )

asVar :: MonadDeriv m => Text -> ScriptTerm m
asVar = var . Var . toDrvStr

multiOutDocs ::
  MonadDeriv m =>
  MultiOutArg ->
  [Text] ->
  [(Script m (ScriptTerm m -> Script m ()), [Text])]
multiOutDocs moa os =
  [ mov "share/info" (outputInfo moa),
    mov "share/doc" (outputDoc moa),
    mov "share/gtk-doc" (outputDevDoc moa),
    mov "share/devhelp/books" (outputDevDoc moa),
    mov "share/man" (outputMan moa),
    mov "share/man/man3" (outputDevMan moa)
  ]
  where
    mov pat dest = (pure (\s -> move s pat (asVar dest)), filter (/= dest) os)

multiOutDevs :: MonadDeriv m => MultiOutArg -> Script m (ScriptTerm m -> Script m ())
multiOutDevs moa =
  pure
    ( \s -> do
        let dev = asVar (outputDev moa)
            include = asVar (outputInclude moa)
        move s "include" include
        move s "lib/pkgconfig" dev
        move s "share/pkgconfig" dev
        move s "lib/cmake" dev
        move s "share/aclocal" dev
        forCmdVar
          "f"
          [quote dev <> "/{lib,share}/pkgconfig/*.pc"]
          ( \f -> do
              echoCmd ["Patching ", var f, " includedir to output ", include]
              runCmd_ "sed" ["-i", quoteTerms ["/^includedir=/s,=${prefix},=", include, ","], varQ f]
          )
    )