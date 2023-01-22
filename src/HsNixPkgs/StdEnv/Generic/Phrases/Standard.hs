{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module HsNixPkgs.StdEnv.Generic.Phrases.Standard
  ( SourceRootArg (..),
    UnpackArg (..),
    unpackPhrase,
    PatchArg (..),
    patchPhrase,
    MultiOutArg (..),
    defMultiOutArg,
    ConfigureArg (..),
    defConfigureArg,
    configurePhrase,
    MakeArg (..),
    BuildArg (..),
    buildPhrase,
    CheckArg (..),
    defCheckArg,
    checkPhrase,
    InstallArg (..),
    installPhrase,
    DefFixupCmdArg (..),
    defFixupCmd,
    FixupArg (..),
    defFixupArg,
    fixupPhrase,
    InstallCheckArg (..),
    installCheckPhrase,
    DistArg (..),
    distPhrase,
    StdPhrasesArg (..),
    mkStdPhrases,
    StdPhraseHook (..),
    StdPhraseDrv (..),
    stdPhraseMkDrv,
  )
where

import Control.Arrow ((>>>))
import Control.Monad
import Data.Bifunctor
import Data.Default
import Data.Foldable (traverse_)
import Data.Functor
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.List.NonEmpty hiding ((<|))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import HsNix.Builtin.AddFile
import qualified HsNix.Derivation as D
import HsNixPkgs.BuildSupport.SetupHooks.AuditTmpDir
import HsNixPkgs.BuildSupport.SetupHooks.CompressManPages
import HsNixPkgs.BuildSupport.SetupHooks.MakeSymlinkRelative
import HsNixPkgs.BuildSupport.SetupHooks.Move
import HsNixPkgs.BuildSupport.SetupHooks.MultipleOutput
import HsNixPkgs.BuildSupport.SetupHooks.PatchShebang
import HsNixPkgs.BuildSupport.SetupHooks.PruneLibtoolFiles
import HsNixPkgs.BuildSupport.SetupHooks.Strip
import HsNixPkgs.ConfigSet
import HsNixPkgs.ExtendDrv
import HsNixPkgs.Shell
import HsNixPkgs.Shell.Util
import HsNixPkgs.StdEnv.Generic.Dependent
import qualified HsNixPkgs.StdEnv.Generic.MakeDerivation as MD
import HsNixPkgs.StdEnv.Generic.Phrases
import HsNixPkgs.System
import qualified HsNixPkgs.Types as Typ
import HsNixPkgs.Util

hookToScript :: (MonadDeriv m) => Text -> Hook p m -> Script m () -> Script m (ScriptTerm m)
hookToScript name hook f =
  funcDef (toDrvStr name <> "_phrase") $
    runWithHook name hook f

data SourceRootArg m
  = SourceRoot Text
  | SetSourceRoot (Script m ())

data UnpackP

data UnpackArg m = UnpackArg
  { unpackHooks :: Hook UnpackP m,
    unpackCmd :: Maybe ((Bool, Text) -> DrvStr m -> Script m ()),
    makeSourcesWritable :: Bool
  }

instance Default (UnpackArg m) where
  def =
    UnpackArg
      { unpackHooks = mempty,
        unpackCmd = Nothing,
        makeSourcesWritable = True
      }

unpackPhrase ::
  (MonadDeriv m) =>
  NonEmpty (FileDeriv m) ->
  SourceRootArg m ->
  UnpackArg m ->
  m (Phrase m)
unpackPhrase src sRoot d = do
  srcS <- traverse (\s -> ((fDrvIsDirectory s, fDrvPath s),) <$> D.storePathStr (fDrvBase s)) src
  pure $
    Phrase
      { phraseName = "unpack_phrase",
        phraseDesc = "unpacking sources",
        phraseEnv =
          Typ.fromList
            ( ( case fmap snd srcS of
                  h :| [] -> ("src", (fromDrvStr h <>))
                  v -> ("srcs", (fromDrvStr (listToStr (toList v)) <>))
              )
                : ( case sRoot of
                      SourceRoot sr -> [("sourceRoot", (fromDrvStr (toDrvStr sr) <>))]
                      _ -> []
                  )
            ),
        phrasePassAsFile = mempty,
        phraseScript =
          hookToScript "unpack" (unpackHooks d) $ do
            traverse_ unpackFile srcS
            sr <- case sRoot of
              SourceRoot s -> pure (str (toDrvStr s))
              SetSourceRoot c ->
                (var <$> newEnv ("sourceRoot" @= "")) <* c
            echoCmd ["source root is ", sr]
            when (makeSourcesWritable d) $
              runCmd_ "chmod" ["-R", "u+w", "--", quote sr]
            runCmd_ "cd" [sr]
      }
  where
    unpackFile ((isDir, n), drv) = do
      echoCmd ["unpacking source archive ", str drv]
      if isDir
        then
          runCmd
            []
            "cp"
            ["-pr", "--reflink=auto", "--", quote (str drv), quote (str (toDrvStr n))]
        else
          if
              | ends n [".tar.xz", ".tar.lzma", ".tzx"] ->
                  (runCmd_ "xz" ["-d"] <| str drv)
                    -|- runCmd_ "tar" ["xf", "-", "--warning=no-timestamp"]
              | ends n [".tar", ".tgz", ".tbz2", ".tbz"]
                  || T.isInfixOf "tar" n ->
                  runCmd_ "tar" ["xf", quote (str drv), "--warning=no-timestamp"]
              | otherwise ->
                  case unpackCmd d of
                    Just c -> c (isDir, n) drv
                    Nothing -> error ("Unknown how to unpack " ++ show drv)
      where
        ends pat = any (`T.isSuffixOf` pat)

data PatchP

data PatchArg m = PatchArg
  { patchFlags :: [ScriptTerm m],
    patchHook :: Hook PatchP m
  }

instance (MonadDeriv m) => Default (PatchArg m) where
  def =
    PatchArg
      { patchFlags = ["-p"],
        patchHook = mempty
      }

data Patch m
  = StrPatch Text Text
  | DrvPatch (DrvOutput m) [(Text, DrvStr m)]

patchPhrase :: (BuiltinAddText m) => [Patch m] -> PatchArg m -> m (Phrase m)
patchPhrase patches pa =
  traverse
    ( \case
        StrPatch n p -> L.singleton . (n,) <$> addTextFileStr n p
        DrvPatch d ps ->
          storePathStr d <&> \sp ->
            fmap (second ((sp <> "/") <>)) ps
    )
    patches
    <&> \pas ->
      Phrase
        { phraseName = "patch_phrase",
          phraseDesc = "patching sources",
          phraseEnv = mempty,
          phrasePassAsFile = mempty,
          phraseScript =
            hookToScript "patch" (patchHook pa) $
              traverse_
                ( \(n, drv) ->
                    let hasSuf s = T.isSuffixOf s n
                     in ( if
                              | hasSuf ".gz" -> runCmd_ "gzip" ["-d"]
                              | hasSuf ".bz2" -> runCmd_ "bzip2" ["-d"]
                              | hasSuf ".xz" -> runCmd_ "xz" ["-d"]
                              | hasSuf ".lzma" -> runCmd_ "lzma" ["-d"]
                              | otherwise -> runCmd_ "cat" []
                        )
                          <| quote (str drv)
                            -|- runCmd_ "patch" (patchFlags pa)
                )
                (concat pas)
        }

data ConfigureP

data ConfigureArg m = ConfigArg
  { configureScript :: DrvStr m,
    configureFlags :: [ScriptTerm m],
    prefix :: Maybe (ScriptTerm m),
    prefixKey :: Text,
    addStaticConfigureFlags,
    addDisableDepTrack,
    fixLibtool,
    disableStatic ::
      Bool,
    configurePlatform :: [(Text, Text)],
    configureHook :: Hook ConfigureP m
  }

defConfigureArg :: MonadDeriv m => System -> System -> ConfigureArg m
defConfigureArg b h =
  ConfigArg
    { configureScript = "./configure",
      configureFlags = [],
      prefix = Just (var "out"),
      prefixKey = "--prefix",
      addStaticConfigureFlags = True,
      addDisableDepTrack = True,
      fixLibtool = True,
      disableStatic = True,
      configurePlatform =
        [ ("build", toConfigTriple b),
          ("host", toConfigTriple h)
        ],
      configureHook = mempty
    }

configurePhrase :: (MonadDeriv m) => Maybe MultiOutArg -> ConfigureArg m -> m (Phrase m)
configurePhrase moa ca =
  pure
    ( Phrase
        { phraseName = "configurePhrase",
          phraseDesc = "configuring",
          phraseEnv = mempty,
          phrasePassAsFile = mempty,
          phraseScript =
            hookToScript "configure" (configureHook ca) $
              do
                when (fixLibtool ca) fixLib
                flags <- localVar ("configure_flags" @= "")

                case prefix ca of
                  Just p -> runCmd_ "mkdir" ["-p", p]
                  Nothing -> pure ()

                let cFlags =
                      var flags
                        : concat
                          [ maybeToList
                              ( fmap
                                  ( \p ->
                                      quoteTerms [str (toDrvStr (prefixKey ca)), "=", p]
                                  )
                                  (prefix ca)
                              ),
                            fmap
                              (\(n, v) -> str (toDrvStr ("--" <> n <> "=" <> v)))
                              (configurePlatform ca),
                            multiOutFlags,
                            configureFlags ca
                          ]

                let addFlagIf b s ef =
                      when b $
                        ifTrue
                          (runCmd_ "grep" ["-q", s, str (configureScript ca)])
                          (setVar (flags @= quoteTerms [ef, var flags]))
                addFlagIf (disableStatic ca) "enable-static" "--disable-static"
                addFlagIf (addDisableDepTrack ca) "dependency-tracking" "--disable-dependency-tracking"
                echoCmd ("config flag is " : cFlags)
                runCmd_ (str (configureScript ca)) cFlags
        }
    )
  where
    multiOutFlags = case moa of
      Just oa ->
        let toTerm = var . Var . toDrvStr
         in fmap
              quoteTerms
              [ ["--bindir=", toTerm (outputBin oa), "/bin"],
                ["--sbindir=", toTerm (outputBin oa), "/sbin"],
                ["--includedir=", toTerm (outputInclude oa), "/include"],
                ["--oldincludedir=", toTerm (outputInclude oa), "/include"],
                ["--mandir=", toTerm (outputMan oa), "/share/man"],
                ["--infodir=", toTerm (outputInfo oa), "/share/info"],
                ["--docdir=", toTerm (outputDoc oa), "/share/doc/", toTerm (shareDocName oa)],
                ["--libdir=", toTerm (outputLib oa), "/lib"],
                ["--libexecdir=", toTerm (outputLib oa), "/libexec"],
                ["--localedir=", toTerm (outputLib oa), "/share/locale"]
              ]
      Nothing -> []
    fixLib = do
      void
        ( newEnv
            ( "lt_cv_deplibs_check_method"
                @= quote (var "lt_cv_deplibs_check_method-pass_all")
            )
        )

      forFind
        "."
        ["-iname", "ltmain.sh"]
        ( \i ->
            do
              echoCmd ["fixing libtool script ", var i]
              runCmd_ "fixLibtool" [var i]
        )

      mtime_ref <-
        localVar
          ( "CONFIGURE_MTIME_REFERENCE"
              @= output
                (runCmd_ "mktemp" ["configure.mtime.reference.XXXXXX"])
          )
      findCmdExec
        "."
        ["-executable", "-type", "f", "-name", "configure"]
        ( \item ->
            [ ["grep", "-l", quote "GNU Libtool is free software; you can redistribute it and/or modify", item],
              ["touch", "-r", item, var mtime_ref],
              ["sed", "-i", "s_/usr/bin/file_file_g", item],
              ["touch", "-r", var mtime_ref, item]
            ]
        )
      rmCmd ["-f", var mtime_ref]

data MakeArg m = MakeArg
  { makefile :: DrvStr m,
    makeFlags :: [ScriptTerm m],
    enableParallelBuilding :: Bool
  }

instance (MonadDeriv m) => Default (MakeArg m) where
  def =
    MakeArg
      { makefile = "Makefile",
        makeFlags = [],
        enableParallelBuilding = True
      }

makeScript :: MonadDeriv m => ScriptTerm m -> [ScriptTerm m] -> MakeArg m -> Script m ()
makeScript txt exf ma =
  let flags =
        quoteTerms ["-f ", str (makefile ma)]
          : quoteTerms ["SHELL=", var "SHELL"]
          : [quoteTerms ["-j", var "NIX_BUILD_CORES"] | enableParallelBuilding ma]
          ++ makeFlags ma
          ++ exf
   in echoCmd (txt : flags)
        >> runCmd_ "make" flags

data BuildP

data BuildArg m = BuildArg
  { buildFlags :: [ScriptTerm m],
    buildHook :: Hook BuildP m
  }

instance Default (BuildArg m) where
  def =
    BuildArg
      { buildFlags = [],
        buildHook = mempty
      }

buildPhrase :: (MonadDeriv m) => MakeArg m -> BuildArg m -> m (Phrase m)
buildPhrase ma ba =
  pure
    ( Phrase
        { phraseName = "buildPhrase",
          phraseDesc = "building",
          phraseEnv = mempty,
          phrasePassAsFile = mempty,
          phraseScript =
            hookToScript "build" (buildHook ba) $
              makeScript "build flags " (buildFlags ba) ma
        }
    )

data CheckP

data CheckArg m = CheckArg
  { checkTarget :: DrvStr m,
    checkFlags :: [ScriptTerm m],
    checkHooks :: Hook CheckP m
  }

defCheckArg :: DrvStr m -> CheckArg m
defCheckArg tar =
  CheckArg
    { checkTarget = tar,
      checkFlags = [],
      checkHooks = mempty
    }

checkPhrase :: (MonadDeriv m) => MakeArg m -> CheckArg m -> m (Phrase m)
checkPhrase ma ca =
  pure
    ( Phrase
        { phraseName = "checkPhrase",
          phraseDesc = "running tests",
          phraseEnv = mempty,
          phrasePassAsFile = mempty,
          phraseScript =
            hookToScript "check" (checkHooks ca) $
              makeScript "check flags " (checkFlags ca ++ [str (checkTarget ca)]) ma
        }
    )

data InstallP

data InstallArg m = InstallArg
  { installTargets :: [ScriptTerm m],
    installFlags :: [ScriptTerm m],
    installHooks :: Hook InstallP m
  }

instance (MonadDeriv m) => Default (InstallArg m) where
  def =
    InstallArg
      { installTargets = ["install"],
        installFlags = [],
        installHooks = mempty
      }

installPhrase :: (MonadDeriv m) => Maybe MultiOutArg -> MakeArg m -> InstallArg m -> m (Phrase m)
installPhrase moa ma ia =
  pure
    ( Phrase
        { phraseName = "installPhrase",
          phraseDesc = "installing",
          phraseEnv = mempty,
          phrasePassAsFile = mempty,
          phraseScript =
            hookToScript "install" (installHooks ia) $
              makeScript
                "install flags "
                ( concat
                    [ case moa of
                        Just oa ->
                          let asVar = var . Var . toDrvStr
                           in [ quoteTerms ["pkgconfigdir=", asVar (outputDev oa), "/lib/pkgconfig"],
                                quoteTerms ["m4datadir=", asVar (outputDev oa), "/share/aclocal"],
                                quoteTerms ["aclocaldir=", asVar (outputDev oa), "/share/aclocal"]
                              ]
                        Nothing -> [],
                      installFlags ia,
                      installTargets ia
                    ]
                )
                ma
        }
    )

data DefFixupCmdArg m = DefFixupCmdArg
  { stripArgsHost, stripArgsTarget :: StripArg m,
    patchSheBangs :: Bool,
    forceShare :: ForceShare m
  }

instance (MonadDeriv m) => Default (DefFixupCmdArg m) where
  def =
    DefFixupCmdArg
      { stripArgsHost = def,
        stripArgsTarget = def,
        patchSheBangs = True,
        forceShare = def
      }

defFixupCmd ::
  MonadDeriv m =>
  [Text] ->
  Maybe MultiOutArg ->
  DefFixupCmdArg m ->
  [(Script m (ScriptTerm m -> Script m ()), [Text])]
defFixupCmd os moa fca =
  L.filter
    (not . null . snd)
    ( concat
        [ [ (moveToShare (forceShare fca), os),
            (makeSymlinkRelative, os),
            (compressManPages, os),
            (stripCmd (Just (stripArgsHost fca)) (Just (stripArgsTarget fca)), os)
          ],
          if patchSheBangs fca
            then
              [ ( patchShebang (var "PATH"),
                  L.filter (\i -> i /= "out" && i == maybe "" outputDev moa) os
                ),
                ( patchShebang (var "HOST_PATH"),
                  L.filter (\i -> i == "out" || i /= maybe "" outputDev moa) os
                )
              ]
            else [],
          [ (pruneLibtoolFiles, os),
            (auditTmpDir, os)
          ],
          case moa of
            Just m ->
              (multiOutDevs m, L.filter (\i -> i /= outputDev m) os)
                : multiOutDocs m os
            Nothing -> [],
          [ (moveSystemdUserUnit, os),
            (moveSbin, os),
            (moveLib64, os)
          ]
        ]
    )

data FixupP

data FixupArg m = FixupArg
  { fixOutputCmd :: [(Script m (ScriptTerm m -> Script m ()), [Text])],
    fixupHooks :: Hook FixupP m
  }

defFixupArg :: [(Script m (ScriptTerm m -> Script m ()), [Text])] -> FixupArg m
defFixupArg foc =
  FixupArg
    { fixOutputCmd = foc,
      fixupHooks = mempty
    }

fixupPhrase :: (MonadDeriv m) => FixupArg m -> m (Phrase m)
fixupPhrase fa =
  pure $
    Phrase
      { phraseName = "fixupPhrase",
        phraseDesc = "post-installation fixup",
        phraseEnv = mempty,
        phrasePassAsFile = mempty,
        phraseScript = do
          fixOut <- traverse (\(s, t) -> (t,) <$> s) (fixOutputCmd fa)
          funcDef "fixup_phrase" $ do
            traverse_
              (\o -> runCmd_ "chmod" ["-R", "u+w", var (Var (toDrvStr o))])
              (L.nub (foldMap fst fixOut))
            runWithHook "fixup" (fixupHooks fa) $
              traverse_
                (\(os, s) -> traverse_ (s . var . Var . toDrvStr) os)
                fixOut
      }

data InstallCheckP

data InstallCheckArg m = InstallCheckArg
  { installCheckTargets :: [ScriptTerm m],
    installCheckFlags :: [ScriptTerm m],
    installCheckHooks :: Hook InstallCheckP m
  }

instance (MonadDeriv m) => Default (InstallCheckArg m) where
  def =
    InstallCheckArg
      { installCheckTargets = ["installcheck"],
        installCheckFlags = [],
        installCheckHooks = mempty
      }

installCheckPhrase :: (MonadDeriv m) => MakeArg m -> InstallCheckArg m -> m (Phrase m)
installCheckPhrase ma ica =
  pure
    ( Phrase
        { phraseName = "installCheckPhrase",
          phraseDesc = "running install tests",
          phraseEnv = mempty,
          phrasePassAsFile = mempty,
          phraseScript =
            hookToScript "install_check" (installCheckHooks ica) $
              makeScript "installCheck flags" (installCheckFlags ica ++ installCheckTargets ica) ma
        }
    )

data DistP

data DistArg m = DistArg
  { distTargets :: [ScriptTerm m],
    distFlags :: [ScriptTerm m],
    copyDist :: Bool,
    tarballs :: ScriptTerm m,
    distHooks :: Hook DistP m
  }

instance (MonadDeriv m) => Default (DistArg m) where
  def =
    DistArg
      { distTargets = ["dist"],
        distFlags = [],
        copyDist = True,
        tarballs = "*.tar.gz",
        distHooks = mempty
      }

distPhrase :: (MonadDeriv m) => MakeArg m -> DistArg m -> m (Phrase m)
distPhrase ma da =
  pure
    ( Phrase
        { phraseName = "distPhrase",
          phraseDesc = "distPhrase",
          phraseEnv = mempty,
          phrasePassAsFile = mempty,
          phraseScript = hookToScript "dist" (distHooks da) $ do
            makeScript "dist flags" (distFlags da ++ distTargets da) ma
            when (copyDist da) $ do
              let dest = quoteTerms [var "out", "/tarballs"]
              runCmd_ "mkdir" ["-p", dest]
              runCmd_ "cp" ["-pvd", tarballs da, dest]
        }
    )

type PhrasesM m = m [Phrase m]

emptyPhrasesM :: (MonadDeriv m) => PhrasesM m
emptyPhrasesM = return []

type DefPhraseArg p m = Either (PhrasesM m) p

toPhrases :: (MonadDeriv m) => (a -> m (Phrase m)) -> DefPhraseArg a m -> PhrasesM m
toPhrases _ (Left p) = p
toPhrases f (Right a) = fmap L.singleton (f a)

data StdPhrasesArg m = StdPhrases
  { makeArgs :: MakeArg m,
    prePhrases :: PhrasesM m,
    unpackArg ::
      DefPhraseArg
        ( NonEmpty (FileDeriv m),
          SourceRootArg m,
          UnpackArg m
        )
        m,
    patchArg :: DefPhraseArg ([Patch m], PatchArg m) m,
    preConfigurePhases :: PhrasesM m,
    configureArg :: DefPhraseArg (ConfigureArg m) m,
    preBuildPhases :: PhrasesM m,
    buildArg :: DefPhraseArg (BuildArg m) m,
    checkArg :: DefPhraseArg (CheckArg m) m,
    preInstallPhases :: PhrasesM m,
    installArg :: DefPhraseArg (InstallArg m) m,
    fixupArg :: DefPhraseArg (FixupArg m) m,
    installCheckArg :: DefPhraseArg (InstallCheckArg m) m,
    preDistPhases :: PhrasesM m,
    distArg :: DefPhraseArg (DistArg m) m,
    postPhases :: PhrasesM m
  }

defStdPhraseArg ::
  MonadDeriv m =>
  [Text] ->
  Maybe MultiOutArg ->
  Maybe (NonEmpty (FileDeriv m), SourceRootArg m) ->
  System ->
  System ->
  System ->
  StdPhrasesArg m
defStdPhraseArg os moa sr b h _ =
  StdPhrases
    { makeArgs = def,
      prePhrases = emptyPhrasesM,
      unpackArg = case sr of
        Just (s, r) -> Right (s, r, def)
        Nothing -> Left emptyPhrasesM,
      patchArg = Left emptyPhrasesM,
      preConfigurePhases = emptyPhrasesM,
      configureArg = Right (defConfigureArg b h),
      preBuildPhases = emptyPhrasesM,
      buildArg = Right def,
      checkArg = Left emptyPhrasesM,
      preInstallPhases = emptyPhrasesM,
      installArg = Right def,
      fixupArg = Right (defFixupArg (defFixupCmd os moa def)),
      installCheckArg = Left emptyPhrasesM,
      preDistPhases = emptyPhrasesM,
      distArg = Left emptyPhrasesM,
      postPhases = emptyPhrasesM
    }

mkStdPhrases ::
  (BuiltinAddText m) =>
  Maybe MultiOutArg ->
  StdPhrasesArg m ->
  PhrasesM m
mkStdPhrases moa dpa@StdPhrases {makeArgs = ma} =
  concat
    <$> sequence
      [ prePhrases dpa,
        toPhrases (\(src, sr, a) -> unpackPhrase src sr a) (unpackArg dpa),
        toPhrases (uncurry patchPhrase) (patchArg dpa),
        preConfigurePhases dpa,
        toPhrases (configurePhrase moa) (configureArg dpa),
        preBuildPhases dpa,
        toPhrases (buildPhrase ma) (buildArg dpa),
        toPhrases (checkPhrase ma) (checkArg dpa),
        preInstallPhases dpa,
        toPhrases (installPhrase moa ma) (installArg dpa),
        toPhrases fixupPhrase (fixupArg dpa),
        toPhrases (installCheckPhrase ma) (installCheckArg dpa),
        preDistPhases dpa,
        toPhrases (distPhrase ma) (distArg dpa)
      ]

data StdPhraseHook m = StdPhraseHook
  { stdPHookEnv, stdPHookPassAsFile :: Typ.EnvHook m,
    stdPHookPhrase :: StdPhrasesArg m -> StdPhrasesArg m
  }

instance Default (StdPhraseHook m) where
  def = mempty

instance Semigroup (StdPhraseHook m) where
  hl <> hr =
    StdPhraseHook
      { stdPHookEnv = stdPHookEnv hl <> stdPHookEnv hr,
        stdPHookPassAsFile = stdPHookPassAsFile hl <> stdPHookPassAsFile hr,
        stdPHookPhrase = stdPHookPhrase hl >>> stdPHookPhrase hr
      }

instance Monoid (StdPhraseHook m) where
  mempty =
    StdPhraseHook
      { stdPHookEnv = mempty,
        stdPHookPassAsFile = mempty,
        stdPHookPhrase = id
      }

data StdPhraseDrv a m = StdPhraseDrv
  { mkDrvArg :: MD.MkDerivationArg a m,
    srcs :: Maybe (NonEmpty (FileDeriv m), SourceRootArg m),
    multiOutDocName :: Maybe Text,
    multiOutArg :: MultiOutArg -> Maybe MultiOutArg,
    buildPlat, hostPlat, targetPlat :: System,
    defStdPhrase ::
      HM.HashMap (DrvOutput m) (DrvStr m) ->
      StdPhrasesArg m ->
      StdPhrasesArg m,
    deps :: SimpleDeps (System -> System -> DrvOutput m),
    argHooks ::
      [ ConfigSet ->
        ( HM.HashMap Text Text,
          HM.HashMap Text (DrvStr m) -> StdPhraseHook m
        )
      ]
  }

stdPhraseMkDrv :: forall a m. (NamedHashAlgo a, BuiltinAddText m) => StdPhraseDrv a m -> ExtendDeriv m
stdPhraseMkDrv
  spd@StdPhraseDrv
    { buildPlat = buildP,
      hostPlat = hostP,
      targetPlat = targetP
    } =
    let depsDrv =
          propagateDep
            ( fmap
                (getDependenciesDrv buildP hostP targetP)
                ( deps spd
                    : ( toList
                          . fmap (MD.oArgPropagatedDeps . snd)
                          . MD.outputOpts
                          . mkDrvArg
                      )
                      spd
                )
            )
        outs = toList (fmap fst (MD.outputOpts (mkDrvArg spd)))
        multiOut =
          multiOutDocName spd >>= \sd ->
            multiOutArg spd (defMultiOutArg outs sd)
     in MD.mkDerivation
          (mkDrvArg spd)
          ( do
              depPath <-
                HM.fromList
                  <$> traverse
                    (\d -> (d,) <$> storePathStr d)
                    ( concat
                        [ depsBuildBuild depsDrv,
                          depsBuildHost depsDrv,
                          depsBuildTarget depsDrv,
                          depsHostHost depsDrv,
                          depsHostTarget depsDrv,
                          depsTargetTarget depsDrv
                        ]
                    )
              setup <- setupDeps depsDrv
              let cfgSet = hookFun setup emptyCfgSet
              argHooksS <-
                mconcat
                  <$> traverse
                    ( \i ->
                        let (files, fun) = i cfgSet
                         in fun <$> HM.traverseWithKey addTextFileStr files
                    )
                    (argHooks spd)
              ba <-
                runPhrases
                  mempty
                  ( ( mkStdPhrases multiOut
                        . stdPHookPhrase argHooksS
                        . defStdPhrase spd depPath
                    )
                      ( defStdPhraseArg
                          outs
                          multiOut
                          (srcs spd)
                          buildP
                          hostP
                          targetP
                      )
                  )
              pure
                ( ba
                    { MD.buildEnv =
                        MD.buildEnv ba
                          <> stdPHookEnv argHooksS
                          <> hookEnv setup,
                      MD.buildPassAsFile =
                        MD.buildPassAsFile ba
                          <> stdPHookPassAsFile argHooksS
                          <> hookPassAsFile setup,
                      MD.builder =
                        hookScript setup >> MD.builder ba
                    }
                )
          )
          buildP
          hostP
          targetP