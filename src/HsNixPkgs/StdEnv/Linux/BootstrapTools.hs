{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module HsNixPkgs.StdEnv.Linux.BootstrapTools (UnpackCfg (..), unpackBootTools) where

import Data.Text (Text)
import HsNix.Builtin.AddFile
import HsNix.Builtin.FetchUrl (BuiltinFetchUrl)
import HsNix.Derivation
import HsNix.Hash
import HsNix.System
import HsNixPkgs.StdEnv.Linux.BootstrapFiles
import HsNixPkgs.Util.TH

unpackScript :: Text
unpackScript = $(includeText "unpack-bootstrap-tools.sh")

unpackScriptMusl :: Text
unpackScriptMusl = $(includeText "unpack-bootstrap-tools-musl.sh")

newtype UnpackCfg = UnpackCfg {isMusl :: Bool}
  deriving (Show, Eq)

unpackBootTools ::
  forall m.
  (BuiltinFetchUrl m, BuiltinAddText m) =>
  UnpackCfg ->
  System ->
  BootstrapFiles m ->
  Derivation m
unpackBootTools (UnpackCfg {isMusl = musl}) sys files =
  derivation $ do
    ash <- storePathStr (busybox files)
    tarball <- storePathStr (bootstrapTools files)
    script <- addTextFileStr "unpack-bootstrap-tools.sh" (if musl then unpackScriptMusl else unpackScript)
    return
      ( (defaultDrvArg "bootstrap-tools" ash sys)
          { drvArgs = ["ash", "-e", script],
            drvEnv =
              [ ("tarball", tarball),
                ("langC", "1"),
                ("langCC", "1"),
                ("isGNU", "1")
              ]
          } ::
          DerivationArg m SHA256
      )