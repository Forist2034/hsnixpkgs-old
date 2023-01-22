{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.EmbedSdist (embedSDist) where

import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.Text as T
import Distribution.Package (Package (packageId))
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.Setup
import Distribution.Simple.SrcDist
import Distribution.Simple.Utils
import Distribution.Verbosity
import HsNix.Builtin.AddFile
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath

embedSDist :: FilePath -> String -> String -> DecsQ
embedSDist cabalFile fs cs = do
  let fn = mkName fs
      cn = mkName cs
  pd <-
    flattenPackageDescription
      <$> liftIO (readGenericPackageDescription verbosity cabalFile)
  liftIO (listPackageSources verbosity "." pd knownSuffixHandlers)
    >>= traverse_ addDependentFile
  dt <-
    liftIO $ do
      tmpDir <- getTemporaryDirectory
      withTempDirectory verbosity tmpDir "sdist" $ \td -> do
        sdist
          pd
          defaultSDistFlags
            { sDistDirectory = toFlag td,
              sDistVerbosity = toFlag verbosity
            }
          (</> "src")
          knownSuffixHandlers
        readDirTree td
  val <-
    [d|
      $(varP fn) = $(lift (T.pack (prettyShow (packageId pd))))

      $(varP cn) = $(lift dt)
      |]
  pure
    ( SigD fn (ConT ''T.Text)
        : SigD cn (ConT ''DirTree)
        : val
    )
  where
    verbosity = normal