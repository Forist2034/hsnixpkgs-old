module HsNixPkgs.BuildLib.Develop.Lang.Haskell.JailbreakCabal (jailbreakCabalFile) where

import Distribution.PackageDescription.Parsec
import Distribution.PackageDescription.PrettyPrint
import Distribution.Verbosity (normal)
import HsNixPkgs.BuildLib.Develop.Lang.Haskell.JailbreakCabal.Internal
  ( stripVersionRestrictions,
  )

jailbreakCabalFile :: FilePath -> FilePath -> IO ()
jailbreakCabalFile src dest =
  readGenericPackageDescription normal src
    >>= writeGenericPackageDescription dest . stripVersionRestrictions