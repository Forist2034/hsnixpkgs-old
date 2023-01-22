{-
 code from https://github.com/NixOS/jailbreak-cabal

Copyright (c) 2012-2016 Peter Simons <simons@cryp.to>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Peter Simons nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
module HsNixPkgs.BuildLib.Develop.Lang.Haskell.JailbreakCabal.Internal (stripVersionRestrictions) where

import Distribution.PackageDescription
import Distribution.Version

-- We don't relax version restrictions inside conditional statements.
-- See https://github.com/peti/jailbreak-cabal/commit/99eac40deb481b185fd93fd307625369ff5e1ec0
stripVersionRestrictions :: GenericPackageDescription -> GenericPackageDescription
stripVersionRestrictions pkg =
  pkg
    { condLibrary = fmap relaxLibraryTree (condLibrary pkg),
      condSubLibraries = map (\(n, l) -> (n, relaxLibraryTree l)) (condSubLibraries pkg),
      condExecutables = map (fmap relaxExeTree) (condExecutables pkg),
      condTestSuites = map (fmap relaxTestTree) (condTestSuites pkg)
    }

relaxTreeConstraints :: CondTree v [Dependency] a -> CondTree v [Dependency] a
relaxTreeConstraints ct = ct {condTreeConstraints = map relax (condTreeConstraints ct)}

relaxLibraryTree :: CondTree v [Dependency] Library -> CondTree v [Dependency] Library
relaxLibraryTree ct = relaxTreeConstraints $ ct {condTreeData = relaxLibrary (condTreeData ct)}

relaxExeTree :: CondTree v [Dependency] Executable -> CondTree v [Dependency] Executable
relaxExeTree ct = relaxTreeConstraints $ ct {condTreeData = relaxExe (condTreeData ct)}

relaxTestTree :: CondTree v [Dependency] TestSuite -> CondTree v [Dependency] TestSuite
relaxTestTree ct = relaxTreeConstraints $ ct {condTreeData = relaxTest (condTreeData ct)}

relaxLibrary :: Library -> Library
relaxLibrary l = l {libBuildInfo = relaxBuildInfo (libBuildInfo l)}

relaxExe :: Executable -> Executable
relaxExe e = e {buildInfo = relaxBuildInfo (buildInfo e)}

relaxTest :: TestSuite -> TestSuite
relaxTest t = t {testBuildInfo = relaxBuildInfo (testBuildInfo t)}

relaxBuildInfo :: BuildInfo -> BuildInfo
relaxBuildInfo bi =
  bi
    { buildTools = map relax (buildTools bi),
      targetBuildDepends = map relax (targetBuildDepends bi)
    }

class DependencyType a where
  relax :: a -> a

instance DependencyType Dependency where
  relax (Dependency d _ deps) = Dependency d anyVersion deps

instance DependencyType LegacyExeDependency where
  relax (LegacyExeDependency d _) = LegacyExeDependency d anyVersion