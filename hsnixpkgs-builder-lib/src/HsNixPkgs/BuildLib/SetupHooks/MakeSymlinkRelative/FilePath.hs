{- makeRelativeEx from shake-0.19.7
Copyright Neil Mitchell 2011-2022.
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.
    * Neither the name of Neil Mitchell nor the names of other
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
module HsNixPkgs.BuildLib.SetupHooks.MakeSymlinkRelative.FilePath (makeRelativeEx) where

import System.Directory
import System.FilePath

makeRelativeEx :: FilePath -> FilePath -> IO (Maybe FilePath)
makeRelativeEx pathA pathB
  | isRelative makeRelativePathAPathB =
      pure (Just makeRelativePathAPathB)
  | otherwise = do
      a' <- canonicalizePath pathA
      b' <- canonicalizePath pathB
      if takeDrive a' /= takeDrive b'
        then pure Nothing
        else Just <$> makeRelativeEx' a' b'
  where
    makeRelativePathAPathB = makeRelative pathA pathB

    makeRelativeEx' :: FilePath -> FilePath -> IO FilePath
    makeRelativeEx' a b = do
      let rel = makeRelative a b
          parent = takeDirectory a
      if isRelative rel
        then pure rel
        else
          if a /= parent
            then do
              parentToB <- makeRelativeEx' parent b
              pure (".." </> parentToB)
            else -- Impossible: makeRelative should have succeeded in finding
            -- a relative path once `a == "/"`.

              error $
                "Error calculating relative path from \""
                  ++ pathA
                  ++ "\" to \""
                  ++ show pathB
                  ++ "\""