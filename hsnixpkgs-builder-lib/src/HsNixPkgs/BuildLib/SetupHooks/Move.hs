module HsNixPkgs.BuildLib.SetupHooks.Move
  ( moveLib64,
    moveSbin,
    moveSystemdUserUnits,
  )
where

import Control.Applicative
import Control.Monad
import Data.Foldable
import HsNixPkgs.BuildLib.Util
import System.Directory
import System.FilePath

move :: FilePath -> FilePath -> FilePath -> IO ()
move from to p =
  let src = p </> from
      dest = p </> to
   in liftA2 (||) (not <$> doesPathExist src) (pathIsSymbolicLink dest) >>= \cond ->
        unless cond $ do
          echo ["moving ", src, " to ", dest]
          createDirectoryIfMissing True dest
          listDirectory src
            >>= traverse_ (\f -> renamePath (src </> f) (dest </> f))
          removeDirectory src
          createDirectoryLink dest src

moveLib64 :: FilePath -> IO ()
moveLib64 = move "lib64" "lib"

moveSbin :: FilePath -> IO ()
moveSbin = move "sbin" "bin"

moveSystemdUserUnits :: FilePath -> IO ()
moveSystemdUserUnits = move "lib/systemd/user" "share/systemd/user"