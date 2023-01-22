module HsNixPkgs.BuildLib.SetupHooks.Strip
  ( StripCfg (..),
    stripDirs,
  )
where

import Control.Monad
import Data.Foldable
import qualified Data.List as L
import HsNixPkgs.BuildLib.Util
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Process

data StripCfg = StripCfg
  { stripCmd :: FilePath,
    ranlibCmd :: FilePath,
    stripArg :: [String]
  }

stripDirs :: FilePath -> [FilePath] -> StripCfg -> IO ()
stripDirs p d StripCfg {stripCmd = strip, stripArg = sa, ranlibCmd = ranlib} =
  filterM doesPathExist (fmap (p </>) d) >>= \ps ->
    unless (null ps) $ do
      echo ["stripping (with command ", strip, " and flags ", show sa, ") in ", show ps]
      traverse_
        ( \fp -> do
            listDirRec fp
              >>= ( let libDebug = p </> "lib" </> "debug"
                     in filterM
                          ( \f ->
                              (not (libDebug `L.isPrefixOf` f) &&) . isRegularFile
                                <$> getFileStatus f
                          )
                  )
              >>= traverse_ (\f -> callProcess strip (f : sa))
            {-'strip' does not normally preserve archive index in .a files.
            This usually causes linking failures against static libs like:
              ld: ...-i686-w64-mingw32-stage-final-gcc-13.0.0-lib/i686-w64-mingw32/lib/libstdc++.dll.a:
                error adding symbols: archive has no index; run ranlib to add one
            Restore the index by running 'ranlib'-}
            listDirRec fp
              >>= filterM
                ( \f ->
                    (".a" `isExtensionOf` f &&) . isRegularFile
                      <$> getFileStatus f
                )
              >>= traverse_ (\f -> callProcess ranlib [f])
        )
        ps