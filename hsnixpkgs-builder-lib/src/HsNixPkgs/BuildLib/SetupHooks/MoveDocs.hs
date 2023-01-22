module HsNixPkgs.BuildLib.SetupHooks.MoveDocs (moveToShare) where

import Control.Monad
import Data.Foldable
import HsNixPkgs.BuildLib.Util
import System.Directory
import System.FilePath

moveToShare :: FilePath -> [FilePath] -> IO ()
moveToShare p =
  traverse_
    ( \d ->
        let src = p </> d
            dest = p </> "share" </> d
         in doesPathExist src >>= \se ->
              when se $ do
                de <- doesPathExist dest
                if de
                  then echo ["both ", src, " and ", dest, " exist"]
                  else do
                    echo ["moving ", src, " to ", dest]
                    createDirectoryIfMissing True dest
                    renamePath src dest
    )
