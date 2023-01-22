module HsNixPkgs.BuildLib.Util
  ( assertE,
    assertEIO,
    echo,
    listDirRec,
  )
where

import Control.Exception
import System.Directory
import System.FilePath
import System.Posix.Files

assertE :: Exception e => Bool -> e -> ()
assertE True _ = ()
assertE False e = throw e

assertEIO :: Exception e => Bool -> e -> IO ()
assertEIO True _ = pure ()
assertEIO False e = throwIO e

echo :: [String] -> IO ()
echo v = putStrLn (concat v)

listDirRec :: FilePath -> IO [FilePath]
listDirRec d =
  listDirectory d
    >>= fmap concat
      . traverse
        ( \f ->
            let fp = d </> f
             in getSymbolicLinkStatus fp >>= \s ->
                  if isDirectory s
                    then (fp :) <$> listDirRec fp
                    else pure [fp]
        )