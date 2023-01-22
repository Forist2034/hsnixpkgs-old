{-# LANGUAGE LambdaCase #-}

module HsNixPkgs.BuildLib.SetupHooks.CompressManPages (compressManPages) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Foldable
import qualified Data.List as L
import HsNixPkgs.BuildLib.Util
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Files
import System.Process

notArchive :: FilePath -> Bool
notArchive f = takeExtension f `notElem` [".bz2", ".gz", ".xz"]

compressManPages :: FilePath -> IO ()
compressManPages p =
  liftA3
    (\a b c -> a || b || not c)
    (pathIsSymbolicLink (p </> "share"))
    (pathIsSymbolicLink (p </> "share" </> "man"))
    (doesDirectoryExist (p </> "share" </> "man"))
    >>= \e ->
      unless e $ do
        putStrLn ("gzipping man pages under " ++ manPath)
        listDirRec manPath
          >>= filterM
            ( \f ->
                (notArchive f &&) . isRegularFile
                  <$> getFileStatus f
            )
          >>= traverse_
            ( \f ->
                let dest = f <.> "gz"
                 in withBinaryFile
                      dest
                      WriteMode
                      ( \h ->
                          bracket
                            ( createProcess_
                                "createProcess"
                                ( (proc "gzip" ["-c", "-n", f])
                                    { std_out = UseHandle h
                                    }
                                )
                            )
                            cleanupProcess
                            (\(_, _, _, ph) -> waitForProcess ph)
                      )
                      >>= \case
                        ExitSuccess -> removeFile f
                        ExitFailure _ -> removeFile dest
            )
        listDirRec manPath
          >>= filterM
            (\f -> (notArchive f &&) <$> pathIsSymbolicLink f)
          >>= traverse_
            ( \f -> do
                dest <- canonicalizePath f
                cond <-
                  liftA2
                    (&&)
                    (doesPathExist dest)
                    (isRegularFile <$> getFileStatus dest)
                when cond $
                  mask_
                    ( do
                        createFileLink (dest <.> "gz") (f <.> "gz")
                        removeFile f
                    )
            )
            . L.sort
  where
    manPath = p </> "share" </> "man"