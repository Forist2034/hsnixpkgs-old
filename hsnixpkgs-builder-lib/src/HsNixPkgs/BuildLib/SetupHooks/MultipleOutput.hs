{-# LANGUAGE LambdaCase #-}

module HsNixPkgs.BuildLib.SetupHooks.MultipleOutput
  ( MultiOutput (..),
    moveDocs,
    moveDev,
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HsNixPkgs.BuildLib.Util
import System.Directory
import System.FilePath
import System.IO.Error

moveToOutput :: FilePath -> FilePath -> FilePath -> IO ()
moveToOutput src dest subDir
  | dest == src = pure ()
  | otherwise =
      let srcPath = src </> subDir
       in doesPathExist srcPath >>= \e ->
            when e $ do
              srcParent <- canonicalizePath (srcPath </> "..")

              let destPath = dest </> subDir
              echo ["Moving ", srcPath, " to ", destPath]
              liftA2 (&&) (doesDirectoryExist srcPath) (doesDirectoryExist destPath)
                >>= \case
                  -- try to merge dir
                  True -> do
                    sl <- listDirectory srcPath
                    unless (null sl) $ do
                      traverse_ (\f -> renamePath (srcPath </> f) (destPath </> f)) sl
                    removeDirectory srcPath
                  False -> do
                    canonicalizePath (takeDirectory destPath)
                      >>= createDirectoryIfMissing True
                    renamePath srcPath destPath

              -- ignore nonexist exceptions
              catch
                ( do
                    np <- null <$> listDirectory srcParent
                    when np $ do
                      echo ["Removing ", srcParent, " and  (possibly) its parents"]
                      removeDirectory srcParent
                      let pp = srcParent </> ".."
                      listDirectory pp >>= \pl ->
                        when (null pl) (removeDirectory pp)
                )
                ( \err ->
                    if isDoesNotExistError err
                      then pure ()
                      else throwIO err
                )

data MultiOutput = MultiOutput
  { outDev :: FilePath,
    outBin :: FilePath,
    outInclude :: FilePath,
    outLib :: FilePath,
    outDoc :: FilePath,
    outDevDoc :: FilePath,
    outMan :: FilePath,
    outDevMan :: FilePath,
    outInfo :: FilePath
  }

-- | Move documentation to the desired outputs.
moveDocs :: MultiOutput -> FilePath -> IO ()
moveDocs mo o = do
  moveToOutput o (outInfo mo) "share/info"
  moveToOutput o (outDoc mo) "share/doc"
  moveToOutput o (outDevDoc mo) "share/gtk-doc"
  moveToOutput o (outDevDoc mo) "share/devhelp/books"

  moveToOutput o (outDevMan mo) "share/man/man3"
  moveToOutput o (outMan mo) "share/man"

-- | Move development-only stuff to the desired outputs.
moveDev :: MultiOutput -> FilePath -> IO ()
moveDev MultiOutput {outDev = dev, outInclude = include} o = do
  moveToOutput o include "include"

  moveToOutput o dev "lib/pkgconfig"
  moveToOutput o dev "share/pkgconfig"
  moveToOutput o dev "lib/cmake"
  moveToOutput o dev "share/aclocal"

  let incl = T.pack ('=' : include)
      patch =
        traverse_
          ( \f -> do
              echo ["Patching ", show f, " includedir to output ", include]
              TIO.readFile f >>= TIO.writeFile f . patchPC incl
          )
          . filter (".pc" `isExtensionOf`)
  listDirectory (dev </> "lib" </> "pkgconfig") >>= patch
  listDirectory (dev </> "share" </> "pkgconfig") >>= patch
  where
    patchPC inc =
      T.unlines
        . fmap
          ( \l ->
              if "includedir=" `T.isPrefixOf` l
                then T.replace "=${prefix}" inc l
                else l
          )
        . T.lines