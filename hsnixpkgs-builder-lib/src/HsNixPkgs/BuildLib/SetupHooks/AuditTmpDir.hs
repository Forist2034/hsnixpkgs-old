{-# LANGUAGE MultiWayIf #-}

module HsNixPkgs.BuildLib.SetupHooks.AuditTmpDir (auditTmpDir) where

import Control.Exception
import Control.Monad
import Control.Monad.Catch.Pure
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Typeable)
import Data.Elf.Dynamic
import Data.Elf.Types
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import ElfUtil.RunPath
import HsNixPkgs.BuildLib.FileType
import HsNixPkgs.BuildLib.Util
import System.Directory
import System.FilePath
import System.Posix.Files

data ForbiddenRef
  = InELF FilePath FilePath
  | InWrapper FilePath FilePath
  deriving (Typeable)

instance Show ForbiddenRef where
  show (InELF e t) = concat ["audit-tmpdir: RPATH of binary ", e, "contains a forbidden reference to ", t]
  show (InWrapper e t) = concat ["audit-tmpdir: wrapper script ", e, " contains a forbidden reference to ", t]

instance Exception ForbiddenRef

auditTmpDir :: FilePath -> IO ()
auditTmpDir d = do
  tmp <- getTemporaryDirectory
  let td = tmp ++ "/"
      rpathT = T.pack (':' : td)
      tdt = LT.pack td
  echo ["checking reference to ", tmp, " in ", d]
  listDirRec d
    >>= filterM (fmap isRegularFile . getFileStatus)
    >>= traverse_
      ( \f ->
          LBS.readFile f >>= \cont ->
            if
                | isELF cont ->
                    case either (const Nothing) Just (runCatch (parseElfInfo cont))
                      >>= parseElfDynInfo
                      >>= getRunPath of
                      Just o ->
                        assertEIO
                          (not (rpathT `T.isInfixOf` TE.decodeUtf8 o))
                          (InELF f tmp)
                      Nothing -> pure ()
                | isScript cont ->
                    doesFileExist
                      (takeDirectory f </> ("." ++ takeBaseName f ++ "-wrapped"))
                      >>= \e ->
                        assertEIO
                          (not (e && LT.isInfixOf tdt (LTE.decodeUtf8 cont)))
                          (InWrapper f tmp)
                | otherwise -> pure ()
      )
      . filter (not . L.isInfixOf "build-id" . tail)