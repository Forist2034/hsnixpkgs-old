{-# LANGUAGE TupleSections #-}

module HsNixPkgs.BuildLib.SetupHooks.PatchShebang (patchSheBang) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Bifunctor
import qualified Data.ByteString.Lazy as LBS
import Data.Data
import Data.Foldable
import qualified Data.List as L
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Text.Lazy.IO as LTIO
import GHC.ResponseFile
import HsNixPkgs.BuildLib.FileType
import HsNixPkgs.BuildLib.Util
import System.Directory
import System.Environment
import System.FilePath
import System.IO

data PatchShebangExcept
  = UnsupportedInterp [String]
  | InvalidEnvArg [String]
  deriving (Typeable)

instance Show PatchShebangExcept where
  show (UnsupportedInterp s) = "patch-shebang: Unsupported interpreter line " ++ escapeArgs s
  show (InvalidEnvArg e) = "patch-shebang: Invalid env argument " ++ show e

instance Exception PatchShebangExcept

interpreterLine :: Text -> ([String], Text)
interpreterLine c =
  let (l, body) =
        second
          (maybe LT.empty snd . LT.uncons)
          (LT.break (== '\n') c)
   in (unescapeArgs (LT.unpack (LT.drop 2 l)), body)

newPath :: String -> [String] -> Maybe (String, [String])
newPath _ [] = Nothing
newPath store ip@(i : xs)
  | store `L.isPrefixOf` i = Nothing
  | "/bin/env" `L.isSuffixOf` i =
      case xs of
        [] -> throw (InvalidEnvArg xs)
        (x1 : xn)
          | "-" `L.isPrefixOf` x1 || '=' `elem` x1 ->
              throw (UnsupportedInterp ip)
          | otherwise -> Just (x1, xn)
  | otherwise = Just (takeBaseName i, xs)

patchSheBang :: FilePath -> [FilePath] -> IO ()
patchSheBang d path = do
  store <- getEnv "NIX_STORE"
  listDirRec d
    >>= filterM (fmap executable . getPermissions)
    >>= mapM (\f -> (f,) <$> LBS.readFile f)
    >>= traverse_
      ( \(f, cont) ->
          let (il, body) = interpreterLine cont
           in case newPath store il of
                Nothing -> pure ()
                Just (pe, pa) -> do
                  execList <- findExecutablesInDirectories path pe
                  case execList of
                    [] -> pure ()
                    (exec : _) ->
                      when (exec /= pe) $ do
                        let newIl = escapeArgs (exec : pa)
                        echo [f, ":  interpreter directive changed from ", show il, " to ", show newIl]
                        -- Preserve times, see: https://github.com/NixOS/nixpkgs/pull/33281
                        time <- getModificationTime f
                        body `deepseq` -- read all contents and close read handle
                          withFile
                            f
                            WriteMode
                            ( \h -> do
                                hPutStr h "#!"
                                hPutStr h newIl
                                LTIO.hPutStr h body
                            )
                        setModificationTime f time
                        setAccessTime f time
      )
      . fmap (second LTE.decodeUtf8)
      . filter (isScript . snd)
