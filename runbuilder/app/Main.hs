module Main (main) where

import Control.Monad
import Data.Aeson
import Data.Foldable
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.String
import qualified Data.Text.IO as TIO
import HsNixPkgs.RunBuilder
import System.Directory
import System.Environment
import System.FilePath
import System.Posix.Process
import System.Process hiding (env)

writeSource :: Bool -> FilePath -> SourceSpec -> IO ()
writeSource keepAll name spec =
  let fileName = name <.> "hs"
   in case spec of
        EnvFile {env = e, keep = k} -> do
          getEnv e >>= (`copyFile` fileName)
          unless (k || keepAll) (unsetEnv e)
        EnvStr {env = e, keep = k} -> do
          getEnv e >>= writeFile fileName
          unless (k || keepAll) (unsetEnv e)
        File f -> copyFile f fileName
        Str s -> TIO.writeFile fileName s

writeChildMod :: Bool -> String -> HM.HashMap String ModTree -> IO ()
writeChildMod keepAll name cm
  | HM.null cm = pure ()
  | otherwise = do
      createDirectory name
      withCurrentDirectory name $
        HM.foldlWithKey (\a k v -> a >> writeModTree keepAll k v) (pure ()) cm

writeModTree :: Bool -> String -> ModTree -> IO ()
writeModTree keepAll name mt = do
  maybe (pure ()) (writeSource keepAll name) (modSelf mt)
  writeChildMod keepAll name (modChild mt)

runBuilder :: Bool -> RunBuilderCfg -> IO ()
runBuilder keepAll cfg@RunBuilderCfg {packages = pkg} = do
  builder <-
    fmap
      (\wd -> wd </> buildDir cfg </> outputName cfg)
      getCurrentDirectory
  createDirectoryIfMissing True (buildDir cfg)
  withCurrentDirectory (buildDir cfg) $ do
    writeChildMod keepAll "src" (modules cfg)

    unless (null pkg) $ do
      createDirectory "package.conf.d"
      traverse_
        ( \p ->
            listDirectory p
              >>= traverse_
                (\f -> copyFile (p </> f) ("package.conf.d" </> f))
                . filter ("conf" `isExtensionOf`)
        )
        pkg
      callProcess (ghcPkg cfg) ["--package-db=package.conf.d", "recache"]

    createDirectoryIfMissing False "src"
    withCurrentDirectory "src" $ do
      writeSource keepAll "Main" (mainModule cfg)
      callProcess
        (ghc cfg)
        ( ["--package-db=package.conf.d" | not (null pkg)]
            ++ ghcFlags cfg
            ++ ["Main.hs", "-o", builder]
        )
  executeFile builder False [] Nothing

getCfg :: [String] -> IO (Bool, RunBuilderCfg)
getCfg args =
  case args of
    "--envFile" : e : es -> do
      getEnv e
        >>= fmap orErr . eitherDecodeFileStrict'
        >>= procKeepEnv e es
    "--env" : e : es ->
      getEnv e
        >>= procKeepEnv e es
          . orErr
          . eitherDecodeStrict'
          . fromString
    "--str" : e : es ->
      pure
        ( ( procKeep es
              . orErr
              . eitherDecodeStrict'
              . fromString
          )
            e
        )
    "--file" : e : es ->
      eitherDecodeFileStrict' e
        <&> procKeep es . orErr
    _ -> error "Invalid arg"
  where
    orErr (Left e) = error ("decode JSON error: " ++ e)
    orErr (Right r) = r

    procKeepEnv e es cfg = case es of
      [] -> (False, cfg) <$ unsetEnv e
      ["--keep"] -> pure (False, cfg)
      ["--keep-all"] -> pure (True, cfg)
      _ -> error "Invalid arg"
    procKeep es cfg =
      case es of
        [] -> (False, cfg)
        ["--keep-all"] -> (True, cfg)
        _ -> error "invalid arg"

main :: IO ()
main =
  getArgs
    >>= getCfg
    >>= uncurry runBuilder