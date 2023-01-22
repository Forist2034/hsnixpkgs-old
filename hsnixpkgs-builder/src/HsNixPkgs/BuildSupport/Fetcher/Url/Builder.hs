{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}

module HsNixPkgs.BuildSupport.Fetcher.Url.Builder
  ( FetchUrlArg (..),
    fetchUrl,
  )
where

import Control.Monad
import Language.Haskell.TH.Syntax (Lift)
import System.Directory
import System.Exit
import System.Process
import Text.Read

data FetchUrlArg = FetchUrlArg
  { hashedMirrors :: [String],
    hashedMirrorTimeout :: Int,
    preferHashedMirror :: Bool,
    curlOpt :: [String],
    name :: String,
    isExecutable :: Bool,
    urls :: [String]
  }
  deriving (Show, Lift)

orM :: IO Bool -> IO Bool -> IO Bool
orM l r =
  l >>= \case
    True -> pure True
    False -> r

anyM :: (String -> IO Bool) -> [String] -> IO Bool
anyM f = foldl (\s -> orM s . f) (pure False)

downloadUrl :: FetchUrlArg -> FilePath -> IO () -> String -> IO Bool
downloadUrl FetchUrlArg {curlOpt = opt, isExecutable = exec} out postFetch url =
  down >>= \suc ->
    suc
      <$ when
        suc
        ( do
            when exec $
              setPermissions
                out
                (emptyPermissions {readable = True, executable = True})
            postFetch
        )
  where
    finalCp = proc "curl" (opt ++ ["-C", "-", "--fail", url, "--output", out])
    down =
      withCreateProcess finalCp (\_ _ _ h -> waitForProcess h)
        >>= \case
          ExitSuccess -> pure True
          ExitFailure 18 -> down -- partial download
          _ -> pure False

downloadHashedMirror :: FetchUrlArg -> String -> String -> IO Bool
downloadHashedMirror fa out url =
  readProcessWithExitCode
    "curl"
    ( curlOpt fa
        ++ [ "--retry",
             "0",
             "--connect-timeout",
             show (hashedMirrorTimeout fa),
             "--fail",
             "--silent",
             "--show-error",
             "--head",
             url,
             "--write-out",
             "%{http_code}",
             "--output",
             "/dev/null"
           ]
    )
    ""
    >>= \case
      (ExitSuccess, _, _) -> downloadUrl fa out (pure ()) url
      (ExitFailure _, code, err) ->
        False
          <$ when
            (readMaybe code /= Just (404 :: Int))
            ( do
                putStrLn ("error checking existence of " ++ url)
                putStrLn err
            )

fetchUrl ::
  FetchUrlArg ->
  -- | output
  FilePath ->
  -- | postFetch
  IO () ->
  IO ()
fetchUrl fa out postFetch =
  ( let simple = anyM (downloadUrl fa out postFetch) (urls fa)
        hashed = anyM (downloadHashedMirror fa out) (hashedMirrors fa)
     in if preferHashedMirror fa
          then hashed `orM` simple
          else simple `orM` hashed
  )
    >>= \suc ->
      if suc
        then pure ()
        else do
          putStrLn ("error: cannot download " ++ name fa ++ " from any mirror")
          exitFailure