{-# LANGUAGE LambdaCase #-}

module HsNixPkgs.BuildLib.StdEnv.Phrases (Phrase (..), runPhrases) where

import Data.Foldable
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Time.Clock
import GHC.IO.Handle.FD
import HsNixPkgs.BuildLib.Hook
import HsNixPkgs.BuildLib.Util
import System.Environment
import System.IO

data Phrase = Phrase
  { phraseName :: Text,
    phraseDesc :: Text,
    phraseFunc :: IO ()
  }

runPhrases :: Hook -> [Phrase] -> IO ()
runPhrases hook ps = runHook hook $ do
  putLog <-
    lookupEnv "NIX_LOG_FD" >>= \case
      Just f -> do
        h <- fdToHandle (read f)
        pure
          ( \p ->
              TIO.hPutStrLn h (mconcat ["@nix { \"action\": \"setPhase\", \"phase\": \"", p, "\""])
                >> hFlush h
          )
      Nothing -> pure (const (pure ()))
  traverse_
    ( \p -> do
        putLog (phraseName p)
        TIO.putStrLn (phraseDesc p)
        startTime <- getCurrentTime
        phraseFunc p
        endTime <- getCurrentTime
        TIO.putStr (phraseName p)
          >> echo [" completed in ", show (diffUTCTime endTime startTime)]
    )
    ps