{-# LANGUAGE TupleSections #-}

module HsNixPkgs.StdEnv.Generic.Phrases (Phrase (..), PhrasesP, runPhrases) where

import Data.Foldable
import Data.Text (Text)
import HsNix.Derivation hiding (quote)
import HsNixPkgs.Shell
import HsNixPkgs.Shell.Util
import HsNixPkgs.StdEnv.Generic.MakeDerivation
import qualified HsNixPkgs.Types as Typ

data Phrase m = Phrase
  { phraseName :: Text,
    phraseDesc :: Text,
    phraseEnv, phrasePassAsFile :: Typ.EnvHook m,
    phraseScript :: Script m (ScriptTerm m)
  }

data PhrasesP

runPhrases :: (MonadDeriv m) => Hook PhrasesP m -> m [Phrase m] -> m (BuilderArg m)
runPhrases hook ps = do
  phS <- ps
  pure
    ( BuilderArg
        { builder = do
            funcs <- traverse (\p -> (phraseName p,phraseDesc p,) <$> phraseScript p) phS
            let getTime = output (runCmd_ "date" [quote "+%s"])
            showTime <-
              funcDef
                "showTime"
                ( do
                    end <- localVar ("end" @= getTime)
                    message <- localVar ("message" @= var "1")
                    start <- localVar ("start" @= var "2")
                    delta <- localVar ("delta" @= arith (AVar end - AVar start))

                    runCmd_ "echo" ["-n", var message]
                    h <- localVar ("H" @= arith (AVar delta `ADiv` 3600))

                    arithCmd (AVar h `AGT` 0)
                      `andCC` runCmd_ "echo" ["-n", quoteTerms [" ", var h, " hours"]]

                    mi <- localVar ("m" @= arith ((AVar delta `AMod` 3600) `ADiv` 60))

                    arithCmd (AVar mi `AGT` 0)
                      `andCC` runCmd_ "echo" ["-n", quoteTerms [" ", var mi, " minutes"]]

                    echoCmd [" ", arith ((AVar delta `AMod` 3600) `AMod` 60), " seconds"]
                )
            funcDef
              "genericBuild"
              ( runWithHook "phrases" hook $ do
                  start <- localVar ("startTime" @= "")
                  traverse_
                    ( \(n, d, f) -> do
                        echoCmd [str (toDrvStr d)]
                        setVar (start @= getTime)
                        runCmd_ f []
                        runCmd_ showTime [quoteTerms [str (toDrvStr n), " completed in "], var start]
                    )
                    funcs
              ),
          buildEnv = foldMap phraseEnv phS,
          buildPassAsFile = foldMap phrasePassAsFile phS
        }
    )