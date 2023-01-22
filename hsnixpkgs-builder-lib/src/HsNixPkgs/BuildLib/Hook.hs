module HsNixPkgs.BuildLib.Hook (Hook (..), emptyHook, runHook) where

data Hook = Hook
  { preHook :: IO (),
    postHook :: IO ()
  }

emptyHook :: Hook
emptyHook =
  Hook
    { preHook = pure (),
      postHook = pure ()
    }

runHook :: Hook -> IO a -> IO a
runHook h f = preHook h >> (f <* postHook h)