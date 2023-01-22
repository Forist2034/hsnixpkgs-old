module HsNixPkgs.Util.TH (moduleDir, includeText) where

import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.FilePath

-- | returns {source file dir}/{module name}
moduleDir :: Q FilePath
moduleDir = do
  Module _ (ModName n) <- thisModule
  return (joinPath ("." : "src" : splitMod n))
  where
    splitMod [] = []
    splitMod s =
      let (h, t) = break (== '.') s
       in h : splitMod (dropWhile (== '.') t)

includeText :: FilePath -> ExpQ
includeText f = do
  modDir <- moduleDir
  let fullPath = modDir </> f
  addDependentFile fullPath
  liftIO (TIO.readFile fullPath) >>= lift
