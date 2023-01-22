module HsNixPkgs.HSScript.Internal.BuildMod.Types (RnM, WalkName (..)) where

import Control.Monad.State.Strict
import Data.Char
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Language.Haskell.TH.Syntax

type RnM =
  State (HM.HashMap (Maybe String, String) String, Int)

abbrev :: Maybe String -> String -> String
abbrev _ = filter isUpper

getMod :: Maybe String -> String -> RnM String
getMod pkg m =
  state
    ( \s@(nm, i) ->
        case HM.lookup (pkg, m) nm of
          Just v -> (v, s)
          Nothing ->
            let sPkg = stripVer <$> pkg
                name = abbrev sPkg m ++ "_" ++ show i
             in (name, (HM.insert (sPkg, m) name nm, i + 1))
    )
  where
    stripVer p =
      if isNumber (last p)
        then init (L.dropWhileEnd (/= '-') p)
        else p

class WalkName a where
  walk :: a -> RnM a

instance WalkName Name where
  walk n@(Name occ flv) =
    case flv of
      NameQ (ModName m) -> Name occ <$> getName Nothing m
      NameG _ (PkgName p) (ModName mn) -> Name occ <$> getName (Just p) mn
      _ -> pure n
    where
      getName :: Maybe String -> String -> RnM NameFlavour
      getName pkg m = NameQ . ModName <$> getMod pkg m

instance WalkName ModName where
  walk (ModName m) = ModName <$> getMod Nothing m
