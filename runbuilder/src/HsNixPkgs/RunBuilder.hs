{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module HsNixPkgs.RunBuilder
  ( SourceSpec (..),
    ModTree (..),
    RunBuilderCfg (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

data SourceSpec
  = EnvFile {env :: String, keep :: Bool}
  | EnvStr {env :: String, keep :: Bool}
  | File FilePath
  | Str Text
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_'} ''SourceSpec

data ModTree = ModTree
  { modSelf :: Maybe SourceSpec,
    modChild :: HM.HashMap String ModTree
  }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3} ''ModTree

data RunBuilderCfg = RunBuilderCfg
  { ghc :: FilePath,
    ghcPkg :: FilePath,
    ghcFlags :: [String],
    packages :: [FilePath],
    buildDir :: FilePath,
    mainModule :: SourceSpec,
    modules :: HM.HashMap String ModTree,
    outputName :: String
  }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_'} ''RunBuilderCfg