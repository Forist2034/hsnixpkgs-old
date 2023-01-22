{-# LANGUAGE TemplateHaskell #-}

module HsNixPkgs.BuildLib.Sources (sourceDistName, sourceDistFile) where

import Data.EmbedSdist

embedSDist "hsnixpkgs-builder-lib.cabal" "sourceDistName" "sourceDistFile"
