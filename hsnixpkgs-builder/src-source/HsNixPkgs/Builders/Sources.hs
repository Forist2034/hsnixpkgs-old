{-# LANGUAGE TemplateHaskell #-}

module HsNixPkgs.Builders.Sources (sourceDistName, sourceDistFile) where

import Data.EmbedSdist

embedSDist "hsnixpkgs-builder.cabal" "sourceDistName" "sourceDistFile"