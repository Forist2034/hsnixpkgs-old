{-# LANGUAGE OverloadedLists #-}

module HsNixPkgs.BuildLib.FileType (isELF, isScript) where

import qualified Data.ByteString.Lazy as LBS

isELF :: LBS.ByteString -> Bool
isELF = LBS.isPrefixOf magic
  where
    magic = [0x7f, 0x45, 0x4c, 0x46] -- 0x7f E L F

isScript :: LBS.ByteString -> Bool
isScript = LBS.isPrefixOf "#!"