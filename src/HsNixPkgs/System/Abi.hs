module HsNixPkgs.System.Abi (Abi (..), abiName) where

import Data.Text (Text)

data Abi
  = Gnu
  | Musl
  | Unknown
  deriving (Eq, Show)

abiName :: Abi -> Text
abiName Gnu = "gnu"
abiName Musl = "musl"
abiName Unknown = "unknown"