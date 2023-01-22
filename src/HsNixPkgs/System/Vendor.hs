module HsNixPkgs.System.Vendor
  ( Vendor (..),
    vendorName,
  )
where

import Data.Text (Text)

data Vendor
  = Pc
  | None
  | UnknownVendor
  deriving (Eq, Show)

vendorName :: Vendor -> Text
vendorName Pc = "pc"
vendorName None = "none"
vendorName UnknownVendor = "unknown"