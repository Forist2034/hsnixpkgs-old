module HsNixPkgs.Util
  ( listToStr,
    boolToStr,
  )
where

import Data.List (intersperse)
import Data.String

listToStr :: (IsString m, Monoid m) => [m] -> m
listToStr s = mconcat (intersperse " " s)

boolToStr :: (IsString m) => Bool -> m
boolToStr True = "1"
boolToStr False = ""