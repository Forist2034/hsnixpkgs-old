module HsNixPkgs.BuildSupport.Fetcher.Url.Boot
  ( FetchUrlArg (..),
    fetchUrlBoot,
  )
where

import Data.Text (Text)
import qualified HsNix.Builtin.FetchUrl as F
import HsNix.Hash
import HsNixPkgs.ExtendDrv

data FetchUrlArg a = FetchUrlArg
  { name, url :: Text,
    outputHash :: Hash a
  }
  deriving (Show, Eq)

fetchUrlBoot :: (F.BuiltinFetchUrl m, NamedHashAlgo a) => FetchUrlArg a -> FileDeriv m
fetchUrlBoot fa =
  let d =
        F.fetchUrl
          (F.defFetchUrlArg (name fa) (url fa) (outputHash fa))
   in FileDrv
        { fDrvBase = d,
          fDrvIsDirectory = False,
          fDrvPath = name fa
        }