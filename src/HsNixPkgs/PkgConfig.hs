{-# LANGUAGE ScopedTypeVariables #-}

module HsNixPkgs.PkgConfig
  ( PkgOverlay,
    emptyOverlay,
    overridePkg,
    getPkg,
    PkgConfig (..),
    PkgConfigT,
    runPkgConfigT,
    PkgConfigM,
    runPkgConfig,
  )
where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Data
import Data.Dynamic
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import HsNixPkgs.System

data TypeId = TI String TypeRep
  deriving (Eq)

instance Hashable TypeId where
  hashWithSalt s (TI v _) = hashWithSalt s v

mkTypeId :: Typeable t => Proxy t -> TypeId
mkTypeId t =
  let rep = typeRep t
   in TI (show rep) rep

newtype PkgOverlay = PO (HM.HashMap TypeId Dynamic)

emptyOverlay :: PkgOverlay
emptyOverlay = PO HM.empty

overridePkg :: forall t. Typeable t => t -> PkgOverlay -> PkgOverlay
overridePkg v (PO o) = PO (HM.insert (mkTypeId (Proxy :: Proxy t)) (toDyn v) o)

getPkg :: forall t. Typeable t => t -> PkgOverlay -> t
getPkg v (PO o) =
  maybe v (`fromDyn` v) $
    HM.lookup (mkTypeId (Proxy :: Proxy t)) o

data PkgConfig = PkgConfig
  { pkgBuildPlatform :: System,
    pkgOverlay :: PkgOverlay
  }

type PkgConfigT = ReaderT PkgConfig

runPkgConfigT :: PkgConfig -> PkgConfigT m a -> m a
runPkgConfigT pc v = runReaderT v pc

type PkgConfigM = PkgConfigT Identity

runPkgConfig :: PkgConfig -> PkgConfigT Identity a -> a
runPkgConfig pc v = runReader v pc