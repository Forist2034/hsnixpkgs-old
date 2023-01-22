{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HsNixPkgs.ConfigSet
  ( ConfigSet,
    emptyCfgSet,
    getConfig,
    hookConfig,
  )
where

import Data.Dynamic
import qualified Data.HashMap.Lazy as HM
import Data.Hashable
import Data.Typeable

data TypeId = TI Int TypeRep
  deriving (Show, Eq)

instance Hashable TypeId where
  hashWithSalt _ (TI h _) = h

mkTypeId :: Typeable a => Proxy a -> TypeId
mkTypeId p =
  let rep = typeRep p
   in TI (hash (show rep)) rep

newtype ConfigSet = CSet (HM.HashMap TypeId Dynamic)
  deriving (Show)

getConfigFun :: Typeable a => TypeId -> ConfigSet -> Maybe (a -> a)
getConfigFun ti (CSet s) = HM.lookup ti s >>= fromDynamic

emptyCfgSet :: ConfigSet
emptyCfgSet = CSet HM.empty

getConfig :: forall a. Typeable a => ConfigSet -> a -> a
getConfig s v = maybe v (\f -> f v) (getConfigFun (mkTypeId @(a -> a) Proxy) s)

hookConfig :: forall a. Typeable a => (a -> a) -> ConfigSet -> ConfigSet
hookConfig f cs@(CSet s) =
  CSet
    ( let ti = mkTypeId @(a -> a) Proxy
       in case getConfigFun ti cs of
            Just f0 -> HM.insert ti (toDyn (f . f0)) s
            Nothing -> HM.insert ti (toDyn f) s
    )