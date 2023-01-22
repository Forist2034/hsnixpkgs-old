module HsNixPkgs.Types
  ( EnvHook,
    insert,
    fromList,
    singleton,
    toDrvEnvs,
  )
where

import Control.Arrow
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import HsNix.Derivation

newtype EnvHook m = EH (HM.HashMap Text (DrvStrBuilder m -> DrvStrBuilder m))

instance Semigroup (EnvHook m) where
  EH l <> EH r = EH (HM.unionWith (.) l r)

instance Monoid (EnvHook m) where
  mempty = EH HM.empty

insert :: Text -> (DrvStrBuilder m -> DrvStrBuilder m) -> EnvHook m -> EnvHook m
insert k v (EH e) = EH (HM.insertWith (>>>) k v e)

fromList :: [(Text, DrvStrBuilder m -> DrvStrBuilder m)] -> EnvHook m
fromList l = EH (HM.fromList l)

singleton :: Text -> (DrvStrBuilder m -> DrvStrBuilder m) -> EnvHook m
singleton k v = EH (HM.singleton k v)

toDrvEnvs :: MonadDeriv m => EnvHook m -> [(Text, DrvStr m)]
toDrvEnvs (EH e) = HM.foldMapWithKey (\k v -> [(k, toDrvStr (v mempty))]) e