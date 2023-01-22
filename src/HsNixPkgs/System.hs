module HsNixPkgs.System
  ( module C,
    module V,
    module A,
    module K,
    System (..),
    canExecute,
    isLinux,
    isStatic,
    toNixSys,
    toConfigTriple,
  )
where

import Data.Text (Text)
import qualified HsNix.System as NS
import HsNixPkgs.System.Abi as A
import HsNixPkgs.System.Cpu as C
import HsNixPkgs.System.Kernel as K
import HsNixPkgs.System.Vendor as V

data System = System
  { cpu :: Cpu,
    vendor :: Vendor,
    kernel :: Kernel,
    abi :: Abi
  }
  deriving (Show, Eq)

canExecute :: System -> System -> Bool
canExecute e host = isCompatible (cpu e) (cpu host) && kernel e == kernel host

isLinux :: System -> Bool
isLinux sys = kernel sys == linux

isStatic :: System -> Bool
isStatic = const False

toNixSys :: System -> NS.System
toNixSys sys =
  if cpu sys == x86_64 && kernel sys == linux
    then NS.x86_64_linux
    else error ("Unsupported system " ++ show sys)

toConfigTriple :: System -> Text
toConfigTriple s =
  mconcat
    [ cpuName (cpu s),
      "-",
      vendorName (vendor s),
      "-",
      kernelName (kernel s),
      if abi s /= Unknown then "-" <> abiName (abi s) else mempty
    ]