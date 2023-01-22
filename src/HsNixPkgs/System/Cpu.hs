{-# OPTIONS_GHC -Wno-partial-fields #-}

module HsNixPkgs.System.Cpu
  ( BitWidth (..),
    SignificantByte,
    CpuType (..),
    cpuSignificantByte,
    Cpu (..),
    isCompatible,
    i686,
    x86_64,
  )
where

import Data.Text (Text)
import GHC.ByteOrder

data BitWidth
  = Cpu8
  | Cpu16
  | Cpu32
  | Cpu64
  deriving (Eq, Show)

type SignificantByte = ByteOrder

data CpuType = X86
  { cpuBitWidth :: BitWidth,
    arch :: Text
  }
  deriving (Eq, Show)

cpuSignificantByte :: CpuType -> SignificantByte
cpuSignificantByte (X86 {}) = LittleEndian

data Cpu = Cpu
  { cpuName :: Text,
    cpuType :: CpuType,
    -- | test if program built for other cpu run on this cpu
    isCompatibleFun :: CpuType -> Bool
  }

instance Show Cpu where
  show c = show (cpuType c)

instance Eq Cpu where
  l == r = cpuType l == cpuType r

-- | isCompatible a b == True -> program built for b can run on a
isCompatible :: Cpu -> Cpu -> Bool
isCompatible l r = l == r || isCompatibleFun r (cpuType l)

i686, x86_64 :: Cpu
i686 = Cpu "i686" (X86 {cpuBitWidth = Cpu32, arch = "i686"}) (const False)
x86_64 = Cpu "x86-64" (X86 {cpuBitWidth = Cpu64, arch = "x86-64"}) (== cpuType i686)
