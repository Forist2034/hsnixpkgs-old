{-# OPTIONS_GHC -Wno-partial-fields #-}

module HsNixPkgs.System.Kernel
  ( ExecFormat (..),
    KernelFamily (..),
    Kernel (..),
    execFormat,
    kernelFamily,
    kernelName,
    linux,
  )
where

import Data.Text (Text)

data ExecFormat
  = Aout
  | Elf
  | UnknownExecFormat
  deriving (Show, Eq)

data KernelFamily
  = Bsd
  | Darwin
  deriving (Show, Eq)

data Kernel = Linux
  deriving (Show, Eq)

execFormat :: Kernel -> ExecFormat
execFormat Linux = Elf

kernelFamily :: Kernel -> Maybe KernelFamily
kernelFamily Linux = Nothing

kernelName :: Kernel -> Text
kernelName Linux = "linux"

linux :: Kernel
linux = Linux