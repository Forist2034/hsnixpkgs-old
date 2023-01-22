module HsNixPkgs.StdEnv.Linux.BootstrapFiles
  ( BootstrapFiles (..),
    bt_i686,
    bt_x86_64,
    bt_x86_84_musl,
  )
where

import Data.Text (Text)
import HsNix.Builtin.FetchUrl
import HsNix.Derivation
import HsNix.Hash

data BootstrapFiles m = BFile
  { busybox, bootstrapTools :: Derivation m
  }

fetch :: BuiltinFetchUrl m => Text -> Text -> Hash SHA256 -> Bool -> Derivation m
fetch n u h e = fetchUrl ((defFetchUrlArg n u h) {isExecutable = e})

mkBT :: (BuiltinFetchUrl m) => (Text, String) -> (Text, String) -> BootstrapFiles m
mkBT (b, hb) (t, ht) =
  BFile
    { busybox = fetch "busybox" b (read hb) True,
      bootstrapTools = fetch "bootstrap-tools.tar.xz" t (read ht) False
    }

bt_i686, bt_x86_64, bt_x86_84_musl :: (BuiltinFetchUrl m) => BootstrapFiles m
bt_i686 =
  mkBT
    ( "http://tarballs.nixos.org/stdenv-linux/i686/4907fc9e8d0d82b28b3c56e3a478a2882f1d700f/busybox",
      "ef4c1be6c7ae57e4f654efd90ae2d2e204d6769364c46469fa9ff3761195cba1"
    )
    ( "http://tarballs.nixos.org/stdenv-linux/i686/c5aabb0d603e2c1ea05f5a93b3be82437f5ebf31/bootstrap-tools.tar.xz",
      "b9bf20315f8c5c0411679c5326084420b522046057a0850367c67d9514794f1c"
    )
bt_x86_64 =
  bt_i686
    { bootstrapTools =
        fetch
          "bootstrap-tools.tar.xz"
          "http://tarballs.nixos.org/stdenv-linux/x86_64/c5aabb0d603e2c1ea05f5a93b3be82437f5ebf31/bootstrap-tools.tar.xz"
          (read "a5ce9c155ed09397614646c9717fc7cd94b1023d7b76b618d409e4fefd6e9d39" :: Hash SHA256)
          False
    }
bt_x86_84_musl =
  mkBT
    ( "https://wdtz.org/files/gywxhjgl70sxippa0pxs0vj5qcgz1wi8-stdenv-bootstrap-tools/on-server/busybox",
      "0779c2wn00467h76xpqil678gfi1y2p57c7zq2d917jsv2qj5009"
    )
    ( "https://wdtz.org/files/gywxhjgl70sxippa0pxs0vj5qcgz1wi8-stdenv-bootstrap-tools/on-server/bootstrap-tools.tar.xz",
      "1dwiqw4xvnm0b5fdgl89lz2qq45f6s9icwxn6n6ams71xw0dbqyi"
    )