{-# LANGUAGE LambdaCase #-}

module P4P.RT.Protocol where

-- external
import qualified Data.ByteString.Char8 as BS
import qualified Network.Socket        as N

import           Data.Word
import           P4P.Proc


toNPort :: Word16 -> N.PortNumber
toNPort = fromInteger . toInteger

fromNPort :: N.PortNumber -> Word16
fromNPort = fromInteger . toInteger

toNAddr :: SockAddr -> N.SockAddr
toNAddr = \case
  SockAddrInet p h      -> N.SockAddrInet (toNPort p) h
  SockAddrInet6 p f h s -> N.SockAddrInet6 (toNPort p) f h s
  SockAddrUnix p        -> N.SockAddrUnix (BS.unpack p)

fromNAddr :: N.SockAddr -> SockAddr
fromNAddr = \case
  N.SockAddrInet p h      -> SockAddrInet (fromNPort p) h
  N.SockAddrInet6 p f h s -> SockAddrInet6 (fromNPort p) f h s
  N.SockAddrUnix p        -> SockAddrUnix (BS.pack p)
