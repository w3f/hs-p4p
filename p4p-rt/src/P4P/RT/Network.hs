{-# LANGUAGE LambdaCase #-}

-- | Compatibility shims and utilities for "Network.Socket" etc.
module P4P.RT.Network where

-- external
import qualified Data.ByteString.Char8 as BS
import qualified P4P.Proc              as P

import           Control.Exception     (IOException, try)
import           Control.Lens.Iso      (Iso', iso)
import           Control.Op
import           Network.Socket
import           Options.Applicative
import           System.IO.Unsafe      (unsafePerformIO)
import           Text.Read             (readEither)


toNPort :: P.PortNumber -> PortNumber
toNPort = fromInteger . toInteger

fromNPort :: PortNumber -> P.PortNumber
fromNPort = fromInteger . toInteger

toNAddr :: P.SockAddr -> SockAddr
toNAddr = \case
  P.SockAddrInet p h      -> SockAddrInet (toNPort p) h
  P.SockAddrInet6 p f h s -> SockAddrInet6 (toNPort p) f h s
  P.SockAddrUnix p        -> SockAddrUnix (BS.unpack p)

fromNAddr :: SockAddr -> P.SockAddr
fromNAddr = \case
  SockAddrInet p h      -> P.SockAddrInet (fromNPort p) h
  SockAddrInet6 p f h s -> P.SockAddrInet6 (fromNPort p) f h s
  SockAddrUnix p        -> P.SockAddrUnix (BS.pack p)

isoPort :: Iso' PortNumber P.PortNumber
isoPort = iso fromNPort toNPort

isoAddr :: Iso' SockAddr P.SockAddr
isoAddr = iso fromNAddr toNAddr

isoNPort :: Iso' P.PortNumber PortNumber
isoNPort = iso toNPort fromNPort

isoNAddr :: Iso' P.SockAddr SockAddr
isoNAddr = iso toNAddr fromNAddr


-- endpoints are due for upstreaming in
-- https://github.com/haskell/network/pull/464
-- in the meantime we keep them here

-- | Socket endpoints.
--
-- A wrapper around socket addresses that also accommodates the
-- popular usage of specifying them by name, e.g. "example.com:80".
-- We don't support service names here (string aliases for port
-- numbers) because they also imply a particular socket type, which
-- is outside of the scope of this data type.
--
-- This roughly corresponds to the "authority" part of a URI, as
-- defined here: https://tools.ietf.org/html/rfc3986#section-3.2
--
-- See also 'Network.Socket.socketFromEndpoint'.
data SockEndpoint
  = EndpointByName !String !PortNumber
  | EndpointByAddr !SockAddr
  deriving (Eq, Ord)

sockAddrFamily :: SockAddr -> Family
sockAddrFamily addr = case addr of
  SockAddrInet _ _ -> AF_INET
  SockAddrInet6{}  -> AF_INET6
  SockAddrUnix _   -> AF_UNIX

-- | Read a string representing a socket endpoint.
readSockEndpoint :: PortNumber -> String -> Either String SockEndpoint
readSockEndpoint defPort hostport = case hostport of
  '/' : _  -> Right $ EndpointByAddr $ SockAddrUnix hostport
  '[' : tl -> case span (']' /=) tl of
    (_   , []      ) -> Left $ "unterminated IPv6 address: " <> hostport
    (ipv6, _ : port) -> case readAddr ipv6 of
      Nothing   -> Left $ "invalid IPv6 address: " <> ipv6
      Just addr -> EndpointByAddr . sockAddrPort addr <$> readPort port
  _ -> case span (':' /=) hostport of
    (host, port) -> case readAddr host of
      Nothing   -> EndpointByName host <$> readPort port
      Just addr -> EndpointByAddr . sockAddrPort addr <$> readPort port
 where
  readPort ""           = Right defPort
  readPort ":"          = Right defPort
  readPort (':' : port) = case readEither port of
    Right p -> Right p
    Left  _ -> Left $ "bad port: " <> port
  readPort x = Left $ "bad port: " <> x
  hints = Just $ defaultHints { addrFlags = [AI_NUMERICHOST] }
  readAddr host =
    case unsafePerformIO (try (getAddrInfo hints (Just host) Nothing)) of
      Left  e -> Nothing where _ = e :: IOException
      Right r -> Just (addrAddress (head r))
  sockAddrPort h p = case h of
    SockAddrInet _ a      -> SockAddrInet p a
    SockAddrInet6 _ f a s -> SockAddrInet6 p f a s
    x                     -> x

showSockEndpoint :: SockEndpoint -> String
showSockEndpoint n = case n of
  EndpointByName h p -> h <> ":" <> show p
  EndpointByAddr a   -> show a

-- | Resolve a socket endpoint into a list of socket addresses.
-- The result is always non-empty; Haskell throws an exception if name
-- resolution fails.
resolveEndpoint :: SockEndpoint -> IO [SockAddr]
resolveEndpoint name = case name of
  EndpointByAddr a -> pure [a]
  EndpointByName host port ->
    fmap addrAddress <$> getAddrInfo hints (Just host) (Just (show port))
  where hints = Just $ defaultHints { addrSocketType = Stream }
  -- prevents duplicates, otherwise getAddrInfo returns all socket types

-- | Shortcut for creating a socket from a socket endpoint.
--
-- >>> import Network.Socket
-- >>> let Right sn = readSockEndpoint 0 "0.0.0.0:0"
-- >>> (s, a) <- socketFromEndpoint sn (pure . head) Stream defaultProtocol
-- >>> bind s a
socketFromEndpoint
  :: SockEndpoint
  -> ([SockAddr] -> IO SockAddr)
  -> SocketType
  -> ProtocolNumber
  -> IO (Socket, SockAddr)
socketFromEndpoint end select stype protocol = do
  a <- resolveEndpoint end >>= select
  s <- socket (sockAddrFamily a) stype protocol
  pure (s, a)


parseRecvAddr :: PortNumber -> Parser SockEndpoint
parseRecvAddr defPort =
  option (eitherReader (readSockEndpoint defPort))
    <| long "recv-addr"
    <> short 'r'
    <> metavar "ADDR"
    <> help
         (  "Endpoint address to receive at. "
         <> "Can be DNS:PORT, IPv6:PORT, IPv4:PORT or a UNIX path."
         )
    <> value (EndpointByName "localhost" defPort)
    <> showDefaultWith showSockEndpoint
