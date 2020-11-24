{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeFamilies        #-}

module P4P.RT.Node.UDP where

-- external
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Map.Strict           as M

import           Codec.Serialise           (Serialise (..), deserialiseOrFail,
                                            serialise)
import           Control.Monad             (when)
import           Data.Foldable             (toList, traverse_)
import           P4P.Proc                  (Observation (..), ProcIface (..),
                                            SockAddr, UMsg (..), UPMsgI, UPMsgO,
                                            UProtocol (..))

-- external, impure
import           Control.Clock.IO          (Clock (..), IOClock)
import           Control.Concurrent.MVar   (newMVar, tryTakeMVar)
import           Network.Socket            hiding (SockAddr)
import           Network.Socket.ByteString
import           System.IO                 (hPutStrLn, stderr)

-- internal
import           P4P.RT.Internal           (RTLoIO)
import           P4P.RT.Network


udpRTLoIO
  :: UProtocol ps
  => Addr ps ~ SockAddr
  => LoI ps ~ UPMsgI ps
  => LoO ps ~ UPMsgO ps
  => Serialise (Msg ps) => IOClock -> ps -> IO (RTLoIO ps, IO ())
udpRTLoIO clock ps = do
  let ep = EndpointByAddr $ toNAddr $ head $ toList $ getAddrs ps
  (sock, addr') <- socketFromEndpoint ep (pure . head) Datagram defaultProtocol
  bind sock addr'
  myAddr <- getSocketName sock >>= newMVar

  -- TODO: it is more accurate to use the techniques described here:
  -- https://blog.powerdns.com/2012/10/08/on-binding-datagram-udp-sockets-to-the-any-addresses/
  -- this becomes necessary when we support binding on multiple sockets
  -- probably much of the work should be filed upstream to Network.Socket
  let logErr s = hPutStrLn stderr $ "udpRTLoIO: " <> s
      i = tryTakeMVar myAddr >>= \case
        Nothing -> do
          (body, src) <- recvFrom sock 65536
          case deserialiseOrFail $ LBS.fromStrict body of
            Right msg -> do
              pure $ Just $ UData (fromNAddr src) msg
            Left e -> do
              logErr $ "bad recv: " <> show e
              i -- try again
        Just addr -> do
          now <- clockNow clock
          let firstObs = M.singleton (fromNAddr addr) (ObsPositive now)
          pure $ Just $ UOwnAddr firstObs

      o = \case
        UData dst msg -> do
          let body = LBS.toStrict $ serialise msg
          sent <- sendTo sock body (toNAddr dst)
          let want = BS.length body
          when (sent < want) $ do
            logErr $ "short send: " <> show want <> " vs " <> show sent
        UOwnAddr _ -> do
          error "not implemented"

  pure ((i, traverse_ o), close sock)
