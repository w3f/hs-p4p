{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}

module P4P.RT.Node.UDP where

-- external
import qualified Data.ByteString           as BS
import qualified Data.Map.Strict           as M
import qualified Data.Text.Extra           as T

import           Control.Monad             (when)
import           Data.Foldable             (toList, traverse_)
import           Data.Text                 (Text)
import           P4P.Proc                  (Observation (..), SockAddr,
                                            UMsg (..), UProtocol (..))

-- external, impure
import           Control.Clock.IO          (Clock (..), IOClock)
import           Control.Concurrent.MVar   (newMVar, tryTakeMVar)
import           Network.Socket            hiding (SockAddr)
import           Network.Socket.ByteString (recvFrom, sendTo)
import           System.IO.Error           (tryIOError)

-- internal
import           P4P.RT.Internal           (RTLoIO)
import           P4P.RT.Network


udpRTLoIO
  :: UProtocol ps
  => Addr ps ~ SockAddr
  => IOClock
  -> (Text -> IO ())
  -> ps
  -> IO (RTLoIO ps, IO ())
udpRTLoIO clock logger ps = do
  let ep = EndpointByAddr $ toNAddr $ head $ toList $ getAddrs ps
  (sock, addr') <- socketFromEndpoint ep (pure . head) Datagram defaultProtocol
  bind sock addr'
  myAddr <- getSocketName sock >>= newMVar

  -- TODO: it is more accurate to use the techniques described here:
  -- https://blog.powerdns.com/2012/10/08/on-binding-datagram-udp-sockets-to-the-any-addresses/
  -- this becomes necessary when we support binding on multiple sockets
  -- probably much of the work should be filed upstream to Network.Socket
  let logErr s = logger $ "udpRTLoIO: " <> s
      i = tryIOError (recvFrom sock 65536) >>= \case
        Left err -> do
          logErr $ "recv failed: " <> T.show err
          i -- try again
        Right (body, src) -> do
          pure $ Just $ UData (fromNAddr src) body

      i' = tryTakeMVar myAddr >>= \case
        Nothing   -> i
        Just addr -> do
          now <- clockNow clock
          let firstObs = M.singleton (fromNAddr addr) (ObsPositive now)
          pure $ Just $ UOwnAddr firstObs

      o = \case
        UData dst body -> tryIOError (sendTo sock body (toNAddr dst)) >>= \case
          Left err -> do
            logErr $ "send failed: " <> T.show err
          Right sent -> do
            let want = BS.length body
            when (sent < want) $ do
              logErr
                $  "sent too short: "
                <> T.show want
                <> " vs "
                <> T.show sent
        UOwnAddr _ -> do
          error "not implemented"

  pure ((i', traverse_ o), close sock)
