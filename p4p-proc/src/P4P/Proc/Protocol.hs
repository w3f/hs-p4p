{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module P4P.Proc.Protocol where

-- external
import qualified Data.Map.Strict   as M

import           Codec.Serialise   (Serialise)
import           Data.Binary       (Binary)
import           Data.ByteString   (ByteString)
import           Data.Function     (on)
import           Data.Kind         (Type)
import           Data.Map.Strict   (Map)
import           Data.Schedule     (Tick)
import           Data.Word
import           GHC.Generics      (Generic)

-- internal
import           P4P.Proc.Internal


type PortNumber = Word16
type HostAddress = Word32
type HostAddress6 = (Word32, Word32, Word32, Word32)
type FlowInfo = Word32
type ScopeID = Word32

{- | Default type of address.

Same as t'Network.Socket.SockAddr' but fully-strict with sane instances.
-}
data SockAddr =
    SockAddrInet !PortNumber !HostAddress
  | SockAddrInet6 !PortNumber !FlowInfo !HostAddress6 !ScopeID
  | SockAddrUnix !ByteString
  deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

data Observation t = ObsPositive !t | ObsNegative !t
 deriving (Eq, Show, Read, Generic, Binary, Serialise)

obsToPair :: Observation t -> (t, Bool)
obsToPair = \case
  ObsPositive t -> (t, True)
  ObsNegative t -> (t, False)

instance Ord t => Ord (Observation t) where
  compare = compare `on` obsToPair

obsIsPositive :: Observation a -> Bool
obsIsPositive = \case
  ObsPositive _ -> True
  ObsNegative _ -> False

obsIsNegative :: Observation a -> Bool
obsIsNegative = \case
  ObsPositive _ -> False
  ObsNegative _ -> True

type Observations a = Map a (Observation Tick)

obsPositiveFromList :: Ord a => Tick -> [a] -> Observations a
obsPositiveFromList tick addrs = M.fromList $ (, ObsPositive tick) <$> addrs

updateTrustedObs :: Ord a => Observations a -> Observations a -> Observations a
updateTrustedObs = M.unionWith max

{- | Protocol sending unordered unreliable datagrams, the simplest protocol.

When defining this instance for your @$ps@, you should also define:

@
instance 'ProcIface' $ps where
  type LoI $ps = 'UPMsgI' $ps
  type LoO $ps = 'UPMsgO' $ps
  -- remaining defs ..
@

See 'UMsg' for more details.
-}
class UProtocol ps where
  {- | Entity address, used for sending and receiving messages.

  Depending on the protocol, this may or may not uniquely identify the entity.
  -}
  type Addr ps :: Type
  type Addr ps = SockAddr
  -- | Main protocol message type, for external communication between entities.
  type Msg ps :: Type

-- | Local message that encodes actions within 'UProtocol'.
--
-- For simplicity, we reuse this in both directions.
data UMsg (dir :: Direction) addr msg
  = UData !addr !msg
    {- ^ Communicate with another entity.

    For 'Incoming' direction, we are receiving the message, and the address is
    the source of the message.

    For 'Outgoing' direction, we are sending the message, and the address is
    the destination of the message.
    -}
  | UOwnAddr !(Observations addr)
    {- ^ Observations about the receive-address(es) of a process.

    For 'Incoming' direction, the other component is telling us about addresses
    they observed. For example if they failed to bind to an address, we would
    get a negative observation for that address. If the whole observation is
    'Data.Foldable.null', we should take this as an implicit request to send
    them our view of what our addresses should be.

    For 'Outgoing' direction, we are telling the other component what our
    addresses should be. This may or may not actually succeed; if this matters
    to us  then we should watch for replies back from the other component and
    take action if we don't get a timely confirmation.
    -}
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

type UMsgI addr msg = UMsg 'Incoming addr msg
type UMsgO addr msg = UMsg 'Outgoing addr msg

type UPMsgI ps = UMsgI (Addr ps) (Msg ps)
type UPMsgO ps = UMsgO (Addr ps) (Msg ps)

-- | Type alias for the address of a process.
type ProcAddr p = Addr (State p)
