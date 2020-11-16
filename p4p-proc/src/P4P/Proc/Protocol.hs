{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module P4P.Proc.Protocol where

-- external
import qualified Data.Map.Strict   as M

import           Codec.Serialise   (Serialise)
import           Data.Binary       (Binary)
import           Data.Kind         (Type)
import           Data.Schedule     (Tick)
import           GHC.Generics      (Generic)

-- internal
import           P4P.Proc.Internal


data Observation t = ObsPositive !t | ObsNegative !t
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

type Observations a = M.Map a (Observation Tick)

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
