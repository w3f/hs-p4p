{-| P4P kademlia DHT protocol. -}

module P4P.Protocol.DHT.Kademlia
  ( NodeId
  , NodeAddr
  , KParams(..)
  , NodeInfo(..)
  , CmdId
  , reqIdWidth
  , Command(..)
  , CommandBody(..)
  , CommandReply(..)
  , RequestRejected(..)
  , CommandReplyBody(..)
  , Key
  , Value
  , State(..)
  , kSelf
  -- High-level types
  , KState
  , KMsg
  , KHiI
  , KHiO
  , KadI
  , KadO
  , defaultParams
  , testingParams
  , newState
  , newRandomState
  -- from Internal
  , kInput
  )
where

import           P4P.Protocol.DHT.Kademlia.Internal
import           P4P.Protocol.DHT.Kademlia.Message
import           P4P.Protocol.DHT.Kademlia.State

type KState = State
type KMsg = Msg

{- implementation conventions:

we postfix partial functions names with '_', and it is up to the caller to use
them in a context where they won't hit their partiality

-}
