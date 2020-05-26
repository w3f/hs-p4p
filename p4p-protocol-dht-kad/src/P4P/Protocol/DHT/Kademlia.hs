{-| P4P kademlia DHT protocol. -}

module P4P.Protocol.DHT.Kademlia
  ( NodeId
  , NodeAddr
  , KParams(..)
  , NodeInfo(..)
  , Key
  , Value
  , State(..)
  , KState
  , KMsg
  , KUserI
  , KUserO
  , KadI
  , KadO
  , defaultParams
  , defaultParams'
  , emptyState
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
