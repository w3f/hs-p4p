{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module P4P.Proc.Types where

-- external
import qualified Data.Map.Strict as M

import           Codec.Serialise (Serialise)
import           Data.Binary     (Binary)
import           Data.Kind       (Type)
import           Data.Schedule   (Tick)
import           Data.Void       (Void)
import           GHC.Generics    (Generic)


{- | Protocol message. -}
class ProtoMsg msg where
  {- | Type of protocol entity that can send and receive messages.

  Depending on the protocol, this may be a name, identity or address, and may
  or may not uniquely identify the entity.
  -}
  type Ent msg

  -- | Get the target of a message sent by a process, used by the execution
  -- environment to decide how to send an outgoing message.
  getTarget :: msg -> Ent msg

  -- | Set the source of a message received by a process, used by the execution
  -- environment to hint the receipt of an incoming message.
  setSource :: Ent msg -> msg -> msg

-- | Void message, for local-only processes such as simulations.
instance ProtoMsg Void where
  type Ent Void = Void
  getTarget = id
  setSource = const id

{- | Protocol with its protocol-messages and user-messages.

Typically one defines this instance on the protocol state type @ps@.
-}
class ProtoMsg (PMsg ps) => Protocol ps where
  -- | Protocol message type.
  type PMsg ps :: Type
  -- | Type of message from the user to a process.
  type UserI ps :: Type
  -- | Type of message from a process to the user.
  type UserO ps :: Type
  -- | Type of auxiliary message from a process.
  type AuxO ps :: Type
  type AuxO ps = Void

-- | Empty (unit) protocol, useful for composing with other protocols.
instance Protocol () where
  type PMsg () = Void
  type UserI () = Void
  type UserO () = Void
  type AuxO () = Void

type PEnt ps = Ent (PMsg ps)

{- | General message, from/to the runtime, the user, or another process. -}
data GMsg r u p a
  = MsgRT !r
  -- ^ Message from/to the runtime.
  | MsgUser !u
  -- ^ Message from/to the user, or another trusted local process.
  | MsgProc !p
  -- ^ Message from/to an external untrusted process.
  | MsgAux !a
  -- ^ Message from/to auxillary sources, e.g. for logging or debugging
  -- purposes. This is exempted from the deterministic behaviour contract.
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

data Observations a = Observations
  { obsPositive :: !(M.Map a Tick)
  , obsNegative :: !(M.Map a Tick)
  }
  deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)
