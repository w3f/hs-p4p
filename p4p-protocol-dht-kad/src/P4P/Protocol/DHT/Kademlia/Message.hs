{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Public messaging types.
module P4P.Protocol.DHT.Kademlia.Message where

-- external
import qualified Control.Monad.Schedule  as SC
import qualified Control.Schedule.Future as F
import qualified Data.ByteString         as BS
import qualified Data.Map.Strict         as M
import qualified Data.Sequence.Extra     as S
import qualified P4P.Proc                as P

import           Control.Lens.TH.Extra   (makeLenses_)
import           GHC.Generics            (Generic)
import           GHC.Stack               (HasCallStack)


type NodeAddr = String -- TODO: real address
type Value = BS.ByteString
type ReqId = BS.ByteString -- TODO: h-bit string, TODO: enforce h
type Key = BS.ByteString -- TODO: h-bit string, TODO: enforce h
type NodeId = Key
type CmdId = BS.ByteString

type KSeq a = S.BSeq a -- sequence of at most k entries, TODO: enforce

data NodeInfo a = NodeInfo
  { niNodeId   :: !NodeId
  , niNodeAddr :: !(KSeq a)
  } deriving (Show, Read, Generic, Eq, Ord, Functor)
makeLenses_ ''NodeInfo

type NodeInfos = KSeq (NodeInfo NodeAddr)

data KTask =
    SelfCheck
  | RefreshBucket !Int
  | RepublishKey !Key
  | ExpireKey !Key
  | TOOReq !NodeId !RequestBody
  | TOOReqReply !NodeId !RequestBody
  | TOIReq !NodeId !RequestBody
  | TOICmdOReq !CmdId !NodeId !RequestBody
  | TOICmd !CmdId
    deriving (Show, Read, Generic, Eq, Ord)

data RequestRejected =
    TryAgainLater !SC.TickDelta
    deriving (Show, Read, Generic, Eq, Ord)

data RequestBody =
    Ping
  | GetNode !NodeId
  | GetValue !Key
  | PutValue !Key !Value
    deriving (Show, Read, Generic, Eq, Ord)

data Request = Request
  { reqId   :: !ReqId
  , reqBody :: !RequestBody
  } deriving (Show, Read, Generic, Eq, Ord)

data ReplyBody =
    Pong
  | GetNodeReply !NodeInfos
  | GetValueReply !(Either NodeInfos Value)
  | PutValueReply !SC.TickDelta
    deriving (Show, Read, Generic, Eq, Ord)

data Reply = Reply
  { repReqId      :: !ReqId
  , repReqDstAddr :: !NodeAddr
  -- ^ The dstAddr field of the original request that triggered this reply.
  , repPing       :: !(Maybe ReqId)
  -- ^ Pings can also be piggy-backed on RPC replies for the RPC recipient to
  -- obtain additional assurance of the sender's network address.
  , repBody       :: !(Either RequestRejected ReplyBody)
  } deriving (Show, Read, Generic, Eq, Ord)

type MsgBody = Either Request Reply

data Msg = Msg
  { src     :: !NodeId
  , srcAddr :: !NodeAddr
  , dst     :: !NodeId
  , dstAddr :: !NodeAddr
  -- ^ The destination address the sender attempted to send the message to.
  -- The insecure network may route it to a different one, but the recipient
  -- will at see what we authenticate here, once we add that functionality.
  , sent    :: !SC.Tick
  , body    :: !MsgBody
  } deriving (Show, Read, Generic, Eq, Ord)

-- | Given an incoming reply, if it is also an implicit ping this function will
-- generate the "virtual request" that represents that ping.
pingReplyToRequest :: Msg -> Maybe Msg
pingReplyToRequest msg@Msg {..} = case body of
  Left  _          -> Nothing
  Right Reply {..} -> f <$> repPing   where
    f reqId = msg { body = Left Request { reqId = reqId, reqBody = Ping } }

replyForRequest
  :: HasCallStack
  => Msg
  -> Either RequestRejected ReplyBody
  -> SC.Tick
  -> NodeAddr
  -> KadO
replyForRequest req@Msg {..} reply now addr = case body of
  Right _            -> error "input message was not a request"
  Left  Request {..} -> P.MsgProc $ Msg
    { src     = dst
    , srcAddr = dstAddr
    , dst     = src
    , dstAddr = addr
    , sent    = now
    , body    = Right Reply { repReqId      = reqId
                            , repReqDstAddr = dstAddr
                            , repPing       = Nothing
                            , repBody       = reply
                            }
    }

{- | High-level command from a user.

Satisfying this takes up several outgoing requests.
-}
data Command = Command
  { cmdId   :: !CmdId
  , cmdBody :: !CommandBody
  } deriving (Show, Read, Generic, Eq, Ord)

data CommandBody =
    JoinNetwork !(NodeInfo NodeAddr)
  | LookupNode !NodeId
  | LookupValue  !Key
  | InsertValue !Key !Value
    deriving (Show, Read, Generic, Eq, Ord)

data CommandReply = CommandReply
  { cmdRepId   :: !CmdId
  , cmdRepBody :: !(Either RequestRejected CommandReplyBody)
  } deriving (Show, Read, Generic, Eq, Ord)

data CommandReplyBody =
    CommandTimedOut !SC.TickDelta
  | JoinNetworkReply !NodeInfos
  | LookupNodeReply !NodeInfos
  | LookupValueReply !(Either NodeInfos Value)
  | InsertValueReply !(M.Map NodeId (Maybe SC.TickDelta))
    deriving (Show, Read, Generic, Eq, Ord)


{- | Logging message types -}

-- | Data structure summarising some event relating to a generic process.
data KProcessEvt =
    KProcessNew
  | KProcessStepAdd
  | KProcessStepRem
  | KProcessDel
  | KProcessNopNew
  | KProcessNopStep
  | KProcessNopDel
    deriving (Show, Read, Generic, Eq, Ord)

-- TODO: CommandUpdate for status / progress reports
data KLogMsg =
    ICmdErr !CmdId !KProcessEvt !F.SFError
  | ICmdEvt !CmdId !KProcessEvt
  | IReqEvt !(ReqId, RequestBody) !KProcessEvt
  | OReqEvt !(ReqId, RequestBody) !KProcessEvt
    deriving (Show, Read, Generic, Eq, Ord)


{- | Top-level message types -}

type KUserI = Command
type KUserO = Either KLogMsg CommandReply

instance P.ProtoMsg Msg where
  type Addr Msg = NodeAddr
  getTarget = dstAddr
  setSource srcAddr' m = m { srcAddr = srcAddr' }

type KadI' = P.GMsg (P.RuntimeI ()) KUserI Msg
type KadI = P.GMsg (P.RuntimeI KTask) KUserI Msg
type KadO = P.GMsg (P.RuntimeO NodeAddr) KUserO Msg

kLog :: KLogMsg -> KadO
kLog = P.MsgUser . Left
