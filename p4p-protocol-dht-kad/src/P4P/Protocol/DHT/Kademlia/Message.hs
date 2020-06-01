{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Public messaging types.
module P4P.Protocol.DHT.Kademlia.Message where

-- external
import qualified Control.Monad.Schedule as SC
import qualified Data.ByteString        as BS
import qualified Data.Map.Strict        as M
import qualified Data.Sequence.Extra    as S
import qualified P4P.Proc               as P

import           Control.Lens.TH.Extra  (makeLenses_)
import           GHC.Generics           (Generic)
import           GHC.Stack              (HasCallStack)


{- | * External message types and other definitions. -}

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
  }
  deriving (Show, Read, Generic, Eq, Ord, Functor)
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

data RequestRejected = TryAgainLater !SC.TickDelta
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
  }
  deriving (Show, Read, Generic, Eq, Ord)

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
  }
  deriving (Show, Read, Generic, Eq, Ord)

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
  }
  deriving (Show, Read, Generic, Eq, Ord)

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
  }
  deriving (Show, Read, Generic, Eq, Ord)

data CommandBody =
    GetNodeId
  | JoinNetwork !(NodeInfo NodeAddr)
  | LookupNode !NodeId
  | LookupValue  !Key
  | InsertValue !Key !Value
    deriving (Show, Read, Generic, Eq, Ord)

data CommandReply = CommandReply
  { cmdRepId   :: !CmdId
  , cmdRepBody :: !(Either RequestRejected CommandReplyBody)
  }
  deriving (Show, Read, Generic, Eq, Ord)

data CommandReplyBody =
    CommandTimedOut !SC.TickDelta
  | OwnNodeId !NodeId
  | JoinNetworkReply !NodeInfos
  | LookupNodeReply !NodeInfos
  | LookupValueReply !(Either NodeInfos Value)
  | InsertValueReply !(M.Map NodeId (Maybe SC.TickDelta))
    deriving (Show, Read, Generic, Eq, Ord)


{- | * Logging message types, mostly for internal usage. -}

data ICmdState lookup insert =
    ICNewlyCreated
    -- ^ The command was just created and has yet to initialise.
  | ICLookingup !NodeId !lookup
    -- ^ The command is performing a lookup operation on some target key/node.
  | ICInserting !Value !insert
    -- ^ The command is performing an insert operation on some value.
  | ICFinished
    deriving (Show, Read, Generic, Eq, Ord)

icmdStateSummary :: ICmdState lookup insert -> ICmdState () ()
icmdStateSummary = \case
  ICNewlyCreated  -> ICNewlyCreated
  ICLookingup k _ -> ICLookingup k ()
  ICInserting v _ -> ICInserting v ()
  ICFinished      -> ICFinished

-- | Data structure summarising an internal ongoing process
data KProcess =
    KPICmd !CmdId
  | KPIReq !ReqId !RequestBody
  | KPOReq !ReqId !RequestBody
    deriving (Show, Read, Generic, Eq, Ord)

data KLogMsg =
    W_SelfCheckFailed !String
    -- ^ A self-check failed.
    -- This suggests a major programming error.
  | W_InvalidMessage !Msg !String
    -- ^ Top-level processing ignored an invalid message.
    -- This means someone on the network is playing tricks with us.
  | W_ICmdIgnoredInvalidInput !CmdId !NodeId !ReplyBody
    -- ^ 'ICommand' ignored an invalid input.
    -- This means someone on the network is playing tricks with us.
  | I_ICmdIgnoredInvalidInputGivenState !CmdId !NodeId !ReplyBody !(ICmdState () ())
    -- ^ 'ICommand' ignored an input which was invalid for the current state.
    -- This probably means the reply was slightly delayed, and not malicious.
  | I_ICmdStateChange !CmdId !(ICmdState () ())
    -- ^ 'ICommand' changed state (summarised).
  | I_OReqIgnoreDupReply !ReqId !RequestBody
    -- ^ Ignored a duplicate reply to an outgoing request.
    -- This probably means the network is experiencing congestion.
  | I_KeyExpired !Key
    -- ^ A key was expired and deleted from our store.
  | I_KProcessNew !KProcess
    -- ^ A kprocess was started.
  | I_KProcessIgnoreDup !KProcess
    -- ^ Ignored a duplicate attempt to start a new kprocess.
  | I_KProcessDel !KProcess
    -- ^ A kprocess was deleted.
  | D_ICmdOReqIgnoreDup !CmdId !ReqId !RequestBody
    -- ^ Ignored a duplicate attempt to expect an outgoing request.
    -- This suggests a minor programming error.
  | D_ICmdOReqIgnoreMis !CmdId !ReqId !RequestBody
    -- ^ Ignored a duplicate attempt to forget an outgoing request.
    -- This suggests a minor programming error.
    deriving (Show, Read, Generic, Eq, Ord)


{- | * Top-level message types -}

type KUserI = Command
type KUserO = CommandReply

instance P.ProtoMsg Msg where
  type Addr Msg = NodeAddr
  getTarget = dstAddr
  setSource srcAddr' m = m { srcAddr = srcAddr' }

type KadI' = P.GMsg (P.RuntimeI ()) KUserI Msg P.Void
type KadI = P.GMsg (P.RuntimeI KTask) KUserI Msg P.Void
type KadO = P.GMsg (P.RuntimeO NodeAddr) KUserO Msg KLogMsg

kLog :: KLogMsg -> KadO
kLog = P.MsgAux
