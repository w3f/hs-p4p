{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Low-level requests.

This contains mostly boilerplate / utility code and no Kademlia-specific logic.
-}
module P4P.Protocol.DHT.Kademlia.Request where

-- external
import qualified Control.Monad.Schedule            as SC
import qualified Control.Schedule.Future           as F
import qualified Crypto.Random.Extra               as R
import qualified Data.Map.Bounded                  as BM
import qualified Data.Map.Strict                   as M
import qualified Data.Strict                       as Z
import qualified P4P.Proc                          as P

import           Codec.Serialise                   (Serialise)
import           Control.Lens                      (Lens', use, (%%=), (%=),
                                                    (.=))
import           Control.Lens.Extra                (at_, (%%=!))
import           Control.Lens.Strict               (at, sans)
import           Control.Lens.TH.Extra             (makeLenses_)
import           Control.Monad.Trans.State.Strict  (runState, state)
import           Data.Binary                       (Binary)
import           Data.Map.Bounded                  (ValueAt (..))
import           GHC.Generics                      (Generic)
import           GHC.Stack                         (HasCallStack)

-- internal
import           P4P.Protocol.DHT.Kademlia.Message


assertNowRunning
  :: HasCallStack
  => SC.Task KTask
  -> SC.Schedule KTask
  -> ((), SC.Schedule KTask)
assertNowRunning k sch = case SC.taskStatus k sch of
  SC.TaskRunning _ -> ((), sch)
  _                -> error "assertNowRunning: assertion failed"

{- | An ongoing process to handle replies to a request from another node.

The process lasts for a specified time, upon which we unconditionally finish
the process & delete all state associated with it. During the process, if we
receive duplicate requests then the reply will be re-sent to the node without
re-calculating the reply (which e.g. might have refreshed other timers etc).

The details of the resending are subject to change, e.g. other addresses may be
tried, or maybe additional waiting periods may be introduced, etc.

Incoming requests are indexed by their source 'NodeId' and 'RequestBody', i.e.
they are treated as a duplicate if those two values are the same.
-}
data IReqProcess = IReqProcess
  { ireqMsg     :: !Msg
  -- ^ The first incoming message that triggered this process. Must be
  -- consistent with source and body.
  , ireqTimeout :: !(SC.Task KTask) -- TOIReq
  -- ^ Task for timing out this whole process.
  , ireqReply   :: !ReplyBody
  -- ^ The result of the request, to be sent (and re-sent) to the requestor.
  }
  deriving (Show, Read, Generic, Binary, Serialise, Eq)
makeLenses_ ''IReqProcess

ireqIndex :: HasCallStack => Msg -> ((NodeId, RequestBody), ReqId)
ireqIndex Msg {..} = case body of
  Right _            -> error "ireqIndex: ireqMsg not a request"
  Left  Request {..} -> ((src, reqBody), reqId)

-- | Run a new or existing 'IReqProcess' for the given incoming request.
ireqEnsure
  :: Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap2 NodeId RequestBody IReqProcess)
  -> SC.TickDelta
  -> NodeAddr
  -> Msg
  -> (s -> (ReplyBody, s))
  -> s
  -> ([KadO], s)
ireqEnsure lsched lireq timeout srcAddr req mkRepBody = runState $ do
  now <- SC.tickNow <$> use lsched
  lireq . at_ (reqSrc, reqBody) %%=! \case
    Present ireqProc -> do
      -- duplicate IRequest
      -- TODO(retry): perhaps try sending to other addresses in nInfo too
      let repBody = ireqReply ireqProc
          reply   = replyForRequest srcAddr req (Right repBody) now
      pure ([reply, kLog $ I_KProcessIgnoreDup kproc], Present ireqProc)
    Absent False -> do
      -- not enough space left in map, rate-limit them
      let reply =
            replyForRequest srcAddr req (Left $ TryAgainLater timeout) now
      pure ([reply], Absent False)
    Absent True -> do
      -- new IRequest
      repBody <- state mkRepBody
      let reply = replyForRequest srcAddr req (Right repBody) now
      lt <- lsched %%= SC.after timeout (TOIReq reqSrc reqBody)
      let ireqProc = IReqProcess { ireqMsg     = req
                                 , ireqTimeout = lt
                                 , ireqReply   = repBody
                                 }
      pure ([reply, kLog $ I_KProcessNew kproc], Present ireqProc)
 where
  (reqSrc, reqBody) = fst $ ireqIndex req
  kproc             = KPIReq reqSrc reqBody

-- | Finish an existing 'IReqProcess', after the timeout is complete.
ireqDelete
  :: HasCallStack
  => Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap2 NodeId RequestBody IReqProcess)
  -> NodeId
  -> RequestBody
  -> s
  -> ([KadO], s)
ireqDelete lsched lireq reqSrc reqBody = runState $ do
  lireq . at_ (reqSrc, reqBody) %%=! \case
    Present ireqProc -> do
      lsched %%= assertNowRunning (ireqTimeout ireqProc)
      pure ([kLog $ I_KProcessDel $ KPIReq reqSrc reqBody], Absent True)
    _ -> error "ireqDelete: called on non-existent process"

data RetryState = RetryState
  { retryCount :: !Int
  -- ^ Number of retries so far.
  , retryTask  :: !(SC.Task KTask)
  -- ^ Task for timing out a single try (resend).
  }
  deriving (Show, Read, Generic, Binary, Serialise, Eq)

{- | An ongoing process to handle replies to requests we make to another node.

ORequests are internally a bit more complex than IRequests, because they
produce a result that other things (e.g. 'Command') are interested in.

Outgoing requests are indexed by their target 'NodeId' and 'RequestBody', i.e.
they are treated as a duplicate if those two values are the same. If several
high-level commands are interested in the same request, only one process is
started, and any results are given to all the commands.

TODO(addr): this needs to hold the set of candidate addresses for the target node.
-}
data OReqProcess = OReqProcess
  { oreqMsg     :: !Msg
  -- ^ The first outgoing message that triggered this process. Its 'NodeId'
  -- and 'RequestBody' must be consistent with the index of this process.
  , oreqTimeout :: !(SC.Task KTask) -- TOOReq
  -- ^ Task for timing out this whole process.
  , oreqFuture  :: !(F.SFuture CmdId (F.TimedResult () ReplyBody))
  -- ^ SFuture representing this OReqProcess. The number of CmdIds in State is
  -- already explicitly bounded so this does not need to be. If the key is
  -- Nothing this means some internal system process (e.g. Ping via insertNode)
  -- is interested in the OReqProcess, and it cannot be cancelled until it
  -- times out naturally.
  , oreqRetry   :: !RetryState
  }
  deriving (Show, Read, Generic, Binary, Serialise, Eq)
makeLenses_ ''OReqProcess

oreqInitTick :: OReqProcess -> SC.Tick
oreqInitTick OReqProcess {..} = sent oreqMsg

oreqIndex :: HasCallStack => Msg -> ((NodeId, RequestBody), ReqId)
oreqIndex Msg {..} = case body of
  Right _            -> error "oreqMetadata: oreqMsg not a request"
  Left  Request {..} -> ((dst, reqBody), reqId)

newReqId :: R.DRG' drg => drg -> (ReqId, drg)
newReqId = R.randomBytesGenerate reqIdWidth

{- | Run a new or existing 'OReqProcess' for the given outgoing request.

This does not link together the 'F.SFuture' and 'F.SExpect', so you /must/ call
'F.sExpectFuture' after calling this function.
-}
oreqEnsure
  :: R.DRG' drg
  => Lens' s drg
  -> Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> Lens' s (M.Map ReqId (Z.Pair NodeId RequestBody))
  -> (SC.TickDelta, SC.TickDelta)
  -> (SC.Tick -> Request -> (NodeAddr, Msg))
  -> NodeId
  -> RequestBody
  -> s
  -> ([KadO], s)
oreqEnsure ldrg lsched loreq loreqId par mkMsg reqDst reqBody = runState $ do
  loreq . at_ (reqDst, reqBody) %%=! \case
    Present oreqProc -> do
      pure ([kLog $ I_KProcessIgnoreDup kproc], Present oreqProc)
      -- TODO(bump-to): do anything else here, e.g bump timeouts?
    Absent False ->
      error
        "oreqStart failed size check, programmer fail to give sufficiently-high rate limit"
    Absent True -> do
      reqId <- ldrg %%= newReqId
      now   <- SC.tickNow <$> use lsched
      let (dstAddr, request) = mkMsg now $ Request reqId reqBody
      lt  <- lsched %%= SC.after timeout (TOOReq reqDst reqBody)
      lt' <- lsched %%= SC.after timeoutRetry (TOOReqReply reqDst reqBody)
      let oreqProc = OReqProcess { oreqMsg     = request
                                 , oreqTimeout = lt
                                 , oreqRetry   = RetryState 0 lt'
                                 , oreqFuture  = F.SFWaiting mempty
                                 }
      loreqId . at reqId .= Just (reqDst Z.:!: reqBody)
      pure
        ( [ P.MsgLo (P.UData dstAddr (P.Val request))
          , kLog $ I_KProcessNew kproc
          ]
        , Present oreqProc
        )
 where
  (timeout, timeoutRetry) = par
  kproc                   = KPOReq reqDst reqBody

{- | Finish an existing 'OReqProcess', after the timeout is complete.

This does not link together the 'F.SFuture' and 'F.SExpect', so you /must/ call
'F.sFutureResult' before calling this function.
-}
oreqDelete
  :: HasCallStack
  => Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> Lens' s (M.Map ReqId (Z.Pair NodeId RequestBody))
  -> NodeId
  -> RequestBody
  -> s
  -> ([KadO], s)
oreqDelete lsched loreq loreqId reqDst reqBody = runState $ do
  loreq . at_ (reqDst, reqBody) %%=! \case
    Present oreqProc -> do
      lsched %%= assertNowRunning (oreqTimeout oreqProc)
      _ <- lsched %%= SC.cancel (retryTask $ oreqRetry oreqProc)
      loreqId %= sans (snd $ oreqIndex $ oreqMsg oreqProc)
      pure ([kLog $ I_KProcessDel $ KPOReq reqDst reqBody], Absent True)
    _ -> error "oreqDelete: called on non-existent process"
