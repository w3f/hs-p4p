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
import qualified P4P.Proc                          as P

import           Control.Lens                      (Lens', at, sans, use, (%%=),
                                                    (%=), (.=))
import           Control.Lens.Extra                (at_, (%%=!))
import           Control.Lens.TH.Extra             (makeLenses_)
import           Control.Monad.Trans.State.Strict  (runState, state)
import           Data.Map.Bounded                  (ValueAt (..))
import           Data.Word                         (Word8)
import           GHC.Generics                      (Generic)
import           GHC.Stack                         (HasCallStack)

-- internal
import           P4P.Protocol.DHT.Kademlia.Message


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
  } deriving (Show, Read, Generic, Eq)
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
  -> Msg
  -> (s -> (ReplyBody, s))
  -> s
  -> ([KadO], s)
ireqEnsure lsched lireq timeout req mkRepBody = runState $ do
  now <- SC.tickNow <$> use lsched
  lireq . at_ (reqSrc, reqBody) %%=! \case
    Present ireqProc -> do
      -- duplicate IRequest
      -- TODO(retry): perhaps try sending to other addresses in nInfo too
      let repBody = ireqReply ireqProc
          reply   = replyForRequest req (Right repBody) now addr
      pure ([reply, logEvt KProcessNopNew], Present ireqProc)
    Absent False -> do
      -- not enough space left in map, rate-limit them
      let reply = replyForRequest req (Left $ TryAgainLater timeout) now addr
      pure ([reply], Absent False)
    Absent True -> do
      -- new IRequest
      repBody <- state mkRepBody
      let reply = replyForRequest req (Right repBody) now addr
      lt <- lsched %%= SC.after timeout (TOIReq reqSrc reqBody)
      let ireqProc =
            IReqProcess { ireqMsg = req, ireqTimeout = lt, ireqReply = repBody }
      pure ([reply, logEvt KProcessNew], Present ireqProc)
 where
  (reqSrc, reqBody) = fst $ ireqIndex req
  addr              = srcAddr req
  logEvt            = kLog . IReqEvt (reqSrc, reqBody)

-- | Finish an existing 'IReqProcess', after the timeout is complete.
ireqFinish
  :: Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap2 NodeId RequestBody IReqProcess)
  -> NodeId
  -> RequestBody
  -> s
  -> ([KadO], s)
ireqFinish lsched lireq reqSrc reqBody = runState $ do
  lireq . at_ (reqSrc, reqBody) %%=! \case
    Present ireqProc -> do
      _ <- lsched %%= SC.cancel (ireqTimeout ireqProc)  -- TODO(cleanup): instead, ensure task is active
      pure ([logEvt KProcessDel], Absent True)
    x -> pure ([logEvt KProcessNopDel], x)
  where logEvt = kLog . IReqEvt (reqSrc, reqBody)

data RetryState = RetryState
  { retryCount :: !Int
  -- ^ Number of retries so far.
  , retryTask  :: !(SC.Task KTask)
  -- ^ Task for timing out a single try (resend).
  } deriving (Show, Read, Generic, Eq)

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
  , oreqFuture  :: !(F.SFuture (Maybe CmdId) (F.TimedResult () ReplyBody))
  -- ^ SFuture representing this OReqProcess. The number of CmdIds in State is
  -- already explicitly bounded so this does not need to be. If the key is
  -- Nothing this means some internal system process (e.g. Ping via insertNode)
  -- is interested in the OReqProcess, and it cannot be cancelled until it
  -- times out naturally.
  , oreqRetry   :: !RetryState
  } deriving (Show, Read, Generic, Eq)
makeLenses_ ''OReqProcess

oreqInitTick :: OReqProcess -> SC.Tick
oreqInitTick OReqProcess {..} = sent oreqMsg

oreqIndex :: HasCallStack => Msg -> ((NodeId, RequestBody), ReqId)
oreqIndex Msg {..} = case body of
  Right _            -> error "oreqMetadata: oreqMsg not a request"
  Left  Request {..} -> ((dst, reqBody), reqId)

newReqId :: R.DRG' drg => Word8 -> drg -> (ReqId, drg)
newReqId = R.randomBytesGenerate . fromIntegral

{- | Run a new or existing 'OReqProcess' for the given outgoing request.

This does not link together the 'SFuture' and 'SExpect', so you /must/ call
'F.sExpectFuture' after calling this function.
-}
oreqEnsure
  :: R.DRG' drg
  => Lens' s drg
  -> Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> Lens' s (M.Map ReqId (NodeId, RequestBody))
  -> (Word8, SC.TickDelta, SC.TickDelta)
  -> (SC.Tick -> Request -> Msg)
  -> NodeId
  -> RequestBody
  -> s
  -> ([KadO], s)
oreqEnsure ldrg lsched loreq loreqId par mkMsg reqDst reqBody = runState $ do
  loreq . at_ (reqDst, reqBody) %%=! \case
    Present oreqProc -> do
      pure ([logEvt KProcessNopNew], Present oreqProc)
      -- TODO(bump-to): do anything else here, e.g bump timeouts?
    Absent False ->
      error
        "oreqStart failed size check, programmer fail to give sufficiently-high rate limit"
    Absent True -> do
      reqId <- ldrg %%= newReqId idWidth
      now   <- SC.tickNow <$> use lsched
      let request = mkMsg now $ Request reqId reqBody
      lt  <- lsched %%= SC.after timeout (TOOReq reqDst reqBody)
      lt' <- lsched %%= SC.after timeoutRetry (TOOReqReply reqDst reqBody)
      let oreqProc = OReqProcess { oreqMsg     = request
                                 , oreqTimeout = lt
                                 , oreqRetry   = RetryState 0 lt'
                                 , oreqFuture  = F.SFWaiting mempty
                                 }
      loreqId . at reqId .= Just (reqDst, reqBody)
      pure ([P.MsgProc request, logEvt KProcessNew], Present oreqProc)
 where
  (idWidth, timeout, timeoutRetry) = par
  logEvt                           = kLog . OReqEvt (reqDst, reqBody)

{- | Finish an existing 'OReqProcess', after the timeout is complete.

This does not link together the 'SFuture' and 'SExpect', so you /must/ call
'F.sFutureResult' before calling this function.
-}
oreqFinish
  :: Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> Lens' s (M.Map ReqId (NodeId, RequestBody))
  -> NodeId
  -> RequestBody
  -> s
  -> ([KadO], s)
oreqFinish lsched loreq loreqId reqDst reqBody = runState $ do
  loreq . at_ (reqDst, reqBody) %%=! \case
    Present oreqProc -> do
      _ <- lsched %%= SC.cancel (oreqTimeout oreqProc)  -- TODO(cleanup): instead, ensure task is active
      _ <- lsched %%= SC.cancel (retryTask $ oreqRetry oreqProc)
      loreqId %= sans (snd $ oreqIndex $ oreqMsg oreqProc)
      pure ([logEvt KProcessDel], Absent True)
    x -> pure ([logEvt KProcessNopDel], x)
  where logEvt = kLog . OReqEvt (reqDst, reqBody)
