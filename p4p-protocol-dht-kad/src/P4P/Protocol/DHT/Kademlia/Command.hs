{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | High-level user commands.

This contains mostly boilerplate / utility code and no Kademlia-specific logic.
-}
module P4P.Protocol.DHT.Kademlia.Command where

-- external
import qualified Control.Monad.Schedule            as SC
import qualified Control.Schedule.Future           as F
import qualified Data.Map.Bounded                  as BM
import qualified Data.Strict                       as Z
import qualified P4P.Proc                          as P

import           Control.Lens                      (Lens', itraversed, use,
                                                    (%%=), (%%@~), (&), (<.))
import           Control.Lens.Extra                (at_, unsafeIx, (%%=!))
import           Control.Lens.TH.Extra             (makeLenses_)
import           Control.Monad.Trans.State.Strict  (runState, state)
import           Data.Foldable                     (toList)
import           Data.Map.Bounded                  (ValueAt (..))
import           Data.Map.Strict                   (Map)
import           Data.Set                          (Set)
import           GHC.Generics                      (Generic)
import           GHC.Stack                         (HasCallStack)

-- internal
import           P4P.Protocol.DHT.Kademlia.Message
import           P4P.Protocol.DHT.Kademlia.Request
import           P4P.Protocol.DHT.Kademlia.Types


-- | Simple data structure representing an iterative query over multiple nodes.
--
-- Note: we re-use this for insert operations as it's convenient.
data SimpleQuery r = SimpleQuery
  { sqAll  :: !NodeInfos'
  -- ^ All nodes we have visited or have heard about.
  , sqOk   :: !(Map NodeId r)
  , sqErr  :: !(Set NodeId)
  , sqWait :: !(Set NodeId)
  -- ^ Nodes we have made a query to and are waiting on. This should be
  -- precisely the number of 'OReqProcess's in our 'F.SExpect'.
  }
  deriving (Show, Read, Generic, Eq, Ord)

newSimpleQuery :: NodeInfos' -> SimpleQuery r
newSimpleQuery initNodes = SimpleQuery initNodes mempty mempty mempty

data ICmdReply =
    ICGotNodes !NodeInfos
  | ICGotValue !Value
  | ICPutOK !SC.TickDelta
    deriving (Show, Read, Generic, Eq, Ord)

{- | Logical input that the state machine actually runs on.

We convert 'ReplyBody' into this, using 'toCmdInput', before processing it.
-}
data ICmdInput =
    ICStart
  | ICReply !NodeId !(F.TimedResult () ICmdReply)
    deriving (Show, Read, Generic, Eq, Ord)

toCmdInput
  :: NodeId -> RequestBody -> F.TimedResult () ReplyBody -> Maybe ICmdInput
toCmdInput reqDst reqBody repBody = case (reqBody, repBody) of
  (_, F.TimedOut ()) -> Just $ ICReply reqDst $ F.TimedOut ()
  (GetNode _, F.GotResult (GetNodeReply ni)) -> reply $ ICGotNodes ni
  (GetValue _, F.GotResult (GetValueReply (Left ni))) -> reply $ ICGotNodes ni
  (GetValue _, F.GotResult (GetValueReply (Right v))) -> reply $ ICGotValue v
  (PutValue _ _, F.GotResult (GetNodeReply ni)) -> reply $ ICGotNodes ni
  (PutValue _ _, F.GotResult (PutValueReply expire)) -> reply $ ICPutOK expire
  _ -> Nothing
  where reply = Just . ICReply reqDst . F.GotResult

type ICmdState' = ICmdState (SimpleQuery ()) (SimpleQuery SC.TickDelta)


-- | An ongoing process to handle commands from the local user.
--
-- These differ from requests from other nodes in two important ways:
-- - We assume the results are sent back over a reliable channel and therefore
--   there is no need to add logic to re-send results.
-- - The command usually cannot be resolved immediately from local data and
--   needs other outgoing requests to be performed, probably several times.
data ICmdProcess = ICmdProcess
  { icmdCmd      :: !Command
  , icmdTimeout  :: !(SC.Task KTask)
  , icmdExpect   :: !(F.SExpect (Z.Pair NodeId RequestBody) KTask)
  -- ^ SExpect for outstanding sent requests.
  -- SExpect timeout tasks are of type TOICmdOReq.
  -- SFuture timeout tasks are of type TOOReq.
  , icmdResult   :: !(Z.Maybe CommandReplyBody)
  -- ^ Some commands give a result, and then have to perform some extra
  -- follow-up work, e.g. 'JoinNetwork' / 'LookupValue'. This field caches the
  -- result in the meantime, which can be resent to duplicate commands.
  , icmdState    :: !ICmdState'
  , icmdExternal :: !Bool
  -- ^ Whether the command was external (from a user) or internal (from an
  -- automated behaviour such as a bucket refresh). This determines whether we
  -- send out a CommandReply when the command finishes.
  }
  deriving (Show, Read, Generic, Eq)
makeLenses_ ''ICmdProcess

cmdUserReply :: CmdId -> CommandReplyBody -> KadO
cmdUserReply cmdId replyBody = P.MsgUser $ CommandReply cmdId $ Right replyBody

type RunSExpect' s r v
  =  Lens' s (F.SFuture CmdId r)
  -> Lens' s (F.SExpect (Z.Pair NodeId RequestBody) KTask)
  -> Lens' s (SC.Schedule KTask)
  -> Z.Pair NodeId RequestBody
  -> CmdId
  -> v

sExpect_
  :: HasCallStack
  => Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap CmdId ICmdProcess)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> RunSExpect' s (F.TimedResult () ReplyBody) v
  -> Z.Pair NodeId RequestBody
  -> CmdId
  -> v
sExpect_ lsched licmd loreq f reqKey cmdId = f
  (loreq . unsafeIx (Z.toLazy reqKey) . _oreqFuture)
  (licmd . unsafeIx cmdId . _icmdExpect)
  lsched
  reqKey
  cmdId

icmdEnsure
  :: Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap CmdId ICmdProcess)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> Command
  -> Bool
  -> SC.TickDelta
  -> s
  -> ([KadO], s)
icmdEnsure lsched licmd loreq cmd isExternal timeout = runState $ do
  licmd . at_ cmdId %%=! \case
    Present cmdProc -> do
      -- duplicate Command
      -- TODO(retry): should we resend below? we are doing so currently, but we
      -- do assume Commands come in through a reliable channel
      let out = case (icmdResult cmdProc, icmdExternal cmdProc) of
            (Z.Just reply, True) -> [cmdUserReply cmdId reply]
            _                    -> []
      pure (out <> [kLog $ I_KProcessIgnoreDup kproc], Present cmdProc)
    Absent False -> do
      -- not enough space left in map, rate-limit them
      let reply = CommandReply cmdId $ Left $ TryAgainLater timeout
      pure ([P.MsgUser reply], Absent False)
    Absent True -> do
      lt <- lsched %%= SC.after timeout (TOICmd cmdId)
      let cmdProc = ICmdProcess { icmdCmd      = cmd
                                , icmdTimeout  = lt
                                , icmdExpect   = mempty
                                , icmdResult   = Z.Nothing
                                , icmdState    = ICNewlyCreated
                                , icmdExternal = isExternal
                                }
      pure ([kLog $ I_KProcessNew kproc], Present cmdProc)
 where
  Command {..} = cmd
  kproc        = KPICmd cmdId

-- assumes the icmd/oreq both exist
icmdOReqExpect_
  :: Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap CmdId ICmdProcess)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> SC.TickDelta
  -> CmdId
  -> NodeId
  -> RequestBody
  -> (F.TimedResult () ReplyBody -> s -> ([KadO], s))
  -> s
  -> ([KadO], s)
icmdOReqExpect_ lsched licmd loreq to cmdId reqDst reqBody runR = runState $ do
  let tk     = TOICmdOReq cmdId reqDst reqBody
  let reqKey = reqDst Z.:!: reqBody
  res <- state $ do
    sExpect_ lsched licmd loreq F.sExpectFuture reqKey cmdId to tk
  case res of
    Left  e        -> pure [kLog $ D_ICmdOReqIgnoreDup cmdId reqDst reqBody]
    Right Nothing  -> pure []
    Right (Just r) -> state $ runR r

-- assumes the icmd/oreq both exist
icmdOReqCancel_
  :: Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap CmdId ICmdProcess)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> CmdId
  -> NodeId
  -> RequestBody
  -> s
  -> ([KadO], s)
icmdOReqCancel_ lsched licmd loreq cmdId reqDst reqBody = runState $ do
  let reqKey = reqDst Z.:!: reqBody
  res <- state $ sExpect_ lsched licmd loreq F.sExpectCancel reqKey cmdId
  case res of
    Left  e  -> pure [kLog $ D_ICmdOReqIgnoreMis cmdId reqDst reqBody]
    Right () -> pure []

oreqResultFuture_
  :: Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap CmdId ICmdProcess)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> F.TimedResult () ReplyBody
  -> NodeId
  -> RequestBody
  -> (F.OSet CmdId -> s -> ([KadO], s))
  -> s
  -> ([KadO], s)
oreqResultFuture_ lsched licmd loreq result reqDst reqBody runW = runState $ do
  res <- state $ F.sFutureSettled
    (loreq . unsafeIx (reqDst, reqBody) . _oreqFuture)
    (licmd . itraversed <. _icmdExpect)
    lsched
    (reqDst Z.:!: reqBody)
    result
  case res of
    Left  e -> pure [kLog $ I_OReqIgnoreDupReply reqDst reqBody]
    Right w -> state $ runW w

icmdCancelAll_
  :: Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap CmdId ICmdProcess)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> CmdId
  -> s
  -> ([KadO], s)
icmdCancelAll_ lsched licmd loreq cmdId = runState $ do
  sse <- use $ licmd . unsafeIx cmdId . _icmdExpect . F._seExpects
  e   <- sse & itraversed %%@~ \(reqDst Z.:!: reqBody) lt -> do
    state $ icmdOReqCancel_ lsched licmd loreq cmdId reqDst reqBody
  pure (concat (toList e))

-- | Finish an existing 'ICmdProcess', after the timeout is complete.
icmdDelete
  :: HasCallStack
  => Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap CmdId ICmdProcess)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> CmdId
  -> SC.TickDelta
  -> s
  -> ([KadO], s)
icmdDelete lsched licmd loreq cmdId timeout = runState $ do
  licmd . at_ cmdId %%=! \case
    Present cmdProc -> do
      errors <- state $ icmdCancelAll_ lsched licmd loreq cmdId
      lsched %%= assertNowRunning (icmdTimeout cmdProc)
      -- send a timeout reply if the command had produced no reply so far
      let out =
            [ cmdUserReply cmdId (CommandTimedOut timeout)
            | icmdExternal cmdProc && Z.isNothing (icmdResult cmdProc)
            ]
      pure (errors <> out <> [kLog $ I_KProcessDel $ KPICmd cmdId], Absent True)
    _ -> error $ "icmdDelete: called on non-existent process: " <> show cmdId
