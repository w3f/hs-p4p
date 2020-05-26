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
import qualified P4P.Proc                          as P

import           Control.Lens                      (Lens', itraversed, use,
                                                    (%%=), (%%@~), (&), (<.))
import           Control.Lens.Extra                (at_, unsafeIx, (%%=!))
import           Control.Lens.TH.Extra             (makeLenses_)
import           Control.Monad.Trans.State.Strict  (StateT (..), runState,
                                                    state)
import           Data.Foldable                     (toList)
import           Data.Map                          (Map)
import           Data.Map.Bounded                  (ValueAt (..))
import           Data.Maybe                        (isNothing)
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
  -- precisely the number of 'ORequest's in our 'SExpect'.
  } deriving (Show, Read, Generic, Eq, Ord)

newSimpleQuery :: NodeInfos' -> SimpleQuery r
newSimpleQuery initNodes = SimpleQuery initNodes mempty mempty mempty

data ICmdState =
    ICSystemForever
    -- ^ The command is an internal pseudo-command that never finishes.
  | ICNewlyCreated
    -- ^ The command was just created and has yet to initialise.
  | ICLookingup !NodeId !(SimpleQuery ())
    -- ^ The command is performing a lookup operation on some target key/node.
  | ICInserting !Value !(SimpleQuery SC.TickDelta)
    -- ^ The command is performing an insert operation on some value.
  | ICFinished
    deriving (Show, Read, Generic, Eq, Ord)

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
  , icmdExpect   :: !(F.SExpect (NodeId, RequestBody) KTask)
  -- ^ SExpect for outstanding sent requests.
  -- SExpect timeout tasks are of type TOICmdOReq.
  -- SFuture timeout tasks are of type TOOReq.
  , icmdResult   :: !(Maybe CommandReplyBody)
  -- ^ Some commands give a result, and then have to perform some extra
  -- follow-up work, e.g. 'JoinNetwork' / 'LookupKey'. This field caches the
  -- result in the meantime, which can be resent to duplicate commands.
  , icmdState    :: !ICmdState
  , icmdExternal :: !Bool
  -- ^ Whether the command was external (from a user) or internal (from an
  -- automated behaviour such as a bucket refresh). This determines whether we
  -- send out a CommandReply when the command finishes.
  } deriving (Show, Read, Generic, Eq)
makeLenses_ ''ICmdProcess

cmdUserReply :: CmdId -> CommandReplyBody -> KadO
cmdUserReply cmdId replyBody =
  P.MsgUser $ Right $ CommandReply cmdId $ Right replyBody

type RunSExpect' s r v
  =  (NodeId, RequestBody)
  -> Maybe CmdId
  -> Lens' s (F.SFuture (Maybe CmdId) r)
  -> Lens' s (F.SExpect (NodeId, RequestBody) KTask)
  -> Lens' s (SC.Schedule KTask)
  -> s
  -> (Either F.SFError v, s)

stateSExpect_
  :: (HasCallStack, Monad m)
  => Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap (Maybe CmdId) ICmdProcess)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> RunSExpect' s (F.TimedResult () ReplyBody) v
  -> NodeId
  -> RequestBody
  -> Maybe CmdId
  -> StateT s m (Either F.SFError v)
stateSExpect_ lsched licmd loreq f reqDst reqBody cmdId = state $ f
  (reqDst, reqBody)
  cmdId
  (loreq . unsafeIx (reqDst, reqBody) . _oreqFuture)
  (licmd . unsafeIx cmdId . _icmdExpect)
  lsched

icmdEnsure
  :: Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap (Maybe CmdId) ICmdProcess)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> Command
  -> Bool
  -> SC.TickDelta
  -> s
  -> ([KadO], s)
icmdEnsure lsched licmd loreq cmd isExternal timeout = runState $ do
  licmd . at_ (Just cmdId) %%=! \case
    Present cmdProc -> do
      -- duplicate Command
      let out = case (icmdResult cmdProc, icmdExternal cmdProc) of
            (Just reply, True) -> [cmdUserReply cmdId reply]
            _                  -> []
      pure (out <> [logEvt KProcessNopNew], Present cmdProc)
    Absent False -> do
      -- not enough space left in map, rate-limit them
      let reply = CommandReply cmdId $ Left $ TryAgainLater timeout
      pure ([P.MsgUser $ Right reply], Absent False)
    Absent True -> do
      lt <- lsched %%= SC.after timeout (TOICmd cmdId)
      let cmdProc = ICmdProcess { icmdCmd      = cmd
                                , icmdTimeout  = lt
                                , icmdExpect   = mempty
                                , icmdResult   = Nothing
                                , icmdState    = ICNewlyCreated
                                , icmdExternal = isExternal
                                }
      pure ([logEvt KProcessNew], Present cmdProc)
 where
  Command {..} = cmd
  logEvt       = kLog . ICmdEvt cmdId

-- assumes the icmd/oreq both exist
icmdOReqExpect_
  :: Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap (Maybe CmdId) ICmdProcess)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> SC.TickDelta
  -> CmdId
  -> NodeId
  -> RequestBody
  -> (F.TimedResult () ReplyBody -> s -> ([KadO], s))
  -> s
  -> ([KadO], s)
icmdOReqExpect_ lsched licmd loreq timeout cmdId reqDst reqBody runR =
  runState $ do
    let tk = TOICmdOReq cmdId reqDst reqBody
    res <- stateSExpect_ lsched
                         licmd
                         loreq
                         (F.sExpectFuture timeout tk)
                         reqDst
                         reqBody
                         (Just cmdId)
    case res of
      Left  e        -> pure [kLog $ ICmdErr cmdId KProcessStepAdd e]
      Right Nothing  -> pure []
      Right (Just r) -> state $ runR r

-- assumes the icmd/oreq both exist
icmdOReqCancel_
  :: Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap (Maybe CmdId) ICmdProcess)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> CmdId
  -> NodeId
  -> RequestBody
  -> s
  -> ([KadO], s)
icmdOReqCancel_ lsched licmd loreq cmdId reqDst reqBody = runState $ do
  res <- stateSExpect_ lsched
                       licmd
                       loreq
                       F.sExpectCancel
                       reqDst
                       reqBody
                       (Just cmdId)
  case res of
    Left  e  -> pure [kLog $ ICmdErr cmdId KProcessStepRem e]
    Right () -> pure []

oreqResultFuture_
  :: Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap (Maybe CmdId) ICmdProcess)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> F.TimedResult () ReplyBody
  -> NodeId
  -> RequestBody
  -> (F.OSet (Maybe CmdId) -> s -> ([KadO], s))
  -> s
  -> ([KadO], s)
oreqResultFuture_ lsched licmd loreq result reqDst reqBody runW = runState $ do
  res <- state $ F.sFutureSettled
    result
    (reqDst, reqBody)
    (loreq . unsafeIx (reqDst, reqBody) . _oreqFuture)
    (licmd . itraversed <. _icmdExpect)
    lsched
  case res of
    Left  e -> pure [kLog $ OReqEvt (reqDst, reqBody) KProcessNopStep]
    Right w -> state $ runW w

icmdCancelAll_
  :: Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap (Maybe CmdId) ICmdProcess)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> CmdId
  -> s
  -> ([KadO], s)
icmdCancelAll_ lsched licmd loreq cmdId = runState $ do
  sse <- use $ licmd . unsafeIx (Just cmdId) . _icmdExpect . F._seExpects
  e   <- sse & itraversed %%@~ \(reqDst, reqBody) lt -> do
    state $ icmdOReqCancel_ lsched licmd loreq cmdId reqDst reqBody
  pure (concat (toList e))

-- | Finish an existing 'ICmdProcess', after the timeout is complete.
icmdFinish
  :: Lens' s (SC.Schedule KTask)
  -> Lens' s (BM.BMap (Maybe CmdId) ICmdProcess)
  -> Lens' s (BM.BMap2 NodeId RequestBody OReqProcess)
  -> CmdId
  -> SC.TickDelta
  -> s
  -> ([KadO], s)
icmdFinish lsched licmd loreq cmdId timeout = runState $ do
  licmd . at_ (Just cmdId) %%=! \case
    Present cmdProc -> do
      errors <- state $ icmdCancelAll_ lsched licmd loreq cmdId
      lsched %%= SC.cancel_ (icmdTimeout cmdProc) -- TODO(cleanup): instead, ensure task is active
      -- send a timeout reply if the command had produced no reply so far
      let out =
            [ cmdUserReply cmdId (CommandTimedOut timeout)
            | icmdExternal cmdProc && isNothing (icmdResult cmdProc)
            ]
      pure (errors <> out <> [logEvt KProcessDel], Absent True)
    Absent b -> pure ([logEvt KProcessNopDel], Absent b)
  where logEvt = kLog . ICmdEvt cmdId
