{-

TODO: initial address for a new emptyState
TODO: lens for Sequence.Extra shit, lens for bounded containers...

-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}
--{-# OPTIONS_GHC -Wno-unused-imports -Wno-redundant-constraints #-}

{- | As close to the paper as possible, with some minor tweaks not inconsistent
with the behaviour of the paper.

- nodes can have multiple addresses
- requests are batched and rate-limited to a reasonable amount

I-over-cache: To avoid “over-caching,” we make the expiration time of a
key,value pair in any node’s database exponentially inversely proportional to
the number of nodes between the current node and the node whose ID is closest
to the key ID

I-bucket-refresh: To avoid pathological cases when no traffic exists, each node
refreshes a bucket in whose range it has not performed a node lookup within an
hour. Refreshing means picking a random ID in the bucket’s range and performing
a node search for that ID.

I-update-k-bucket: When a Kademlia node receives any message (request or reply)
from another node, it updates the appropriate k-bucket for the sender’s node
ID.

I-insert-node-lru-fail-ping
"If the least-recently seen node fails to respond, it is evicted from
        -- the k-bucket and the new sender inserted at the tail."

-}
module P4P.Protocol.DHT.Kademlia.Internal where

-- external
import qualified Control.Schedule.Future           as F
import qualified Crypto.Random.Extra               as R
import qualified Data.Map.Strict                   as M
import qualified Data.Schedule                     as SC
import qualified Data.Sequence.Extra               as S
import qualified Data.Set                          as Set
import qualified P4P.Proc                          as P
import qualified P4P.Proc.Lens                     as P

import           Control.Lens                      (Lens', at, ix, sans, use,
                                                    (%%=), (%%~), (%=), (.=),
                                                    (^?))
import           Control.Lens.Extra                (at_, unsafeIx, (%%=!),
                                                    (%&&&%))
import           Control.Monad                     (unless, when)
import           Control.Monad.Extra               (whenJust)
import           Control.Monad.Schedule            (tickTask)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Except        (runExceptT)
import           Control.Monad.Trans.State.Strict  (StateT (..), execStateT,
                                                    get, modify, runState,
                                                    state)
import           Control.Monad.Trans.Writer.CPS    (WriterT, runWriterT, tell)
import           Data.Bifunctor                    (first, second)
import           Data.Foldable                     (for_, toList)
import           Data.Functor.Identity             (Identity (..))
import           Data.List                         (sortOn)
import           Data.Map.Bounded                  (ValueAt (..), sizeBMap2)
import           Data.Maybe                        (isNothing)
import           Data.Tuple                        (swap)
--import           Debug.Pretty.Simple               (pTraceShowId)
import           Safe                              (fromJustNote)

-- internal
import           P4P.Protocol.DHT.Kademlia.Command
import           P4P.Protocol.DHT.Kademlia.Message
import           P4P.Protocol.DHT.Kademlia.Request
import           P4P.Protocol.DHT.Kademlia.State
import           P4P.Protocol.DHT.Kademlia.Types


{-# RULES "runStateWT/stateWT" forall t. runStateWT (stateWT t) = t #-}
{-# INLINE [1] runStateWT #-}
runStateWT :: (Monad m, Monoid w) => StateT s (WriterT w m) () -> s -> m (w, s)
runStateWT m s0 = fmap swap $ runWriterT $ execStateT m s0

{-# RULES "stateWT/runStateWT" forall m. stateWT (runStateWT m) = m #-}
{-# INLINE [1] stateWT #-}
stateWT :: (Monoid w, Monad m) => (s -> m (w, s)) -> StateT s (WriterT w m) ()
stateWT x = StateT (lift . x) >>= lift . tell

{-# INLINE stateWT' #-}
stateWT'
  :: (Monoid wo, Monad m)
  => (wi -> wo)
  -> (s -> m (wi, s))
  -> StateT s (WriterT wo m) ()
stateWT' f x = StateT (lift . x) >>= lift . tell . f

{-# INLINE statewT #-}
statewT :: (Monoid w, Monad m) => (s -> (w, s)) -> StateT s (WriterT w m) ()
statewT x = stateWT (pure . x)

{-# INLINE statewT' #-}
statewT'
  :: (Monoid wo, Monad m)
  => (wi -> wo)
  -> (s -> (wi, s))
  -> StateT s (WriterT wo m) ()
statewT' f x = stateWT' f (pure . x)

{-# INLINE writeLog #-}
writeLog :: Monad m => KLogMsg -> StateT (State g) (WriterT [KadO] m) ()
writeLog msg = lift $ tell [kLog msg]


kPutValue :: Key -> Value -> State drg -> (SC.TickDelta, State drg)
kPutValue k v s0 = flip runState s0 $ do
  whenJust (kStore ^? ix k) $ \StoreEntry {..} -> do
    -- cancel any old tasks associated with the old value
    _kSchedule %%= SC.cancel_ sRepub
    _kSchedule %%= SC.cancel_ sExpire
  -- we should take the minimum of the above value and parIntvValExpire
  let expire = parIntvValExpire
  lt  <- _kSchedule %%= SC.after parIntvValRepub (RepublishKey k)
  lt' <- _kSchedule %%= SC.after expire (ExpireKey k)
  let ent = StoreEntry { sValue = v, sOwn = False, sRepub = lt, sExpire = lt' }
  _kStore . at k .= Just ent
  pure expire
 where
  State {..}   = s0
  KParams {..} = kParams

{- | Refresh a bucket, but don't start a new command.

Used by 'icmdRunInput' when running 'JoinNetwork'.
-}
kRefreshBucketOnly
  :: (Monad m, R.DRG' g) => Int -> State g -> m (NodeId, State g)
kRefreshBucketOnly idx s0 = flip runStateT s0 $ do
  nId <- state $ newNodeIdR $ kBucketIndexToPrefix idx s0
  _kBuckets . unsafeIx idx . _bRefresh %%=! \lt -> do
    -- we are either currently running this task, or being manually refreshed
    -- before the timer is up, e.g. by JoinNetwork. either way, cancel_ is
    -- appropriate here as renew only works if the task is pending.
    _kSchedule %%= SC.cancel_ lt
    fmap ((), ) $ _kSchedule %%= SC.after parIntvKBRefresh (RefreshBucket idx)
  pure nId
 where
  State {..}   = s0
  KParams {..} = kParams

{- | Refresh a bucket.

I-bucket-refresh
-}
kRefreshBucket :: (Monad m, R.DRG' g) => Int -> State g -> m ([KadO], State g)
kRefreshBucket idx s0 = flip runStateWT s0 $ do
  nId   <- StateT $ kRefreshBucketOnly idx
  cmdId <- state newCmdId
  stateWT $ icmdStart (Command cmdId (LookupNode nId)) False

-- | Bump the bucket of the given nodeId, resetting its refresh time.
-- This should be done whenever we send a GetNode or GetValue RPC to that node.
kBucketBump :: Monad m => NodeId -> State g -> m ([KadO], State g)
kBucketBump n s0 = flip runStateWT s0 $ do
  whenJust (kBucketGetIndex n s0) $ \idx -> do
    -- this usage of lensProduct is sound, because:
    -- - _kBuckets and _kSchedule are disjoint parts of (State g)
    _kBuckets . unsafeIx idx %&&&% _kSchedule %= kBucketBumpRefresh
 where
  State {..}   = s0
  KParams {..} = kParams
  kBucketBumpRefresh (b@KBucket {..}, sched0) =
    (b { bMostRecentLookup = SC.tickNow sched0, bRefresh = refresh }, sched1)
   where
    (refresh', sched1) = SC.renew parIntvKBRefresh bRefresh sched0
    refresh = fromJustNote "bucket refresh had run w/o replacing..." refresh'

nodeNotFound :: String -> NodeId -> String -> NodeId -> String
nodeNotFound tag self ty nodeId =
  show self <> ": " <> tag <> " did not find " <> ty <> " node: " <> show nodeId

insertNodeId
  :: (Monad m, R.DRG' g)
  => Maybe SC.Tick
  -> Bool
  -> NodeId
  -> NodeAddr
  -> State g
  -> m ([KadO], State g)
insertNodeId rTick isReply nodeId srcAddr s0 = flip runStateWT s0 $ do
  -- I-update-k-bucket
  if nodeId == kSelf s0
    then do
      -- TODO: ensure reasonable data about myself doesn't get evicted, since
      -- data coming into this function is only ever from other people, and
      -- we'll end up propagating it to others.
      _kOwnInfo %= updateNodeAddr
    else do
      r <- state $ kBucketModifyAtNode nodeId $ _bEntries %%~ bEntriesTryInsert
      let
        (op, nodeToPing) =
          fromJustNote
              "kBucketModifyAtNode gave Nothing, but we checked nodeId /= kSelf"
            $ r
      whenJust nodeToPing $ \nInfo -> do
        stateWT $ oreqRequest (nodeInfoOf nInfo) Ping
      writeLog $ I_KBucketOp op
 where
  State {..}   = s0
  KParams {..} = kParams
  tick         = case rTick of
    Just t  -> Right t
    Nothing -> Left $! SC.tickNow kSchedule
  updateNodeAddr = nodeUpdateAddr (fromIntegral parRepRouting) tick srcAddr
  nodeMatch      = (==) nodeId . nodeIdOf
  newInfo        = newNodeLocalInfo nodeId tick srcAddr

  -- Try to insert a node address into a k-bucket, returning possibly an
  -- existing node to ping, to maybe replace it with (if the ping fails).
  bEntriesTryInsert e0 = flip runState e0 $ do
    -- Is the node already in this k-bucket?
    maybeOp <- state $ S.bStatePL (nodeMatch . fst) id $ \case
        -- note, in this implementation we explicitly store the timestamp of each
        -- node rather than "moving it to the tail".
      Just ent -> if isReply
        -- "If the sending node already exists in the recipient’s k-bucket, the
        -- recipient moves it to the tail of the list."
        --
        -- "[..] if the least-recently seen node responds [i.e. with Pong], it
        -- is moved to the tail of the list, and the new sender’s contact is
        -- discarded."
        then
          -- fromJust is safe: isReply is only true if it matches an existing
          -- pending Ping OReqRequest, which we do only in the outer function,
          -- which we only do if we set (snd ent) to Just below.
          let
            errmsg = nodeNotFound "KBucketNodePingCheckSuccess"
                                  (kSelf s0)
                                  "pending"
                                  nodeId
          in  ( Just $ KBucketNodePingCheckSuccess
                (nodeIdOf (fromJustNote errmsg $ snd ent))
                nodeId
              , Just (updateNodeAddr (fst ent), Nothing)
              )
        else
          (Just $ KBucketNodeAlreadyIn nodeId, Just (first updateNodeAddr ent))
      Nothing -> if not (S.lenBSeq e0 >= fromIntegral parRepRouting)
        -- "If the node is not already in the appropriate k-bucket and the
        -- bucket has fewer than k entries, then the recipient just inserts the
        -- new sender at the tail of the list."
        then
          ( Just $ KBucketNodeInserted nodeId
          , Just (updateNodeAddr newInfo, Nothing)
          )
        -- "If the appropriate k-bucket is full, however, then the recipient
        -- pings the k-bucket’s least-recently seen node to decide what to do"
        else (Nothing, Nothing)

    case maybeOp of
      Just op -> pure (op, Nothing)
      Nothing -> do
        -- Node not in this k-bucket, and potentially should be added
        alreadyPending <- state $ S.bStatePL (any nodeMatch . snd) id $ \case
          -- Node already pending, bump its nodeAddr and return the corresponding
          -- active node that is awaiting a ping, we may want to ping it again.
          Just ent -> (Just (fst ent), Just (second (fmap updateNodeAddr) ent))
          -- Node not pending, go to next block
          Nothing  -> (Nothing, Nothing)

        case alreadyPending of
          Just nInfo ->
            pure (KBucketNodeAlreadyPending nodeId (nodeIdOf nInfo), Just nInfo)
          Nothing -> do
            -- Node not pending, select the least-recently seen node with no pending
            modify $ S.bSortBy compare
            state $ S.bStatePL (isNothing . snd) id $ \case
              Just ent ->
                ( ( KBucketNodePingCheckStart nodeId (nodeIdOf (fst ent))
                  , Just (fst ent)
                  )
                , Just (fst ent, Just newInfo)
                )
              Nothing ->
                -- No more pending spaces left, have to ignore
                ((KBucketTotallyFullIgnoring nodeId, Nothing), Nothing)

insertNodeIdTOReqPing
  :: Monad m => SC.Tick -> NodeId -> State g -> m ([KadO], State g)
insertNodeIdTOReqPing realNow nodeId s0 = flip runStateWT s0 $ do
  alreadySucceeded <- use (_kSentReq . at_ (nodeId, Ping)) >>= \case
    Present oreqProc -> case oreqFuture oreqProc of
      F.SFSettled (F.GotResult Pong) -> pure True
      _                              -> pure False
    _ ->
      error "insertNodeIdTOReqPing failed to find corresponding Ping request"
  unless alreadySucceeded $ do
    r <- state $ kBucketModifyAtNode nodeId $ _bEntries %%~ do
      S.bStatePL (nodeMatch . fst) (S.bSortBy compare) $ \case
        Just ent ->
          -- I-insert-node-lru-fail-ping
          let
            errmsg = nodeNotFound "KBucketNodePingCheckFail"
                                  (kSelf s0)
                                  "pending"
                                  nodeId
            pendingNode = fromJustNote errmsg $ snd ent
          in
            ( KBucketNodePingCheckFail (nodeIdOf pendingNode) nodeId
            , Just (pendingNode, Nothing)
            )
        Nothing -> error $ nodeNotFound "KBucketNodePingCheckFail"
                                        (kSelf s0)
                                        "original"
                                        nodeId
    let op = fromJustNote "kBucketModifyAtNode gave Nothing in TOReqPing" r
    writeLog $ I_KBucketOp op
  -- kHandleInput already calls oreqDelete so no need to do it here
  where nodeMatch = (==) nodeId . nodeIdOf

-- TODO: this should be removeable, only used in JoinNetwork
insertNodes
  :: (Monad m, Traversable t, R.DRG' g)
  => t (NodeInfo NodeAddr)
  -> State g
  -> m ([KadO], State g)
insertNodes nInfos = runStateWT $ do
  for_ nInfos $ \NodeInfo {..} -> do
    for_ niNodeAddr $ do
      stateWT . insertNodeId Nothing False niNodeId

-- | Behaviour for updating k-buckets based on which nodes send stuff to us.
kBucketMaint
  :: (R.DRG' g, Monad m)
  => KadI
  -> Maybe OReqProcess
  -> State g
  -> m ([KadO], State g)
kBucketMaint input oreqProc' = case input of
  P.MsgRT (P.RTTick realNow task) -> case task of
    TOOReq dst reqBody -> case reqBody of
      Ping -> insertNodeIdTOReqPing realNow dst
      _    -> doNothing
    _ -> doNothing
  P.MsgUser _        -> doNothing
  P.MsgProc Msg {..} -> case body of
    Left Request{} -> do
      insertNodeId Nothing False src srcAddr
    Right Reply {..} -> case oreqProc' of
      Nothing ->
        -- we do not call insertNodeId here since it feels wrong to react to a
        -- reply we don't want. the paper is silent on this matter. this could be
        -- spam or it could be that we simply timed out waiting for the request.
        doNothing
      Just OReqProcess{} -> do
        let isPong = case repBody of
              Right Pong -> True
              _          -> False
        insertNodeId (Just sent) isPong src srcAddr
  where doNothing s0 = pure (mempty, s0)

-- | Run a new or existing ICmdProcess for the given incoming command.
icmdStart
  :: (Monad m, R.DRG' g) => Command -> Bool -> State g -> m ([KadO], State g)
icmdStart cmd isExternal s0 = flip runStateWT s0 $ do
  statewT $ icmdEnsure _kSchedule _kRecvCmd _kSentReq cmd isExternal parTOICmd
  cmdProc <- use $ _kRecvCmd . unsafeIx cmdId
  stateWT $ icmdRunInput cmdId cmdProc Nothing ICStart
 where
  State {..}   = s0
  KParams {..} = kParams
  Command {..} = cmd

-- | Initial lookup subject of a command
cmdSubj :: NodeId -> CommandBody -> NodeId
cmdSubj self = \case
  JoinNetwork _     -> self
  LookupNode  nId   -> nId
  LookupValue key   -> key
  InsertValue key _ -> key
  c -> error $ "programmer error, invalid argument for cmdSubj: " <> show c

-- | Lookup type of a command
cmdOReq :: CommandBody -> NodeId -> RequestBody
cmdOReq = \case
  JoinNetwork _   -> GetNode
  LookupNode  _   -> GetNode
  LookupValue _   -> GetValue
  InsertValue _ _ -> GetNode
  c -> error $ "programmer error, invalid argument for cmdOReq: " <> show c

kSimpleLookup
  :: Int
  -> Int
  -> NodeId
  -> SimpleQuery ()
  -> (Either NodeInfos' NodeInfos', SimpleQuery ())
kSimpleLookup parallel numResults target qState =
  -- "Of the k nodes the initiator has heard of closest to the target,
  -- it picks α that it has not yet queried and resends the FIND_NODE
  -- RPC to them. [..] If [this] fails to return a node any closer than
  -- the closest already seen, the initiator resends the FIND_NODE to
  -- all of the k closest nodes it has not already queried."
  --
  -- The below is a minor variation that deals with replies arriving
  -- one-by-one rather than all at once.
  let closestK =
        Set.fromList
          $ take numResults
          $ sortOn (distance target)
          $ toList
          $ M.keysSet sqAll
      candidates =
        closestK
          `Set.difference` M.keysSet sqOk
          `Set.difference` sqErr
          `Set.difference` sqWait
      toQuery =
        Set.fromList
          $ take (parallel - Set.size sqWait)
          $ sortOn (distance target)
          $ toList candidates
  in  if null candidates && null (Set.intersection closestK sqWait)
        then (Right (M.restrictKeys sqAll closestK), qState)
        else
          ( Left (M.restrictKeys sqAll toQuery)
          , qState { sqWait = Set.union sqWait toQuery }
          )
  where SimpleQuery {..} = qState

kSimpleInsert
  :: Int
  -> SimpleQuery SC.TickDelta
  -> ( Either NodeInfos' (M.Map NodeId (Maybe SC.TickDelta))
     , SimpleQuery SC.TickDelta
     )
kSimpleInsert parallel qState =
  -- "To store a key,value pair, a participant locates the k closest nodes to
  -- the key and sends them STORE RPCs.
  --
  -- The below just tries all of them, honouring the parallel parameter.
  let candidates =
        M.keysSet sqAll
          `Set.difference` M.keysSet sqOk
          `Set.difference` sqErr
          `Set.difference` sqWait
      toQuery =
        Set.fromList $ take (parallel - Set.size sqWait) $ toList candidates
      result = M.unionWith (error "unreachable")
                           (Just <$> sqOk)
                           (M.fromSet (const Nothing) sqErr)
  in  if null candidates && null sqWait
        then (Right result, qState)
        else
          ( Left (M.restrictKeys sqAll toQuery)
          , qState { sqWait = Set.union sqWait toQuery }
          )
  where SimpleQuery {..} = qState

icmdRunInput
  :: (Monad m, R.DRG' g)
  => CmdId
  -> ICmdProcess
  -> Maybe (F.TimedResult () ReplyBody)
  -> ICmdInput
  -> State g
  -> m ([KadO], State g)
icmdRunInput cmdId cmdProc rep input s0 = flip runStateWT s0 $ do
  s     <- use qState
  reply <- kQuery (s, input)
  whenJust reply $ \r -> do
    when (icmdExternal cmdProc) $ do
      lift $ tell [P.MsgUser $ CommandReply cmdId $ Right r]
    _kRecvCmd . unsafeIx cmdId . _icmdResult .= Just r
 where
  State { kParams }   = s0
  KParams {..}        = kParams
  Command { cmdBody } = icmdCmd cmdProc

  qState :: Lens' (State g) ICmdState'
  qState = _kRecvCmd . unsafeIx cmdId . _icmdState

  qStateSet qs1 = lift . tell =<< qState %%= \qs0 ->
    let qs0' = icmdStateSummary qs0
        qs1' = icmdStateSummary qs1
        out  = [ kLog $ I_ICmdStateChange cmdId qs1' | qs1' /= qs0' ]
    in  (out, qs1)

  inputIgnored s = do
    case (rep, input) of
      (Just (F.GotResult r), ICReply repSrc (F.GotResult _)) -> writeLog $ do
        I_ICmdIgnoredInvalidInputForState cmdId repSrc r (icmdStateSummary s)
      (Nothing, ICStart) -> pure ()
      _ ->
        error
          $  "should not be ignoring this: "
          <> show rep
          <> "; input: "
          <> show input
    pure Nothing

  finishWithResult r = do
    qStateSet ICFinished
    pure $ Just r

  cancelExistingOReqs = statewT $ do
    icmdCancelAll_ _kSchedule _kRecvCmd _kSentReq cmdId

  -- | Main Kademlia query state machine.
  kQuery = \case
    -- Note: duplicate replies are already filtered out by 'icmdOReqExpect_'
    -- before control hits this function, but perhaps we should assert that none
    -- of the below "Set.delete" are nops

    (ICNewlyCreated, ICStart) -> case cmdBody of
      GetNodeId          -> finishWithResult $ OwnNodeId $ kSelf s0
      GetNumOngoingProcs -> do
        let oreq = sizeBMap2 (kSentReq s0)
            ireq = sizeBMap2 (kRecvReq s0)
            icmd = sizeBMap2 (kSentReq s0)
        finishWithResult $ NumOngoingProcs oreq ireq icmd
      _ -> do
        case cmdBody of
          -- "To join the network, a node u must have a contact to an
          -- already participating node w. u inserts w into the appropriate
          -- k-bucket. u then performs a node lookup for its own node ID."
          JoinNetwork n -> stateWT $ insertNodes [n]
          _             -> pure ()
        -- start of request, populate from our own k-buckets
        let key = cmdSubj (kSelf s0) $ cmdBody
        nInfos <- kGetNodes key <$> get
        continueLookup key $ newSimpleQuery (toNodeInfos' nInfos)

    (ICLookingup key qs, ICReply repSrc (F.TimedOut ())) -> do
      continueLookup key $ qs { sqErr  = Set.insert repSrc (sqErr qs)
                              , sqWait = Set.delete repSrc (sqWait qs)
                              }

    (ICLookingup key qs, ICReply repSrc (F.GotResult (ICGotNodes ni))) -> do
      continueLookup key $ qs { sqAll  = niUnion (sqAll qs) (toNodeInfos' ni)
                              , sqOk   = M.insert repSrc () (sqOk qs)
                              , sqWait = Set.delete repSrc (sqWait qs)
                              }

    (ICLookingup key qs, ICReply repSrc (F.GotResult (ICGotValue val))) -> do
      case cmdBody of
        LookupValue _ -> do
          cancelExistingOReqs
          -- "[When finding] a key,value pair, the procedure halts immediately
          -- when any node returns the value. For caching purposes, once a lookup
          -- succeeds, the requesting node stores the key,value pair at the closest
          -- node it observed to the key that did not return the value."
          --
          -- TODO: this is not great, eventually we should have values signed &
          -- associated with creation timestamps, wait for the query to finish &
          -- insert the most-recent one.
          --continueInsert val $ newSimpleQuery undefined {- TODO -}
          qStateSet ICFinished
          -- below will remain, even after we perform the above change - i.e.
          -- we output a result before we actually finish the ICmdProcess.
          pure $ Just $ LookupValueReply $ Right val

        _ -> inputIgnored $ ICLookingup key qs

    (ICInserting val qs, ICReply repSrc (F.TimedOut ())) -> do
      continueInsert val $ qs { sqErr  = Set.insert repSrc (sqErr qs)
                              , sqWait = Set.delete repSrc (sqWait qs)
                              }

    (ICInserting val qs, ICReply repSrc (F.GotResult (ICPutOK expire))) -> do
      -- sanity check
      case cmdBody of
        LookupValue _   -> pure ()
        InsertValue k v -> pure ()
        _               -> error "unreachable"
      continueInsert val $ qs { sqOk   = M.insert repSrc expire (sqOk qs)
                              , sqWait = Set.delete repSrc (sqWait qs)
                              }

    (s, _) -> inputIgnored s

  continueLookup key qs = do
    let (next, qs') = kSimpleLookup (fromIntegral parParallel)
                                    (fromIntegral parRepRouting)
                                    key
                                    qs
    qStateSet $ ICLookingup key qs'
    case next of
      Left toQuery -> do
        let reqBody = cmdOReq cmdBody key
        -- JoinNetwork involves looking up multiple keys, so we take it from
        -- the key and not the original command
        for_ (M.toList toQuery) $ \(nId, nAddr) -> do
          stateWT $ kBucketBump nId
          -- TODO(addr): pass in timestamp data so we can select between addresses
          -- in a more intelligent way. This requires this info to be available
          -- in the 'GetNodeReply' etc messages.
          let nInfo = NodeInfo nId $ S.bFromList $ toList nAddr
          stateWT $ icmdOReqRequest cmdId nInfo reqBody
        pure Nothing
      Right res -> cancelExistingOReqs >> case cmdBody of
        JoinNetwork nInfo -> do
          -- "Finally, u refreshes all k-buckets further away than its
          -- closest neighbor."
          s <- get
          let nextIdx = case kBucketGetIndex key s of
                Nothing -> kBucketGetNextIndex 0 s
                Just i  -> kBucketGetNextIndex (i + 1) s
          case nextIdx of
            Just idx -> do
              nextKey <- StateT $ kRefreshBucketOnly idx
              nInfos  <- kGetNodes nextKey <$> get
              continueLookup nextKey $ newSimpleQuery (toNodeInfos' nInfos)
            Nothing -> do
              nInfos <- kGetNodes (kSelf s) <$> get
              finishWithResult $ JoinNetworkReply nInfos
        LookupNode _ -> do
          finishWithResult $ LookupNodeReply $ toNodeInfos res
        LookupValue _ -> do
          finishWithResult $ LookupValueReply $ Left $ toNodeInfos res
        InsertValue k v -> do
          continueInsert v $ newSimpleQuery res
        c -> do
          error $ "programmer error, invalid arg for continueLookup: " <> show c

  continueInsert val qs = do
    let (next, qs') = kSimpleInsert (fromIntegral parParallel) qs
    qStateSet $ ICInserting val qs'
    case next of
      Left toQuery -> do
        let key     = cmdSubj (error "unreachable") $ cmdBody
        let reqBody = PutValue key val
        for_ (M.toList toQuery) $ \(nId, nAddr) -> do
          -- TODO(addr): pass in timestamp data, etc as above
          let nInfo = NodeInfo nId $ S.bFromList $ toList nAddr
          stateWT $ icmdOReqRequest cmdId nInfo reqBody
        pure Nothing
      Right res -> cancelExistingOReqs >> case cmdBody of
        LookupValue _ -> do
          qStateSet $ ICFinished
          pure Nothing -- result was previously output already
        InsertValue k v -> do
          finishWithResult $ InsertValueReply res
        _ -> error "unreachable"

{- | Make an outgoing request. -}
oreqRequest
  :: (Monad m, R.DRG' g)
  => NodeInfo NodeAddr
  -> RequestBody
  -> State g
  -> m ([KadO], State g)
oreqRequest nInfo reqBody s0 = flip runStateWT s0 $ do
  let NodeInfo reqDst reqDstAddrs = nInfo
  let mkMsg now req = Msg { src     = kSelf s0
                          , srcAddr = mempty -- Proc runtime will set this properly, via setSource
                          , dst     = reqDst
                          , dstAddr = S.bLast reqDstAddrs
                          -- TODO(addr): better selection method for addr, use multiple
                          , sent    = now
                          , body    = Left req
                          }
  statewT $ oreqEnsure' mkMsg reqDst reqBody
 where
  State {..}   = s0
  KParams {..} = kParams
  par          = (parTOOReq, parTOOReqRetry)
  oreqEnsure'  = oreqEnsure _kRng _kSchedule _kSentReq _kSentReqId par

{- | Make an outgoing request on behalf of the given 'ICmdProcess'.

Partial function: The 'ICmdProcess' must already exist.
-}
icmdOReqRequest
  :: (Monad m, R.DRG' g)
  => CmdId
  -> NodeInfo NodeAddr
  -> RequestBody
  -> State g
  -> m ([KadO], State g)
icmdOReqRequest cmdId nInfo reqBody s0 = flip runStateWT s0 $ do
  stateWT $ oreqRequest nInfo reqBody
  let NodeInfo reqDst _ = nInfo
  statewT $ icmdOReqExpect_' cmdId reqDst reqBody $ \r -> do
    runIdentity . icmdOReqResult cmdId reqDst reqBody r
 where
  State {..}       = s0
  KParams {..}     = kParams
  icmdOReqExpect_' = do
    icmdOReqExpect_ _kSchedule _kRecvCmd _kSentReq parTOICmdOReq

{- | Handle an incoming result on behalf of the given 'ICmdProcess'.

Partial function: The 'ICmdProcess' must already exist.
-}
icmdOReqResult
  :: (Monad m, R.DRG' g)
  => CmdId
  -> NodeId
  -> RequestBody
  -> F.TimedResult () ReplyBody
  -> State g
  -> m ([KadO], State g)
icmdOReqResult cmdId reqDst reqBody rep s0 = flip runStateWT s0 $ do
  -- If cmdId is Nothing, we don't need to clear results, since we never put
  -- them there in the first place, in icmdOReqRequest
  case kRecvCmd ^? ix cmdId of
    Nothing ->
      -- should be unreachable due to other checks
      error $ "icmdOReqResult: non-existent cmdId: " <> show cmdId
    Just cmdProc -> do
      case toCmdInput reqDst reqBody rep of
        Nothing -> case rep of
          F.GotResult r -> writeLog $ W_ICmdIgnoredInvalidInput cmdId reqDst r
          F.TimedOut  _ -> error "icmdOReqResult called with TimedOut"
        Just cmdInput ->
          stateWT $ icmdRunInput cmdId cmdProc (Just rep) cmdInput
  where State {..} = s0

kHandleInput
  :: (Monad m, R.DRG' g)
  => KadI
  -> Maybe OReqProcess
  -> State g
  -> m ([KadO], State g)
kHandleInput input oreqProc' s0 = flip runStateWT s0 $ case input of
  P.MsgRT (P.RTTick realNow task) -> case task of
    SelfCheck -> do
      runExceptT (checkState s0) >>= \case
        Left  err -> writeLog $ W_SelfCheckFailed err
        Right ()  -> writeLog $ D_SelfCheckSuccess
      t <- _kSchedule %%= SC.after parIntvSelfCheck SelfCheck
      _kSelfCheck .= t
    RefreshBucket idx -> stateWT $ kRefreshBucket idx
    RepublishKey  key -> do
      let StoreEntry {..} =
            fromJustNote "RepublishKey couldn't find key" $ kStore ^? ix key
      cmdId <- state newCmdId
      stateWT $ icmdStart (Command cmdId (InsertValue key sValue)) False
    ExpireKey key -> do
      let StoreEntry {..} =
            fromJustNote "ExpireKey couldn't find key" $ kStore ^? ix key
      _kSchedule %%= SC.cancel_ sRepub
      -- don't need to cancel expire since that is already running
      _kStore %= sans key
      writeLog $ I_KeyExpired key
    TOOReqReply dst    reqBody -> pure ()
      -- TODO(retry): forward to the OReqProcess logic, to trigger a retry
    TOOReq      reqDst reqBody -> do
      stateWT $ oreqResult_ reqDst reqBody (F.TimedOut ()) icmdOReqResult
      statewT $ oreqDelete' reqDst reqBody
    TOIReq reqSrc reqBody           -> statewT $ ireqDelete' reqSrc reqBody
    TOICmdOReq cmdId reqDst reqBody -> do
      -- note: we don't distinguish between the icmd vs the oreq timing out,
      -- but here is where we would do it in future, if we need to
      statewT $ icmdOReqCancel_' cmdId reqDst reqBody
      stateWT $ icmdOReqResult cmdId reqDst reqBody (F.TimedOut ())
    TOICmd cmdId -> statewT $ icmdDelete' cmdId parTOICmd
  P.MsgUser cmd -> stateWT $ icmdStart cmd True
  P.MsgProc msg -> case body msg of
    Left Request {..} -> do
      statewT $ ireqEnsure' msg $ \s -> case reqBody of
        Ping         -> (Pong, s)
        GetNode  nId -> (GetNodeReply $ kGetNodes nId s, s)
        GetValue k   -> case kStore ^? ix k of
          Nothing              -> (GetValueReply $ Left $ kGetNodes k s, s)
          Just StoreEntry {..} -> (GetValueReply $ Right sValue, s)
        PutValue k v -> first PutValueReply $ kPutValue k v s
    Right Reply {..} -> case repBody of
      Left  rej -> pure () -- TODO(retry): handle this, OReqProcess should wait & retry
      Right r   -> case oreqProc' of
        Nothing -> do
          writeLog $ W_InvalidMessage msg "reply to a request we didn't make"
        Just oreqProc -> stateWT $ do
          let (reqDst, reqBody) = fst $ oreqIndex $ oreqMsg oreqProc
          oreqResult_ reqDst reqBody (F.GotResult r) icmdOReqResult
 where
  State {..}         = s0
  KParams {..}       = kParams
  ireqEnsure'        = ireqEnsure _kSchedule _kRecvReq parTOIReq
  ireqDelete'        = ireqDelete _kSchedule _kRecvReq
  oreqDelete'        = oreqDelete _kSchedule _kSentReq _kSentReqId
  icmdDelete'        = icmdDelete _kSchedule _kRecvCmd _kSentReq
  oreqResultFuture_' = oreqResultFuture_ _kSchedule _kRecvCmd _kSentReq
  icmdOReqCancel_'   = icmdOReqCancel_ _kSchedule _kRecvCmd _kSentReq

  -- (reqDst, reqBody) must exist, the caller ensures this
  oreqResult_ reqDst reqBody result handleResult = runStateWT $ do
    statewT $ oreqResultFuture_' result reqDst reqBody $ \waiting ->
      fmap runIdentity . runStateWT $ for_ waiting $ \cmdId -> do
        stateWT $ handleResult cmdId reqDst reqBody result

{- | Top-level input processing function.

Main entry point for this Kademlia protocol state-machine.

Uses 'kBucketMaint' and 'kHandleInput'.
-}
kInput :: (R.DRG' g, Monad m) => KadI -> State g -> m ([KadO], State g)
kInput input s0 = flip runStateWT s0 $ do
  -- prelim processing for child functions
  let (oreqProc', maybePing, maybeDst) = case input of
        P.MsgProc recvMsg@Msg {..} -> case body of
          Right Reply {..} ->
            ( kSentReqId ^? ix repReqId >>= \k -> kSentReq ^? ix k
            , P.MsgProc <$> pingReplyToRequest recvMsg
            , Just (dst, recvMsg)
            )
          _ -> (Nothing, Nothing, Just (dst, recvMsg))
        _ -> (Nothing, Nothing, Nothing)
  case maybeDst of
    Just (dst, msg) | dst /= kSelf s0 -> do
      writeLog $ W_InvalidMessage msg "message with destination not ourselves"
    _ -> do
      stateWT $ kBucketMaint input oreqProc'
      stateWT $ kHandleInput input oreqProc'
      whenJust maybePing $ stateWT . kInput
  where State {..} = s0

kInput'
  :: (R.DRG' g, Monad m) => KadI' -> StateT (State g) (WriterT [KadO] m) ()
kInput' = tickTask (_kSchedule %%=) P._MsgI_RTTick (stateWT . kInput)

instance P.Protocol (State g) where
  type PMsg (State g) = Msg
  type UserI (State g) = KUserI
  type UserO (State g) = KUserO
  type AuxO (State g) = KLogMsg

instance R.DRG' g => P.Proc (State g) where
  getAddrs s = toList $ niNodeAddr $ ownNodeInfo s
  localNow s = SC.tickNow $ kSchedule s
  react i s = runIdentity (runStateWT (kInput' i) s)
