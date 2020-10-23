{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module P4P.Protocol.DHT.Kademlia.State where

-- external
import qualified Control.Monad.Schedule            as SC
import qualified Control.Schedule.Future           as F
import qualified Crypto.Random.Extra               as R
import qualified Data.ByteString                   as BS
import qualified Data.Map.Bounded                  as BM
import qualified Data.Map.Strict                   as M
import qualified Data.Sequence.Extra               as S
import qualified Data.Strict                       as Z
import qualified Data.Vector                       as V

import           Codec.Serialise                   (Serialise)
import           Control.Applicative               (liftA2)
import           Control.Lens                      (itraversed, traversed,
                                                    (%%@~), (%%~), (%~), (^?!),
                                                    _Wrapped)
import           Control.Lens.Extra                (unsafeIx)
import           Control.Lens.Strict               (Ixed (..))
import           Control.Lens.TH                   (makeWrapped)
import           Control.Lens.TH.Extra             (makeLenses_)
import           Control.Monad                     (void)
import           Control.Monad.Extra               (whenJust)
import           Control.Monad.Trans.Except        (ExceptT, throwE)
import           Control.Monad.Trans.Except.Extra  (ErrMsg, checkConsistent,
                                                    checkConstrName)
import           Control.Monad.Trans.State.Strict  (StateT (..), state)
import           Data.Binary                       (Binary)
import           Data.Bits                         (complement,
                                                    countLeadingZeros, shiftL,
                                                    xor, (.&.), (.|.))
import           Data.Foldable                     (foldl')
import           Data.Function                     (on, (&))
import           Data.Functor.Identity             (runIdentity)
import           Data.Traversable                  (for)
import           Data.Tuple.Extra                  (dupe)
import           Data.Vector.Binary                ()
import           Data.Word                         (Word16, Word8)
--import           Debug.Pretty.Simple               (pTraceShowId)
import           GHC.Generics                      (Generic)
import           Safe.Foldable                     (maximumBound)

-- internal
import           P4P.Protocol.DHT.Kademlia.Command
import           P4P.Protocol.DHT.Kademlia.Message
import           P4P.Protocol.DHT.Kademlia.Request


data KParams = KParams
  { parKeyBytes      :: !Word8
  -- ^ Number of bytes of a key, e.g. 32 for 256 bits.
  , parRepRouting    :: !Word8
  -- ^ Max-size of a k-bucket. From the paper:
  -- "k is chosen such that any given k nodes are very unlikely to fail within
  -- an hour of each other (for example k = 20)".
  , parRepStorage    :: !Word8
  -- ^ Storage replication factor. TODO: used in S-Kademlia, not yet implemented
  , parParallel      :: !Word8
  -- ^ Max number of outstanding outgoing requests used to serve each incoming
  -- request. From the paper:
  -- "alpha is a system-wide concurrency parameter, such as 3"
  , parAddrsPerNode  :: !Word8
  -- ^ Max number of addresses to store for a node.
  --------
  , parMaxReqPerNode :: !Word16
  -- ^ Max number of outstanding outgoing or incoming requests per peer, for
  -- rate-limiting. e.g. 64.
  , parMaxReqNodes   :: !Word16
  -- ^ Max number of outstanding outgoing or incoming peers, for rate-limiting.
  -- e.g. 1024.
  , parMaxCmd        :: !Word16
  -- ^ Max number of outstanding local-user commands, for rate-limiting,
  -- e.g. 4096.
  --------
  , parTOOReqRetry   :: !SC.TickDelta
  -- ^ When handling outgoing requests, this is the base retry time that the
  -- OReqProcess will attempt exponential-backoff retries at. It should be
  -- slightly higher than the expected network round-trip latency.
  , parTOOReq        :: !SC.TickDelta
  -- ^ Overall request timeout time, for handling outgoing requests. This is
  -- the overall time that each OReqProcess will wait. This should be longer
  -- than 'parTOICmdOReq' since we deduplicate outgoing requests per-peer:
  -- suppose we have two ICmdProcesses interested in the same OReqProcess, then
  -- we want the later one to have a chance of succeeding even if the earlier
  -- one times out for that OReqProcess.
  , parTOIReq        :: !SC.TickDelta
  -- ^ Overall reply timeout time, for handling replies to incoming requests.
  -- This is the time that we wait for after sending back the initial reply,
  -- for duplicate incoming requests where we'll send back the same reply.
  --------
  , parTOICmdOReq    :: !SC.TickDelta
  -- ^ When handling incoming requests, this is the time that the ReqProcess
  -- will wait for each outgoing request that it is interested in.
  , parTOICmd        :: !SC.TickDelta
  -- ^ Overall request timeout time, for handling incoming requests. This is
  -- the overall time that each ICmdProcess will wait, before timing out.
  --------
  , parIntvKBRefresh :: !SC.TickDelta
  -- ^ Bucket refresh interval. Paper recommends 1 hr.
  , parIntvValRepub  :: !SC.TickDelta
  -- ^ Value republish interval. Paper recommends 1 hr.
  , parIntvValExpire :: !SC.TickDelta
  -- ^ Value expire interval. Paper recommends 24 hrs.
  , parIntvSelfCheck :: !SC.TickDelta
  -- ^ Self-consistency time.
  }
  deriving (Show, Read, Generic, Binary, Serialise, Eq, Ord)

parKeyBits :: KParams -> Int
parKeyBits KParams {..} = fromIntegral parKeyBytes * 8

testingParams :: Int -> Int -> KParams
testingParams speedAuto tickHz = KParams
  { parKeyBytes      = 32
  , parRepRouting    = 32
  , parRepStorage    = 16
  , parParallel      = 8
  , parAddrsPerNode  = 16
  , parMaxReqPerNode = 64
  , parMaxReqNodes   = 1024
  , parMaxCmd        = 4096
  , parTOOReqRetry   = 1 * t
  , parTOOReq        = 16 * t
  , parTOIReq        = 16 * t
  , parTOICmdOReq    = 16 * t
  , parTOICmd        = 64 * t
  , parIntvKBRefresh = 4096 * t `div` s
  , parIntvValRepub  = 4096 * t `div` s
  , parIntvValExpire = 262144 * t `div` s
  , parIntvSelfCheck = 4096 * t `div` s
  }
 where
  t = fromIntegral tickHz
  s = fromIntegral speedAuto

defaultParams :: Int -> KParams
defaultParams = testingParams 1

{- | Local info about a node, including its addresses and last-seen timestamps.
-}
newtype NodeLocalInfo = NodeLocalInfo {
  getNodeInfo :: NodeInfo (Z.Pair (Z.Either SC.Tick SC.Tick) NodeAddr)
  -- ^ NodeInfo with its addresses and last-seen timestamps
  -- Left t means the timestamp is a local receipt timestamp of some non-reply
  -- message. Right t means the timestamp is of a reply to a previous ReqId
  -- sent by this node at local timestamp t. Reply timestamps are preferred
  -- over non-reply timestamps since they cannot be forged.
  } deriving (Show, Read, Generic, Eq)
instance Binary NodeLocalInfo
instance Serialise NodeLocalInfo
makeLenses_ ''NodeLocalInfo
makeWrapped ''NodeLocalInfo

newNodeLocalInfo
  :: NodeId -> Either SC.Tick SC.Tick -> NodeAddr -> NodeLocalInfo
newNodeLocalInfo nodeId seen nodeAddr = NodeLocalInfo NodeInfo
  { niNodeId   = nodeId
  , niNodeAddr = pure (Z.toStrict seen Z.:!: nodeAddr)
  }

nodeIdOf :: NodeLocalInfo -> NodeId
nodeIdOf (NodeLocalInfo NodeInfo {..}) = niNodeId

nodeInfoOf :: NodeLocalInfo -> NodeInfo NodeAddr
nodeInfoOf (NodeLocalInfo nInfo) = fmap Z.snd nInfo

nodeLastSeen :: NodeLocalInfo -> (SC.Tick, NodeId)
nodeLastSeen (NodeLocalInfo NodeInfo {..}) =
  ( either id id $ maximumBound (Left 0) (Z.toLazy . Z.fst <$> niNodeAddr)
  , niNodeId
  )

-- | Update one of the addresses in a 'NodeLocalInfo', possibly pruning some
-- old addresses.
nodeUpdateAddr
  :: Int -> Either SC.Tick SC.Tick -> NodeAddr -> NodeLocalInfo -> NodeLocalInfo
nodeUpdateAddr maxSz tick addr nInfo = nInfo & _Wrapped . _niNodeAddr %~ update
 where
  update x = snd $ S.bStatePL ((==) addr . Z.snd)
                              (S.bPruneL maxSz . S.bSortBy compare)
                              st
                              x
  st (Just (tick' Z.:!: _)) = ((), Just (z Z.:!: addr))
    where z = max (Z.toStrict tick) tick'
  st Nothing = ((), Just (Z.toStrict tick Z.:!: addr))

instance Ord NodeLocalInfo where
  compare = compare `on` nodeLastSeen

type Distance = [Word8]

distance :: Key -> Key -> Distance
distance = BS.zipWith xor

distanceLeadingZeros :: Distance -> Int
distanceLeadingZeros dist = either id id $ foldl' f (Left 0) $ fmap
  countLeadingZeros
  dist
 where
  f (Right x) _ = Right x
  f (Left  x) a = if a == 8 then Left (x + a) else Right (x + a)

data KBucket = KBucket
  { bEntries          :: !(KSeq (Z.Pair NodeLocalInfo (Z.Maybe NodeLocalInfo)))
  -- ^ Entries in the KBucket, along with any newly discovered node that might
  -- replace it according to the protocol.
  , bMostRecentLookup :: !SC.Tick
  -- ^ "To avoid pathological cases when no traffic exists, each node refreshes
  -- a bucket in whose range it has not performed a node lookup within an hour."
  , bRefresh          :: !(SC.Task KTask) -- RefreshBucket
  }
  deriving (Show, Read, Generic, Binary, Serialise, Eq)
makeLenses_ ''KBucket

newKBucket :: SC.Tick -> SC.Task KTask -> KBucket
newKBucket now refresh =
  KBucket { bEntries = mempty, bMostRecentLookup = now, bRefresh = refresh }

kBucketEntriesByDistance :: NodeId -> KBucket -> KSeq (NodeInfo NodeAddr)
kBucketEntriesByDistance refNode KBucket {..} = S.bSortBy cmp
  $ fmap (fmap Z.snd . getNodeInfo . Z.fst) bEntries
  where cmp = compare `on` distance refNode . niNodeId

data StoreEntry = StoreEntry
  { sValue  :: !Value
  , sOwn    :: !Bool
  -- ^ Whether this was stored by myself or via a request from others.
  -- This basically just controls the republish interval.
  , sRepub  :: !(SC.Task KTask) -- RepublishKey
  , sExpire :: !(SC.Task KTask) -- ExpireValue
  }
  deriving (Show, Read, Generic, Binary, Serialise, Eq)

type BoundedMap k v = BM.BMap2 k RequestBody v

checkBounded
  :: (Eq k, Show k, Monad m)
  => String
  -> BoundedMap k v
  -> (v -> ExceptT ErrMsg m (k, Maybe RequestBody))
  -> (String -> v -> ExceptT ErrMsg m ())
  -> ExceptT ErrMsg m ()
checkBounded prefix rl ex vcheck = void $ rl & itraversed %%@~ \(k, b) v -> do
  let p = prefix <> "/" <> show k <> "/" <> show b
  (k', b') <- ex v
  checkConsistent p "key" k k'
  whenJust b' $ checkConsistent p "body" b
  vcheck p v

data State drg = State
  { kRng       :: !drg
  , kParams    :: !KParams
  , kSchedule  :: !(SC.Schedule KTask)
  , kSelfCheck :: !(SC.Task KTask) -- SelfCheck
  , kOwnInfo   :: !NodeLocalInfo
  , kBuckets   :: !(V.Vector KBucket)
  -- ^ k-buckets. Index 0 stores the nodes closest to us.
  , kStore     :: !(M.Map Key StoreEntry)
  -- ^ TODO(rate): this should also be bounded...

  -- | Requests we've sent out and are waiting results for.
  --
  -- TODO(rate): we rate-limit this only to check that our assumption is correct. The
  -- real enforcement should be done via the rate-limiting of RecvCmd, and if
  -- this ever overflows then it's a logic error.
  --
  -- TODO(rate): figure out the actual rate limit
  , kSentReq   :: !(BoundedMap NodeId OReqProcess)
  -- | OReqProcess by ReqId, needed since replies only contain a reqId
  , kSentReqId :: !(M.Map ReqId (Z.Pair NodeId RequestBody))
  -- | Requests we've received (from other nodes) and have sent replies for.
  , kRecvReq   :: !(BoundedMap NodeId IReqProcess)

  -- | Commands we've received (from either the local user or internally e.g.
  -- automatically refreshing a bucket) and are currently handling.
  -- TODO(rate): bounded Map, except for "system" commands
  , kRecvCmd   :: !(BM.BMap CmdId ICmdProcess)
  }
  deriving (Show, Read, Generic, Binary, Serialise, Eq)
makeLenses_ ''State

checkState :: Monad m => State drg -> ExceptT ErrMsg m ()
checkState State {..} = do
  -- TODO: check data structure sizes are the same as specificed in kParams
  -- TODO: check consistency between ICmdProcess and kSentReq
  checkTaskPending "kSelfCheck" kSelfCheck "SelfCheck"
  whenJust (SC.checkValidity kSchedule) $ \e -> do
    throwE $ "schedule not consistent: " <> show e
  void $ kBuckets & itraversed %%@~ \i KBucket {..} -> do
    checkTaskPending ("kBuckets/" <> show i) bRefresh "RefreshBucket"
  void $ kStore & itraversed %%@~ \k StoreEntry {..} -> do
    checkTaskPending ("kStore/" <> show k) sRepub  "RepublishKey"
    checkTaskPending ("kStore/" <> show k) sExpire "ExpireKey"
  checkBounded "kSentReq" kSentReq getSentReqIdx $ \prefix v -> do
    -- TODO: retries are not currently implemented
    --checkTaskPending prefix (retryTask $ oreqRetry v) "TOOReqReply"
    checkTaskPending prefix (oreqTimeout v) "TOOReq"
  checkBounded "kRecvReq" kRecvReq getRecvReqIdx $ \prefix v -> do
    checkTaskPending prefix (ireqTimeout v) "TOIReq"
  void $ kRecvCmd & itraversed %%@~ \k ICmdProcess {..} -> do
    void $ F.seExpects icmdExpect & traversed %%~ \t -> do
      checkTaskPending ("kRecvCmd/" <> show k) t           "TOICmdOReq"
      checkTaskPending ("kRecvCmd/" <> show k) icmdTimeout "TOICmd"
 where
  checkTaskPending n t c = case SC.taskStatus t kSchedule of
    SC.TaskPending _ k -> checkConstrName n k c
    SC.TaskRunning _   -> checkConsistent "kSelfCheck" n c "SelfCheck"
    _                  -> throwE $ n <> ": task not pending: " <> show t
  getSentReqIdx OReqProcess {..} = (dst, ) . Just <$> getReqBody body
    where Msg {..} = oreqMsg
  getRecvReqIdx IReqProcess {..} = (src, ) . Just <$> getReqBody body
    where Msg {..} = ireqMsg
  getReqBody (Left  Request {..}) = pure reqBody
  getReqBody (Right rep         ) = throwE $ "not a request: " <> show rep

newState
  :: (R.ByteArrayAccess seed, R.DRG' drg)
  => NodeId
  -> [NodeAddr]
  -> seed
  -> KParams
  -> State drg
newState self addrs seed params = State
  { kRng       = R.initialize seed
  , kParams    = params
  , kSchedule  = sched
  , kSelfCheck = selfCheck
  , kOwnInfo   = NodeLocalInfo
                   (NodeInfo self ((Z.Right 0 Z.:!:) <$> S.bFromList addrs))
  , kBuckets   = newKBucket (SC.tickNow sched) <$> refreshes
  , kStore     = mempty
  , kSentReq   = BM.newBMap2 (fromIntegral parMaxReqNodes)
                             (fromIntegral parMaxReqPerNode)
    -- FIXME: the above bounds are wrong, we should be calculating it based on
    -- the bounds for kRecvCmd and a generous margin, the intent being that it
    -- never goes over this limit at runtime.
  , kSentReqId = mempty
  , kRecvReq   = BM.newBMap2 (fromIntegral parMaxReqNodes)
                             (fromIntegral parMaxReqPerNode)
  , kRecvCmd   = BM.newBMap (fromIntegral parMaxCmd)
  }
 where
  KParams {..} = params
  parKeyBits'  = parKeyBits params
  ((refreshes, selfCheck), sched) =
    runIdentity $ flip runStateT SC.newSchedule $ liftA2
      (,)
      (for (V.generate parKeyBits' id) $ \i -> do
        -- stagger the refreshes evenly
        let t =
              parIntvKBRefresh * fromIntegral i `div` fromIntegral parKeyBits'
        state $ SC.after t (RefreshBucket i)
      )
      (state $ SC.after 0 SelfCheck)

newRandomState
  :: forall drg f
   . (R.DRG' drg, Applicative f)
  => (forall seed . R.ByteArray seed => Int -> f seed)
  -> [NodeAddr]
  -> KParams
  -> f (State drg)
newRandomState getEntropy addrs params =
  newState @BS.ByteString
    <$> getEntropy (fromIntegral (parKeyBytes params))
    <*> pure addrs
    <*> getEntropy (R.seedLength @drg)
    <*> pure params

newCmdId :: R.DRG' drg => State drg -> (ReqId, State drg)
newCmdId s@State {..} = (reqid, s { kRng = kRng' })
  where (reqid, kRng') = R.randomBytesGenerate reqIdWith kRng

newNodeIdR :: R.DRG' drg => Int -> State drg -> (NodeId, State drg)
newNodeIdR prefixMatching s0 = (nId, s1)
 where
  (rId, s1) = newCmdId s0
  mkMask p = replicate a maxBound <> m <> replicate b 0   where
    a = p `div` 8
    r = (parKeyBits (kParams s0) - p) `rem` 8
    m = [ shiftL maxBound r | r /= 0 ]
    b = (parKeyBits (kParams s0) - p) `div` 8
  bs x b1 b2 = BS.pack $ BS.zipWith x b1 b2
  -- p 1s then 0s
  maskL = mkMask prefixMatching
  -- p+1 0s then 1s
  maskR = fmap complement $ mkMask $ succ prefixMatching
  -- all 1s with the (p+1)th bit 0
  maskM = zipWith xor maskL maskR
  -- first p bits of kSelf
  valL  = bs (.&.) (kSelf s0) (BS.pack maskL)
  -- last (H-p-1) bits of rId
  valR  = bs (.&.) rId (BS.pack maskR)
  -- all 0s with the (p+1)th bit of kSelf complemented
  valS  = bs ((.&.) `on` complement) (kSelf s0) (BS.pack maskM)
  nId   = bs (.|.) valS $ bs (.|.) valL valR

-- | Get the next-furthest non-empty bucket, from the given bucket.
kBucketGetNextIndex :: Int -> State drg -> Maybe Int
kBucketGetNextIndex idx s0@State {..} =
  let nonempty bkt = S.lenBSeq (bEntries bkt) > 0
  in  fmap (idx +) $ V.findIndex nonempty $ V.drop idx kBuckets

-- | If Nothing, means refNode == kSelf
kBucketGetIndex :: NodeId -> State drg -> Maybe Int
kBucketGetIndex refNode s0@State {..} =
  let d = parKeyBits kParams
        - distanceLeadingZeros (distance refNode (kSelf s0))
  in  if d == 0 then Nothing else Just (pred d)

kBucketIndexToPrefix :: Int -> State drg -> Int
kBucketIndexToPrefix idx s0@State {..} = parKeyBits kParams - succ idx

-- | If result is Nothing, means refNode == kSelf
kBucketModifyAtNode
  :: NodeId -> (KBucket -> (a, KBucket)) -> State drg -> (Maybe a, State drg)
kBucketModifyAtNode refNode f s0 = case kBucketGetIndex refNode s0 of
  Nothing -> (Nothing, s0)
  Just distancePrefix ->
    let (res', kBuckets') = kBuckets & unsafeIx distancePrefix %%~ f
    in  (Just res', s0 { kBuckets = kBuckets' })
  where State {..} = s0

ownNodeInfo :: State drg -> NodeInfo NodeAddr
ownNodeInfo State {..} = nodeInfoOf kOwnInfo

kSelf :: State drg -> NodeId
kSelf = niNodeId . ownNodeInfo

kGetNodeInfo :: NodeId -> State drg -> Maybe NodeLocalInfo
kGetNodeInfo nodeId s0 = case kBucketGetIndex nodeId s0 of
  Nothing -> Just kOwnInfo
  Just distancePrefix ->
    let KBucket {..} = kBuckets ^?! ix distancePrefix
        nodeMatch    = (==) nodeId . niNodeId . getNodeInfo
    in  fmap Z.fst . fst $ S.bStatePL (nodeMatch . Z.fst) id dupe bEntries
  where State {..} = s0

-- | Return the k closest nodes we know about, from a given key.
-- Results are sorted by distance to the given refNode, closest first.
kGetNodes :: NodeId -> State drg -> KSeq (NodeInfo NodeAddr)
kGetNodes refNode s0 = do
  foldl' f base [start .. pred parKeyBits']
 where
  State {..}    = s0
  KParams {..}  = kParams
  parKeyBits'   = parKeyBits kParams
  (start, base) = case kBucketGetIndex refNode s0 of
    Nothing             -> (0, mempty)
    -- when joining the network we look up our own id and so it's important we
    -- don't return ourself immediately here because that would exit-early the
    -- lookup attempt with "success", e.g. we can't do:
    -- (0, S.BSeq $ S.singleton $ ownNodeInfo s0)
    Just distancePrefix -> (distancePrefix, mempty)
  f accum i = do
    let bkt  = kBuckets ^?! ix i
        want = fromIntegral parRepRouting - S.lenBSeq accum
    if want <= 0 || null (bEntries bkt)
      then accum
      else do
        let res = kBucketEntriesByDistance refNode bkt
        accum <> S.bTake want res
