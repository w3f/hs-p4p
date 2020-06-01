{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- external
import qualified Data.Map.Strict                as M
import qualified Data.Sequence.Extra            as Seq
import qualified Data.Set                       as S

import           Control.Applicative            ((<|>))
import           Control.Concurrent.Async       (async)
import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, readTBQueue,
                                                 writeTBQueue)
import           Control.Lens                   ((%%=), (%%~), (%=), (&), (.=))
import           Control.Lens.TH.Extra          (makeLenses_)
import           Control.Monad                  (forever, unless, when)
import           Control.Monad.Trans.Class      (MonadTrans (..))
import           Control.Monad.Trans.State      (StateT (..), get)
import           Data.Either                    (isLeft)
import           Data.Foldable                  (for_)
import           Data.Map.Strict                (Map)
import           Data.Maybe                     (fromJust)
import           Data.Sequence                  (Seq)
import           GHC.Generics                   (Generic)
import           System.Environment             (getArgs)
import           System.Exit                    (exitWith)
import           System.IO                      (stdin)

-- external, p4p
import           P4P.Proc.Util.FlowControl
import           P4P.Protocol.DHT.Kademlia
import           P4P.Sim
import           P4P.Sim.IO                     (combineSimUserIO,
                                                 hGetLineOrEOF,
                                                 tbQueueSimUserIO')
import           P4P.Sim.Options
import           P4P.Sim.Util                   (ChaChaDRGInsecure, PMut', Pid,
                                                 getEntropy, mkInitPids)


type KS = KState ChaChaDRGInsecure
type KProc = PMut' KS

mkAddr :: Pid -> String
mkAddr p = "addr:" <> show p

mkPState :: SimOptions -> Pid -> IO KS
mkPState opt p = do
  let params = defaultParams $ fromIntegral $ 1000 `div` simMsTick opt
      addr   = mkAddr p
  newRandomState @ChaChaDRGInsecure getEntropy [addr] params

data KSimState = KSimState
  { ksQueue   :: !(Seq (Maybe (SimUserI Pid KS)))
    -- ^ output queue, for flow control
  , ksJoining :: !(Maybe (Either (Map Pid (Maybe NodeId)) (Map Pid NodeId)))
  }
  deriving (Show, Read, Generic, Eq)
makeLenses_ ''KSimState

data KSimIn = KSimJoinAll
  deriving (Show, Read, Generic, Eq, Ord)
makeLenses_ ''KSimIn

sendCommand :: Pid -> CommandBody -> StateT KSimState IO ()
sendCommand pid cmd = do
  cId <- lift $ getEntropy 32 -- FIXME: where should this magic number go?
  _ksQueue %= pushQ (Just $ SimProcUserI pid $ Command cId cmd)

-- TODO: this doesn't interact with replay very well - once we suspend and
-- resume, KSimState is lost...
kSim :: Bool -> Either KSimIn (SimUserO Pid KS) -> StateT KSimState IO ()
kSim autoQuit input = do
  ks <- get
  case (ks, input) of
    (KSimState _ Nothing, Left KSimJoinAll) -> do
      -- get all pids
      _ksQueue %= pushQ (Just $ SimGetAllPids)
      _ksJoining .= Just (Left mempty)
    (KSimState _ (Just (Left ns)), Right (SimAllPids pids)) | null ns -> do
      -- get all node ids
      for_ pids $ \pid -> do
        sendCommand pid $ GetNodeId
      _ksJoining .= Just (Left (M.fromSet (const Nothing) pids))
    (KSimState _ (Just (Left ns)), Right (SimProcUserO pid (CommandReply _ (Right (OwnNodeId nId)))))
      | M.member pid ns
      -> do
      -- receive node ids
        let ns' = M.insert pid (Just nId) ns
        r <- if elem Nothing $ snd <$> M.toList ns'
          then pure (Left ns')
          else do
            let nIds = fmap fromJust ns'
            let pids = M.keysSet nIds
            -- send JoinNetwork to everyone
            for_ (zip (S.toList pids) (drop 1 $ S.toList pids)) $ \(i, j) -> do
              let nId' = fromJust $ M.lookup i nIds
              sendCommand j $ do
                JoinNetwork (NodeInfo nId' (Seq.bFromList [mkAddr i]))
            when autoQuit $ do
              _ksQueue %= pushQ Nothing -- EOF signal to sim
            pure (Right nIds)
        _ksJoining .= Just r
    _ -> pure ()
      -- TODO: WIP: more stuff, like random lookups & inserts

main :: IO ()
main = do
  let parser =
        parserInfo "sim-kad" "kademlia test" (simOptions (pure ProtoKad))
  opt                      <- parseArgsIO parser =<< getArgs
  ((tbWI, tbRO), tbUserIO) <- tbQueueSimUserIO' @_ @KS
  -- TODO: intercept user commands that parse into KSimIn and run them
  (simUserIO   , close   ) <- combineSimUserIO
    [tbUserIO, defaultSimUserIO @Pid (hGetLineOrEOF stdin)]
  simCtl <- newTBQueueIO 1

  -- auto-join if we're
  --   - not reading input state
  let autoJoin = case simIActRead (simIState (simIOAction opt)) of
        Nothing -> True
        Just _  -> False

  -- auto-quit if we're perform an init-mode action. in this case, we will
  -- auto-join (i.e. send some messages), then write the output state to the
  -- specified input-state-file (if any), then quit
  let (autoQuit, opt') = opt & _simIOAction %%~ delayedInitMode

  ar <- async $ flip runStateT (KSimState mempty Nothing) $ forever $ do
    inputs <-
      lift
      $   atomically
      $   fmap (pure . Left) (readTBQueue simCtl)
      <|> fmap (fmap Right)  tbRO
    --lift $ print inputs
    for_ inputs $ kSim autoQuit
    -- if we got an output from the sim (i.e. own input /= [Left _])
    -- then flow control can unblock, i.e. we can now send the sim one input
    unless (any isLeft inputs) $ do
      _ksQueue %%= maybePopQ >>= \case
        Just out -> lift $ atomically $ tbWI out
        Nothing  -> pure ()
  aw <- async $ when autoJoin $ do
    atomically $ writeTBQueue simCtl KSimJoinAll
{-
sim-kad: Safe.fromJustNote Nothing, insertNodeIdTOReqPing did not find pending node
CallStack (from HasCallStack):
  fromJustNote, called at src/P4P/Protocol/DHT/Kademlia/Internal.hs:286:20 in p4p-protocol-dht-kad-0.0-inplace:P4P.Protocol.DHT.Kademlia.Internal
1
-- probably we didn't cancel a timeout when evicting a node
-}
  -- FIXME: check we get the right number of replies
  runSimIO @_ @KProc opt' (mkInitPids opt') (mkPState opt') simUserIO
    >>= handleSimResult
    >>= exitWith
