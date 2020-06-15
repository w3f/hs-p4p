{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- external
import qualified Data.Map.Strict                  as M
import qualified Data.Sequence.Extra              as Seq
import qualified Data.Set                         as S

import           Control.Lens                     ((%%=), (%%~), (&), (.=))
import           Control.Lens.TH.Extra            (makeLenses_)
import           Control.Monad.Trans.Class        (MonadTrans (..))
import           Control.Monad.Trans.State        (StateT (..), get, runState)
import           Control.Monad.Trans.Writer.CPS   (WriterT, execWriterT)
import           Control.Monad.Trans.Writer.Extra (tell1)
import           Control.Op
import           Crypto.Random.Extra              (initializeFrom,
                                                   randomBytesGenerate)
import           Data.Foldable                    (for_)
import           Data.Map.Strict                  (Map)
import           Data.Maybe                       (fromJust)
import           GHC.Generics                     (Generic)

-- external, impure
import           System.Environment               (getArgs)
import           System.Exit                      (exitWith)

-- external, p4p
import           P4P.Proc
import           P4P.Protocol.DHT.Kademlia
import           P4P.Sim
import           P4P.Sim.Options                  (SimIAction (..),
                                                   SimIOAction (..),
                                                   delayedInitMode,
                                                   _simIOAction)
import           P4P.Sim.Util                     (ChaChaDRGInsecure, PMut',
                                                   getEntropy)
import           P4P.Sim.Util.IO                  (bracketHEF, hookAutoJoinQuit,
                                                   optionTerminalStdIO)


type KS = KState ChaChaDRGInsecure
type KProc = PMut' KS

mkAddr :: Pid -> String
mkAddr p = "addr:" <> show p

mkPState :: KParams -> Pid -> IO KS
mkPState params p =
  let addr = mkAddr p
  in  newRandomState @ChaChaDRGInsecure getEntropy [addr] params

data KSimState = KSimState
  { ksDRG     :: !ChaChaDRGInsecure
  , ksJoining :: !(Maybe (Either (Map Pid (Maybe NodeId)) (Map Pid NodeId)))
  }
  deriving (Show, Read, Generic, Eq)
makeLenses_ ''KSimState

data KSimI = KSimJoinAll
  deriving (Show, Read, Generic, Eq, Ord)
makeLenses_ ''KSimI

data KSimO = KSimJoinStarted | KSimJoinFinished
  deriving (Show, Read, Generic, Eq, Ord)
makeLenses_ ''KSimO

instance Protocol KSimState where
  type PMsg KSimState = Void
  type UserI KSimState = Either (SimUserO KS) KSimI
  type UserO KSimState = Either (SimUserI KS) KSimO
  type AuxO KSimState = Void

instance SimXProtocol KS KSimState where
  type XUserI KSimState = KSimI
  type XUserO KSimState = KSimO

instance Proc KSimState where
  getAddrs = const []
  localNow = const 0
  react i = runState $ case i of
    MsgUser ui -> (MsgUser <$>) <$> kSim ui
    _          -> pure []

sendCommand
  :: Monad m
  => Pid
  -> CommandBody
  -> WriterT [Either (SimUserI KS) KSimO] (StateT KSimState m) ()
sendCommand pid cmd = do
  cId <- _ksDRG %%= randomBytesGenerate reqIdWith
  tell1 $ Left $ SimProcUserI pid $ Command cId cmd

kSim
  :: Monad m
  => Either (SimUserO KS) KSimI
  -> StateT KSimState m [Either (SimUserI KS) KSimO]
kSim input = execWriterT $ do
  ks <- lift get
  case (ksJoining ks, input) of
    (Nothing, Right KSimJoinAll) -> do
      -- get all pids
      tell1 $ Left SimGetAllPids
      _ksJoining .= Just (Left mempty)
    (Just (Left ns), Left (SimAllPids pids)) | null ns -> do
      -- get all node ids
      for_ pids $ \pid -> do
        sendCommand pid GetNodeId
      _ksJoining .= Just (Left (M.fromSet (const Nothing) pids))
    (Just (Left ns), Left (SimProcUserO pid (CommandReply _ (Right (OwnNodeId nId)))))
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
            tell1 $ Right KSimJoinStarted
            -- TODO: check we get the right number of replies later, and output
            -- KSimJoinFinished
            pure (Right nIds)
        _ksJoining .= Just r
    _ -> pure ()
      -- TODO: WIP: more stuff, like random lookups & inserts

kadOptions :: Parser (Int -> KParams)
kadOptions =
  mkParams
    <$> (  option auto
        <| long "x-kad-key-bytes"
        <> help "Number of bytes of a key, e.g. 32 for 256 bits."
        <> metavar "NUM"
        <> value 32
        <> showDefault
        )
    <*> (  option auto
        <| long "x-kad-rep-routing"
        <> help "Routing replication factor. i.e. Max-size of a k-bucket."
        <> metavar "NUM"
        <> value 32
        <> showDefault
        )
    <*> (  option auto
        <| long "x-kad-rep-storage"
        <> help "Storage replication factor. TODO: not yet implemented."
        <> metavar "NUM"
        <> value 16
        <> showDefault
        )
    <*> (  option auto
        <| long "x-kad-parallel"
        <> help "Max number of outstanding outgoing requests per query."
        <> metavar "NUM"
        <> value 8
        <> showDefault
        )
    <*> (  option auto
        <| long "x-kad-addrs-per-node"
        <> help "Max number of addresses to store for a node"
        <> metavar "NUM"
        <> value 16
        <> showDefault
        )
    <*> (  option auto
        <| long "x-kad-speed-auto"
        <> help "Speed up automatic behaviours e.g. refresh, for testing"
        <> metavar "NUM"
        <> value 1
        <> showDefault
        )
 where
  mkParams kb rr rs p apn t i = (testingParams t i) { parKeyBytes     = kb
                                                    , parRepRouting   = rr
                                                    , parRepStorage   = rs
                                                    , parParallel     = p
                                                    , parAddrsPerNode = apn
                                                    }

main :: IO ()
main = do
  let parser = mkParser "sim-kad" "kademlia test" (simXOptions kadOptions)
  SimXOptions {..} <- parseArgsIO parser =<< getArgs

  -- auto-join if we're
  --   - not reading input state
  let autoJoin = case simIActRead (simIState (simIOAction simOpts)) of
        Nothing -> True
        Just _  -> False
  -- auto-quit if we're perform an init-mode action. in this case, we will
  -- auto-join (i.e. send some messages), then write the output state to the
  -- specified input-state-file (if any), then quit
  let (autoQuit, simOpts') = simOpts & _simIOAction %%~ delayedInitMode

  let mkStdIO =
        optionTerminalStdIO simOpts "p4p" ".sim-kad_history" "p4p Kad> "
  bracketHEF mkStdIO $ \stdio -> do
    let joinStarted = \case
          KSimJoinStarted -> True
          _               -> False
    simUserIO <-
      hookAutoJoinQuit @_ @KSimState autoJoin autoQuit KSimJoinAll joinStarted
        $ defaultSimUserIO @KS @KSimState stdio

    drg <- initializeFrom getEntropy
    let initXState = KSimState drg Nothing
    let params = simXOpts $ fromIntegral $ 1000 `div` simMsTick simOpts
    grunSimIO @KProc @KSimState (runSimXS @KProc @KSimState)
                                simOpts'
                                initXState
                                (mkPState params)
                                simUserIO
      >>= handleSimResult
      >>= exitWith
