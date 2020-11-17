{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module P4P.Sim.Experiments.Extension.Kademlia where

-- external
import qualified Data.ByteString.Char8            as BS
import qualified Data.Map.Strict                  as M
import qualified Data.Sequence.Extra              as Seq
import qualified Data.Set                         as S

import           Codec.Serialise                  (Serialise)
import           Control.Lens                     ((%%=), (.=))
import           Control.Lens.TH.Extra            (makeLenses_)
import           Control.Monad.Trans.Class        (MonadTrans (..))
import           Control.Monad.Trans.State        (StateT (..), get, runState)
import           Control.Monad.Trans.Writer.CPS   (WriterT, execWriterT)
import           Control.Monad.Trans.Writer.Extra (tell1)
import           Control.Op
import           Crypto.Random.Extra              (randomBytesGenerate)
import           Data.Binary                      (Binary)
import           Data.Foldable                    (for_)
import           Data.Map.Strict                  (Map)
import           Data.Maybe                       (fromJust)
import           Data.Void                        (Void)
import           GHC.Generics                     (Generic)

-- external, p4p
import           P4P.Proc
import           P4P.Protocol.DHT.Kademlia
import           P4P.Sim
import           P4P.Sim.Util                     (ChaChaDRGInsecure)


mkAddr :: Pid -> NodeAddr
mkAddr p = BS.pack $ "addr:" <> show p

type KS = KState ChaChaDRGInsecure

data KSimState = KSimState
  { ksDRG     :: !ChaChaDRGInsecure
  , ksJoining :: !(Maybe (Either (Map Pid (Maybe NodeId)) (Map Pid NodeId)))
  }
  deriving (Show, Read, Generic, Binary, Serialise, Eq)
makeLenses_ ''KSimState

data KSimI = KSimJoinAll
  deriving (Show, Read, Generic, Binary, Serialise, Eq, Ord)
makeLenses_ ''KSimI

data KSimO = KSimJoinStarted | KSimJoinFinished
  deriving (Show, Read, Generic, Binary, Serialise, Eq, Ord)
makeLenses_ ''KSimO

instance ProcIface KSimState where
  type LoI KSimState = Void
  type LoO KSimState = Void
  type HiI KSimState = Either (SimHiO KS) KSimI
  type HiO KSimState = Either (SimHiI KS) KSimO

instance SimXProcIface KS KSimState where
  type XHiI KSimState = KSimI
  type XHiO KSimState = KSimO

instance Proc KSimState where
  react i = runState $ case i of
    MsgHi ui -> (MsgHi <$>) <$> kSim ui
    _        -> pure []

sendCommand
  :: Monad m
  => Pid
  -> CommandBody
  -> WriterT [Either (SimHiI KS) KSimO] (StateT KSimState m) ()
sendCommand pid cmd = do
  cId <- _ksDRG %%= randomBytesGenerate reqIdWidth
  tell1 $ Left $ SimProcHiI pid $ Command cId cmd

kSim
  :: Monad m
  => Either (SimHiO KS) KSimI
  -> StateT KSimState m [Either (SimHiI KS) KSimO]
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
    (Just (Left ns), Left (SimProcHiO pid (CommandReply _ (Right (OwnNodeId nId)))))
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
