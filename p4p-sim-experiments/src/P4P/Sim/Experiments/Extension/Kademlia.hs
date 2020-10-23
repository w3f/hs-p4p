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
import           Crypto.Random.Extra              (randomBytesGenerate)
import           Data.Binary                      (Binary)
import           Data.Foldable                    (for_)
import           Data.Map.Strict                  (Map)
import           Data.Maybe                       (fromJust)
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
