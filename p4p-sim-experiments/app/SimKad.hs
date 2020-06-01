{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- external
import qualified Data.Map.Strict             as M
import qualified Data.Sequence.Extra         as Seq
import qualified Data.Set                    as S

import           Control.Concurrent.Async    (async)
import           Control.Concurrent.STM      (atomically, retry)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO,
                                              readTVar, readTVarIO)
import           Data.Foldable               (for_)
import           Data.Map.Strict             (Map)
import           Data.Maybe                  (fromJust)
import           System.Environment          (getArgs)
import           System.Exit                 (exitWith)
import           System.IO                   (stdin)

-- external, p4p
import           P4P.Protocol.DHT.Kademlia
import           P4P.Sim
import           P4P.Sim.IO                  (combineSimUserIO, hGetLineOrEOF,
                                              tbQueueSimUserIO)
import           P4P.Sim.Util                (ChaChaDRGInsecure, PMut', Pid,
                                              getEntropy, mkInitPids)


type KProc = PMut' (KState ChaChaDRGInsecure)

mkAddr :: Pid -> String
mkAddr p = "addr:" <> show p

mkPState
  :: SimOptions -> TVar (Map Pid NodeId) -> Pid -> IO (KState ChaChaDRGInsecure)
mkPState opt nodes p = do
  let params = defaultParams $ fromIntegral $ 1000 `div` simMsTick opt
      addr   = mkAddr p
  s <- newRandomState @ChaChaDRGInsecure getEntropy [addr] params
  atomically $ modifyTVar' nodes $ M.insert p (kSelf s)
  pure s

sendCommand
  :: (Maybe (SimUserI Pid (KState ChaChaDRGInsecure)) -> IO ())
  -> Pid
  -> CommandBody
  -> IO ()
sendCommand wi pid cmd = do
  cId <- getEntropy 32 -- FIXME: where should this magic number go?
  wi $ Just $ SimProcUserI pid $ Command cId cmd

main :: IO ()
main = do
  let parser = parseArgsIO
        $ parserInfo "sim-kad" "kademlia test" (simOptions (pure ProtoKad))
  opt <- parser =<< getArgs
  let initPids = mkInitPids opt
  nodes                  <- newTVarIO mempty
  ((tbWI, _), (tbRI, _)) <- tbQueueSimUserIO @_ @(KState ChaChaDRGInsecure)
  (simUserIO, close    ) <- combineSimUserIO
    [(tbRI, const (pure ())), defaultSimUserIO @Pid (hGetLineOrEOF stdin)]

  -- WIP: obviously, do some less trivial stuff
  a <- async $ do
    atomically $ do
      s <- M.size <$> readTVar nodes
      if s < S.size initPids then retry else pure ()
    for_ (zip (S.toList initPids) (drop 1 $ S.toList initPids)) $ \(i, j) -> do
      nId <- fromJust . M.lookup i <$> readTVarIO nodes
      sendCommand tbWI j $ do
        JoinNetwork (NodeInfo nId (Seq.bFromList [mkAddr i]))
  -- FIXME: check we get the right number of replies

  runSimIO @_ @KProc opt initPids (mkPState opt nodes) simUserIO
    >>= handleSimResult
    >>= exitWith
