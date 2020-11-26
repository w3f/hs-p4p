{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module P4P.Sim.IO where

-- external, pure
import qualified Data.Map.Strict                as M
import qualified Data.Set                       as S

import           Codec.Serialise                (Serialise (..))
import           Control.Monad                  (when)
import           Data.Foldable                  (traverse_)
import           Data.Kind                      (Constraint, Type)
import           Data.List.NonEmpty             (NonEmpty)
import           Data.Void                      (absurd)
import           P4P.Proc                       (GMsg (..), ProcIO (..),
                                                 ProcIface (..), Process (..),
                                                 Protocol (..), Tick, UProtocol)
import           Text.Read                      (readEither)

-- external, IO
import           Control.Clock.IO               (voidInput)
import           Control.Concurrent.Async       (race)
import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, readTBQueue,
                                                 writeTBQueue)
import           Control.Concurrent.STM.TVar    (newTVarIO, readTVarIO,
                                                 writeTVar)
import           Crypto.Random.Entropy          (getEntropy)
import           Crypto.Random.Extra            (ScrubbedBytes)
import           P4P.RT                         hiding (RTLogging (..))

-- internal
import           P4P.Sim.Extension
import           P4P.Sim.Internal
import           P4P.Sim.Options                (SimInitOptions (..),
                                                 SimLogging (..),
                                                 SimOptions (..))
import           P4P.Sim.Types


type SimMsgRe (codec :: Type -> Constraint) ps
  = ( codec (EnvI ps)
    , codec (LoI ps)
    , codec (LoO ps)
    , codec (HiI ps)
    , codec (HiO ps)
    , codec (Addr ps)
    , codec ps -- since SimHiI/SimHiO contains ps
    )

type SimRe (codec :: Type -> Constraint) ps = SimMsgRe codec ps
-- if SimMsgRe did not already contain @codec ps@, this would.

type SimLog (codec :: Type -> Constraint) ps
  = (SimMsgRe codec ps, codec (EnvO ps), codec (AuxO ps))

type SimXMsgRe (codec :: Type -> Constraint) xs
  = (codec (XHiI xs), codec (XHiO xs))

type SimXRe (codec :: Type -> Constraint) xs = (codec xs, SimXMsgRe codec xs)

type SimXLog (codec :: Type -> Constraint) xs = SimXMsgRe codec xs
-- the extension doesn't have EnvO or AuxO

grunSimIO
  :: forall ps xs xo
   . Ord (Addr ps)
  => SimLog Show ps
  => SimXLog Show xs
  => SimRe Serialise ps
  => SimXRe Serialise xs
  => UProtocol ps
  => (  ProcIO (SimFullState ps xs) IO
     -> Tick
     -> SimFullState ps xs
     -> IO (SimFullState ps xs)
     )
  -> SimOptions xo
  -> IO xs
  -> (Pid -> IO ps)
  -> IO (SimUserIO ps xs, IO ())
  -> IO (Either (NonEmpty RTError) ())
grunSimIO runReact opt mkXState mkPState mkSimUserIO =
  runProcIO @(SimFullState ps xs)
    runReact
    simRTInitOptions
    simRTOptions
    mkNewState
    logFilter
    (\_ _ _ -> pure ((Just <$> voidInput, traverse_ absurd), pure ()))
    (\_ _ _ -> mkSimUserIO)
 where
  SimOptions {..}     = opt
  SimInitOptions {..} = rtInitOpts simRTInitOptions

  mkNewState          = do
    let initPids = S.fromList [0 .. pred (fromIntegral simInitNodes)]
    states <- M.traverseWithKey (const . mkPState)
      $ M.fromSet (const ()) initPids
    tick <- initializeTick simRTInitOptions
    seed <- getEntropy @ScrubbedBytes 64
    let simState = newSimState tick seed simInitLatency (getAddrs <$> states)
    SimFullState states simState <$> mkXState

  logFilter = case rtLogging simRTOptions of
    LogNone -> Nothing
    lv      -> Just (\msg -> fromEnum lv >= fromEnum (msgLv msg))

  msgLv :: LogEvt (SimXI ps xs) (SimXO ps xs) -> SimLogging
  msgLv = \case
    LogProcI (MsgEnv _)                          -> LogAuxHiProcEnvIO
    LogProcO (MsgAux (SimProcRecv _ (MsgEnv _))) -> LogAuxHiProcEnvIO

    LogProcO (MsgEnv _)                          -> LogAuxHiProcEnvO
    LogProcO (MsgAux (SimProcSend _ (MsgEnv _))) -> LogAuxHiProcEnvO

    LogProcO (MsgAux (SimProcRecv _ (MsgLo _)))  -> LogAuxHiProc
    LogProcO (MsgAux (SimProcSend _ (MsgLo _)))  -> LogAuxHiProc

    LogProcI (MsgHi _)                           -> LogAuxHi
    LogProcO (MsgHi _)                           -> LogAuxHi
    LogProcO (MsgAux (SimProcRecv _ (MsgHi _)))  -> LogAuxHi
    LogProcO (MsgAux (SimProcSend _ (MsgHi _)))  -> LogAuxHi

    LogProcO (MsgAux (SimProcSend _ (MsgAux _))) -> LogAux
    LogProcO (MsgAux _)                          -> LogAux
    LogLoAux _                                   -> LogAux
    LogHiAux _                                   -> LogAux

runSimIO
  :: forall p xo
   . SimProcess p
  => Ctx p IO
  => SimLog Show (State p)
  => SimRe Serialise (State p)
  => SimOptions xo
  -> (Pid -> IO (State p))
  -> IO (SimUserIO (State p) (), IO ())
  -> IO (Either (NonEmpty RTError) ())
runSimIO opt =
  let run = if simDbgEmptySimX opt then runSimXS @p @() else runSim @p
  in  grunSimIO @(State p) run opt (pure ())

-- | Equivalent to @(IO (Maybe (SimXHiI ps xs)), [SimXHiO ps xs] -> IO ())@.
type SimUserIO ps xs = RTHiIO (SimFullState ps xs)

-- | Convenience type alias for being able to type in and out a simulation.
type SimUserRe ps xs
  = ( Ord (Addr ps)
    , Read (HiI ps)
    , Show (HiO ps)
    , Read (XHiI xs)
    , Show (XHiO xs)
    -- the remaining are needed because we can input/output SimProcState,
    -- which contains input messages, address, and the process state
    , Show ps
    , Read ps
    , Show (EnvI ps)
    , Read (EnvI ps)
    , Show (HiI ps)
    -- , Read (HiI ps) -- already mentioned above, mentioned here for clarity
    , Show (LoI ps)
    , Read (LoI ps)
    , Show (Addr ps)
    , Read (Addr ps)
    )

defaultSimUserIO :: forall ps xs . SimUserRe ps xs => StdIO -> SimUserIO ps xs
defaultSimUserIO = defaultRTHiIO @(SimFullState ps xs) readCustom showCustom where
  -- support special "pid :~ msg" syntax for SimProcUserI / SimProcUserO
  readCustom s = pure $ case readEither s of
    Right (pid :~ ui) -> Right (SimProcHiI pid ui)
    Left  e           -> Left e
  showCustom = \case
    SimProcHiO pid uo -> pure $ Just $ show $ pid :~ uo
    _                 -> pure $ Nothing

{- | Convert a 'SimUserIO' to have auto-join and auto-quit behaviour.

By design, processes are meant to be suspended at any time, and so they have no
explicit awareness of, or any control over, when they are started or stopped.
This slight hack adds support for auto-join/quit, by having the sim extension
communicate this implicitly with the 'SimUserIO' that is driving it.
-}
hookAutoJoinQuit
  :: forall ps xs
   . Bool
  -> Bool
  -> XHiI xs
  -> (XHiO xs -> Bool)
  -> SimUserIO ps xs
  -> IO (SimUserIO ps xs)
hookAutoJoinQuit autoJoin autoQuit joinMsg isQuitMsg (ui, uo) = do
  if not autoJoin && not autoQuit
    then pure (ui, uo)
    else do
      started  <- newTVarIO False
      finished <- newTBQueueIO 1
      let ui' = readTVarIO started >>= \case
            False -> do
              atomically $ writeTVar started True
              pure (Just (SimExtensionI joinMsg))
            -- note: race in this context is typically unsafe, but since we're
            -- quitting the program on one of the branches it's ok here. see
            -- https://github.com/simonmar/async/issues/113 for details.
            -- we have a safe version in 'foreverInterleave' in p4p-common but
            -- that carries additional overhead.
            True -> do
              fmap (either id id) $ race ui $ do
                atomically (readTBQueue finished)
                pure Nothing
          isQuitMsg' = \case
            SimExtensionO m | isQuitMsg m -> True
            _                             -> False
          uo' outs = do
            uo outs
            when (autoQuit && any isQuitMsg' outs) $ do
              atomically $ writeTBQueue finished ()
      pure (ui', uo')
