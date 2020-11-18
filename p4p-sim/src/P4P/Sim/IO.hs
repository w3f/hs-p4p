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
import           P4P.Proc                       (GMsg (..), GMsgO, PMsgI, PMsgO,
                                                 ProcEnv (..), ProcIface (..),
                                                 Process (..), Tick,
                                                 UProtocol (..))
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
import           P4P.Sim.Options                (SimLogging (..),
                                                 SimOptions (..))
import           P4P.Sim.Types


-- | Convenience type alias for being able to type in and out a simulation.
type SimUserRe ps xs
  = ( Show ps
    , Read ps
    , Show (EnvI ps)
    , Read (EnvI ps)
    , Read (LoI ps)
    , Read (HiI ps)
    , Show (HiO ps)
    , Show (LoI ps)     -- as part of HiO we can dump the state, which contains LoI messages
    , Show (HiI ps)     -- as part of HiO we can dump the state, which contains HiI messages
    , Show (Addr ps)
    , Read (Addr ps)
    , Ord (Addr ps)
    , Show (Msg ps)
    , Read (Msg ps)
    , Read (XHiI xs)
    , Show (XHiO xs)
    )

type SimReReP (codec :: Type -> Constraint) ps
  = ( codec ps
    , codec (EnvI ps)
    , codec (LoI ps)
    , codec (LoO ps)
    , codec (HiI ps)
    , codec (HiO ps)
    , codec (Addr ps)
    , codec (Msg ps)
    )

type SimReReX (codec :: Type -> Constraint) xs
  = (codec xs, codec (XHiI xs), codec (XHiO xs))

-- | Convenience type alias for being able to record and replay a simulation.
type SimReRe (codec :: Type -> Constraint) ps xs
  = (SimReReP codec ps, SimReReX codec xs)

type SimLog ps xs
  = ( Show ps
    , Show (Addr ps)
    , Show (PMsgI ps)
    , Show (PMsgO ps)
    , Show (HiO ps)
    , Show (AuxO ps)
    , Show xs
    , Show (XHiI xs)
    , Show (XHiO xs)
    )

logAllNoUser :: GMsgO e l h -> Bool
logAllNoUser = \case
  MsgHi _ -> False
  _       -> True

logAllNoUserTicks :: GMsgO (SimAuxO ps) l h -> Bool
logAllNoUserTicks = \case
  MsgHi  _ -> False
  MsgEnv (SimProcEvent (SimMsgRecv _ (MsgEnv _))) -> False
  _        -> True

grunSimIO
  :: forall ps xs
   . SimLog ps xs
  => SimUserRe ps xs
  => SimReRe Serialise ps xs
  => UProtocol ps
  => (  ProcEnv Tick (SimFullState ps xs) IO
     -> SimFullState ps xs
     -> IO (SimFullState ps xs)
     )
  -> SimOptions
  -> IO xs
  -> (Pid -> IO ps)
  -> IO (SimUserIO ps xs, IO ())
  -> IO (Either (NonEmpty RTError) ())
grunSimIO runReact opt mkXState mkPState mkSimUserIO =
  runProcIO @(SimFullState ps xs)
    runReact
    simRTOptions
    mkNewState
    logFilter
    (\_ _ -> pure ((Just <$> voidInput, traverse_ absurd), pure ()))
    (\_ _ -> mkSimUserIO)
 where
  SimOptions {..} = opt

  mkNewState      = do
    seed <- getEntropy @ScrubbedBytes 64
    let initPids = S.fromList [0 .. pred (fromIntegral simInitNodes)]
    states <- M.traverseWithKey (const . mkPState)
      $ M.fromSet (const ()) initPids
    SimFullState states (newSimState seed simInitLatency (getAddrs <$> states))
      <$> mkXState

  logFilter = case rtLogging simRTOptions of
    LogAll            -> Just $ const True
    LogAllNoUser      -> Just $ logAllNoUser
    LogAllNoUserTicks -> Just $ logAllNoUserTicks @ps
    LogNone           -> Nothing

runSimIO
  :: forall p
   . SimProcess p
  => Ctx p IO
  => SimLog (State p) ()
  => SimUserRe (State p) ()
  => SimReRe Serialise (State p) ()
  => SimOptions
  -> (Pid -> IO (State p))
  -> IO (SimUserIO (State p) (), IO ())
  -> IO (Either (NonEmpty RTError) ())
runSimIO opt =
  let run = if simDbgEmptySimX opt then runSimXS @p @() else runSim @p
  in  grunSimIO @(State p) run opt (pure ())

-- | Equivalent to @(IO (Maybe (SimXHiI ps xs)), [SimXHiO ps xs] -> IO ())@.
type SimUserIO ps xs = RTHiIO (SimFullState ps xs)

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
