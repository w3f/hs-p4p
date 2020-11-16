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
import           Control.Monad.Extra            (untilJustM)
import           Data.Foldable                  (traverse_)
import           Data.Kind                      (Constraint, Type)
import           Data.List.NonEmpty             (NonEmpty)
import           P4P.Proc                       (GMsg (..), GMsgO, PMsgI, PMsgO,
                                                 ProcEnv (..), ProcIface (..),
                                                 Process (..), Tick,
                                                 UProtocol (..))
import           Text.Read                      (readEither)

-- external, IO
import           Control.Concurrent.Async       (race)
import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, readTBQueue,
                                                 writeTBQueue)
import           Control.Concurrent.STM.TVar    (newTVarIO, readTVarIO,
                                                 writeTVar)
import           Crypto.Random.Entropy          (getEntropy)
import           Crypto.Random.Extra            (ScrubbedBytes)
import           P4P.RT

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
  => (  ProcEnv Tick (SimFullState ps xs) IO
     -> SimFullState ps xs
     -> IO (SimFullState ps xs)
     )
  -> SimOptions
  -> xs
  -> (Pid -> IO ps)
  -> Bool
  -> SimUserIO ps xs
  -> IO (Either (NonEmpty RTError) ())
grunSimIO runReact opt initXState mkPState isInteractive simUserIO =
  runProcIO @(SimFullState ps xs) runReact
                                  simRTOptions
                                  mkNewState
                                  getNow
                                  logFilter
                                  mkSimUserIO
 where
  SimOptions {..} = opt

  mkNewState      = do
    seed <- getEntropy @ScrubbedBytes 64
    let initPids = S.fromList [0 .. pred (fromIntegral simInitNodes)]
    states <- M.traverseWithKey (const . mkPState)
      $ M.fromSet (const ()) initPids
    pure $ SimFullState states
                        (newSimState seed simInitLatency initPids)
                        initXState

  getNow fs = simNow (simState fs)

  logFilter = case simLogging of
    LogAll            -> Just $ const True
    LogAllNoUser      -> Just $ logAllNoUser
    LogAllNoUserTicks -> Just $ logAllNoUserTicks @ps
    LogNone           -> Nothing
  -- inject initial SimResetAddrs when starting out, otherwise we have no addresses

  mkSimUserIO new = if not new
    then pure (isInteractive, simUserIO)
    else do
      let (simUserI, simUserO) = simUserIO
      injected <- newTVarIO False
      let simUserI' = do
            readTVarIO injected >>= \case
              True  -> simUserI
              False -> do
                atomically $ writeTVar injected True
                pure $ Just SimResetAddrs
      pure (isInteractive, (simUserI', simUserO))

runSimIO
  :: forall p
   . SimProcess p
  => Ctx p IO
  => SimLog (State p) ()
  => SimUserRe (State p) ()
  => SimReRe Serialise (State p) ()
  => SimOptions
  -> (Pid -> IO (State p))
  -> Bool
  -> SimUserIO (State p) ()
  -> IO (Either (NonEmpty RTError) ())
runSimIO opt =
  let run = if simDbgEmptySimX opt then runSimXS @p @() else runSim @p
  in  grunSimIO @(State p) run opt ()

-- | Equivalent to @(IO (Maybe (SimXHiI ps xs)), [SimXHiO ps xs] -> IO ())@.
type SimUserIO ps xs = RTHiIO (SimFullState ps xs)

defaultSimUserIO :: SimUserRe ps xs => StdIO -> SimUserIO ps xs
defaultSimUserIO (getInput, doOutput) =
  -- support special "pid :~ msg" syntax for SimProcUserI / SimProcUserO
  let i = untilJustM $ getInput >>= \case
        Nothing -> pure (Just Nothing) -- EOF, quit
        Just s  -> if null s
          then pure Nothing
          else case readEither s of
            Right (pid :~ ui) -> pure (Just (Just (SimProcHiI pid ui)))
            Left  _           -> case readEither s of
              Right r -> pure (Just (Just r))
              Left  e -> doOutput e >> pure Nothing
                -- TODO: add some help text, ideally with some introspection
                -- that prints out some generated concrete examples
      o = \case
        SimProcHiO pid uo -> doOutput $ show $ pid :~ uo
        x                 -> doOutput $ show x
  in  (i, traverse_ o)

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
