{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module P4P.Sim.Internal where

-- external
import qualified Data.Map.Strict                  as M
import qualified Data.Sequence                    as SQ
import qualified Data.Set                         as S

import           Control.Lens                     (Iso', anon, at, contains,
                                                   itraversed, use, (%%=),
                                                   (%%@=), (%=), (.=), _1, _2)
import           Control.Monad                    (void, when)
import           Control.Monad.Trans.Class        (MonadTrans (..))
import           Control.Monad.Trans.State.Strict (StateT (..), execStateT,
                                                   runState, state)
import           Control.Op
import           Crypto.Random.Extra              (ChaChaDRGInsecure)
import           Data.Bool                        (bool)
import           Data.Foldable                    (for_, toList)
import           Data.List.NonEmpty               (NonEmpty (..))
import           Data.Map.Strict                  (Map)
import           Data.Maybe                       (catMaybes)
import           Data.Schedule                    (Tick, TickDelta, whileJustM)
import           Data.Traversable                 (for)
import           P4P.Proc                         (GMsg (..), ProcAddr,
                                                   ProcMsgI, Process (..),
                                                   ProtoMsg (..), RuntimeI (..),
                                                   RuntimeO (..))
import           Safe                             (headNote)

-- internal
import           P4P.Sim.Types


-- convenience wrapper around 'anon', see its documentation for details
nom :: (Monoid (f a), Foldable f) => Iso' (Maybe (f a)) (f a)
nom = anon mempty null

int :: (Integral a, Num b) => a -> b
int = fromIntegral


proceedAll
  :: (Process p, Applicative m, Ctx p m) => Map pid (State p) -> m (Map pid p)
proceedAll = traverse proceed

suspendAll
  :: (Process p, Applicative m, Ctx p m) => Map pid p -> m (Map pid (State p))
suspendAll = traverse suspend

-- | Given a set of source addresses and a target address, select a single
-- source address and a simulated latency for sending.
sampleLatency
  :: [addr]
  -> addr
  -> SimLatency
  -> ChaChaDRGInsecure
  -> ((TickDelta, addr), ChaChaDRGInsecure)
sampleLatency s t = \case
  SLatAddrIndep dist -> sample dist >$> (, s') |> runState
 where
  s'     = headNote "simSend got empty addresses" s
  sample = \case
    DistConstant k            -> pure k
    DistLogNormal mean stddev -> error "not implemented"
    DistWeibull   mean stddev -> error "not implemented"
      -- see https://stats.stackexchange.com/a/159522

-- | Execution runtime for the simulation.
data SimRT pid ps m = SimRT {
    simClose  :: !(m ())
  , simStatus :: !(m (Either (NonEmpty SimError) ()))
  , simError  :: !(SimError -> m ())
  , simRunI   :: !(m (Maybe (SimI pid ps)))
  , simRunO   :: !((Tick, SimO pid ps) -> m ())
  }

-- TODO: SimT should be a newtype, so it doesn't clash with MonadState
type SimT pid p = StateT (Map pid p, SimState pid (State p))

type Sim pid p m = (Ord pid, Ord (ProcAddr p), Process p, Ctx p m, Monad m)

simulate
  :: forall pid p m . Sim pid p m => SimRT pid (State p) m -> SimT pid p m ()
simulate simRT = void $ whileJustM $ do
  realNow <- use $ _2 . _simNow
  procs   <- use _1
  let logU = lift . simRunO . (realNow, ) . MsgUser

  -- run messages up-to-and-including current tick
  _ <- whileJustM $ do
    -- keep running until all inboxes are empty
    M.traverseWithKey (runInbox realNow) procs
      >$> M.mapMaybe id
      >$> M.null
      >$> bool (Just ()) Nothing

  -- wait for next external input, either user or runtime
  lift simRunI >>= \case
    Nothing                         -> pure Nothing -- eof
    Just (MsgRT (SimRTTick newNow)) -> do
      -- TODO: assert newNow > realNow
      void $ M.traverseWithKey (runTick newNow) procs
      _2 . _simNow .= newNow
      pure (Just ())
    Just (MsgUser (SimProcAdd pid sp)) -> do
      -- insert process
      let SimProcState {..} = sp
      if M.member pid procs
        then logU $ SimProcAddResult pid False
        else do
          proc <- lift $ proceed @p spState
          _1 . at pid .= Just proc
          _2 . _simIn . at pid .= Just spInbox
          for_ spAddr $ \a -> do
            _2 . _simAddr . at a . nom . contains pid .= True
          logU $ SimProcAddResult pid True
      pure (Just ())
    Just (MsgUser (SimProcDel pid)) -> do
      -- delete process
      case M.lookup pid procs of
        Nothing   -> logU $ SimProcDelResult pid Nothing
        Just proc -> do
          spState <- lift $ suspend @p proc
          _1 . at pid .= Nothing
          spInbox <- _2 . _simIn . at pid . nom %%= (, mempty)
          spAddr  <- _2 . _simAddr . itraversed %%@= \addr pids -> do
            if S.member pid pids
              then (S.singleton addr, S.delete pid pids)
              else (S.empty, pids)
          let sp = SimProcState { .. }
          logU $ SimProcDelResult pid $ Just sp
      pure (Just ())
    Just (MsgUser (SimProcUserI pid userI)) -> do
      -- user message to a process
      case M.lookup pid procs of
        Nothing -> do
          lift $ simRunO $ (realNow, MsgProc $ SimNoSuchPid (Left ()) pid)
        Just _ -> do
          pushPMsgI realNow pid (MsgUser userI)
      pure (Just ())
 where
  SimRT {..} = simRT

  -- pop a single inbox message up-to-and-including @realNow@
  popPMsgI :: Tick -> pid -> SimT pid p m (Maybe (ProcMsgI p))
  popPMsgI realNow pid = _2 . _simIn . at pid . nom %%= \inboxes ->
    case M.minViewWithKey inboxes of
      Just ((t, ibx), remain) | t <= realNow -> case ibx of
        msgI SQ.:<| rest ->
          (Just msgI, if null rest then remain else M.insert t rest remain)
        SQ.Empty ->
          -- "if null rest" above should prevent this from being reached
          error "got empty Seq despite guard-logic"
      _ -> (Nothing, inboxes) -- empty inboxes or min tick > now

  -- push a single inbox message for @future@
  pushPMsgI :: Tick -> pid -> ProcMsgI p -> SimT pid p m ()
  pushPMsgI future pid msgI =
    _2 . _simIn . at pid . nom . at future . nom %= (SQ.|> msgI)

  -- map (or unmap) an address to a pid
  mapAddress :: ProcAddr p -> pid -> Bool -> SimT pid p m ()
  mapAddress addr pid b = _2 . _simAddr . at addr . nom . contains pid .= b

  -- run the next message from the inbox, if available
  runInbox :: Tick -> pid -> p -> SimT pid p m (Maybe p)
  runInbox realNow pid proc = popPMsgI realNow pid >>= \case
    Nothing   -> pure Nothing
    Just msgI -> do
      procInput realNow pid proc msgI
      pure $ Just proc

  -- run the next tick
  runTick :: Tick -> pid -> p -> SimT pid p m ()
  runTick realNow pid proc = do
    let tickMsg = MsgRT (RTTick realNow ())
    procInput realNow pid proc tickMsg
    pure ()

  -- process input and deliver outgoing messages
  procInput :: Tick -> pid -> p -> ProcMsgI p -> SimT pid p m ()
  procInput realNow pid proc msgI = do
    let logS = lift . simRunO . (realNow, ) . MsgProc
    logS $ SimMsgRecv pid msgI
    outs  <- lift $ reactM proc msgI

    -- add all current addresses to address-book
    -- note that we don't check for deregistrations here, but later
    -- TODO: handle RTAddr below instead of getAddrs here, more efficient
    addrs <- lift $ getAddrsM proc
    for_ addrs $ \addr -> mapAddress addr pid True

    -- deliver outputs
    for_ outs $ \case
      MsgRT (RTAddr _ _) -> do
        pure () -- TODO: handle this properly
      msg@(MsgUser uo) -> do
        logS $ SimMsgSend pid msg
        lift $ simRunO (realNow, MsgUser $ SimProcUserO pid uo)
      MsgProc msg -> do
        -- to proc, send it to other's inbox
        let dst = getTarget msg
        procs <- use _1
        pids' <- use (_2 . _simAddr . at dst . nom) >$> toList
        pids  <- for pids' $ \p -> case M.lookup p procs of
          Nothing -> do
            logS $ SimNoSuchPid (Right pid) p
            pure Nothing
          Just dstProc -> lift (getAddrsM dstProc) >>= \case
            dstAddrs | dst `notElem` dstAddrs -> do
              -- proc no longer wants this address, deregister it
              mapAddress dst p False
              pure Nothing
            _ -> pure (Just p)
        when (null (catMaybes pids)) $ do
          logS $ SimMsgSend pid (MsgProc msg)
          logS $ SimNoSuchAddr pid dst
        for_ (catMaybes pids) $ \p -> do
          simLatency   <- use $ _2 . _simLatency
          (delta, src) <- state $ _2 . _simDRG $ sampleLatency addrs
                                                               dst
                                                               simLatency
          let msg'   = setSource src msg
          let future = int realNow + int delta
          pushPMsgI future p (MsgProc msg')
          logS $ SimMsgSend pid (MsgProc msg')

runSimulation
  :: forall pid p m
   . Sim pid p m
  => SimRT pid (State p) m
  -> (Map pid (State p), SimState pid (State p))
  -> m (Map pid (State p), SimState pid (State p))
runSimulation simRT (procState0, simState0) = do
  procs0              <- proceedAll @p procState0
  (procs1, simState1) <- simulate @_ @p simRT `execStateT` (procs0, simState0)
  procState1          <- suspendAll @p procs1
  pure (procState1, simState1)
