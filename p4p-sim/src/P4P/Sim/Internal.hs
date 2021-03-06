{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module P4P.Sim.Internal where

-- external
import qualified Data.Map.Strict                  as M
import qualified Data.Sequence                    as SQ
import qualified Data.Set                         as S

import           Control.Lens                     (Iso', _1, _2, anon, contains,
                                                   itraversed, use, (%%=),
                                                   (%%@=), (%=), (.=))
import           Control.Lens.Mutable             (Allocable (..), AsLens (..))
import           Control.Lens.Mutable.Extra       (FakeAlloc1 (..),
                                                   newFakeAlloc1)
import           Control.Monad                    (void, when)
import           Control.Monad.Compat.Extra       ()
import           Control.Monad.Extra              (whileJustM)
import           Control.Monad.Trans.Class        (MonadTrans (..))
import           Control.Monad.Trans.Extra        (UnMonadTrans, lift2)
import           Control.Monad.Trans.State.Strict (StateT (..), evalStateT,
                                                   runState, state)
import           Control.Monad.Trans.Writer.CPS   (WriterT, execWriterT)
import           Control.Monad.Trans.Writer.Extra (tell1)
import           Control.Op
import           Crypto.Random.Extra              (ChaChaDRGInsecure)
import           Data.Bool                        (bool)
import           Data.Foldable                    (for_, toList)
import           Data.Functor.Const               (Const (..))
import           Data.Map.Strict                  (Map)
import           Data.Maybe                       (catMaybes)
import           Data.Schedule                    (Tick, TickDelta)
import           Data.Traversable                 (for)
import           P4P.Proc                         (GMsg (..), ProcAddr,
                                                   ProcIO (..), ProcIface (..),
                                                   ProcMsgI, Process (..),
                                                   UMsg (..), UProtocol,
                                                   liftProcIO, obsIsPositive,
                                                   runReactProcess')
import           Safe                             (headNote)

-- internal
import           P4P.Sim.Numeric
import           P4P.Sim.Types


-- convenience wrapper around 'anon', see its documentation for details
nom :: (Monoid (f a), Foldable f) => Iso' (Maybe (f a)) (f a)
nom = anon mempty null

at
  :: (Functor f, Ord k)
  => k
  -> (Maybe a -> f (Maybe a))
  -> M.Map k a
  -> f (M.Map k a)
at k f = M.alterF f k
{-# INLINE at #-}

int :: (Integral a, Num b) => a -> b
int = fromIntegral


proceedAll
  :: (Process p, Applicative m, Ctx p m) => Map Pid (State p) -> m (Map Pid p)
proceedAll = traverse proceed

suspendAll
  :: (Process p, Applicative m, Ctx p m) => Map Pid p -> m (Map Pid (State p))
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
  SLatAddrIndep dist ->
    state (sampleDist dist) >$> toTickDelta >$> (, s') |> runState
 where
  s'          = headNote "simSend got empty addresses" s
  toTickDelta = round

-- TODO: SimT should be a newtype, so it doesn't clash with MonadState
type SimRunState p = (Map Pid p, SimState (State p))
type SimT p = StateT (SimRunState p)
type SimWT p m a = WriterT [SimO (State p)] (SimT p m) a

type SimProcess p
  = (Ord (ProcAddr p), Process p, EnvI (State p) ~ Tick, UProtocol (State p))

insProc
  :: forall p m
   . SimProcess p
  => Ctx p m
  => Monad m
  => Pid
  -> SimProcState (State p) (ProcMsgI p) (ProcAddr p)
  -> SimWT p m p
insProc pid sp = do
  let SimProcState {..} = sp
  proc <- lift2 $ proceed @p spState
  _1 . at pid .= Just proc
  _2 . _simIn . at pid .= Just spInbox
  for_ spAddr $ \a -> do
    _2 . _simAddr . at a . nom . contains pid .= True
  pure proc

delProc
  :: forall p m
   . SimProcess p
  => Ctx p m
  => Monad m
  => Pid
  -> p
  -> SimWT p m (SimProcState (State p) (ProcMsgI p) (ProcAddr p))
delProc pid proc = do
  spState <- lift2 $ suspend @p proc
  _1 . at pid .= Nothing
  spInbox <- _2 . _simIn . at pid . nom %%= (, mempty)
  spAddr  <- extractAddress pid True
  pure $ SimProcState { .. }

-- extract the addresses of a pid, and maybe delete them
extractAddress
  :: Ord (ProcAddr p)
  => Monad m => Pid -> Bool -> SimWT p m (S.Set (ProcAddr p))
extractAddress pid del = do
  _2 . _simAddr . itraversed %%@= \addr pids -> do
    if S.member pid pids
      then (S.singleton addr, if del then S.delete pid pids else pids)
      else (S.empty, pids)

-- map (or unmap) an address to a pid
mapAddress
  :: Ord (ProcAddr p) => Monad m => ProcAddr p -> Pid -> Bool -> SimWT p m ()
mapAddress addr pid b = _2 . _simAddr . at addr . nom . contains pid .= b

-- pop a single inbox message up-to-and-including @realNow@
popPMsgI :: Monad m => Tick -> Pid -> SimWT p m (Maybe (ProcMsgI p))
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
pushPMsgI :: Monad m => Tick -> Pid -> ProcMsgI p -> SimWT p m ()
pushPMsgI future pid msgI =
  _2 . _simIn . at pid . nom . at future . nom %= (SQ.|> msgI)

-- process input and deliver outgoing messages
procInput
  :: SimProcess p
  => Ctx p m => Monad m => Tick -> Pid -> p -> ProcMsgI p -> SimWT p m ()
procInput realNow pid proc msgI = do
  let logS = tell1 . MsgAux
      logU = tell1 . MsgHi
  logS $ SimProcRecv pid msgI
  outs     <- lift2 $ reactM proc msgI
  srcAddrs <- S.toList <$> extractAddress pid False

  -- deliver outputs
  for_ outs $ \out -> do
    logS $ SimProcSend pid out
    case out of
      msg@(MsgHi hi) -> do
        logU $ SimProcHiO pid hi
      MsgLo (UData dst msg) -> do
        -- to proc, send it to other's inbox
        procs <- use _1
        pids' <- use (_2 . _simAddr . at dst . nom) >$> toList
        pids  <- fmap catMaybes $ for pids' $ \p -> case M.lookup p procs of
          Nothing -> do
            logU $ SimStaleAddrPid dst p
            pure Nothing
          Just dstProc -> pure (Just p)
        when (null pids) $ do
          logS $ SimProcAddrEmpty pid dst
        for_ pids $ \p -> do
          simLatency   <- use $ _2 . _simLatency
          (delta, src) <- _2 . _simDRG %%= sampleLatency srcAddrs dst simLatency
          let future = int realNow + int delta
          pushPMsgI future p (MsgLo (UData src msg))
      MsgLo (UOwnAddr obs) -> do
        void $ flip M.traverseWithKey obs $ \addr ob -> do
          mapAddress addr pid (obsIsPositive ob)
      _ -> pure ()

{- | React to a simulation input. -}
simReact
  :: forall p m
   . SimProcess p
  => Ctx p m => Monad m => SimI (State p) -> SimT p m [SimO (State p)]
simReact input = execWriterT $ do
  realNow <- use $ _2 . _simNow
  procs   <- use _1
  let logU = tell1 . MsgHi

  -- run messages up-to-and-including current tick
  _ <- whileJustM $ do
    -- keep running until all inboxes are empty
    M.traverseWithKey (runInbox realNow) procs
      >$> M.mapMaybe id
      >$> M.null
      >$> bool (Just ()) Nothing

  case input of
    MsgEnv newNow -> do
      -- TODO: assert newNow > realNow
      void $ M.traverseWithKey (runTick newNow) procs
      -- it's important that this is the last thing we do
      _2 . _simNow .= newNow
    MsgHi SimResetAddrs -> do
      void $ flip M.traverseWithKey procs $ \pid proc -> do
        procInput realNow pid proc (MsgLo (UOwnAddr mempty))
    MsgHi SimGetAllPids -> do
      logU $ SimAllPids $ M.keysSet procs
    MsgHi SimGetAllInboxSizes -> do
      inboxes <- use $ _2 . _simIn
      let count inbox = sum (SQ.length <$> inbox)
      logU $ SimAllInboxSizes $ fmap count inboxes
    MsgHi SimGetTickNow -> do
      logU $ SimTickNow realNow
    MsgHi (SimGetState f) -> do
      simSt <- use _2
      simPs <- flip M.traverseWithKey procs $ \pid proc -> do
        spState <- lift2 $ suspend @p proc
        proc'   <- lift2 $ proceed @p spState
        _1 . at pid .= Just proc'
        pure spState
      when (f >= 1) $ do
        logU $ simPs `seq` SimGetStateResult simPs simSt
      when (f == 0) $ do
        logU $ simPs `seq` SimGetStateResult mempty simSt
    MsgHi (SimProcAdd pid sp) -> do
      -- insert process
      if M.member pid procs
        then logU $ SimProcAddResult pid False
        else do
          _ <- insProc pid sp
          logU $ SimProcAddResult pid True
    MsgHi (SimProcGet pid) -> do
      case M.lookup pid procs of
        Nothing   -> logU $ SimProcGetResult pid Nothing
        Just proc -> do
          sp <- delProc pid proc
          _  <- insProc pid sp
          logU $ SimProcGetResult pid $ Just sp
    MsgHi (SimProcDel pid) -> do
      case M.lookup pid procs of
        Nothing   -> logU $ SimProcDelResult pid Nothing
        Just proc -> do
          sp <- delProc pid proc
          logU $ SimProcDelResult pid $ Just sp
    MsgHi (SimProcHiI pid hiI) -> do
      -- user message to a process
      case M.lookup pid procs of
        Nothing -> do
          tell1 $ MsgHi $ SimNoSuchPid pid
        Just _ -> do
          pushPMsgI realNow pid (MsgHi hiI)
 where
  -- run the next message from the inbox, if available
  runInbox :: Tick -> Pid -> p -> SimWT p m (Maybe p)
  runInbox realNow pid proc = popPMsgI realNow pid >>= \case
    Nothing   -> pure Nothing
    Just msgI -> do
      procInput realNow pid proc msgI
      pure $ Just proc

  -- run the next tick
  runTick :: Tick -> Pid -> p -> SimWT p m ()
  runTick realNow pid proc = do
    let tickMsg = MsgEnv realNow
    procInput realNow pid proc tickMsg
    pure ()

newtype PSim ref st p = PSim (ref (SimRunState p))

instance (
  SimProcess p,
  Allocable st (SimRunState p) ref
 ) => Process (PSim ref st p) where
  type State (PSim ref st p) = SimFullState (State p) ()
  type Ctx (PSim ref st p) m
    = ( Ctx p (UnMonadTrans m)
      , Monad (UnMonadTrans m)
      , m ~ StateT st (UnMonadTrans m)
      )

  {- The type family equation for Ctx is complex but it's vital we keep it.
  Some alternative such as (Ctx p m, MonadState st m) would leak the Ctx p m
  onto the transformer m, forcing us to declare the constraint everywhere,
  which is much worse than keeping the complexity restricted to just here. -}

  proceed (SimFullState ps ss ()) = do
    procs <- lift $ proceedAll ps
    fmap PSim $ state $ alloc (procs, ss)

  suspend (PSim r) = do
    (procs, ss) <- state $ free r
    ps          <- lift $ suspendAll procs
    pure (SimFullState ps ss ())

  reactM (PSim r) i = do
    ps0      <- use $ asLens r
    (o, ps1) <- lift $ simReact i `runStateT` ps0
    asLens r .= ps1
    pure o

-- note: @p@ here does refer to the process type of individual processes
runSim
  :: forall p m
   . SimProcess p
  => Ctx p m
  => Monad m
  => ProcIO (SimFullState (State p) ()) m
  -> Tick
  -> SimFullState (State p) ()
  -> m (SimFullState (State p) ())
runSim env i0 s0 =
  runReactProcess' @(PSim (Const ()) (FakeAlloc1 (SimRunState p)) p)
      (liftProcIO env)
      i0
      s0
    `evalStateT` newFakeAlloc1
