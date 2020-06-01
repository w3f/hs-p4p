{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
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

import           Control.Lens                     (Iso', anon, at, contains,
                                                   itraversed, use, (%%=),
                                                   (%%@=), (%=), (.=), _1, _2)
import           Control.Lens.Extra               ()
import           Control.Lens.Mutable
import           Control.Monad                    (void, when)
import           Control.Monad.Compat.Extra       ()
import           Control.Monad.Trans.Class        (MonadTrans (..))
import           Control.Monad.Trans.State.Strict (StateT (..), evalStateT,
                                                   runState, state)
import           Control.Monad.Trans.Writer.CPS   (WriterT, execWriterT, tell)
import           Control.Op
import           Crypto.Random.Extra              (ChaChaDRGInsecure)
import           Data.Bool                        (bool)
import           Data.Foldable                    (for_, toList)
import           Data.Functor.Const               (Const (..))
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
data SimRT pid ps m = SimRT
  { simClose  :: !(m ())
  , simStatus :: !(m (Either (NonEmpty SimError) ()))
  , simError  :: !(SimError -> m ())
  , simRunI   :: !(m (Maybe (SimI pid ps)))
  , simRunO   :: !((Tick, SimO pid ps) -> m ())
  }

-- TODO: SimT should be a newtype, so it doesn't clash with MonadState
type SimRunState pid p = (Map pid p, SimState pid (State p))
type SimT pid p = StateT (SimRunState pid p)
type SimWT pid p m a = WriterT [SimO pid (State p)] (SimT pid p m) a

type Sim pid p m = (Ord pid, Ord (ProcAddr p), Process p, Ctx p m, Monad m)

lift2
  :: (MonadTrans t1, MonadTrans t2, Monad (t2 m), Monad m) => m a -> t1 (t2 m) a
lift2 = lift . lift

tell1 :: Monad m => a -> WriterT [a] m ()
tell1 a = tell [a]

{- | React to a simulation input. -}
simReact
  :: forall pid p m
   . Sim pid p m
  => SimI pid (State p)
  -> SimT pid p m [SimO pid (State p)]
simReact input = execWriterT $ do
  realNow <- use $ _2 . _simNow
  procs   <- use _1
  let logU = tell1 . MsgUser

  -- run messages up-to-and-including current tick
  _ <- whileJustM $ do
    -- keep running until all inboxes are empty
    M.traverseWithKey (runInbox realNow) procs
      >$> M.mapMaybe id
      >$> M.null
      >$> bool (Just ()) Nothing

  case input of
    MsgRT (RTTick newNow ()) -> do
      -- TODO: assert newNow > realNow
      void $ M.traverseWithKey (runTick newNow) procs
      -- it's important that this is the last thing we do
      _2 . _simNow .= newNow
      pure ()
    MsgUser (SimProcAdd pid sp) -> do
      -- insert process
      let SimProcState {..} = sp
      if M.member pid procs
        then logU $ SimProcAddResult pid False
        else do
          proc <- lift2 $ proceed @p spState
          _1 . at pid .= Just proc
          _2 . _simIn . at pid .= Just spInbox
          for_ spAddr $ \a -> do
            _2 . _simAddr . at a . nom . contains pid .= True
          logU $ SimProcAddResult pid True
      pure ()
    MsgUser (SimProcDel pid) -> do
      -- delete process
      case M.lookup pid procs of
        Nothing   -> logU $ SimProcDelResult pid Nothing
        Just proc -> do
          spState <- lift2 $ suspend @p proc
          _1 . at pid .= Nothing
          spInbox <- _2 . _simIn . at pid . nom %%= (, mempty)
          spAddr  <- _2 . _simAddr . itraversed %%@= \addr pids -> do
            if S.member pid pids
              then (S.singleton addr, S.delete pid pids)
              else (S.empty, pids)
          let sp = SimProcState { .. }
          logU $ SimProcDelResult pid $ Just sp
      pure ()
    MsgUser (SimProcUserI pid userI) -> do
      -- user message to a process
      case M.lookup pid procs of
        Nothing -> do
          tell1 $ MsgAux $ SimProcEvent $ SimNoSuchPid (Left ()) pid
        Just _ -> do
          pushPMsgI realNow pid (MsgUser userI)
      pure ()
 where
  -- pop a single inbox message up-to-and-including @realNow@
  popPMsgI :: Tick -> pid -> SimWT pid p m (Maybe (ProcMsgI p))
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
  pushPMsgI :: Tick -> pid -> ProcMsgI p -> SimWT pid p m ()
  pushPMsgI future pid msgI =
    _2 . _simIn . at pid . nom . at future . nom %= (SQ.|> msgI)

  -- map (or unmap) an address to a pid
  mapAddress :: ProcAddr p -> pid -> Bool -> SimWT pid p m ()
  mapAddress addr pid b = _2 . _simAddr . at addr . nom . contains pid .= b

  -- run the next message from the inbox, if available
  runInbox :: Tick -> pid -> p -> SimWT pid p m (Maybe p)
  runInbox realNow pid proc = popPMsgI realNow pid >>= \case
    Nothing   -> pure Nothing
    Just msgI -> do
      procInput realNow pid proc msgI
      pure $ Just proc

  -- run the next tick
  runTick :: Tick -> pid -> p -> SimWT pid p m ()
  runTick realNow pid proc = do
    let tickMsg = MsgRT (RTTick realNow ())
    procInput realNow pid proc tickMsg
    pure ()

  -- process input and deliver outgoing messages
  procInput :: Tick -> pid -> p -> ProcMsgI p -> SimWT pid p m ()
  procInput realNow pid proc msgI = do
    let logS = tell1 . MsgAux . SimProcEvent
    logS $ SimMsgRecv pid msgI
    outs  <- lift2 $ reactM proc msgI

    -- add all current addresses to address-book
    -- note that we don't check for deregistrations here, but later
    -- TODO: handle RTAddr below instead of getAddrs here, more efficient
    addrs <- lift2 $ getAddrsM proc
    for_ addrs $ \addr -> mapAddress addr pid True

    -- deliver outputs
    for_ outs $ \case
      MsgRT (RTAddr _ _) -> do
        pure () -- TODO: handle this properly
      msg@(MsgUser uo) -> do
        logS $ SimMsgSend pid msg
        tell1 $ MsgUser $ SimProcUserO pid uo
      MsgAux ao -> do
        tell1 $ MsgAux $ SimUserAuxO pid ao
      MsgProc msg -> do
        -- to proc, send it to other's inbox
        let dst = getTarget msg
        procs <- use _1
        pids' <- use (_2 . _simAddr . at dst . nom) >$> toList
        pids  <- for pids' $ \p -> case M.lookup p procs of
          Nothing -> do
            logS $ SimNoSuchPid (Right pid) p
            pure Nothing
          Just dstProc -> lift2 (getAddrsM dstProc) >>= \case
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
          (delta, src) <- _2 . _simDRG %%= sampleLatency addrs dst simLatency
          let msg'   = setSource src msg
          let future = int realNow + int delta
          pushPMsgI future p (MsgProc msg')
          logS $ SimMsgSend pid (MsgProc msg')

newtype PSim ref st pid p = PSim (ref (SimRunState pid p))

type family UnliftTrans m where
  UnliftTrans (StateT st m) = m

instance (
  Ord pid, Ord (ProcAddr p), Process p,
  Allocable st (SimRunState pid p) ref
 ) => Process (PSim ref st pid p) where
  type State (PSim ref st pid p) = SimFullState pid (State p)
  type Ctx (PSim ref st pid p) m
    = ( Ctx p (UnliftTrans m)
      , Monad (UnliftTrans m)
      , m ~ StateT st (UnliftTrans m)
      )

  {- The type family equation for Ctx is complex but it's vital we keep it.
  Some alternative such as (Ctx p m, MonadState st m) would leak the Ctx p m
  onto the transformer m, forcing us to declare the constraint everywhere,
  which is much worse than keeping the complexity restricted to just here. -}

  proceed (SimFullState ps ss) = do
    procs <- lift $ proceedAll ps
    fmap PSim $ state $ alloc (procs, ss)

  suspend (PSim r) = do
    (procs, ss) <- state $ free r
    ps          <- lift $ suspendAll procs
    pure (SimFullState ps ss)

  getAddrsM (PSim r) = pure []

  reactM (PSim r) i = do
    ps0      <- use $ asLens r
    (o, ps1) <- lift $ simReact i `runStateT` ps0
    asLens r .= ps1
    pure o

simulate
  :: forall pid p st ref m
   . Sim pid p m
  => Allocable st (SimRunState pid p) ref
  => SimRT pid (State p) m
  -> PSim ref st pid p
  -> StateT st m ()
simulate simRT (PSim ppp) = void $ whileJustM $ do
  realNow <- use $ asLens ppp . _2 . _simNow
  -- we tick over after handling RTTick, so use realNow from before handling it
  lift simRunI >>= \case
    Nothing -> pure Nothing -- eof
    Just i  -> do
      outs <- reactM (PSim ppp) i
      for_ outs $ lift . simRunO . (realNow, )
      pure (Just ())
  where SimRT {..} = simRT

runSimulation
  :: forall pid p st ref m
   . Sim pid p m
  => Allocable st (SimRunState pid p) ref
  => SimRT pid (State p) m
  -> SimFullState pid (State p)
  -> StateT st m (SimFullState pid (State p))
runSimulation simRT s0 = do
  ppp <- proceed @(PSim ref st pid p) s0
  simulate simRT ppp
  suspend ppp

runSimulation1
  :: forall pid p m
   . Sim pid p m
  => SimRT pid (State p) m
  -> SimFullState pid (State p)
  -> m (SimFullState pid (State p))
runSimulation1 simRT s0 =
  -- uses 'instance Allocable (Maybe a) a (Const ())'
  -- defined in Control.Lens.Extra
  runSimulation @_ @p @(Maybe (SimRunState pid p)) @(Const ()) @_ simRT s0
    `evalStateT` Nothing
