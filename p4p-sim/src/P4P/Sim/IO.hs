{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module P4P.Sim.IO where

-- external, pure
import qualified Data.Map.Strict                as M
import qualified Data.Set                       as S

import           Control.Clock                  (Clock (..), Clocked (..))
import           Control.Monad                  (void, when)
import           Control.Monad.Extra            (whenJust)
import           Control.Op
import           Data.Either                    (fromRight, lefts)
import           Data.Foldable                  (toList)
import           Data.List                      (intercalate)
import           Data.List.NonEmpty             (NonEmpty, fromList)
import           Data.Maybe                     (fromMaybe)
import           Data.Schedule                  (Tick)
import           Data.Traversable               (for)
import           P4P.Proc                       (GMsg (..), GMsgI, GMsgO, PAddr,
                                                 Process (..), Protocol (..),
                                                 RuntimeI (..))
import           Safe                           (readNote)
import           Text.Read                      (readEither)

-- external, IO
import           Control.Clock.IO               (Intv (..), interval, newClock)
import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, readTBQueue,
                                                 writeTBQueue)
import           Control.Concurrent.STM.TVar    (modifyTVar', newTVarIO,
                                                 readTVarIO)
import           Crypto.Random.Entropy          (getEntropy)
import           Crypto.Random.Extra            (ScrubbedBytes)
import           Data.Time                      (defaultTimeLocale, formatTime,
                                                 getZonedTime)
import           Foreign.C.Types                (CInt)
import           GHC.IO.Handle.FD               (fdToHandle)
import           System.Directory               (doesPathExist)
import           System.IO                      (BufferMode (..), Handle,
                                                 IOMode (..), hClose, hGetLine,
                                                 hPutStrLn, hSetBuffering,
                                                 openFile)
import           System.IO.Error                (catchIOError, isEOFError)
import           UnliftIO.Exception             (bracket)

-- internal
import           P4P.Sim.Internal
import           P4P.Sim.Options                (SimIAction (..),
                                                 SimIOAction (..),
                                                 SimLogging (..),
                                                 SimOAction (..),
                                                 SimOptions (..))
import           P4P.Sim.Types

-- TODO: export to upstream extra
untilJustM :: Monad m => m (Maybe a) -> m a
untilJustM act = go
 where
  go = act >>= \case
    Just r  -> pure r
    Nothing -> go

hGetLineOrEOF :: Handle -> IO (Maybe String)
hGetLineOrEOF h = catchIOError
  (Just <$> hGetLine h)
  (\e -> if isEOFError e then pure Nothing else ioError e)

type SimUserIO pid ui uo = (IO (Maybe (KV pid ui)), KV pid uo -> IO ())

defaultSimUserIO
  :: (Read pid, Show pid, Read ui, Show uo)
  => IO (Maybe String)
  -> SimUserIO pid ui uo
defaultSimUserIO getInput =
  let i = untilJustM $ getInput >>= \case
        Nothing -> pure (Just Nothing) -- EOF, quit
        Just s  -> if null s
          then pure Nothing
          else case readEither s of
            Left  e -> putStrLn e >> pure Nothing
            Right r -> pure (Just (Just r))
      o = print
  in  (i, o)

-- | SimUserIO that reads/writes from TBQueues.
tbQueueSimUserIO
  :: IO (Maybe (KV pid ui) -> IO (), IO (KV pid uo), SimUserIO pid ui uo)
tbQueueSimUserIO = do
  qi <- newTBQueueIO 1
  qo <- newTBQueueIO 1
  let ri = atomically $ readTBQueue qi
      ro = atomically $ readTBQueue qo
      wi = atomically . writeTBQueue qi
      wo = atomically . writeTBQueue qo
  pure (wi, ro, (ri, wo))

type SimLog pid ps
  = ( Show pid
    , Show (PAddr ps)
    , Show (GMsgI ps)
    , Show (GMsgO ps)
    , Show (UserO ps)
    )

defaultSimLog
  :: SimLog pid ps
  => (SimO pid ps -> Bool)
  -> String
  -> Handle
  -> (Tick, SimO pid ps)
  -> IO ()
defaultSimLog f tFmt h (t, evt) = when (f evt) $ do
  tstr <- formatTime defaultTimeLocale tFmt <$< getZonedTime
  hPutStrLn h $ tstr <> " | " <> show t <> " | " <> show evt

mkHandle :: Either CInt FilePath -> IO Handle
mkHandle fdOrFile = do
  h <- case fdOrFile of
    Right path -> openFile path AppendMode
    Left  fd   -> fdToHandle fd
  hSetBuffering h LineBuffering
  pure h

type DeferredIO a = Either String (IO a)

runDeferredIO :: Traversable t => t (DeferredIO a) -> IO (t a)
runDeferredIO res = case lefts (toList res) of
  -- check if there are any errors; only if none, then run all deferred IO at once
  []  -> for res $ fromRight $ error "unreachable"
  err -> fail $ "errors: " <> intercalate "; " err

readIfExist :: String -> FilePath -> IO (DeferredIO Handle)
readIfExist n p = doesPathExist p >$> \case
  True  -> Right $ openFile p ReadMode
  False -> Left $ "path " <> n <> ": does not exist for reading: " <> p

writeIfNonexist :: String -> FilePath -> IO (DeferredIO Handle)
writeIfNonexist n p = doesPathExist p >$> \case
  False -> Right $ openFile p WriteMode
  True  -> Left $ "path " <> n <> ": conflict exists for writing: " <> p

dOpenIAction
  :: String -> SimIAction FilePath -> IO (SimIAction (DeferredIO Handle))
dOpenIAction n SimIAction {..} =
  SimIAction
    <$> traverse (readIfExist $ n <> "/IActRead")      simIActRead
    <*> traverse (writeIfNonexist $ n <> "/IActWrite") simIActWrite

dOpenOAction
  :: String -> SimOAction FilePath -> IO (SimOAction (DeferredIO Handle))
dOpenOAction n SimOAction {..} =
  SimOAction
    <$> traverse (writeIfNonexist $ n <> "/OActWrite") simOActWrite
    <*> traverse (readIfExist $ n <> "/OActCompare")   simOActCompare

dOpenIOAction :: SimIOAction FilePath -> IO (SimIOAction (DeferredIO Handle))
dOpenIOAction SimIOAction {..} =
  SimIOAction
    <$> dOpenIAction "IState" simIState
    <*> dOpenIAction "IMsg"   simIMsg
    <*> dOpenOAction "OMsg"   simOMsg
    <*> dOpenOAction "OState" simOState

openIOAction :: SimIOAction FilePath -> IO (SimIOAction Handle)
openIOAction a = dOpenIOAction a >>= runDeferredIO

closeIOAction :: SimIOAction Handle -> IO ()
closeIOAction = void . traverse hClose

-- | Convenience type alias for being able to record and replay a simulation.
-- TODO: use ByteString/Generic-based thing, e.g. Serialise
type SimReRe pid ps
  = ( Show pid
    , Read pid
    , Show ps
    , Read ps
    , Show (UserI ps)
    , Read (UserI ps)
    , Show (UserO ps)
    , Read (UserO ps)
    , Show (PAddr ps)
    , Read (PAddr ps)
    , Show (PMsg ps)
    , Read (PMsg ps)
    )

-- | Convert a 'runClocked' input to 'SimI' format, with 'Nothing' (EOF) lifted
-- to the top of the ADT structure.
c2i :: Either Tick (Maybe a) -> Maybe (GMsg SimRuntimeI a v)
c2i (Right Nothing ) = Nothing
c2i (Left  t       ) = Just (MsgRT (SimRTTick t))
c2i (Right (Just a)) = Just (MsgUser a)

logAllNoUser :: GMsg r u (SimProcEvt pid ps) -> Bool
logAllNoUser = \case
  MsgUser _ -> False
  _         -> True

logAllNoUserTicks :: GMsg r u (SimProcEvt pid ps) -> Bool
logAllNoUserTicks = \case
  MsgUser _ -> False
  MsgProc (SimMsgRecv _ (MsgRT (RTTick _ _))) -> False
  _         -> True

compareOMsg :: (SimError -> IO ()) -> Tick -> Maybe String -> Handle -> IO ()
compareOMsg simError t om h = do
  om' <- hGetLineOrEOF h
  when (om /= om') $ simError $ do
    SimFailedReplayCompare "simOMsg" t (s om') (s om)
  where s = fromMaybe "<EOF>"

defaultRT
  :: forall pid ps
   . (SimLog pid ps, SimReRe pid ps)
  => SimOptions
  -> Tick
  -> SimUserIO pid (UserI ps) (UserO ps)
  -> SimIAction Handle
  -> SimOAction Handle
  -> IO (SimRT pid ps IO)
defaultRT opt initTick (simUserI, simUserO) simIMsg simOMsg = do
  let picosPerMs   = 1000000000
      picosPerTick = simMsTick * picosPerMs

      logFilter    = case simLogging of
        LogAll            -> const True
        LogAllNoUser      -> logAllNoUser @_ @_ @pid @ps
        LogAllNoUserTicks -> logAllNoUserTicks @_ @_ @pid @ps
        LogNone           -> const False

  simLog <- case simLogging of
    LogNone -> pure (\_ -> pure ())
    _ ->
      mkHandle simLogOutput >$> defaultSimLog @pid @ps logFilter simLogTimeFmt

  (simI, simIClose) <- case simIActRead simIMsg of
    Nothing -> do
      simClock <- newClock initTick (interval picosPerTick Ps)
      clkEnvI  <- clockWith simClock simUserI
      pure (runClocked clkEnvI >$> c2i, finClocked clkEnvI)
    Just h -> do
      pure (hGetLineOrEOF h >$> fmap (readNote "simIMsg read"), pure ())

  simErrors <- newTVarIO []

  let simClose = simIClose
      simError e = atomically $ modifyTVar' simErrors $ \ee -> (: ee) $! e
      simStatus = readTVarIO simErrors >$> \case
        [] -> Right ()
        x  -> Left (fromList (reverse x))

      simRunI = do
        i <- simI
        whenJust (simIActWrite simIMsg) $ \h -> do
          whenJust i $ hPutStrLn h . show
        pure i

      simRunO (t, o) = do
        simLog (t, o)

        let om = show (t, o)
        case simOActWrite simOMsg of
          Nothing -> case o of
            MsgUser uo -> simUserO uo
            _          -> pure ()
          Just h -> hPutStrLn h om

        whenJust (simOActCompare simOMsg) $ compareOMsg simError t (Just om)

  pure SimRT { .. }
  where SimOptions {..} = opt

runSimIO
  :: forall pid p
   . (Sim pid p IO, SimLog pid (State p), SimReRe pid (State p), Ctx p IO)
  => SimOptions
  -> SimUserIO pid (UserI (State p)) (UserO (State p))
  -> S.Set pid
  -> (S.Set pid -> IO (M.Map pid p))
  -> (pid -> State p)
  -> IO (Either (NonEmpty SimError) ())
runSimIO opt simUserIO initPids mkProcs mkPState =
  bracket (openIOAction simIOAction) closeIOAction $ \SimIOAction {..} -> do
    --print opt
    (iSimState, istate) <- case simIActRead simIState of
      Nothing -> do
        seed <- getEntropy @ScrubbedBytes 64
        pure
          ( newSimState seed simInitLatency initPids
          , M.fromSet mkPState initPids
          )
      Just h -> hGetLine h >$> readNote "simIState read"
    whenJust (simIActWrite simIState)
      $ flip hPutStrLn (show (iSimState, istate))

    let realNow = simNow iSimState
        mkRT    = defaultRT opt realNow simUserIO simIMsg simOMsg
    bracket mkRT simClose $ \rt@SimRT {..} -> do
      procs <- mkProcs (M.keysSet istate)

      proceedAll $ M.intersectionWith (,) procs istate
      oSimState <- runSimulation rt procs iSimState
      ostate    <- suspendAll procs

      let t = simNow oSimState
      whenJust (simOActCompare simOMsg) $ compareOMsg simError t Nothing

      let os = show (oSimState, ostate)
      whenJust (simOActWrite simOState) $ flip hPutStrLn os
      whenJust (simOActCompare simOState) $ \h -> do
        os' <- hGetLine h
        when (os /= os') $ simError $ do
          SimFailedReplayCompare "simOState" t os' os

      simStatus
  where SimOptions {..} = opt
