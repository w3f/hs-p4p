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

import           Control.Clock                  (Clocked (..))
import           Control.Monad                  (forever, join, void, when)
import           Control.Monad.Extra            (whenJust)
import           Control.Op
import           Data.Either                    (fromRight, lefts)
import           Data.Foldable                  (for_, toList, traverse_)
import           Data.List                      (intercalate)
import           Data.List.NonEmpty             (NonEmpty, fromList)
import           Data.Maybe                     (fromMaybe, mapMaybe)
import           Data.Schedule                  (Tick)
import           Data.Traversable               (for)
import           P4P.Proc                       (GMsg (..), GMsgI, GMsgO, PAddr,
                                                 ProcEnv (..), Process (..),
                                                 Protocol (..), RuntimeI (..))
import           Safe                           (readNote)
import           Text.Read                      (readEither)

-- external, IO
import           Control.Clock.IO               (Intv (..), clockWithIO,
                                                 interval, newClock)
import           Control.Concurrent.Async       (async, cancel, link, link2,
                                                 wait)
import           Control.Concurrent.Async.Extra (foreverInterleave)
import           Control.Concurrent.STM         (STM, atomically)
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
import           System.Exit                    (ExitCode (..))
import           System.IO                      (BufferMode (..), Handle,
                                                 IOMode (..), hClose, hGetLine,
                                                 hPutStrLn, hSetBuffering,
                                                 openFile, stderr, stdin)
import           System.IO.Error                (annotateIOError, catchIOError,
                                                 isEOFError)
import           Text.Pretty.Simple             (pHPrint)
import           UnliftIO.Exception             (bracket, mask, throwIO)

-- internal
import           P4P.Sim.Extension
import           P4P.Sim.Internal
import           P4P.Sim.Options                (SimIAction (..),
                                                 SimIOAction (..),
                                                 SimLogging (..),
                                                 SimOAction (..),
                                                 SimOptions (..),
                                                 isInteractiveMode)
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

mkHandle :: Either CInt FilePath -> IO Handle
mkHandle fdOrFile = do
  h <- case fdOrFile of
    Right path -> openFile path AppendMode
    Left  fd   -> fdToHandle fd
  hSetBuffering h LineBuffering
  pure h

annotateRethrow :: IO a -> (IOError -> IOError) -> IO a
annotateRethrow act f = catchIOError act $ throwIO . f

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
type SimReRe ps xs
  = ( Show ps
    , Read ps
    , Show (UserI ps)
    , Read (UserI ps)
    , Show (UserO ps)
    , Read (UserO ps)
    , Show (PAddr ps)
    , Read (PAddr ps)
    , Show (PMsg ps)
    , Read (PMsg ps)
    , Ord (PAddr ps)
    , Show xs
    , Read xs
    , Show (XUserI xs)
    , Read (XUserI xs)
    , Show (XUserO xs)
    , Read (XUserO xs)
    )

type SimUserIO ps xs
  = (IO (Maybe (SimXUserI ps xs)), [SimXUserO ps xs] -> IO ())
type UserSimIO ps xs = (Maybe (SimXUserI ps xs) -> IO (), IO [SimXUserO ps xs])
type UserSimSTM ps xs
  = (Maybe (SimXUserI ps xs) -> STM (), STM [SimXUserO ps xs])
type StdIO = (IO (Maybe String), String -> IO ())

data UserSimAsync ps xs = UserSimAsync
  { simWI         :: !(Maybe (SimXUserI ps xs) -> IO ())
  , simRO         :: !(IO [SimXUserO ps xs])
  , simCancel     :: !(IO ())
  , simWaitFinish :: !(IO (Either (NonEmpty SimError) ()))
  }

defaultStdIO :: StdIO
defaultStdIO = (hGetLineOrEOF stdin, putStrLn)

defaultSimUserIO :: SimReRe ps xs => StdIO -> SimUserIO ps xs
defaultSimUserIO (getInput, doOutput) =
  -- support special "pid :~ msg" syntax for SimProcUserI / SimProcUserO
  let i = untilJustM $ getInput >>= \case
        Nothing -> pure (Just Nothing) -- EOF, quit
        Just s  -> if null s
          then pure Nothing
          else case readEither s of
            Right (pid :~ ui) -> pure (Just (Just (SimProcUserI pid ui)))
            Left  _           -> case readEither s of
              Right r -> pure (Just (Just r))
              Left  e -> doOutput e >> pure Nothing
                -- TODO: add some help text, ideally with some introspection
                -- that prints out some generated concrete examples
      o = \case
        SimProcUserO pid uo -> doOutput $ show $ pid :~ uo
        x                   -> doOutput $ show x
  in  (i, traverse_ o)

-- | SimUserIO that reads/writes from TBQueues.
tbQueueSimUserIO :: IO (UserSimIO ps xs, SimUserIO ps xs)
tbQueueSimUserIO = do
  qi <- newTBQueueIO 1
  qo <- newTBQueueIO 1
  let ri = atomically $ readTBQueue qi
      ro = atomically $ readTBQueue qo
      wi = atomically . writeTBQueue qi
      wo = atomically . writeTBQueue qo
  pure ((wi, ro), (ri, wo))

-- | SimUserIO that reads/writes from TBQueues.
tbQueueSimUserIO' :: IO (UserSimSTM ps xs, SimUserIO ps xs)
tbQueueSimUserIO' = do
  qi <- newTBQueueIO 1
  qo <- newTBQueueIO 1
  let ri = atomically $ readTBQueue qi
      ro = readTBQueue qo
      wi = writeTBQueue qi
      wo = atomically . writeTBQueue qo
  pure ((wi, ro), (ri, wo))

{- | Combine a bunch of 'SimUserIO' together.

The composed version will:

  * read from any input, preserving relative order of inputs
  * write to every output, in the same order as the given list

EOF ('Nothing') on any input stream will be interpreted as EOF for the whole
sim. An extra function is also returned, which the caller can use to close the
input stream proactively in a graceful way: the sim will see an explicit EOF
after consuming any outstanding unconsumed inputs.
-}
combineSimUserIO :: [SimUserIO ps xs] -> IO (SimUserIO ps xs, IO ())
combineSimUserIO ios = do
  let (is, os) = unzip ios
  (i, close) <- foreverInterleave is
  let o x = for_ os ($ x)
  pure ((join <$> i, o), close)

-- | Convert a 'runClocked' input to 'SimI' format, with 'Nothing' (EOF) lifted
-- to the top of the ADT structure.
c2i :: Either Tick (Maybe x) -> Maybe (GMsg (RuntimeI ()) x v a)
c2i (Right Nothing ) = Nothing
c2i (Left  t       ) = Just (MsgRT (RTTick t ()))
c2i (Right (Just a)) = Just (MsgUser a)

type SimLog ps xs
  = ( Show ps
    , Show (PAddr ps)
    , Show (GMsgI ps)
    , Show (GMsgO ps)
    , Show (UserO ps)
    , Show (AuxO ps)
    , Show xs
    , Show (XUserI xs)
    , Show (XUserO xs)
    )

defaultSimLog
  :: SimLog ps xs
  => (SimXO ps xs -> Bool)
  -> String
  -> Handle
  -> Tick
  -> SimXO ps xs
  -> IO ()
defaultSimLog f tFmt h t evt = when (f evt) $ do
  tstr <- formatTime defaultTimeLocale tFmt <$< getZonedTime
  hPutStrLn h $ tstr <> " | " <> show t <> " | " <> show evt

logAllNoUser :: GMsg r u p a -> Bool
logAllNoUser = \case
  MsgUser _ -> False
  _         -> True

logAllNoUserTicks :: GMsg r u p (SimAuxO ps) -> Bool
logAllNoUserTicks = \case
  MsgUser _ -> False
  MsgAux (SimProcEvent (SimMsgRecv _ (MsgRT (RTTick _ _)))) -> False
  _         -> True

compareOMsg :: (SimError -> IO ()) -> Tick -> Maybe String -> Handle -> IO ()
compareOMsg simError t om h = do
  om' <- hGetLineOrEOF h
  when (om /= om') $ simError $ do
    SimFailedReplayCompare "simOMsg" t (s om') (s om)
  where s = fromMaybe "<EOF>"

-- | Execution runtime for the simulation.
data SimRT ps xs m = SimRT
  { simClose  :: !(m ())
  , simStatus :: !(m (Either (NonEmpty SimError) ()))
  , simError  :: !(SimError -> m ())
  , simRunI   :: !(m (Maybe (SimXI ps xs)))
  , simRunO   :: !(Tick -> [SimXO ps xs] -> m ())
  }

defaultRT
  :: forall ps xs
   . (SimLog ps xs, SimReRe ps xs)
  => SimOptions
  -> Tick
  -> SimUserIO ps xs
  -> SimIAction Handle
  -> SimOAction Handle
  -> IO (SimRT ps xs IO)
defaultRT opt initTick (simUserI, simUserO) simIMsg simOMsg = do
  let picosPerMs   = 1000000000
      picosPerTick = simMsTick * picosPerMs

      logFilter    = case simLogging of
        LogAll            -> const True
        LogAllNoUser      -> logAllNoUser
        LogAllNoUserTicks -> logAllNoUserTicks @_ @_ @_ @ps
        LogNone           -> const False

  simLog <- case simLogging of
    LogNone -> pure (\_ _ -> pure ())
    _ ->
      mkHandle simLogOutput >$> defaultSimLog @ps @xs logFilter simLogTimeFmt

  (simI, simIClose) <- case simIActRead simIMsg of
    Nothing -> do
      simClock    <- newClock initTick (interval picosPerTick Ps)
      Clocked r f <- clockWithIO simClock simUserI
      pure $ (r >$> c2i, f)
    Just h -> do
      let input = hGetLineOrEOF h >$> fmap (readNote "simIMsg read")
      pure $ (input, pure ())

  simErrors <- newTVarIO []

  let
    simClose = simIClose
    simError e = atomically $ modifyTVar' simErrors $ \ee -> (: ee) $! e
    simStatus = readTVarIO simErrors >$> \case
      [] -> Right ()
      x  -> Left (fromList (reverse x))

    simRunI = do
      i <- simI
      whenJust (simIActWrite simIMsg) $ \h -> do
        whenJust i $ hPutStrLn h . show
      pure i

    ignoreAux = \case
      MsgAux _ -> False
      _        -> True

    onlyUser = \case
      MsgUser uo -> Just uo
      _          -> Nothing

    simRunO t outs = do
      for_ outs $ simLog t

      for_ (filter ignoreAux outs) $ \o -> do
        let om = show (t, o)
        whenJust (simOActWrite simOMsg) $ flip hPutStrLn om
        whenJust (simOActCompare simOMsg) $ compareOMsg simError t (Just om)

      -- send all the outs to simUserO in one batch
      -- this allows it to perform flow control more pro-actively, since it now
      -- receives a signal when 'reactM' outputs []
      case simOActWrite simOMsg of
        Nothing -> simUserO $ mapMaybe onlyUser outs
        Just _  -> case simIActRead simIMsg of
          Nothing -> simUserO $ mapMaybe onlyUser outs -- echo back
          Just _  -> pure ()

  pure SimRT { .. }
  where SimOptions {..} = opt

grunSimIO
  :: forall p xs
   . SimLog (State p) xs
  => SimReRe (State p) xs
  => (  ProcEnv (SimFullState (State p) xs) IO
     -> SimFullState (State p) xs
     -> IO (SimFullState (State p) xs)
     )
  -> SimOptions
  -> xs
  -> (Pid -> IO (State p))
  -> SimUserIO (State p) xs
  -> IO (Either (NonEmpty SimError) ())
grunSimIO lrunSim opt initXState mkPState simUserIO =
  bracket (openIOAction simIOAction) closeIOAction $ \SimIOAction {..} -> do
    --print opt
    ifs <- case simIActRead simIState of
      Nothing -> do
        seed <- getEntropy @ScrubbedBytes 64
        let initPids = S.fromList [0 .. pred (fromIntegral simInitNodes)]
        states <- M.traverseWithKey (const . mkPState)
          $ M.fromSet (const ()) initPids
        pure $ SimFullState states
                            (newSimState seed simInitLatency initPids)
                            initXState
      Just h -> do
        r <- annotateRethrow (hGetLine h) $ \e -> do
          annotateIOError e "simIState" Nothing Nothing
        pure (readNote "simIState read failed" r)
    whenJust (simIActWrite simIState) $ flip hPutStrLn (show ifs)

    {-
    TODO: this is a hack that prevents ctrl-c from quitting the program in
    interactive mode, similar to the behaviour of other REPLs. Without this, it
    is possible for ctrl-c to quit the program if the user is quick and gives
    the signal *in between* calls to libreadline [1], since we only ignore
    Interrupt during those calls when it is interrupted.

    However the way this is implemented is a bit shitty, as it prevents a user
    from aborting an computation in interactive mode. (This is possible in
    other REPLs). Not that this matters for a network simulator where things
    are IO-bound not CPU-bound, but ideally we'd abort the current computation
    and rewind the state back to the end of the previous computation. That's
    hard since we support fake-pure impure 'Process' (we'd have to @suspend@
    and @proceed@ in between every input, for all processes being simulated),
    but would be easy if we only supported 'Proc'.

    In non-interactive mode, nothing is changed and the user can abort whenever
    they want, as expected. This exits the program so we don't need to worry
    about restoring previous state, etc.

    [1] With the new haskeline integration, this seems not to happen *in
    practise* (you need to rm mask below to test it) although in theory it
    should, perhaps the haskeline event loop is tighter than our old hacky
    readline event loop... If we are convinced this will never be a problem, we
    could drop the whole guard mechanism.
    -}
    let guard :: forall b . ((forall a . IO a -> IO a) -> IO b) -> IO b
        guard act = if isInteractiveMode opt
          then mask $ \_ -> act id -- guard masks, unguard doesn't restore
          else act id -- guard does nothing, no mask to restore

    let realNow = simNow (simState ifs)
        mkRT    = defaultRT @_ @xs opt realNow simUserIO simIMsg simOMsg
    bracket mkRT simClose $ \rt@SimRT {..} -> do
      let env = ProcEnv guard simRunI simRunO
      when simDbgPprState $ pHPrint stderr ifs
      ofs <- lrunSim env ifs
      when simDbgPprState $ pHPrint stderr ofs

      let t = simNow (simState ofs)
      whenJust (simOActCompare simOMsg) $ compareOMsg simError t Nothing

      let os = show ofs
      whenJust (simOActWrite simOState) $ flip hPutStrLn os
      whenJust (simOActCompare simOState) $ \h -> do
        os' <- annotateRethrow (hGetLine h)
          $ \e -> annotateIOError e "simOState" Nothing Nothing
        when (os /= os') $ simError $ do
          SimFailedReplayCompare "simOState" t os' os

      simStatus
  where SimOptions {..} = opt

runSimIO
  :: forall p
   . SimProcess p
  => Ctx p IO
  => SimLog (State p) ()
  => SimReRe (State p) ()
  => SimOptions
  -> (Pid -> IO (State p))
  -> SimUserIO (State p) ()
  -> IO (Either (NonEmpty SimError) ())
runSimIO opt =
  let run = if simDbgEmptySimX opt then runSimXS @p @() else runSim @p
  in  grunSimIO @p run opt ()

handleSimResult :: Either (NonEmpty SimError) () -> IO ExitCode
handleSimResult = \case
  Right ()  -> pure ExitSuccess
  Left  err -> do
    hPutStrLn stderr $ "simulation gave errors: " <> show err
    pure (ExitFailure 1)

newSimAsync
  :: forall p
   . Maybe (SimXUserO (State p) () -> IO ())
  -> (SimUserIO (State p) () -> IO (Either (NonEmpty SimError) ()))
  -> IO (UserSimAsync (State p) ())
newSimAsync maybeEat runSimIO' = do
  ((simWI, simRO), simUserIO) <- tbQueueSimUserIO @(State p) @()
  aMain                       <- async $ runSimIO' simUserIO
  link aMain
  simCancel <- case maybeEat of
    Nothing  -> pure (cancel aMain)
    Just eat -> do
      aRead <- async $ forever $ simRO >>= traverse_ eat
      link aRead
      link2 aMain aRead
      pure (cancel aMain >> cancel aRead)
  let simWaitFinish = wait aMain
  pure $ UserSimAsync { .. }
