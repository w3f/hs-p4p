{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module P4P.RT.Internal where

-- external, pure
import qualified Data.ByteString.Lazy.Char8     as LBS

import           Codec.Serialise                (Serialise (..), serialise)
import           Control.Clock                  (Clocked (..))
import           Control.Monad                  (forever, unless, void, when)
import           Control.Monad.Extra            (whenJust)
import           Control.Op
import           Data.Binary                    (Binary)
import           Data.Either                    (fromRight, lefts)
import           Data.Foldable                  (for_, toList, traverse_)
import           Data.Kind                      (Constraint, Type)
import           Data.List                      (intercalate)
import           Data.List.NonEmpty             (NonEmpty, fromList)
import           Data.Maybe                     (isNothing, mapMaybe)
import           Data.Schedule                  (HasNow (..))
import           Data.Traversable               (for)
import           Data.Void                      (Void)
import           GHC.Generics                   (Generic)
import           P4P.Proc                       (GMsg (..), GMsgI, GMsgO, PMsgI,
                                                 PMsgI', PMsgO, PMsgO_, PMsgO_',
                                                 Proc, ProcIO (..),
                                                 ProcIface (..), Tick,
                                                 UProtocol (..), runReactProc)

-- external, impure
import qualified Control.Exception              as E

import           Control.Clock.IO               (Clock (..), IOClock, Intv (..),
                                                 clockWithIOs, interval,
                                                 newClock, newClockSystem)
import           Control.Clock.IO.Internal      (foreverInterleave)
import           Control.Concurrent.Async       (async, cancel, link, link2,
                                                 wait)
import           Control.Concurrent.STM         (STM, atomically)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, readTBQueue,
                                                 writeTBQueue)
import           Control.Concurrent.STM.TVar    (modifyTVar', newTVarIO,
                                                 readTVarIO)
import           Data.Time                      (DiffTime, defaultTimeLocale,
                                                 formatTime, getZonedTime)
import           Foreign.C.Types                (CInt)
import           GHC.IO.Handle.FD               (fdToHandle)
import           System.Directory               (doesPathExist)
import           System.Exit                    (ExitCode (..))
import           System.IO                      (BufferMode (..), Handle,
                                                 IOMode (..), hClose, hIsEOF,
                                                 hPutStrLn, hSetBuffering,
                                                 openBinaryFile, stderr)
import           System.IO.Error                (annotateIOError, catchIOError)
import           UnliftIO.Exception             (bracket, finally, throwIO)

-- internal
import           P4P.RT.Internal.Serialise
import           P4P.RT.Options


onExceptionShow :: String -> IO a -> IO a
onExceptionShow tag io = E.catch
  io
  (\e -> hPutStrLn stderr (tag <> ": " <> show e)
    >> E.throwIO (e :: E.SomeException)
  )

mkHandle :: IOMode -> Either CInt FilePath -> IO Handle
mkHandle mode fdOrFile = do
  h <- case fdOrFile of
    Right path -> openBinaryFile path mode
    Left  fd   -> fdToHandle fd
  hSetBuffering h LineBuffering
  pure h

rethrow :: (IOError -> IOError) -> IO a -> IO a
rethrow f act = catchIOError act $ throwIO . f

type DeferredIO a = Either String (IO a)

runDeferredIO :: Traversable t => t (DeferredIO a) -> IO (t a)
runDeferredIO res = case lefts (toList res) of
  -- check if there are any errors; only if none, then run all deferred IO at once
  []  -> for res $ fromRight $ error "unreachable"
  err -> fail $ "errors: " <> intercalate "; " err

readIfExist :: String -> FilePath -> IO (DeferredIO Handle)
readIfExist n p = doesPathExist p >$> \case
  True  -> Right $ openBinaryFile p ReadMode
  False -> Left $ "path " <> n <> ": does not exist for reading: " <> p

writeIfNonexist :: String -> FilePath -> IO (DeferredIO Handle)
writeIfNonexist n p = doesPathExist p >$> \case
  False -> Right $ openBinaryFile p WriteMode
  True  -> Left $ "path " <> n <> ": conflict exists for writing: " <> p

dOpenIAction
  :: String -> ProcIAction FilePath -> IO (ProcIAction (DeferredIO Handle))
dOpenIAction n ProcIAction {..} =
  ProcIAction
    <$> traverse (readIfExist $ n <> "/IActRead")      procIActRead
    <*> traverse (writeIfNonexist $ n <> "/IActWrite") procIActWrite

dOpenOAction
  :: String -> ProcOAction FilePath -> IO (ProcOAction (DeferredIO Handle))
dOpenOAction n ProcOAction {..} =
  ProcOAction
    <$> traverse (writeIfNonexist $ n <> "/OActWrite") procOActWrite
    <*> traverse (readIfExist $ n <> "/OActCompare")   procOActCompare

dOpenIOAction :: ProcIOAction FilePath -> IO (ProcIOAction (DeferredIO Handle))
dOpenIOAction ProcIOAction {..} =
  ProcIOAction
    <$> dOpenIAction "IState" procIState
    <*> dOpenIAction "IMsg"   procIMsg
    <*> dOpenOAction "OMsg"   procOMsg
    <*> dOpenOAction "OState" procOState

openIOAction :: ProcIOAction FilePath -> IO (ProcIOAction Handle)
openIOAction a = dOpenIOAction a >>= runDeferredIO

closeIOAction :: ProcIOAction Handle -> IO ()
closeIOAction = void . traverse hClose

data RTError = RTFailedReplayCompare
  { rtFailedReplayCompareType     :: !String
  , rtFailedReplayCompareExpected :: !LBS.ByteString
  , rtFailedReplayCompareActual   :: !LBS.ByteString
  }
    -- ^ Failed to compare replay at the given tick.
  deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

compareOMsg :: (RTError -> IO ()) -> Maybe LBS.ByteString -> Handle -> IO ()
compareOMsg rtError mom h = case mom of
  Nothing -> hIsEOF h >>= \case
    True  -> pure ()
    False -> err (LBS.pack "<not EOF>") (LBS.pack "<EOF>")
  Just om -> hIsEOF h >>= \case
    True  -> err (LBS.pack "<EOF>") om
    False -> do
      om' <- LBS.hGet h $ fromIntegral $ LBS.length om
      when (om /= om') $ err om' om
  where err x y = rtError $ RTFailedReplayCompare "procOMsg" x y

-- | Execution runtime for a process.
data RTEnv ps m = RTEnv
  { rtClose  :: !(m ())
  , rtStatus :: !(m (Either (NonEmpty RTError) ()))
  , rtError  :: !(RTError -> m ())
  , rtProcIO :: !(ProcIO ps m)
  }

defaultLog
  :: (Show i, Show o)
  => (Either i o -> Bool)
  -> String
  -> Handle
  -> Tick
  -> Either i o
  -> IO ()
defaultLog f tFmt h t evt = when (f evt) $ do
  tstr <- formatTime defaultTimeLocale tFmt <$< getZonedTime
  hPutStrLn h $ tstr <> " | " <> show t <> " | " <> case evt of
    Left  i -> "<- " <> show i
    Right o -> "-> " <> show o

-- | A type for the runtime to receive input and send output, to a client.
type RTHiIO ps = (IO (Maybe (HiI ps)), [HiO ps] -> IO ())

-- | A type for the runtime to receive input and send output, to the network.
type RTLoIO ps = (IO (Maybe (LoI ps)), [LoO ps] -> IO ())

defaultRTWouldInteract :: RTOptions log -> Bool
defaultRTWouldInteract opt =
  case procIActRead (procIMsg (rtProcIOAction opt)) of
    Nothing -> True
    Just _  -> False

rtMsTickInterval :: Integer -> DiffTime
rtMsTickInterval rtMsTick =
  let picosPerMs   = 1000000000
      picosPerTick = rtMsTick * picosPerMs
  in  interval picosPerTick Ps

-- | Convenience type alias for being able to record-and-relay a protocol.
--
-- Note: being able to suspend-and-resume a protocol is just @codec ps@.
type ProcReRe (codec :: Type -> Constraint) ps
  = ( codec (EnvI ps)
    , codec (EnvO ps)
    , codec (LoI ps)
    , codec (LoO ps)
    , codec (HiI ps)
    , codec (HiO ps)
    )

-- | Convenience type alias for being able to log a protocol, including AuxO.
type ProcLog (codec :: Type -> Constraint) ps
  = (codec (AuxO ps), ProcReRe codec ps)

-- | Convert a 'runClocked' input to 'GMsgI' format, with 'Nothing' (EOF)
-- lifted to the top of the ADT structure.
c2i :: Either Tick (Either l h) -> GMsgI Tick l h a
c2i (Left  t        ) = MsgEnv t
c2i (Right (Left  l)) = MsgLo l
c2i (Right (Right h)) = MsgHi h

{- | RT state to be stored alongside process state.

This is a minimal amount of information the RT needs to ensure that resuming
a suspended process proceeds with similar settings to the original run. This
does not affect the determinism of the process.

Currently this contains only the clock-rate. Any additions must be thoroughly
justified including why it can't go elsewhere.

- clock-rate can't go elsewhere as this is an impure detail that is outside of
  our model of what a pure process is.

In particular, "current tick" and "current addresses" are tracked by the
process itself as these are details relevant to the pure logic of a protocol.
-}
data RTCfg = RTCfg
  { rtMsTick :: !Integer
  -- ^ Milliseconds per tick, of the runtime clock.
  }
  deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

defaultRT
  :: forall ps log
   . HasNow ps
  => EnvI ps ~ Tick
  => ProcLog Show ps
  => ProcReRe Serialise ps
  => RTOptions log
  -> RTCfg
  -> ps
  -> Maybe (Either (PMsgI ps) (PMsgO ps) -> Bool)
  -> (IOClock -> ps -> IO (RTLoIO ps, IO ()))
  -> (IOClock -> ps -> IO (RTHiIO ps, IO ()))
  -> ProcIAction Handle
  -> ProcOAction Handle
  -> IO (RTEnv ps IO)
defaultRT opt cfg initState logFilter' mkLoIO mkHiIO procIMsg procOMsg = do
  let initTick = getNow initState
      intv     = rtMsTickInterval $ rtMsTick cfg

  (rtClock, rtLoIO, closeLo, rtHiIO, closeHi) <-
    if defaultRTWouldInteract opt
       || isNothing (procIActRead procIMsg)
       || isNothing (procOActWrite procOMsg)
    then
      do
        rtClock <- if rtFFToSystemTick && isNothing (procIActRead procIMsg)
          then do
            sysClock <- newClockSystem intv
            sysNow   <- clockNow sysClock
            -- if the process is already ahead of the system then don't use it
            if sysNow >= initTick then pure sysClock else newClock initTick intv
          else newClock initTick intv
        (rtLoIO, closeLo) <- mkLoIO rtClock initState
        (rtHiIO, closeHi) <- mkHiIO rtClock initState
        pure (rtClock, rtLoIO, closeLo, rtHiIO, closeHi)
    else
      pure
        ( error "programmer error, failed to define rtClock"
        , error "programmer error, failed to define rtLoIO"
        , pure ()
        , error "programmer error, failed to define rtHiIO"
        , pure ()
        )

  rtLog1 <- case logFilter' of
    Nothing -> pure (\_ _ -> pure ())
    Just logFilter ->
      mkHandle AppendMode rtLogOutput
        >$> defaultLog @(PMsgI ps) @(PMsgO ps) logFilter rtLogTimeFmt

  let (rtLoI, rtLoO) = rtLoIO
      (rtHiI, rtHiO) = rtHiIO

  (rtI, rtIClose) <- case procIActRead procIMsg of
    -- use rtLoI / rtHiI only if we're not reading input
    Nothing -> do
      Clocked r f <- clockWithIOs
        rtClock
        [(Left <$>) <$> rtLoI, (Right <$>) <$> rtHiI]
      pure $ ((c2i <$>) <$> r, f)
    Just h -> do
      s <- newLBStream h
      let input = deserialiseOrEOF "procIMsg read" s
      pure $ (input, pure ())

  rtErrors <- newTVarIO []
  devnull  <- openBinaryFile systemEmptyFile AppendMode

  let
    rtClose =
      hClose devnull `finally` rtIClose `finally` closeHi `finally` closeLo
    rtError e = atomically $ modifyTVar' rtErrors $ \ee -> (: ee) $! e
    rtStatus = readTVarIO rtErrors >$> \case
      [] -> Right ()
      x  -> Left (fromList (reverse x))

    rtLog t = \case
      Left  i    -> rtLog1 t (Left i)
      Right outs -> traverse_ (rtLog1 t . Right) outs

    rtRunI = do
      i <- rtI
      whenJust (procIActWrite procIMsg) $ \h -> do
        whenJust i $ LBS.hPut h . serialise
      LBS.hPut devnull $ serialise i -- force to avoid leaking thunks
      pure i

    ignoreAux :: GMsgO e l h a -> Maybe (GMsgO e l h Void)
    ignoreAux = \case
      MsgEnv e -> Just $ MsgEnv e
      MsgLo  l -> Just $ MsgLo l
      MsgHi  h -> Just $ MsgHi h
      MsgAux _ -> Nothing

    onlyHi = \case
      MsgHi ho -> Just ho
      _        -> Nothing

    onlyLo = \case
      MsgLo lo -> Just lo
      _        -> Nothing

    rtRunO outs = do
      for_ (mapMaybe ignoreAux outs) $ \o -> do
        let om = serialise o
        whenJust (procOActWrite procOMsg) $ flip LBS.hPut om
        whenJust (procOActCompare procOMsg) $ compareOMsg rtError (Just om)

      case procOActWrite procOMsg of
        -- use rtLoO / rtHiO only if we're not writing output
        Nothing -> do
          rtHiO $ mapMaybe onlyHi outs
          rtLoO $ mapMaybe onlyLo outs
        -- additionally, if we have interactive input, then echo back the output
        Just _ | defaultRTWouldInteract opt -> do
          rtHiO $ mapMaybe onlyHi outs
        _ -> pure ()

    rtProcIO = ProcIO rtLog rtRunI rtRunO

  pure RTEnv { .. }
  where RTOptions {..} = opt

runProcIO
  :: forall ps init log
   . HasNow ps
  => EnvI ps ~ Tick
  => ProcLog Show ps
  => ProcReRe Serialise ps
  => Serialise ps
  => (ProcIO ps IO -> Tick -> ps -> IO ps)
  -> RTInitOptions init
  -> RTOptions log
  -> IO ps
  -> Maybe (Either (PMsgI ps) (PMsgO ps) -> Bool)
  -> (IOClock -> ps -> IO (RTLoIO ps, IO ()))
  -> (IOClock -> ps -> IO (RTHiIO ps, IO ()))
  -> IO (Either (NonEmpty RTError) ())
runProcIO runReact initOpts opt mkNewState logFilter mkLoIO mkHiIO =
  bracket (openIOAction rtProcIOAction) closeIOAction $ \ProcIOAction {..} -> do
    --print opt
    (cfg, ifs) <- case procIActRead procIState of
      Nothing -> (RTCfg rtInitMsTick, ) <$> mkNewState
      Just h  -> do
        rethrow (\e -> annotateIOError e "procIState" Nothing Nothing) $ do
          r <- LBS.hGetContents h
          pure $ deserialiseNote "procIState read failed" r
    whenJust (procIActWrite procIState) $ flip LBS.hPut (serialise (cfg, ifs))
    LBS.appendFile systemEmptyFile $ serialise ifs -- force to avoid leaking thunks

    let (imsg, omsg) = (procIMsg, procOMsg)
        mkRT = defaultRT @ps opt cfg ifs logFilter mkLoIO mkHiIO imsg omsg
    bracket mkRT rtClose $ \rt@RTEnv {..} -> do
      ofs <- onExceptionShow "runReact" $ runReact rtProcIO (getNow ifs) ifs
      whenJust (procOActCompare procOMsg) $ do
        compareOMsg rtError Nothing

      let os = serialise (cfg, ofs)
      whenJust (procOActWrite procOState) $ flip LBS.hPut os
      whenJust (procOActCompare procOState) $ \h -> do
        rethrow (\e -> annotateIOError e "procOState" Nothing Nothing) $ do
          os' <- LBS.hGetContents h
          when (os /= os') $ do
            rtError $ RTFailedReplayCompare "procOState" os' os

      rtStatus
 where
  RTOptions {..}     = opt
  RTInitOptions {..} = initOpts

runProcIO'
  :: forall ps init log
   . Proc ps
  => HasNow ps
  => EnvI ps ~ Tick
  => ProcLog Show ps
  => ProcReRe Serialise ps
  => Serialise ps
  => RTInitOptions init
  -> RTOptions log
  -> IO ps
  -> Maybe (Either (PMsgI ps) (PMsgO ps) -> Bool)
  -> (IOClock -> ps -> IO (RTLoIO ps, IO ()))
  -> (IOClock -> ps -> IO (RTHiIO ps, IO ()))
  -> IO (Either (NonEmpty RTError) ())
runProcIO' = runProcIO runReactProc

defaultRTLogging
  :: RTOptions RTLogging
  -> Maybe (Either (GMsgI ei li hi Void) (GMsgO eo lo ho ao) -> Bool)
defaultRTLogging opt = case rtLogging opt of
  LogNone -> Nothing
  lv      -> Just (\msg -> fromEnum lv >= msgLv msg)
 where
  msgLv :: Either (GMsgI ei li hi Void) (GMsgO eo lo ho ao) -> Int
  msgLv = \case
    Left  (MsgEnv _) -> 5
    Right (MsgEnv _) -> 4
    Left  (MsgHi  _) -> 3
    Right (MsgHi  _) -> 3
    Left  (MsgLo  _) -> 2
    Right (MsgLo  _) -> 2
    Right (MsgAux _) -> 1

handleRTResult :: Either (NonEmpty RTError) () -> IO ExitCode
handleRTResult = \case
  Right ()  -> pure ExitSuccess
  Left  err -> do
    hPutStrLn stderr $ "p4p runtime gave errors: " <> show err
    pure (ExitFailure 1)

writeProcData
  :: forall a
   . (Serialise a, Show a)
  => Handle
  -> Bool
  -> ((CodecFormat, a), SomeResidue a)
  -> IO ()
writeProcData ho p ((k, v), res) = do
  let showLn' = if p then pShowLn else showLn
  let (enc, i, o) = case k of
        CodecCbor -> (LBS.pack . showLn', CodecCbor, CodecRead)
        CodecRead -> (serialise, CodecRead, CodecCbor)
  hPutStrLn stderr $ "input  format: " <> show i
  hPutStrLn stderr $ "output format: " <> show o
  withDecodeStream "convertProcData decode" (LBS.hPut ho . enc) v res

convertProcErrors :: [String] -> [String] -> [String] -> IO ()
convertProcErrors erri erro errs = do
  let errors =
        fmap ("imsg : " <>) erri
          <> fmap ("omsg : " <>) erro
          <> fmap ("state: " <>) errs
  hPutStrLn stderr
    $  "convertProcData: could not detect format, "
    <> "perhaps try a different protocol and/or extension:\n"
    <> intercalate "\n" errors

convertProcData
  :: forall ps
   . ProcReRe Serialise ps
  => Serialise ps
  => ProcReRe Show ps
  => Show ps => ProcReRe Read ps => Read ps => ConvOptions -> IO ()
convertProcData opt = do
  hi  <- mkHandle ReadMode convIFile
  ho  <- mkHandle WriteMode convOFile
  bs0 <- LBS.hGetContents hi
  unless (LBS.null bs0) $ do
    case someDecodeStream bs0 (allCodecs @(PMsgI ps)) of
      Right result -> writeProcData ho False result
      Left  erri   -> case someDecodeStream bs0 (allCodecs @(PMsgO_ ps)) of
        Right result -> writeProcData ho False result
        Left  erro   -> case someDecodeStream bs0 (allCodecs @(RTCfg, ps)) of
          Right result -> writeProcData ho True result
          Left  errs   -> convertProcErrors erri erro errs
  where ConvOptions {..} = opt

type UProcReRe (codec :: Type -> Constraint) ps
  = (codec (Addr ps), codec (XMsg ps))

-- | Like 'convertProcData' but transparently encode/decode the 'XMsg' type.
convertUProcData
  :: forall ps
   . Ord (Addr ps)
  => ProcReRe Serialise ps
  => UProcReRe Serialise ps
  => Serialise ps
  => ProcReRe Show ps
  => UProcReRe Show ps
  => Show ps
  => ProcReRe Read ps
  => UProcReRe Read ps
  => Read ps => ConvOptions -> IO ()
convertUProcData opt = do
  hi  <- mkHandle ReadMode convIFile
  ho  <- mkHandle WriteMode convOFile
  bs0 <- LBS.hGetContents hi
  unless (LBS.null bs0) $ do
    case someDecodeStream bs0 (allCodecs @(PMsgI' ps)) of
      Right result -> writeProcData ho False result
      Left  erri   -> case someDecodeStream bs0 (allCodecs @(PMsgO_' ps)) of
        Right result -> writeProcData ho False result
        Left  erro   -> case someDecodeStream bs0 (allCodecs @(RTCfg, ps)) of
          Right result -> writeProcData ho True result
          Left  errs   -> convertProcErrors erri erro errs
  where ConvOptions {..} = opt

-- ** External interface out of the runtime

-- | A type to send input to and receive output from the runtime.
--
-- To be used by clients of the runtime.
type XTHiM m ps = (Maybe (HiI ps) -> m (), m [HiO ps])
type XTHiIO ps = XTHiM IO ps

data RTAsync ps = RTAsync
  { rtWI         :: !(Maybe (HiI ps) -> IO ())
  , rtRO         :: !(IO [HiO ps])
  , rtCancel     :: !(IO ())
  , rtWaitFinish :: !(IO (Either (NonEmpty RTError) ()))
  }

-- | XTHiIO/RTHiIO that reads/writes from TBQueues.
tbQueueHiIO :: IO (XTHiIO ps, RTHiIO ps)
tbQueueHiIO = do
  qi <- newTBQueueIO 1
  qo <- newTBQueueIO 1
  let ri = atomically $ readTBQueue qi
      ro = atomically $ readTBQueue qo
      wi = atomically . writeTBQueue qi
      wo = atomically . writeTBQueue qo
  pure ((wi, ro), (ri, wo))

-- | XTHiIO/RTHiIO that reads/writes from TBQueues.
tbQueueHiIO' :: IO (XTHiM STM ps, RTHiIO ps)
tbQueueHiIO' = do
  qi <- newTBQueueIO 1
  qo <- newTBQueueIO 1
  let ri = atomically $ readTBQueue qi
      ro = readTBQueue qo
      wi = writeTBQueue qi
      wo = atomically . writeTBQueue qo
  pure ((wi, ro), (ri, wo))

{- | Combine a bunch of 'RTHiIO' together.

The composed version will:

  * read from any input, preserving relative order of inputs
  * write to every output, in the same order as the given list

EOF ('Nothing') on any input stream will be interpreted as EOF for the combined
stream. An extra function is also returned, which the caller can use to close
the input stream proactively in a graceful way: the RT will see an explicit EOF
after consuming any outstanding unconsumed inputs.
-}
combineRTHiIO :: [RTHiIO ps] -> IO (RTHiIO ps, IO ())
combineRTHiIO ios = do
  let (is, os) = unzip ios
  (i, close) <- foreverInterleave (const (pure True)) is
  let o x = for_ os ($ x)
  pure ((i, o), close)

newRTAsync
  :: forall ps
   . Maybe (HiO ps -> IO ())
  -> (RTHiIO ps -> IO (Either (NonEmpty RTError) ()))
  -> IO (RTAsync ps)
newRTAsync maybeEat runProcIO_ = do
  ((rtWI, rtRO), rtHiIO) <- tbQueueHiIO @ps
  aMain                  <- async $ runProcIO_ rtHiIO
  link aMain
  rtCancel <- case maybeEat of
    Nothing  -> pure (cancel aMain)
    Just eat -> do
      aRead <- async $ forever $ rtRO >>= traverse_ eat
      link aRead
      link2 aMain aRead
      pure (cancel aMain >> cancel aRead)
  let rtWaitFinish = wait aMain
  pure $ RTAsync { .. }
