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
import           Data.Maybe                     (mapMaybe)
import           Data.Traversable               (for)
import           Data.Void                      (Void)
import           GHC.Generics                   (Generic)
import           P4P.Proc                       (GMsg (..), GMsgI, GMsgO, PMsgI,
                                                 PMsgO, PMsgO', ProcEnv (..),
                                                 ProcIface (..), Tick)

-- external, impure
import           Control.Clock.IO               (Intv (..), clockWithIOs,
                                                 interval, newClock)
import           Control.Clock.IO.Internal      (foreverInterleave)
import           Control.Concurrent.Async       (async, cancel, link, link2,
                                                 wait)
import           Control.Concurrent.STM         (STM, atomically)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, readTBQueue,
                                                 writeTBQueue)
import           Control.Concurrent.STM.TVar    (modifyTVar', newTVarIO,
                                                 readTVarIO)
import           Data.Time                      (defaultTimeLocale, formatTime,
                                                 getZonedTime)
import           Foreign.C.Types                (CInt)
import           GHC.IO.Handle.FD               (fdToHandle)
import           System.Directory               (doesPathExist)
import           System.Exit                    (ExitCode (..))
import           System.IO                      (BufferMode (..), Handle,
                                                 IOMode (..), hClose, hIsEOF,
                                                 hPutStrLn, hSetBuffering,
                                                 openBinaryFile, stderr)
import           System.IO.Error                (annotateIOError, catchIOError)
import           UnliftIO.Exception             (bracket, mask, throwIO)

-- internal
import           P4P.RT.Internal.Serialise
import           P4P.RT.Options


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
  , rtFailedReplayCompareTick     :: !Tick
  , rtFailedReplayCompareExpected :: !LBS.ByteString
  , rtFailedReplayCompareActual   :: !LBS.ByteString
  }
    -- ^ Failed to compare replay at the given tick.
  deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

compareOMsg
  :: (RTError -> IO ()) -> Tick -> Maybe LBS.ByteString -> Handle -> IO ()
compareOMsg rtError t mom h = case mom of
  Nothing -> hIsEOF h >>= \case
    True  -> pure ()
    False -> err (LBS.pack "<not EOF>") (LBS.pack "<EOF>")
  Just om -> hIsEOF h >>= \case
    True  -> err (LBS.pack "<EOF>") om
    False -> do
      om' <- LBS.hGet h $ fromIntegral $ LBS.length om
      when (om /= om') $ err om' om
  where err x y = rtError $ RTFailedReplayCompare "procOMsg" t x y

-- | Execution runtime for a process.
data RTEnv ps m = RTEnv
  { rtClose  :: !(m ())
  , rtStatus :: !(m (Either (NonEmpty RTError) ()))
  , rtError  :: !(RTError -> m ())
  , rtRunI   :: !(m (Maybe (PMsgI ps)))
  , rtRunO   :: !(Tick -> [PMsgO ps] -> m ())
  }

defaultLog :: Show a => (a -> Bool) -> String -> Handle -> Tick -> a -> IO ()
defaultLog f tFmt h t evt = when (f evt) $ do
  tstr <- formatTime defaultTimeLocale tFmt <$< getZonedTime
  hPutStrLn h $ tstr <> " | " <> show t <> " | " <> show evt

-- | A type for the runtime to receive input and send output, to a client.
type RTHiIO ps = (IO (Maybe (HiI ps)), [HiO ps] -> IO ())

-- | A type for the runtime to receive input and send output, to the network.
type RTLoIO ps = (IO (Maybe (LoI ps)), [LoO ps] -> IO ())

-- | Convert a 'runClocked' input to 'GMsgI' format, with 'Nothing' (EOF)
-- lifted to the top of the ADT structure.
c2i :: Either Tick (Either l h) -> GMsgI Tick l h
c2i (Left  t        ) = MsgEnv t
c2i (Right (Left  l)) = MsgLo l
c2i (Right (Right h)) = MsgHi h

defaultRTWouldInteract :: RTOptions -> Bool
defaultRTWouldInteract opt =
  case procIActRead (procIMsg (rtProcIOAction opt)) of
    Nothing -> True
    Just _  -> False

defaultRT
  :: forall ps
   . EnvI ps ~ Tick
  => Show (AuxO ps)
  => Show (LoO ps)
  => Show (HiO ps)
  => Serialise (LoI ps)
  => Serialise (LoO ps)
  => Serialise (HiI ps)
  => Serialise (HiO ps)
  => RTOptions
  -> Tick
  -> Maybe (PMsgO ps -> Bool)
  -> RTLoIO ps
  -> RTHiIO ps
  -> ProcIAction Handle
  -> ProcOAction Handle
  -> IO (RTEnv ps IO)
defaultRT opt initTick logFilter' rtLoIO rtHiIO procIMsg procOMsg = do
  let (rtLoI, rtLoO) = rtLoIO
      (rtHiI, rtHiO) = rtHiIO
      picosPerMs     = 1000000000
      picosPerTick   = rtMsTick * picosPerMs

  rtLog <- case logFilter' of
    Nothing -> pure (\_ _ -> pure ())
    Just logFilter ->
      mkHandle AppendMode rtLogOutput
        >$> defaultLog @(PMsgO ps) logFilter rtLogTimeFmt

  (rtI, rtIClose) <- case procIActRead procIMsg of
    -- use rtLoI / rtHiI only if we're not reading input
    Nothing -> do
      rtClock     <- newClock initTick (interval picosPerTick Ps)
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
    rtClose = rtIClose >> hClose devnull
    rtError e = atomically $ modifyTVar' rtErrors $ \ee -> (: ee) $! e
    rtStatus = readTVarIO rtErrors >$> \case
      [] -> Right ()
      x  -> Left (fromList (reverse x))

    rtRunI = do
      i <- rtI
      whenJust (procIActWrite procIMsg) $ \h -> do
        whenJust i $ LBS.hPut h . serialise
      LBS.hPut devnull $ serialise i -- force to avoid leaking thunks
      pure i

    ignoreAux :: GMsgO a l h -> Maybe (GMsgO Void l h)
    ignoreAux = \case
      MsgEnv _ -> Nothing
      MsgLo  l -> Just $ MsgLo l
      MsgHi  h -> Just $ MsgHi h

    onlyHi = \case
      MsgHi ho -> Just ho
      _        -> Nothing

    onlyLo = \case
      MsgLo lo -> Just lo
      _        -> Nothing

    rtRunO t outs = do
      for_ outs $ rtLog t

      for_ (mapMaybe ignoreAux outs) $ \o -> do
        let om = serialise (t, o)
        whenJust (procOActWrite procOMsg) $ flip LBS.hPut om
        whenJust (procOActCompare procOMsg) $ compareOMsg rtError t (Just om)

      case procOActWrite procOMsg of
        -- use rtLoO / rtHiO only if we're not writing output
        Nothing -> do
          -- send all the relevant outs in one batch
          -- this allows the other component to perform flow control more
          -- pro-actively, since they now receive a signal on []
          rtHiO $ mapMaybe onlyHi outs
          rtLoO $ mapMaybe onlyLo outs
        -- additionally, if we have interactive input, then echo back the output
        Just _ | defaultRTWouldInteract opt -> do
          rtHiO $ mapMaybe onlyHi outs
        _ -> pure ()

  pure RTEnv { .. }
  where RTOptions {..} = opt

type ProcReRe (codec :: Type -> Constraint) ps
  = ( codec ps
    , codec (EnvI ps)
    , codec (LoI ps)
    , codec (LoO ps)
    , codec (HiI ps)
    , codec (HiO ps)
    )

runProcIO
  :: forall ps
   . EnvI ps ~ Tick
  => Show (AuxO ps)
  => Show (LoO ps)
  => Show (HiO ps)
  => ProcReRe Serialise ps
  => (ProcEnv Tick ps IO -> ps -> IO ps)
  -> RTOptions
  -> IO ps
  -> (ps -> Tick)
  -> Maybe (PMsgO ps -> Bool)
  -> RTLoIO ps
  -> (Bool -> IO (Bool, RTHiIO ps))
  -> IO (Either (NonEmpty RTError) ())
runProcIO runReact procOpt mkNewState getNow logFilter rtLoIO mkRTHiIO =
  bracket (openIOAction rtProcIOAction) closeIOAction $ \ProcIOAction {..} -> do
    --print opt
    (new, ifs) <- case procIActRead procIState of
      Nothing -> (True, ) <$> mkNewState
      Just h  -> do
        rethrow (\e -> annotateIOError e "procIState" Nothing Nothing) $ do
          r <- LBS.hGetContents h
          pure (False, deserialiseNote "procIState read failed" r)
    whenJust (procIActWrite procIState) $ flip LBS.hPut (serialise ifs)
    LBS.appendFile systemEmptyFile $ serialise ifs -- force to avoid leaking thunks

    (isInteractive, rtHiIO) <- mkRTHiIO new
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
        guard act = if isInteractive && defaultRTWouldInteract procOpt
          then mask $ \_ -> act id -- guard masks, unguard doesn't restore
          else act id -- guard does nothing, no mask to restore

    let
      realNow = getNow ifs
      mkRT =
        defaultRT @ps procOpt realNow logFilter rtLoIO rtHiIO procIMsg procOMsg
    bracket mkRT rtClose $ \rt@RTEnv {..} -> do
      let env = ProcEnv guard rtRunI rtRunO
      ofs <- runReact env ifs

      let t = getNow ofs
      whenJust (procOActCompare procOMsg) $ compareOMsg rtError t Nothing

      let os = serialise ofs
      whenJust (procOActWrite procOState) $ flip LBS.hPut os
      whenJust (procOActCompare procOState) $ \h -> do
        rethrow (\e -> annotateIOError e "procOState" Nothing Nothing) $ do
          os' <- LBS.hGetContents h
          when (os /= os') $ do
            rtError $ RTFailedReplayCompare "procOState" t os' os

      rtStatus
  where RTOptions {..} = procOpt

handleRTResult :: Either (NonEmpty RTError) () -> IO ExitCode
handleRTResult = \case
  Right ()  -> pure ExitSuccess
  Left  err -> do
    hPutStrLn stderr $ "p4p runtime gave errors: " <> show err
    pure (ExitFailure 1)

convertProcData
  :: forall ps xo
   . ProcReRe Serialise ps
  => ProcReRe Show ps => ProcReRe Read ps => ConvOptions xo -> IO ()
convertProcData opt = do
  hi  <- mkHandle ReadMode convIFile
  ho  <- mkHandle WriteMode convOFile
  bs0 <- LBS.hGetContents hi
  unless (LBS.null bs0) $ do
    case someDecodeStream bs0 (allCodecs @(PMsgI ps)) of
      Right ((k, v), res) -> writeAll ho False k v res
      Left erri -> case someDecodeStream bs0 (allCodecs @(PMsgO' ps)) of
        Right ((k, v), res) -> writeAll ho False k v res
        Left  erro          -> case someDecodeStream bs0 (allCodecs @ps) of
          Right ((k, v), res) -> writeAll ho True k v res
          Left  errs          -> do
            let errors =
                  fmap ("imsg : " <>) erri
                    <> fmap ("omsg : " <>) erro
                    <> fmap ("state: " <>) errs
            hPutStrLn stderr
              $  "convertProcData: could not detect format, "
              <> "perhaps try a different protocol and/or extension:\n"
              <> intercalate "\n" errors
 where
  ConvOptions {..} = opt

  writeAll
    :: (Serialise a, Show a)
    => Handle
    -> Bool
    -> CodecFormat
    -> a
    -> SomeResidue a
    -> IO ()
  writeAll ho p k v res = do
    let showLn' = if p then pShowLn else showLn
    let (enc, i, o) = case k of
          CodecCbor -> (LBS.pack . showLn', CodecCbor, CodecRead)
          CodecRead -> (serialise, CodecRead, CodecCbor)
    hPutStrLn stderr $ "input  format: " <> show i
    hPutStrLn stderr $ "output format: " <> show o
    withDecodeStream "convertProcData decode" (LBS.hPut ho . enc) v res

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
newRTAsync maybeEat runProcIO' = do
  ((rtWI, rtRO), rtHiIO) <- tbQueueHiIO @ps
  aMain                  <- async $ runProcIO' rtHiIO
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
