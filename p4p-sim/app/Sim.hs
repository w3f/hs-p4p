{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- external
import qualified Data.ByteString           as BS
import qualified Data.Set                  as S

import           Control.Monad             (forever)
import           Control.Op
import           Data.Primitive.MutVar     (MutVar (..))
import           Data.String               (IsString (..))
import           Data.Word                 (Word64)
import           P4P.Proc                  (Protocol (..))
import           P4P.Proc.Instances        (PMut (..), PrimMonad (..),
                                            PrimOpGroup (..))

-- external, kademlia
import           Crypto.Random.Extra       (ChaChaDRGInsecure, seedFromWords)
import           P4P.Protocol.DHT.Kademlia (KParams (..), KState, defaultParams,
                                            emptyState)

-- external, IO / readline
import           Control.Concurrent.Async  (async, cancel, link, link2)
import           Data.Dependent.Sum        (DSum (..))
import           Data.IORef                (atomicModifyIORef, newIORef,
                                            writeIORef)
import           Options.Applicative       (defaultPrefs, execParserPure,
                                            handleParseResult)
import           System.Console.Readline   (addHistory, readline)
import           System.Directory          (XdgDirectory (..),
                                            createDirectoryIfMissing,
                                            getXdgDirectory)
import           System.Environment        (getArgs)
import           System.Exit               (ExitCode (..), exitWith)
import           System.IO                 (BufferMode (..), IOMode (..),
                                            hGetLine, hIsEOF, hPutStrLn,
                                            hSetBuffering, openFile, stderr,
                                            stdin)
import           System.Posix.IO           (stdInput)
import           System.Posix.Terminal     (queryTerminal)

-- internal
import           P4P.Sim                   (NonEmpty, Sim, SimError, SimUserI,
                                            SimUserO)
import           P4P.Sim.EchoProcess       (EchoState (..))
import           P4P.Sim.IO                (SimLog, SimReRe, SimUserIO,
                                            defaultSimUserIO, hGetLineOrEOF,
                                            runSimIO, tbQueueSimUserIO,
                                            untilJustM)
import           P4P.Sim.Options           (SimOptions (..), SimProto (..),
                                            parserInfo)


-- | Load & save history to/from a file.
--
-- Returns a monadic action. When it is run on a user input, it will add to the
-- history only if the input does not duplicate the previous entry, similar to
-- how bash works. The result of the action is a 'Maybe' 'String' representing
-- whether the input was added to the history or not.
--
-- Example usage:
--
-- @
--    dir <- getXdgDirectory XdgData "myApp"
--    createDirectoryIfMissing True dir
--    let history = dir <> "/.myApp_history"
--    maybeAddHistory <- setupReadlineHistory history
--    pure $ readline "myApp> " >>= \case
--      Nothing -> hPutStrLn stderr "" >> pure Nothing
--      Just s  -> maybeAddHistory s >> pure (Just s)
-- @
--
-- TODO: export to upstream readline
setupReadlineHistory :: FilePath -> IO (String -> IO (Maybe String))
setupReadlineHistory history = do
  hist <- openFile history ReadWriteMode
  hSetBuffering hist LineBuffering
  prev <- newIORef ""
  -- load existing history
  untilJustM $ hIsEOF hist >>= \case
    True  -> pure (Just ())
    False -> do
      s <- hGetLine hist
      addHistory s
      writeIORef prev s
      pure Nothing
  pure $ \s -> if null s
    then pure Nothing
    else do
    -- append new history if different from previous
      atomicModifyIORef prev (\s' -> (s, if s' == s then Nothing else Just s))
        >>= maybe
              (pure Nothing)
              (\s' -> addHistory s' >> hPutStrLn hist s' >> pure (Just s'))

type Pid = Word64
type MVP = PMut (MutVar (PrimState IO)) 'OpST (PrimState IO)

type SimC ps = (Sim Pid (MVP ps) IO, SimLog Pid ps, SimReRe Pid ps)

runSimIO'
  :: forall ps
   . SimC ps
  => SimOptions
  -> SimUserIO Pid ps
  -> (Pid -> ps)
  -> IO (Either (NonEmpty SimError) ())
runSimIO' opt@SimOptions {..} simUserIO mkPState =
  let initPids = S.fromList [0 .. pred (fromIntegral simInitNodes)]
  in  runSimIO @_ @(MVP ps) opt simUserIO initPids mkPState

data SProt p where
  SEcho :: SProt EchoState
  SKad :: SProt (KState ChaChaDRGInsecure)

withSProt
  :: SProt ps
  -> ((Read (UserI ps), Show (UserO ps), Read ps, Show ps) => a)
  -> a
withSProt prot a = case prot of
  SEcho -> a
  SKad  -> a

mkDummyId :: (IsString a, Integral n) => n -> String -> a
mkDummyId len pid =
  fromString (replicate (fromIntegral len - length pid) ' ' <> pid)

withSimProto
  :: SimOptions
  -> (forall ps . SimC ps => SProt ps -> (Pid -> ps) -> IO a)
  -> IO a
withSimProto opt f = case simProto of
  ProtoEcho -> f SEcho $ \p -> EState [p] 0
  ProtoKad  -> f SKad $ \p ->
    let params = defaultParams $ fromIntegral $ 1000 `div` simMsTick
        seed   = seedFromWords @BS.ByteString (p, p, p, p, p)
        nid    = mkDummyId (parH params) (show p)
        addr   = show p
    in  emptyState @_ @ChaChaDRGInsecure nid (pure addr) seed params
  where SimOptions {..} = opt

-- run via stdin/stdout
runStd :: SimOptions -> IO ExitCode
runStd opt = withSimProto opt $ \(p :: SProt ps) mkPState -> withSProt p $ do
  getInput <- queryTerminal stdInput >>= \case
    False -> pure (hGetLineOrEOF stdin)
    True  -> do
      dir <- getXdgDirectory XdgData "p4p"
      createDirectoryIfMissing True dir
      let history = dir <> "/.sim_history"
      maybeAddHistory <- setupReadlineHistory history
      pure $ readline ("p4p " <> drop 5 (show simProto) <> "> ") >>= \case
        Nothing -> hPutStrLn stderr "" >> pure Nothing
        Just s  -> maybeAddHistory s >> pure (Just s)
  runSimIO' @ps opt (defaultSimUserIO @_ @ps getInput) mkPState >>= \case
    Right ()  -> pure ExitSuccess
    Left  err -> do
      hPutStrLn stderr $ "simulation gave errors: " <> show err
      pure (ExitFailure 1)
  where SimOptions {..} = opt

data SimTBHandles pid ps = SimTBHandles {
    tbWI  :: !(Maybe (SimUserI pid ps) -> IO ())
  , tbRO  :: !(IO (SimUserO pid ps))
  , tbFin :: !(IO ())
  }

-- run via tb-queues, can be loaded from GHCI
runTB :: SimOptions -> IO (DSum SProt (SimTBHandles Pid))
runTB opt = withSimProto opt $ \(p :: SProt ps) mkPState -> withSProt p $ do
  (tbWI, tbRO, simUserIO) <- tbQueueSimUserIO @_ @ps
  aMain                   <- async $ runSimIO' @ps opt simUserIO mkPState
  link aMain
  aRead <- async $ forever $ tbRO >>= print
  link aRead
  link2 aMain aRead
  let tbFin = cancel aMain >> cancel aRead
  pure $ p :=> SimTBHandles { .. }

parseOptions :: [String] -> IO SimOptions
parseOptions args =
  let
    parser = parserInfo
      "sim - a simulator for p4p protocols"
      (  "Simulate a p4p protocol. Commands are given on stdin and replies "
      <> "are given on stdout. The syntax is $pid :~ $command where $pid "
      <> "and $command are Haskell Show/Read instance expressions, e.g. 0 :~ "
      <> "\"Hello, world!\". Give -v for more detailed output."
      )
  in  execParserPure defaultPrefs parser args |> handleParseResult

main :: IO ()
main =
  getArgs
    >>= parseOptions
    -- "if True" avoids "unused" warnings for runTB
    >>= (if True then runStd else runTB >=> const (pure ExitSuccess))
    >>= exitWith
