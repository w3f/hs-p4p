{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- external
import           Control.Op
import           Data.Dependent.Sum        (DSum (..))
import           P4P.Proc                  (Proc, Protocol (..))

-- external, kademlia
import           P4P.Protocol.DHT.Kademlia (KState, defaultParams,
                                            newRandomState)

-- external, IO / readline
import           Data.IORef                (atomicModifyIORef, newIORef,
                                            writeIORef)
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
import           P4P.Sim
import           P4P.Sim.EchoProcess       (EchoState (..))
import           P4P.Sim.IO                (hGetLineOrEOF, untilJustM)
import           P4P.Sim.Options           (simParseOptions)
import           P4P.Sim.Util              (ChaChaDRGInsecure, PMut', Pid,
                                            getEntropy, mkInitPids)


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

data SProt ps where
  SEcho :: SProt EchoState
  SKad :: SProt (KState ChaChaDRGInsecure)

withSProt
  :: SProt ps
  -> ((Read (UserI ps), Show (UserO ps), Read ps, Show ps) => a)
  -> a
withSProt prot a = case prot of
  SEcho -> a
  SKad  -> a

type SimC ps
  = (SimProcess Pid (PMut' ps), SimLog Pid ps (), SimReRe Pid ps (), Proc ps)

withSimProto
  :: SimOptions
  -> (forall ps . SimC ps => SProt ps -> (Pid -> IO ps) -> IO a)
  -> IO a
withSimProto opt f = case simProto of
  ProtoEcho -> f SEcho $ \p -> pure (EState [p] 0)
  ProtoKad  -> f SKad $ \p -> do
    let params = defaultParams $ fromIntegral $ 1000 `div` simMsTick
        addr   = "addr:" <> show p
    newRandomState @ChaChaDRGInsecure getEntropy [addr] params
  where SimOptions {..} = opt

-- run via stdin/stdout
runStd :: SimOptions -> IO ExitCode
runStd opt = withSimProto opt $ \(p :: SProt ps) mkPS -> withSProt p $ do
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
  let initPids  = mkInitPids opt
  let simUserIO = defaultSimUserIO @_ @ps @() getInput
  runSimIO @_ @(PMut' ps) opt initPids mkPS simUserIO >>= handleSimResult
  where SimOptions {..} = opt

newtype UserSimAsync' pid ps = UserSimAsync' (UserSimAsync pid ps ())

-- run via tb-queues, can be loaded from GHCI
runTB :: SimOptions -> IO (DSum SProt (UserSimAsync' Pid))
runTB opt = withSimProto opt $ \(p :: SProt ps) mkPS -> withSProt p $ do
  let runSimIO' = runSimIO @_ @(PMut' ps) opt (mkInitPids opt) mkPS
  handles <- newSimAsync @_ @(PMut' ps) (Just print) runSimIO'
  pure $ p :=> UserSimAsync' handles

main :: IO ()
main =
  getArgs
    >>= simParseOptions
    -- "if True" avoids "unused" warnings for runTB
    >>= (if True then runStd else runTB >=> const (pure ExitSuccess))
    >>= exitWith
