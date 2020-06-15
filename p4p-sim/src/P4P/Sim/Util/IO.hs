{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}

module P4P.Sim.Util.IO
  ( onExceptionShow
  , bracketHEF
  , maybeTerminalStdIO
  , optionTerminalStdIO
  , hookAutoJoinQuit
  )
where

-- external
import           Control.Monad                    (when)
import           Control.Monad.Extra              (whenJust)

-- external, IO & system
import qualified Control.Exception                as E

import           Control.Concurrent.Async         (race)
import           Control.Concurrent.STM           (atomically)
import           Control.Concurrent.STM.TBQueue   (newTBQueueIO, readTBQueue,
                                                   writeTBQueue)
import           Control.Concurrent.STM.TVar      (newTVarIO, readTVarIO,
                                                   writeTVar)
import           Control.Monad.Catch              (MonadMask, bracketOnError,
                                                   handle)
import           Control.Monad.IO.Class           (MonadIO)
import           System.Console.Haskeline         (InputT, Interrupt (..),
                                                   Settings (..),
                                                   defaultSettings,
                                                   getExternalPrint,
                                                   getInputLine, modifyHistory,
                                                   outputStrLn, withInterrupt)
import           System.Console.Haskeline.History (addHistoryUnlessConsecutiveDupe)
import           System.Console.Haskeline.IO      (cancelInput, closeInput,
                                                   initializeInput, queryInput)
import           System.Directory                 (XdgDirectory (..),
                                                   createDirectoryIfMissing,
                                                   getXdgDirectory)
import           System.IO                        (hPutStrLn, stderr)
import           System.Posix.IO                  (stdInput)
import           System.Posix.Terminal            (queryTerminal)

-- internal
import           P4P.Sim.IO
import           P4P.Sim.Options
import           P4P.Sim.Types


onExceptionShow :: String -> IO a -> IO a
onExceptionShow tag io = E.catch
  io
  (\e -> hPutStrLn stderr (tag <> ": " <> show e)
    >> E.throwIO (e :: E.SomeException)
  )

bracketHEF :: IO (h, IO (), IO ()) -> (h -> IO a) -> IO a
bracketHEF mkHandles action = do
  bracketOnError mkHandles (\(_, e, _) -> e) $ \(h, _, f) -> do
    r <- action h
    f
    pure r

tryAction :: (MonadIO m, MonadMask m) => InputT m a -> InputT m a
tryAction action = withInterrupt loop
 where
  loop = handle (\Interrupt -> outputStrLn "Input cancelled" >> loop) action

-- | Set up a nice prompt if on a terminal, otherwise 'defaultStdIO'.
maybeTerminalStdIO
  :: Bool -> String -> String -> String -> IO (StdIO, IO (), IO ())
maybeTerminalStdIO interactive dirname filename prompt = do
  queryTerminal stdInput >>= \case
    True | interactive -> do
      dir <- getXdgDirectory XdgData dirname
      createDirectoryIfMissing True dir
      let history = dir <> "/" <> filename
      hd <- initializeInput $ defaultSettings { historyFile    = Just history
                                              , autoAddHistory = False
                                              }
      let e = cancelInput hd
          f = closeInput hd
          i = queryInput hd $ tryAction $ do
            s <- getInputLine prompt
            whenJust s $ \s' -> do
              modifyHistory (addHistoryUnlessConsecutiveDupe s')
            pure s
      o <- queryInput hd getExternalPrint
      pure ((i, o), e, f)
    _ -> pure (defaultStdIO, pure (), pure ())

optionTerminalStdIO
  :: SimOptions -> String -> String -> String -> IO (StdIO, IO (), IO ())
optionTerminalStdIO opt = maybeTerminalStdIO (isInteractiveMode opt)

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
  -> XUserI xs
  -> (XUserO xs -> Bool)
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
