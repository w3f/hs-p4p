{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}

module P4P.Sim.Util.IO where

-- external
import           Control.Monad                  (when)

-- external, IO & system
import           Control.Concurrent.Async       (race)
import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, readTBQueue,
                                                 writeTBQueue)
import           Control.Concurrent.STM.TVar    (newTVarIO, readTVarIO,
                                                 writeTVar)
import           Data.IORef                     (atomicModifyIORef, newIORef,
                                                 writeIORef)
import           System.Console.Readline        (addHistory, readline)
import           System.Directory               (XdgDirectory (..),
                                                 createDirectoryIfMissing,
                                                 getXdgDirectory)
import           System.IO                      (BufferMode (..), IOMode (..),
                                                 hGetLine, hIsEOF, hPutStrLn,
                                                 hSetBuffering, openFile,
                                                 stderr)
import           System.Posix.IO                (stdInput)
import           System.Posix.Terminal          (queryTerminal)

-- internal
import           P4P.Sim.IO
import           P4P.Sim.Types


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

-- | Set up a nice prompt if on a terminal, otherwise 'defaultGetInput'.
maybeTerminalGetInput :: String -> String -> String -> IO (IO (Maybe String))
maybeTerminalGetInput dirname filename prompt = do
  queryTerminal stdInput >>= \case
    False -> pure defaultGetInput
    True  -> do
      dir <- getXdgDirectory XdgData dirname
      createDirectoryIfMissing True dir
      let history = dir <> "/" <> filename
      maybeAddHistory <- setupReadlineHistory history
      pure $ readline prompt >>= \case
        Nothing -> hPutStrLn stderr "" >> pure Nothing
        Just s  -> maybeAddHistory s >> pure (Just s)

{- | Convert a 'SimUserIO' to have auto-join and auto-quit behaviour.

By design, processes are meant to be suspended at any time, and so they have no
explicit awareness of, or any control over, when they are started or stopped.
This slight hack adds support for auto-join/quit, by having the sim extension
communicate this implicitly with the 'SimUserIO' that is driving it.
-}
hookAutoJoinQuit
  :: forall pid ps xs
   . Bool
  -> Bool
  -> XUserI xs
  -> (XUserO xs -> Bool)
  -> SimUserIO pid ps xs
  -> IO (SimUserIO pid ps xs)
hookAutoJoinQuit autoJoin autoQuit joinMsg isQuitMsg (ui, uo) = do
  if not autoJoin && not autoQuit
    then pure (ui, uo)
    else do
      -- by design principle, processes are meant to be suspended at any time,
      -- and so they have no explicit awareness of when they are started or
      -- stopped. so we have this slight hack to support autoJoin/autoQuit
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
            True -> fmap (either id id) $ race ui $ do
              atomically (readTBQueue finished)
              pure Nothing
          isJoinStarted = \case
            SimExtensionO m | isQuitMsg m -> True
            _                             -> False
          uo' outs = do
            uo outs
            when (autoQuit && any isJoinStarted outs) $ do
              atomically $ writeTBQueue finished ()
      pure (ui', uo')
