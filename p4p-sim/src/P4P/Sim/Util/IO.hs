{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}

module P4P.Sim.Util.IO
  ( hookAutoJoinQuit
  , maybeTerminalStdIO
  , optionTerminalStdIO
  , setupReadlineHistory
  -- re-exports
  , UE.bracket
  , onExceptionShow
  )
where

-- external
import           Control.Monad                  (forever, void, when)

-- external, IO & system
import qualified UnliftIO.Exception             as UE (bracket)

import           Control.Concurrent             (myThreadId, threadWaitRead)
import           Control.Concurrent.Async       (async, cancel, link, race)
import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, readTBQueue,
                                                 writeTBQueue)
import           Control.Concurrent.STM.TVar    (newTVarIO, readTVarIO,
                                                 writeTVar)
import           Control.Exception              (AsyncException (..),
                                                 SomeException, catch,
                                                 catchJust, throwIO, throwTo)
import           Data.IORef                     (atomicModifyIORef, newIORef,
                                                 writeIORef)
import           Foreign.C.String               (peekCString, withCString)
import           Foreign.C.Types                (CChar, CInt (..))
import           Foreign.Ptr                    (FunPtr, Ptr, freeHaskellFunPtr,
                                                 nullPtr)
import           GHC.IO.FD                      (FD (..))
import           GHC.IO.Handle.FD               (handleToFd)
import           System.Console.Readline        (addHistory, callbackReadChar,
                                                 clearMessage, resetLineState)
import           System.Directory               (XdgDirectory (..),
                                                 createDirectoryIfMissing,
                                                 getXdgDirectory)
import           System.IO                      (BufferMode (..), IOMode (..),
                                                 hGetLine, hIsEOF, hPutStrLn,
                                                 hSetBuffering, openFile,
                                                 stderr, stdin)
import           System.Posix.IO                (stdInput)
import           System.Posix.Signals           hiding (Handler)
import           System.Posix.Terminal          (queryTerminal)
import           System.Posix.Types             (Fd (..))

-- internal
import           P4P.Sim.IO
import           P4P.Sim.Options
import           P4P.Sim.Types


-- TODO: export to upstream readline
type Handler = Ptr CChar -> IO ()
-- fixes NULL segfault
callbackHandlerInstall' :: String -> (Maybe String -> IO ()) -> IO (IO ())
callbackHandlerInstall' prompt lhandler = do
  lhandlerPtr <- exportHandler $ \linePtr -> if nullPtr == linePtr
    then lhandler Nothing
    else peekCString linePtr >>= lhandler . Just
  withCString prompt $ \promptPtr -> do
    rl_callback_handler_install promptPtr lhandlerPtr
  return
    (do
      rl_callback_handler_remove
      freeHaskellFunPtr lhandlerPtr
    )
foreign import ccall "wrapper"
  exportHandler :: Handler -> IO (FunPtr Handler)
foreign import ccall unsafe "rl_callback_handler_install"
  rl_callback_handler_install :: Ptr CChar -> FunPtr Handler -> IO ()
foreign import ccall unsafe "rl_callback_handler_remove"
  rl_callback_handler_remove :: IO ()

clearVisibleLine :: IO ()
clearVisibleLine = void rl_clear_visible_line
foreign import ccall unsafe "rl_clear_visible_line"
  rl_clear_visible_line :: IO CInt

newPromptLine :: IO ()
newPromptLine = do
  clearVisibleLine
  resetLineState
  clearMessage

ghcNoKillOn2CtrlC :: IO ()
ghcNoKillOn2CtrlC = do
  -- https://stackoverflow.com/a/7941166
  tid <- myThreadId
  _ <- installHandler keyboardSignal (Catch (throwTo tid UserInterrupt)) Nothing
  pure ()

onExceptionShow :: String -> IO a -> IO a
onExceptionShow tag io = catch
  io
  (\e ->
    hPutStrLn stderr (tag <> ": " <> show e) >> throwIO (e :: SomeException)
  )

-- | Ignore user interrupts, by running the action again when an interrupt
-- happens. This should be a small action like reading from something.
ignoreUserInterrupt :: IO a -> IO a
ignoreUserInterrupt io = do
  r <- catchJust f (Just <$> io) $ \e -> do
    putStrLn $ "\nUserInterrupt"
    newPromptLine
    pure Nothing
  case r of
    Nothing -> ignoreUserInterrupt io
    Just r' -> pure r'
 where
  f e@UserInterrupt = Just e
  f _               = Nothing

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

-- | Set up a nice prompt if on a terminal, otherwise 'defaultStdIO'.
maybeTerminalStdIO :: Bool -> String -> String -> String -> IO (StdIO, IO ())
maybeTerminalStdIO interactive dirname filename prompt = do
  queryTerminal stdInput >>= \case
    True | interactive -> do
      dir <- getXdgDirectory XdgData dirname
      createDirectoryIfMissing True dir
      let history = dir <> "/" <> filename
      maybeAddHistory <- setupReadlineHistory history

      -- readline BS below
      -- we have to use the async API because the sync API can't be interrupted
      -- and therefore doesn't interact well with exceptions in other threads
      ghcNoKillOn2CtrlC
      input <- newTBQueueIO 1
      hSetBuffering stdin NoBuffering
      cleanup <- callbackHandlerInstall' prompt $ \s -> do
        atomically $ writeTBQueue input s
      fd <- handleToFd stdin
      a  <- async $ forever $ do
        threadWaitRead $ Fd $ fdFD fd
        callbackReadChar
      link a
      let i = ignoreUserInterrupt $ do
            atomically (readTBQueue input) >>= \case
              Nothing -> hPutStrLn stderr "" >> pure Nothing
              Just s  -> maybeAddHistory s >> pure (Just s)
      let o s = do
            clearVisibleLine
            putStrLn $ "\r\x1b[0K" <> s
            newPromptLine
      let f = do
            cancel a
            cleanup
            newPromptLine
            putStrLn ""
      pure ((i, o), f)
    _ -> pure (defaultStdIO, pure ())

optionTerminalStdIO
  :: SimOptions -> String -> String -> String -> IO (StdIO, IO ())
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
