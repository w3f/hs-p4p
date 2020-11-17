{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}

module P4P.RT.Client
  ( StdIO
  , onExceptionShow
  , bracketHEF
  , maybeTerminalStdIO
  , optionTerminalStdIO
  )
where

-- external
import           Control.Monad.Extra              (unless, whenJust)

-- external, IO & system
import qualified Control.Exception                as E

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
import           System.Console.Haskeline.IO      (cancelInput, initializeInput,
                                                   queryInput)
import           System.Directory                 (XdgDirectory (..),
                                                   createDirectoryIfMissing,
                                                   getXdgDirectory)
import           System.IO                        (Handle, hGetLine, hPutStrLn,
                                                   stderr, stdin)
import           System.IO.Error                  (catchIOError, isEOFError)
import           System.Posix.IO                  (stdInput)
import           System.Posix.Terminal            (queryTerminal)

-- internal
import           P4P.RT.Internal                  (defaultRTWouldInteract)
import           P4P.RT.Options                   (RTOptions)


type StdIO = (IO (Maybe String), String -> IO ())

hGetLineOrEOF :: Handle -> IO (Maybe String)
hGetLineOrEOF h = catchIOError
  (Just <$> hGetLine h)
  (\e -> if isEOFError e then pure Nothing else ioError e)

defaultStdIO :: StdIO
defaultStdIO = (hGetLineOrEOF stdin, putStrLn)

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
  :: Bool -> String -> String -> String -> IO ((Bool, StdIO), IO (), IO ())
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
          -- `queryInput hd x` actually runs `x` in a separate thread, so when
          -- this thread receives an exception, we want to cancel that thread
          -- using `cancelInput`
          i = flip E.onException e $ queryInput hd $ tryAction $ do
            s <- getInputLine prompt
            whenJust s $ \s' -> unless (null s') $ do
              modifyHistory (addHistoryUnlessConsecutiveDupe s')
            pure s
          -- Don't use `closeInput hd`, it clashes with `flip E.onException e`
          -- above - if `queryInput` is being called in a separate thread and
          -- an exception is thrown there, `closeInput hd` would deadlock. This
          -- gets broken by GHC RTS but it still looks bad. If `queryInput`
          -- completes with no exception then the cleanup is redundant anyway.
          f = pure ()
      o <- queryInput hd getExternalPrint
      pure ((True, (i, o)), e, f)
    _ -> pure ((False, defaultStdIO), pure (), pure ())

optionTerminalStdIO
  :: RTOptions -> String -> String -> String -> IO ((Bool, StdIO), IO (), IO ())
optionTerminalStdIO opt = maybeTerminalStdIO (defaultRTWouldInteract opt)
