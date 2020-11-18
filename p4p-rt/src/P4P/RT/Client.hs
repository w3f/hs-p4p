{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module P4P.RT.Client where

-- external
import           Control.Monad.Extra              (unless, untilJustM, whenJust)
import           Data.Foldable                    (traverse_)
import           GHC.Generics                     (Generic)
import           P4P.Proc                         (Observations, ProcIface (..),
                                                   SockAddr, Tick,
                                                   obsPositiveFromList)
import           Text.Read                        (readEither)

-- external, IO & system
import           Control.Clock.IO                 (Clock (..), newClockSystem)
import           Control.Monad.Catch              (MonadMask, bracket, handle,
                                                   onException)
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
import           System.IO                        (Handle, hGetLine, stdin)
import           System.IO.Error                  (catchIOError, isEOFError)
import           System.Posix.IO                  (stdInput)
import           System.Posix.Terminal            (queryTerminal)

-- internal
import           P4P.RT.Internal                  (RTHiIO,
                                                   defaultRTWouldInteract,
                                                   rtTickInterval)
import           P4P.RT.Network                   (SockEndpoint, fromNAddr,
                                                   resolveEndpoint,
                                                   showSockEndpoint)
import           P4P.RT.Options                   (RTOptions)


type StdIO = (IO (Maybe String), String -> IO ())

hGetLineOrEOF :: Handle -> IO (Maybe String)
hGetLineOrEOF h = catchIOError
  (Just <$> hGetLine h)
  (\e -> if isEOFError e then pure Nothing else ioError e)

defaultStdIO :: StdIO
defaultStdIO = (hGetLineOrEOF stdin, putStrLn)

defaultRTHiIO
  :: Read (HiI ps)
  => Show (HiO ps)
  => (String -> IO (Either e (HiI ps)))
  -> (HiO ps -> IO (Maybe String))
  -> StdIO
  -> RTHiIO ps
defaultRTHiIO readCustom showCustom (getInput, doOutput) =
  let i = untilJustM $ getInput >>= \case
        Nothing -> pure (Just Nothing) -- EOF, quit
        Just s  -> if null s
          then pure Nothing
          else readCustom s >>= \case
            Right r -> pure (Just (Just r))
            Left  _ -> case readEither s of
              Right r -> pure (Just (Just r))
              Left  e -> doOutput e >> pure Nothing
                -- TODO: add some help text, ideally with some introspection
                -- that prints out some generated concrete examples
      o x = showCustom x >>= \case
        Just r  -> doOutput r
        Nothing -> doOutput $ show x
  in  (i, traverse_ o)

defaultRTHiIO'
  :: forall ps . Read (HiI ps) => Show (HiO ps) => StdIO -> RTHiIO ps
defaultRTHiIO' =
  defaultRTHiIO @ps (const (pure (Left ()))) (const (pure Nothing))

-- | A pair that is slightly easier to type
data KV k v = !k :~ !v
  deriving (Eq, Ord, Show, Read, Generic)

bracket2 :: IO (h, IO ()) -> (h -> IO a) -> IO a
bracket2 mkHandles action = bracket mkHandles snd (action . fst)

tryAction :: (MonadIO m, MonadMask m) => InputT m a -> InputT m a
tryAction action = withInterrupt loop
 where
  loop = handle (\Interrupt -> outputStrLn "Input cancelled" >> loop) action

-- | Set up a nice prompt if on a terminal, otherwise 'defaultStdIO'.
maybeTerminalStdIO :: Bool -> String -> String -> String -> IO (StdIO, IO ())
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
          i = flip onException e $ queryInput hd $ tryAction $ do
            s <- getInputLine prompt
            whenJust s $ \s' -> unless (null s') $ do
              modifyHistory (addHistoryUnlessConsecutiveDupe s')
            pure s
          -- Don't use `closeInput hd`, it clashes with `flip E.onException e`
          -- above - if `queryInput` is being called in a separate thread and
          -- an exception is thrown there, `closeInput hd` would deadlock. This
          -- gets broken by GHC RTS but it still looks bad. If `queryInput`
          -- completes with no exception then the cleanup is redundant anyway.
      o <- queryInput hd getExternalPrint
      pure ((i, o), e)
    _ -> pure (defaultStdIO, pure ())

optionTerminalStdIO
  :: RTOptions log -> String -> String -> String -> IO (StdIO, IO ())
optionTerminalStdIO opt = maybeTerminalStdIO (defaultRTWouldInteract opt)

initializeTick :: RTOptions log -> IO Tick
initializeTick opt = newClockSystem (rtTickInterval opt) >>= clockNow

initializeTickAddrs
  :: RTOptions log -> SockEndpoint -> IO (Tick, Observations SockAddr)
initializeTickAddrs opt ep = resolveEndpoint ep >>= \case
  [] -> fail $ "could not resolve hostname: " <> showSockEndpoint ep
  l  -> do
    tick <- initializeTick opt
    pure (tick, obsPositiveFromList tick $ fromNAddr <$> l)
