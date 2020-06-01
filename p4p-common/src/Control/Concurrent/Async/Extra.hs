{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Control.Concurrent.Async.Extra where

import           Control.Concurrent.Async       (AsyncCancelled (..), async,
                                                 cancel, pollSTM)
import           Control.Concurrent.STM         (atomically, retry)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, readTBQueue,
                                                 tryPeekTBQueue, writeTBQueue)
import           Control.Concurrent.STM.TVar    (newTVarIO, readTVar,
                                                 readTVarIO, stateTVar,
                                                 writeTVar)
import           Control.Exception              (Exception, SomeException,
                                                 fromException, mask, throwIO,
                                                 tryJust)
import           Control.Monad.Extra            (whileM)
import           Data.Foldable                  (for_)
import           Data.Function                  (on)
import           Data.List                      (minimumBy)
import           Data.Maybe                     (catMaybes, isNothing)
import           Data.Traversable               (for)


notCancel :: SomeException -> Maybe SomeException
notCancel e | Just AsyncCancelled <- fromException e = Nothing
            | otherwise                              = Just e

untry :: Exception e => Either e a -> IO a
untry = \case
  Left  e -> throwIO e
  Right r -> pure r

{- | Combine and interleave a bunch of repeated IO actions. -}
foreverInterleave :: [IO a] -> IO (IO (Maybe a), IO ())
foreverInterleave actions = do
  -- queue of results, to hold as they arrive
  -- this really should have capacity 0 not 1, but:
  -- https://github.com/haskell/stm/issues/28
  qos      <- for actions (const (newTBQueueIO 1))
  -- counter to ensure arrival order
  c        <- newTVarIO (0 :: Int)
  -- tracks a shutdown request
  shutdown <- newTVarIO False

  ths      <- for (zip actions qos) $ \(action, qo) -> do
    async $ mask $ \restore -> do
      whileM $ readTVarIO shutdown >>= \case
        True  -> pure False
        False -> do
          -- keep running action, catching exceptions until shutdown
          out <- restore (tryJust notCancel action)
          atomically $ do
            -- store the output with its arrival index
            c' <- stateTVar c (\i -> (i, succ i))
            writeTBQueue qo (c', out)
          pure True

  let getElem = (traverse untry =<<) $ atomically $ do
        outs <- fmap catMaybes $ for qos $ \qo -> do
          fmap (, qo) <$> tryPeekTBQueue qo
        if not (null outs)
          then do
            let ((_, out), qo) = minimumBy (compare `on` fst . fst) outs
            _ <- readTBQueue qo -- same as out
            pure (Just out)
          else readTVar shutdown >>= \case
            True -> do
              -- shutdown, check if any further results expected
              expects <- for ths $ fmap isNothing . pollSTM
              if True `notElem` expects
                then pure Nothing -- nothing expected, finished
                else retry -- stuff still active, retry
            False -> retry -- assume stuff still active, retry

      cancelAll = do
        atomically $ writeTVar shutdown True
        for_ ths cancel

  pure (getElem, cancelAll)
