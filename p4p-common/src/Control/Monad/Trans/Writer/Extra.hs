module Control.Monad.Trans.Writer.Extra where

import           Control.Monad.Trans.Writer.CPS (WriterT, tell)

tell1 :: Monad m => a -> WriterT [a] m ()
tell1 a = tell [a]
