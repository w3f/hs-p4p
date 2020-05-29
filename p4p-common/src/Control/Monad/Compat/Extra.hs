{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-error=orphans #-}

-- | Various compatibility shims, should only be temporary.
module Control.Monad.Compat.Extra where

-- external
import qualified Control.Monad.Trans.RWS.CPS    as CPSRWS (RWST, get, put,
                                                           state)

import           Control.Monad.State.Class
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Writer.CPS as CPS


-- https://github.com/haskell/mtl/issues/69
instance Monad m => MonadState s (CPSRWS.RWST r w s m) where
  get   = CPSRWS.get
  put   = CPSRWS.put
  state = CPSRWS.state

-- https://github.com/haskell/mtl/issues/69
instance MonadState s m => MonadState s (CPS.WriterT w m) where
  get   = lift get
  put   = lift . put
  state = lift . state
