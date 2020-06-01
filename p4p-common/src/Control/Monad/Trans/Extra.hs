{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Trans.Extra where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Writer.CPS
import           Data.Kind                        (Type)


lift2
  :: (MonadTrans t1, MonadTrans t2, Monad (t2 m), Monad m) => m a -> t1 (t2 m) a
lift2 = lift . lift

type family UnMonadTrans (t :: Type -> Type) :: Type -> Type
type instance UnMonadTrans (StateT st m) = m
type instance UnMonadTrans (WriterT st m) = m
