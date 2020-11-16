{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module P4P.RT.EchoProcess where

-- external
import qualified Data.Map.Strict as M

import           Codec.Serialise (Serialise (..))
import           Data.Binary     (Binary (..))
import           GHC.Generics    (Generic)
import           P4P.Proc


data Direction = Rwd | Fwd
  deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

type EchoMsg = (Direction, String)

data EchoState = EState
  { addrs :: ![SockAddr]
  , count :: !Tick
  }
  deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

instance UProtocol EchoState where
  type Msg EchoState = EchoMsg

instance ProcIface EchoState where
  type LoI EchoState = UPMsgI EchoState
  type LoO EchoState = UPMsgO EchoState
  type HiI EchoState = (SockAddr, EchoMsg)
  type HiO EchoState = (SockAddr, EchoMsg)

instance Proc EchoState where
  react i s = case i of
    MsgEnv t                           -> ([], s { count = t })
    MsgHi  (dst, msg)                  -> ([MsgLo (UData dst msg)], s) -- pass it on
    MsgLo  (UData src msg@(dir, body)) -> case dir of
      Rwd -> ([MsgLo (UData src (Fwd, body))], s) -- echo reply back
      Fwd -> ([MsgHi (src, msg)], s) -- forward it to the user
    MsgLo (UOwnAddr obs) -> if null obs
      then ([MsgLo (UOwnAddr (M.fromList $ (, ObsPositive 0) <$> addrs s))], s)
      else error "not implemented"
