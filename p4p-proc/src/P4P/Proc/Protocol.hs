{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module P4P.Proc.Protocol where

-- external
import qualified Data.Set                         as S

import           Control.Lens                     (Prism, review)
import           Control.Lens.Extra               (matching')
import           Control.Monad.Trans.State.Strict (runState, state)
import           Data.ByteString                  (ByteString)
import           Data.Kind                        (Type)
import           Data.Schedule                    (HasNow)
import           Data.Void                        (Void)

-- internal
import           P4P.Proc.Internal
import           P4P.Proc.Types


{- | Protocol sending unordered unreliable datagrams, the simplest protocol.

When defining this instance for your @$ps@, you should also define:

@
instance 'ProcIface' $ps where
  type LoI $ps = 'UPMsgI' $ps
  type LoO $ps = 'UPMsgO' $ps
  -- remaining defs ..
@

See 'UMsg' for more details.

'HasNow' is a superclass, in order to support record-and-replay.
-}
class (HasNow ps, LoI ps ~ UPMsgI ps, LoO ps ~ UPMsgO ps) => UProtocol ps where
  {- | Entity address, used for sending and receiving messages.

  Depending on the protocol, this may or may not uniquely identify the entity.
  -}
  type Addr ps :: Type
  type Addr ps = SockAddr
  -- | Main protocol message type, for external communication between entities.
  type XMsg ps :: Type

  -- | Get the current receive addresses from the protocol state.
  --
  -- This is needed in order to support record-and-replay.
  getAddrs :: ps -> S.Set (Addr ps)

-- | Type alias for the address of a process.
type ProcAddr p = Addr (State p)

type UMsgI addr msg = UMsg 'Incoming addr msg
type UMsgO addr msg = UMsg 'Outgoing addr msg

type UPMsgI ps = UMsgI (Addr ps) ByteString
type UPMsgO ps = UMsgO (Addr ps) ByteString

-- | Variant of 'UPMsgI' that preserves the external message type.
type UPMsgI' ps = UMsgI (Addr ps) (ExtVal (XMsg ps))
-- | Variant of 'UPMsgO' that preserves the external message type.
type UPMsgO' ps = UMsgO (Addr ps) (ExtVal (XMsg ps))

-- | Variant of 'PMsgI' that preserves the external message type.
type PMsgI' ps = GMsgI (EnvI ps) (UPMsgI' ps) (HiI ps) Void
-- | Variant of 'PMsgO' that preserves the external message type.
type PMsgO' ps = GMsgO (EnvO ps) (UPMsgO' ps) (HiO ps) (AuxO ps)
-- | Variant of 'PMsgO_' that preserves the external message type.
type PMsgO_' ps = GMsgO (EnvO ps) (UPMsgO' ps) (HiO ps) Void

withCodecF
  :: forall ps f
   . UProtocol ps
  => Functor f
  => Codec (XMsg ps)
  -> (ProtocolCodecError -> AuxO ps)
  -> (PMsgI' ps -> f [PMsgO' ps])
  -> (PMsgI ps -> f [PMsgO ps])
withCodecF codec mkLog rx i = case prs `matching'` i of
  Left  i'        -> (>>= encOut) <$> rx i'
  Right (src, bs) -> case codecDec bs of
    Left (Left err) -> fmap (mkLog' (DecodeMalformed err) :) $ do
      (>>= encOut) <$> rx (review prs (src, Mal bs))
    Left (Right l) -> fmap (mkLog' (DecodeLeftovers l) :) $ do
      (>>= encOut) <$> rx (review prs (src, Mal bs))
    Right v -> (>>= encOut) <$> rx (review prs (src, Val v))
 where
  Codec {..} = codec
  prs
    :: Prism
         (GMsg dir e (UMsg dir addr msg0) h a)
         (GMsg dir e (UMsg dir addr msg1) h a)
         (addr, msg0)
         (addr, msg1)
  prs    = _MsgLo . _UData
  mkLog' = MsgAux . mkLog
  encOut :: PMsgO' ps -> [PMsgO ps]
  encOut o = case prs `matching'` o of
    Left  o'           -> pure o'
    Right (dst, Val v) -> case codecEnc v of
      Left  l  -> [mkLog' (EncodeTooBig l)]
      Right bs -> [review prs (dst, bs)]
    Right (dst, Mal _) -> [mkLog' EncodeMalformed]

withCodec
  :: forall ps
   . UProtocol ps
  => Codec (XMsg ps)
  -> (ProtocolCodecError -> AuxO ps)
  -> (PMsgI' ps -> ps -> ([PMsgO' ps], ps))
  -> (PMsgI ps -> ps -> ([PMsgO ps], ps))
withCodec codec mkLog rx = runState . withCodecF @ps codec mkLog (state . rx)
