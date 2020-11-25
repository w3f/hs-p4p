{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module P4P.Proc.Types where

-- external
import qualified Data.Binary          as BN
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict      as M
import qualified Data.Set             as S
import qualified Data.Text            as T

import           Codec.Serialise      (Serialise (..), deserialiseFullyOrFail,
                                       serialise)
import           Control.Lens.TH      (makePrisms)
import           Control.Op
import           Data.Binary          (Binary (..))
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Function        (on)
import           Data.Schedule        (Tick)
import           Data.Text            (Text)
import           Data.Word
import           GHC.Generics         (Generic)


type PortNumber = Word16
type HostAddress = Word32
type HostAddress6 = (Word32, Word32, Word32, Word32)
type FlowInfo = Word32
type ScopeID = Word32

{- | Default type of address.

Same as t'Network.Socket.SockAddr' but fully-strict with sane instances.
-}
data SockAddr =
    SockAddrInet !PortNumber !HostAddress
  | SockAddrInet6 !PortNumber !FlowInfo !HostAddress6 !ScopeID
  | SockAddrUnix !ByteString
  deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)
makePrisms ''SockAddr

data Observation t = ObsPositive !t | ObsNegative !t
 deriving (Eq, Show, Read, Generic, Binary, Serialise)
makePrisms ''Observation

obsToPair :: Observation t -> (t, Bool)
obsToPair = \case
  ObsPositive t -> (t, True)
  ObsNegative t -> (t, False)

instance Ord t => Ord (Observation t) where
  compare = compare `on` obsToPair

obsIsPositive :: Observation a -> Bool
obsIsPositive = \case
  ObsPositive _ -> True
  ObsNegative _ -> False

obsIsNegative :: Observation a -> Bool
obsIsNegative = \case
  ObsPositive _ -> False
  ObsNegative _ -> True

type Observations a = M.Map a (Observation Tick)

obsPositiveFromList :: Ord a => Tick -> [a] -> Observations a
obsPositiveFromList tick addrs = M.fromList $ (, ObsPositive tick) <$> addrs

obsPositiveToSet :: Observations a -> S.Set a
obsPositiveToSet = M.keysSet . M.filter obsIsPositive

updateTrustedObs :: Ord a => Observations a -> Observations a -> Observations a
updateTrustedObs = M.unionWith max

{- | Typed value meant to be transmitted externally.

This must have the same encoding as (strict) 'ByteString' in every codec used
for external transmission. That is, the constructors of this data type must
*not* be represented.

Codecs used for internal purposes e.g. 'Show'/'Read' for logging, may use their
normal encoding that exposes the constructors.
-}
data ExtVal a =
    Mal !ByteString
    -- ^ malformed string that failed decoding
  | Val !a
    -- ^ a well-formed value that can be encoded
  deriving (Eq, Ord, Show, Read, Generic)
makePrisms ''ExtVal

cborEncode :: Serialise a => a -> ByteString
cborEncode = toStrict . serialise

cborDecode :: Serialise a => ByteString -> Either (Either Text Int) a
cborDecode bs = case deserialiseFullyOrFail (fromStrict bs) of
  Left  (Left  err) -> Left (Left (T.pack (show err)))
  Left  (Right r  ) -> Left (Right (BS.length (toStrict r)))
  Right a           -> Right a

instance Serialise a => Serialise (ExtVal a) where
  encode = \case
    Mal bs -> encode bs
    Val a  -> encode (cborEncode a)
  decode = do
    decode >$> \bs -> cborDecode bs |> \case
      Left  _   -> Mal bs
      Right val -> Val val

haskEncode :: Binary a => a -> ByteString
haskEncode = toStrict . BN.encode

haskDecode :: Binary a => ByteString -> Either (Either Text Int) a
haskDecode bs = case BN.decodeOrFail (fromStrict bs) of
  Left err -> Left (Left (T.pack (show err)))
  Right (r, _, a) ->
    if LBS.null r then Right a else Left (Right (BS.length (toStrict r)))

instance Binary a => Binary (ExtVal a) where
  put = \case
    Mal bs -> put bs
    Val a  -> put (haskEncode a)
  get = do
    get >$> \bs -> haskDecode bs |> \case
      Left  _   -> Mal bs
      Right val -> Val val

data Codec a = Codec
  { codecEnc :: !(a -> Either Int ByteString)
    -- ^ encode a value, or how many bytes it was too big by
  , codecDec :: !(ByteString -> Either (Either Text Int) a)
    -- ^ decode some bytes, or show the decode error, or the size of leftovers
  }

_mkCodec
  :: (a -> ByteString)
  -> (ByteString -> Either (Either Text Int) a)
  -> Int
  -> Codec a
_mkCodec enc dec maxLen = Codec { .. }
 where
  codecEnc a =
    let bs = enc a
        l  = BS.length bs
    in  if l > maxLen then Left (l - maxLen) else Right bs
  codecDec bs =
    let l = BS.length bs
    in  if l > maxLen then Left (Right (l - maxLen)) else dec bs

-- | CBOR codec with a maximum payload length of 2^16 = 65536.
cborCodec16 :: Serialise a => Codec a
cborCodec16 = _mkCodec cborEncode cborDecode 65536

-- | Haskell ('Binary') codec with a maximum payload length of 2^16 = 65536.
haskCodec16 :: Binary a => Codec a
haskCodec16 = _mkCodec haskEncode haskDecode 65536

data ProtocolCodecError =
   DecodeMalformed !Text
   -- ^ decode payload was malformed, with the given error message
 | DecodeLeftovers !Int
   -- ^ decode payload had nonzero amount of leftovers
 | EncodeTooBig !Int
   -- ^ encode gave payload too big by this length
 | EncodeMalformed
   -- ^ got 'Mal' to encode which is inappropriate
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)

data Direction = Incoming | Outgoing
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)
makePrisms ''Direction

{- | General message to/from a process. -}
data GMsg (dir :: Direction) e l h a
  = MsgEnv !e
  -- ^ Message from/to the environment.
  | MsgLo !l
  -- ^ Message from/to the lower layer.
  | MsgHi !h
  -- ^ Message from/to the higher layer.
  | MsgAux !a
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)
makePrisms ''GMsg

-- | Local message that encodes actions within t'P4P.Proc.Protocol.UProtocol'.
--
-- For simplicity, we reuse this in both directions.
data UMsg (dir :: Direction) addr msg
  = UData !addr !msg
    {- ^ Communicate with another entity.

    For 'Incoming' direction, we are receiving the message, and the address is
    the source of the message.

    For 'Outgoing' direction, we are sending the message, and the address is
    the destination of the message.
    -}
  | UOwnAddr !(Observations addr)
    {- ^ Observations about the receive-address(es) of a process.

    For 'Incoming' direction, the other component is telling us about addresses
    they observed. For example if they failed to bind to an address, we would
    get a negative observation for that address. If the whole observation is
    'Data.Foldable.null', we should take this as an implicit request to send
    them our view of what our addresses should be.

    For 'Outgoing' direction, we are telling the other component what our
    addresses should be. This may or may not actually succeed; if this matters
    to us  then we should watch for replies back from the other component and
    take action if we don't get a timely confirmation.
    -}
 deriving (Eq, Ord, Show, Read, Generic, Binary, Serialise)
makePrisms ''UMsg
