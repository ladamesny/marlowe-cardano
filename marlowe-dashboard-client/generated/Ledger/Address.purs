-- File auto generated by purescript-bridge! --
module Ledger.Address where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut.Core (jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Argonaut.Encode.Aeson as E
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Plutus.V1.Ledger.Crypto (PubKey, PubKeyHash)
import Type.Proxy (Proxy(Proxy))

newtype PaymentPubKey = PaymentPubKey { unPaymentPubKey :: PubKey }

derive instance eqPaymentPubKey :: Eq PaymentPubKey

derive instance ordPaymentPubKey :: Ord PaymentPubKey

instance showPaymentPubKey :: Show PaymentPubKey where
  show a = genericShow a

instance encodeJsonPaymentPubKey :: EncodeJson PaymentPubKey where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { unPaymentPubKey: E.value :: _ PubKey }
    )

instance decodeJsonPaymentPubKey :: DecodeJson PaymentPubKey where
  decodeJson = defer \_ -> D.decode $
    ( PaymentPubKey <$> D.record "PaymentPubKey"
        { unPaymentPubKey: D.value :: _ PubKey }
    )

derive instance genericPaymentPubKey :: Generic PaymentPubKey _

derive instance newtypePaymentPubKey :: Newtype PaymentPubKey _

--------------------------------------------------------------------------------

_PaymentPubKey :: Iso' PaymentPubKey { unPaymentPubKey :: PubKey }
_PaymentPubKey = _Newtype

--------------------------------------------------------------------------------

newtype PaymentPubKeyHash = PaymentPubKeyHash
  { unPaymentPubKeyHash :: PubKeyHash }

derive instance eqPaymentPubKeyHash :: Eq PaymentPubKeyHash

derive instance ordPaymentPubKeyHash :: Ord PaymentPubKeyHash

instance showPaymentPubKeyHash :: Show PaymentPubKeyHash where
  show a = genericShow a

instance encodeJsonPaymentPubKeyHash :: EncodeJson PaymentPubKeyHash where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { unPaymentPubKeyHash: E.value :: _ PubKeyHash }
    )

instance decodeJsonPaymentPubKeyHash :: DecodeJson PaymentPubKeyHash where
  decodeJson = defer \_ -> D.decode $
    ( PaymentPubKeyHash <$> D.record "PaymentPubKeyHash"
        { unPaymentPubKeyHash: D.value :: _ PubKeyHash }
    )

derive instance genericPaymentPubKeyHash :: Generic PaymentPubKeyHash _

derive instance newtypePaymentPubKeyHash :: Newtype PaymentPubKeyHash _

--------------------------------------------------------------------------------

_PaymentPubKeyHash :: Iso' PaymentPubKeyHash
  { unPaymentPubKeyHash :: PubKeyHash }
_PaymentPubKeyHash = _Newtype

--------------------------------------------------------------------------------

newtype StakePubKey = StakePubKey { unStakePubKey :: PubKey }

derive instance eqStakePubKey :: Eq StakePubKey

derive instance ordStakePubKey :: Ord StakePubKey

instance showStakePubKey :: Show StakePubKey where
  show a = genericShow a

instance encodeJsonStakePubKey :: EncodeJson StakePubKey where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { unStakePubKey: E.value :: _ PubKey }
    )

instance decodeJsonStakePubKey :: DecodeJson StakePubKey where
  decodeJson = defer \_ -> D.decode $
    ( StakePubKey <$> D.record "StakePubKey"
        { unStakePubKey: D.value :: _ PubKey }
    )

derive instance genericStakePubKey :: Generic StakePubKey _

derive instance newtypeStakePubKey :: Newtype StakePubKey _

--------------------------------------------------------------------------------

_StakePubKey :: Iso' StakePubKey { unStakePubKey :: PubKey }
_StakePubKey = _Newtype

--------------------------------------------------------------------------------

newtype StakePubKeyHash = StakePubKeyHash { unStakePubKeyHash :: PubKeyHash }

derive instance eqStakePubKeyHash :: Eq StakePubKeyHash

derive instance ordStakePubKeyHash :: Ord StakePubKeyHash

instance showStakePubKeyHash :: Show StakePubKeyHash where
  show a = genericShow a

instance encodeJsonStakePubKeyHash :: EncodeJson StakePubKeyHash where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { unStakePubKeyHash: E.value :: _ PubKeyHash }
    )

instance decodeJsonStakePubKeyHash :: DecodeJson StakePubKeyHash where
  decodeJson = defer \_ -> D.decode $
    ( StakePubKeyHash <$> D.record "StakePubKeyHash"
        { unStakePubKeyHash: D.value :: _ PubKeyHash }
    )

derive instance genericStakePubKeyHash :: Generic StakePubKeyHash _

derive instance newtypeStakePubKeyHash :: Newtype StakePubKeyHash _

--------------------------------------------------------------------------------

_StakePubKeyHash :: Iso' StakePubKeyHash { unStakePubKeyHash :: PubKeyHash }
_StakePubKeyHash = _Newtype