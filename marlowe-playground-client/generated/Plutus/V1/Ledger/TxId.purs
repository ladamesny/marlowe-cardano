-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.TxId where

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
import Type.Proxy (Proxy(Proxy))

newtype TxId = TxId { getTxId :: String }

derive instance eqTxId :: Eq TxId

derive instance ordTxId :: Ord TxId

instance showTxId :: Show TxId where
  show a = genericShow a

instance encodeJsonTxId :: EncodeJson TxId where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { getTxId: E.value :: _ String }
    )

instance decodeJsonTxId :: DecodeJson TxId where
  decodeJson = defer \_ -> D.decode $
    (TxId <$> D.record "TxId" { getTxId: D.value :: _ String })

derive instance genericTxId :: Generic TxId _

derive instance newtypeTxId :: Newtype TxId _

--------------------------------------------------------------------------------

_TxId :: Iso' TxId { getTxId :: String }
_TxId = _Newtype