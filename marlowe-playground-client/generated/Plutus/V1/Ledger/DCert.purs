-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.DCert where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut.Core (jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Argonaut.Encode.Aeson as E
import Data.BigInt.Argonaut (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Plutus.V1.Ledger.Credential (StakingCredential)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Type.Proxy (Proxy(Proxy))

data DCert
  = DCertDelegRegKey StakingCredential
  | DCertDelegDeRegKey StakingCredential
  | DCertDelegDelegate StakingCredential PubKeyHash
  | DCertPoolRegister PubKeyHash PubKeyHash
  | DCertPoolRetire PubKeyHash BigInt
  | DCertGenesis
  | DCertMir

derive instance eqDCert :: Eq DCert

derive instance ordDCert :: Ord DCert

instance showDCert :: Show DCert where
  show a = genericShow a

instance encodeJsonDCert :: EncodeJson DCert where
  encodeJson = defer \_ -> case _ of
    DCertDelegRegKey a -> E.encodeTagged "DCertDelegRegKey" a E.value
    DCertDelegDeRegKey a -> E.encodeTagged "DCertDelegDeRegKey" a E.value
    DCertDelegDelegate a b -> E.encodeTagged "DCertDelegDelegate" (a /\ b)
      (E.tuple (E.value >/\< E.value))
    DCertPoolRegister a b -> E.encodeTagged "DCertPoolRegister" (a /\ b)
      (E.tuple (E.value >/\< E.value))
    DCertPoolRetire a b -> E.encodeTagged "DCertPoolRetire" (a /\ b)
      (E.tuple (E.value >/\< E.value))
    DCertGenesis -> encodeJson { tag: "DCertGenesis", contents: jsonNull }
    DCertMir -> encodeJson { tag: "DCertMir", contents: jsonNull }

instance decodeJsonDCert :: DecodeJson DCert where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "DCert"
    $ Map.fromFoldable
        [ "DCertDelegRegKey" /\ D.content (DCertDelegRegKey <$> D.value)
        , "DCertDelegDeRegKey" /\ D.content (DCertDelegDeRegKey <$> D.value)
        , "DCertDelegDelegate" /\ D.content
            (D.tuple $ DCertDelegDelegate </$\> D.value </*\> D.value)
        , "DCertPoolRegister" /\ D.content
            (D.tuple $ DCertPoolRegister </$\> D.value </*\> D.value)
        , "DCertPoolRetire" /\ D.content
            (D.tuple $ DCertPoolRetire </$\> D.value </*\> D.value)
        , "DCertGenesis" /\ pure DCertGenesis
        , "DCertMir" /\ pure DCertMir
        ]

derive instance genericDCert :: Generic DCert _

--------------------------------------------------------------------------------

_DCertDelegRegKey :: Prism' DCert StakingCredential
_DCertDelegRegKey = prism' DCertDelegRegKey case _ of
  (DCertDelegRegKey a) -> Just a
  _ -> Nothing

_DCertDelegDeRegKey :: Prism' DCert StakingCredential
_DCertDelegDeRegKey = prism' DCertDelegDeRegKey case _ of
  (DCertDelegDeRegKey a) -> Just a
  _ -> Nothing

_DCertDelegDelegate :: Prism' DCert { a :: StakingCredential, b :: PubKeyHash }
_DCertDelegDelegate = prism' (\{ a, b } -> (DCertDelegDelegate a b)) case _ of
  (DCertDelegDelegate a b) -> Just { a, b }
  _ -> Nothing

_DCertPoolRegister :: Prism' DCert { a :: PubKeyHash, b :: PubKeyHash }
_DCertPoolRegister = prism' (\{ a, b } -> (DCertPoolRegister a b)) case _ of
  (DCertPoolRegister a b) -> Just { a, b }
  _ -> Nothing

_DCertPoolRetire :: Prism' DCert { a :: PubKeyHash, b :: BigInt }
_DCertPoolRetire = prism' (\{ a, b } -> (DCertPoolRetire a b)) case _ of
  (DCertPoolRetire a b) -> Just { a, b }
  _ -> Nothing

_DCertGenesis :: Prism' DCert Unit
_DCertGenesis = prism' (const DCertGenesis) case _ of
  DCertGenesis -> Just unit
  _ -> Nothing

_DCertMir :: Prism' DCert Unit
_DCertMir = prism' (const DCertMir) case _ of
  DCertMir -> Just unit
  _ -> Nothing