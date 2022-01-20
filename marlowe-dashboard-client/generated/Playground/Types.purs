-- File auto generated by purescript-bridge! --
module Playground.Types where

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
import Data.List.Types (NonEmptyList)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Ledger.CardanoWallet (WalletNumber)
import Plutus.V1.Ledger.Slot (Slot)
import Plutus.V1.Ledger.Value (TokenName, Value)
import Type.Proxy (Proxy(Proxy))
import Wallet.Types (EndpointDescription)

newtype FunctionSchema a = FunctionSchema
  { endpointDescription :: EndpointDescription
  , argument :: a
  }

derive instance functorFunctionSchema :: Functor FunctionSchema

instance showFunctionSchema :: (Show a) => Show (FunctionSchema a) where
  show a = genericShow a

derive instance eqFunctionSchema :: (Eq a) => Eq (FunctionSchema a)

instance encodeJsonFunctionSchema ::
  ( EncodeJson a
  ) =>
  EncodeJson (FunctionSchema a) where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { endpointDescription: E.value :: _ EndpointDescription
        , argument: E.value :: _ a
        }
    )

instance decodeJsonFunctionSchema ::
  ( DecodeJson a
  ) =>
  DecodeJson (FunctionSchema a) where
  decodeJson = defer \_ -> D.decode $
    ( FunctionSchema <$> D.record "FunctionSchema"
        { endpointDescription: D.value :: _ EndpointDescription
        , argument: D.value :: _ a
        }
    )

derive instance genericFunctionSchema :: Generic (FunctionSchema a) _

derive instance newtypeFunctionSchema :: Newtype (FunctionSchema a) _

--------------------------------------------------------------------------------

_FunctionSchema
  :: forall a
   . Iso' (FunctionSchema a)
       { endpointDescription :: EndpointDescription, argument :: a }
_FunctionSchema = _Newtype

--------------------------------------------------------------------------------

newtype KnownCurrency = KnownCurrency
  { hash :: String
  , friendlyName :: String
  , knownTokens :: NonEmptyList TokenName
  }

instance showKnownCurrency :: Show KnownCurrency where
  show a = genericShow a

derive instance eqKnownCurrency :: Eq KnownCurrency

instance encodeJsonKnownCurrency :: EncodeJson KnownCurrency where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { hash: E.value :: _ String
        , friendlyName: E.value :: _ String
        , knownTokens: E.value :: _ (NonEmptyList TokenName)
        }
    )

instance decodeJsonKnownCurrency :: DecodeJson KnownCurrency where
  decodeJson = defer \_ -> D.decode $
    ( KnownCurrency <$> D.record "KnownCurrency"
        { hash: D.value :: _ String
        , friendlyName: D.value :: _ String
        , knownTokens: D.value :: _ (NonEmptyList TokenName)
        }
    )

derive instance genericKnownCurrency :: Generic KnownCurrency _

derive instance newtypeKnownCurrency :: Newtype KnownCurrency _

--------------------------------------------------------------------------------

_KnownCurrency :: Iso' KnownCurrency
  { hash :: String
  , friendlyName :: String
  , knownTokens :: NonEmptyList TokenName
  }
_KnownCurrency = _Newtype

--------------------------------------------------------------------------------

data ContractCall a
  = CallEndpoint
      { caller :: WalletNumber
      , argumentValues :: FunctionSchema a
      }
  | AddBlocks { blocks :: BigInt }
  | AddBlocksUntil { slot :: Slot }
  | PayToWallet
      { sender :: WalletNumber
      , recipient :: WalletNumber
      , amount :: Value
      }

instance showContractCall :: (Show a) => Show (ContractCall a) where
  show a = genericShow a

derive instance eqContractCall :: (Eq a) => Eq (ContractCall a)

instance encodeJsonContractCall :: (EncodeJson a) => EncodeJson (ContractCall a) where
  encodeJson = defer \_ -> case _ of
    CallEndpoint { caller, argumentValues } -> encodeJson
      { tag: "CallEndpoint"
      , caller: flip E.encode caller E.value
      , argumentValues: flip E.encode argumentValues E.value
      }
    AddBlocks { blocks } -> encodeJson
      { tag: "AddBlocks"
      , blocks: flip E.encode blocks E.value
      }
    AddBlocksUntil { slot } -> encodeJson
      { tag: "AddBlocksUntil"
      , slot: flip E.encode slot E.value
      }
    PayToWallet { sender, recipient, amount } -> encodeJson
      { tag: "PayToWallet"
      , sender: flip E.encode sender E.value
      , recipient: flip E.encode recipient E.value
      , amount: flip E.encode amount E.value
      }

instance decodeJsonContractCall :: (DecodeJson a) => DecodeJson (ContractCall a) where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "ContractCall"
    $ Map.fromFoldable
        [ "CallEndpoint" /\
            ( CallEndpoint <$> D.object "CallEndpoint"
                { caller: D.value :: _ WalletNumber
                , argumentValues: D.value :: _ (FunctionSchema a)
                }
            )
        , "AddBlocks" /\
            (AddBlocks <$> D.object "AddBlocks" { blocks: D.value :: _ BigInt })
        , "AddBlocksUntil" /\
            ( AddBlocksUntil <$> D.object "AddBlocksUntil"
                { slot: D.value :: _ Slot }
            )
        , "PayToWallet" /\
            ( PayToWallet <$> D.object "PayToWallet"
                { sender: D.value :: _ WalletNumber
                , recipient: D.value :: _ WalletNumber
                , amount: D.value :: _ Value
                }
            )
        ]

derive instance genericContractCall :: Generic (ContractCall a) _

--------------------------------------------------------------------------------

_CallEndpoint
  :: forall a
   . Prism' (ContractCall a)
       { caller :: WalletNumber, argumentValues :: FunctionSchema a }
_CallEndpoint = prism' CallEndpoint case _ of
  (CallEndpoint a) -> Just a
  _ -> Nothing

_AddBlocks :: forall a. Prism' (ContractCall a) { blocks :: BigInt }
_AddBlocks = prism' AddBlocks case _ of
  (AddBlocks a) -> Just a
  _ -> Nothing

_AddBlocksUntil :: forall a. Prism' (ContractCall a) { slot :: Slot }
_AddBlocksUntil = prism' AddBlocksUntil case _ of
  (AddBlocksUntil a) -> Just a
  _ -> Nothing

_PayToWallet
  :: forall a
   . Prism' (ContractCall a)
       { sender :: WalletNumber, recipient :: WalletNumber, amount :: Value }
_PayToWallet = prism' PayToWallet case _ of
  (PayToWallet a) -> Just a
  _ -> Nothing