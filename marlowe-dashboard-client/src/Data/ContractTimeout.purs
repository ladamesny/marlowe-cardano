module Data.ContractTimeout
  ( ContractTimeout
  , ContractTimeoutError(..)
  , fromBigInt
  , fromString
  , toString
  , toBigInt
  ) where

import Prologue

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(..)
  , decodeJson
  )
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic
  ( genericCardinality
  , genericFromEnum
  , genericPred
  , genericSucc
  , genericToEnum
  )
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (null)

data ContractTimeoutError
  = Empty
  | Past
  | Invalid

derive instance genericContractTimeoutError :: Generic ContractTimeoutError _
derive instance eqContractTimeoutError :: Eq ContractTimeoutError
derive instance ordContractTimeoutError :: Ord ContractTimeoutError

instance boundedContractTimeoutError :: Bounded ContractTimeoutError where
  bottom = genericBottom
  top = genericTop

instance enumContractTimeoutError :: Enum ContractTimeoutError where
  succ = genericSucc
  pred = genericPred

instance boundedEnumContractTimeoutError :: BoundedEnum ContractTimeoutError where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance showContractTimeoutError :: Show ContractTimeoutError where
  show = genericShow

newtype ContractTimeout = ContractTimeout BigInt

derive instance Eq ContractTimeout
derive instance Ord ContractTimeout
derive newtype instance Show ContractTimeout
derive newtype instance EncodeJson ContractTimeout

instance DecodeJson ContractTimeout where
  decodeJson =
    lmap (const $ TypeMismatch "ContractTimeout") <<< fromString
      <=< decodeJson

fromString :: String -> Either ContractTimeoutError ContractTimeout
fromString s
  | null s = Left Empty
  | otherwise = case BigInt.fromString s of
      Nothing -> Left Invalid
      Just i -> fromBigInt $ BigInt.fromInt 60 * i

fromBigInt :: BigInt -> Either ContractTimeoutError ContractTimeout
fromBigInt i
  | i < zero = Left Past
  | otherwise = Right $ ContractTimeout i

toString :: ContractTimeout -> String
toString = BigInt.toString <<< (_ / BigInt.fromInt 60) <<< toBigInt

toBigInt :: ContractTimeout -> BigInt
toBigInt (ContractTimeout i) = i
