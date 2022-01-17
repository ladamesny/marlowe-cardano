-- File auto generated by servant-purescript! --
module Marlowe where

import Prelude

import Affjax (defaultRequest, request)
import Affjax.RequestBody (json) as Request
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (json) as Response
import Cardano.Wallet.Mock.Types (WalletInfo)
import Component.Contacts.Types (WalletId)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader.Class (class MonadAsk, asks)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Argonaut.Encode.Aeson as E
import Data.Array (fromFoldable, null)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Effect.Aff.Class (class MonadAff, liftAff)
import Marlowe.Run.Wallet.V1 (GetTotalFundsResponse)
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types
  ( CheckPostData
  , RestoreError
  , RestorePostData
  )
import Servant.PureScript
  ( class ToURLPiece
  , AjaxError
  , ErrorDescription(..)
  , toURLPiece
  )

foreign import encodeURIComponent :: String -> String

type SPSettings_
  =
  { baseURL :: String
  }

class HasSPSettings a where
  spSettings :: a -> SPSettings_

getApiVersion
  :: forall env m
   . HasSPSettings env
  => MonadAsk env m
  => MonadError AjaxError m
  => MonadAff m
  => m String
getApiVersion = do
  spSettings <- asks spSettings
  let baseURL = spSettings.baseURL
  let httpMethod = Left GET
  let
    encodeQueryItem :: forall a. ToURLPiece a => String -> a -> String
    encodeQueryItem name val = name <> "=" <> toURLPiece val
  let
    queryArgs :: Array String
    queryArgs =
      []
  let
    queryString = if null queryArgs then "" else "?" <> (joinWith "&" queryArgs)
  let
    reqURL =
      baseURL
        <> "api"
        <> "/"
        <> "version"
        <> queryString
  let
    reqHeaders =
      [
      ]
  let
    affReq =
      defaultRequest
        { method = httpMethod
        , url = reqURL
        , headers = defaultRequest.headers <> reqHeaders
        , responseFormat = Response.json
        }
  let
    decoder =
      D.value
  result <- liftAff $ request affReq
  response <- case result of
    Left err -> throwError $
      { request: affReq, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: affReq, description: UnexpectedHTTPStatus response }
  case D.decode decoder response.body of
    Left err -> throwError $ { request: affReq, description: DecodingError err }
    Right body -> pure body

getApiWalletV1ByWalletidTotalfunds
  :: forall env m
   . HasSPSettings env
  => MonadAsk env m
  => MonadError AjaxError m
  => MonadAff m
  => WalletId
  -> m GetTotalFundsResponse
getApiWalletV1ByWalletidTotalfunds wallet_id = do
  spSettings <- asks spSettings
  let baseURL = spSettings.baseURL
  let httpMethod = Left GET
  let
    encodeQueryItem :: forall a. ToURLPiece a => String -> a -> String
    encodeQueryItem name val = name <> "=" <> toURLPiece val
  let
    queryArgs :: Array String
    queryArgs =
      []
  let
    queryString = if null queryArgs then "" else "?" <> (joinWith "&" queryArgs)
  let
    reqURL =
      baseURL
        <> "api"
        <> "/"
        <> "wallet"
        <> "/"
        <> "v1"
        <> "/"
        <> encodeURIComponent (toURLPiece wallet_id)
        <> "/"
        <> "total-funds"
        <> queryString
  let
    reqHeaders =
      [
      ]
  let
    affReq =
      defaultRequest
        { method = httpMethod
        , url = reqURL
        , headers = defaultRequest.headers <> reqHeaders
        , responseFormat = Response.json
        }
  let
    decoder =
      D.value
  result <- liftAff $ request affReq
  response <- case result of
    Left err -> throwError $
      { request: affReq, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: affReq, description: UnexpectedHTTPStatus response }
  case D.decode decoder response.body of
    Left err -> throwError $ { request: affReq, description: DecodingError err }
    Right body -> pure body

postApiWalletV1CentralizedtestnetRestore
  :: forall env m
   . HasSPSettings env
  => MonadAsk env m
  => MonadError AjaxError m
  => MonadAff m
  => RestorePostData
  -> m (Either RestoreError WalletInfo)
postApiWalletV1CentralizedtestnetRestore reqBody = do
  spSettings <- asks spSettings
  let baseURL = spSettings.baseURL
  let httpMethod = Left POST
  let
    encodeQueryItem :: forall a. ToURLPiece a => String -> a -> String
    encodeQueryItem name val = name <> "=" <> toURLPiece val
  let
    queryArgs :: Array String
    queryArgs =
      []
  let
    queryString = if null queryArgs then "" else "?" <> (joinWith "&" queryArgs)
  let
    reqURL =
      baseURL
        <> "api"
        <> "/"
        <> "wallet"
        <> "/"
        <> "v1"
        <> "/"
        <> "centralized-testnet"
        <> "/"
        <> "restore"
        <> queryString
  let
    reqHeaders =
      [
      ]
  let
    affReq =
      defaultRequest
        { method = httpMethod
        , url = reqURL
        , headers = defaultRequest.headers <> reqHeaders
        , responseFormat = Response.json
        , content = Just
            $ Request.json
            $ flip E.encode reqBody
            $ E.value
        }
  let
    decoder =
      (D.either D.value D.value)
  result <- liftAff $ request affReq
  response <- case result of
    Left err -> throwError $
      { request: affReq, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: affReq, description: UnexpectedHTTPStatus response }
  case D.decode decoder response.body of
    Left err -> throwError $ { request: affReq, description: DecodingError err }
    Right body -> pure body

postApiWalletV1CentralizedtestnetCheckmnemonic
  :: forall env m
   . HasSPSettings env
  => MonadAsk env m
  => MonadError AjaxError m
  => MonadAff m
  => CheckPostData
  -> m Boolean
postApiWalletV1CentralizedtestnetCheckmnemonic reqBody = do
  spSettings <- asks spSettings
  let baseURL = spSettings.baseURL
  let httpMethod = Left POST
  let
    encodeQueryItem :: forall a. ToURLPiece a => String -> a -> String
    encodeQueryItem name val = name <> "=" <> toURLPiece val
  let
    queryArgs :: Array String
    queryArgs =
      []
  let
    queryString = if null queryArgs then "" else "?" <> (joinWith "&" queryArgs)
  let
    reqURL =
      baseURL
        <> "api"
        <> "/"
        <> "wallet"
        <> "/"
        <> "v1"
        <> "/"
        <> "centralized-testnet"
        <> "/"
        <> "check-mnemonic"
        <> queryString
  let
    reqHeaders =
      [
      ]
  let
    affReq =
      defaultRequest
        { method = httpMethod
        , url = reqURL
        , headers = defaultRequest.headers <> reqHeaders
        , responseFormat = Response.json
        , content = Just
            $ Request.json
            $ flip E.encode reqBody
            $ E.value
        }
  let
    decoder =
      D.value
  result <- liftAff $ request affReq
  response <- case result of
    Left err -> throwError $
      { request: affReq, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: affReq, description: UnexpectedHTTPStatus response }
  case D.decode decoder response.body of
    Left err -> throwError $ { request: affReq, description: DecodingError err }
    Right body -> pure body
