-- File auto generated by purescript-bridge! --
module Language.Haskell.Interpreter where

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
import Data.Tuple.Nested ((/\))
import Type.Proxy (Proxy(Proxy))

newtype SourceCode = SourceCode String

instance encodeJsonSourceCode :: EncodeJson SourceCode where
  encodeJson = defer \_ -> E.encode $ unwrap >$< E.value

instance decodeJsonSourceCode :: DecodeJson SourceCode where
  decodeJson = defer \_ -> D.decode $ (SourceCode <$> D.value)

derive instance genericSourceCode :: Generic SourceCode _

derive instance newtypeSourceCode :: Newtype SourceCode _

--------------------------------------------------------------------------------

_SourceCode :: Iso' SourceCode String
_SourceCode = _Newtype

--------------------------------------------------------------------------------

data CompilationError
  = RawError String
  | CompilationError
      { filename :: String
      , row :: Int
      , column :: Int
      , text :: Array String
      }

instance encodeJsonCompilationError :: EncodeJson CompilationError where
  encodeJson = defer \_ -> case _ of
    RawError a -> E.encodeTagged "RawError" a E.value
    CompilationError { filename, row, column, text } -> encodeJson
      { tag: "CompilationError"
      , filename: flip E.encode filename E.value
      , row: flip E.encode row E.value
      , column: flip E.encode column E.value
      , text: flip E.encode text E.value
      }

instance decodeJsonCompilationError :: DecodeJson CompilationError where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "CompilationError"
    $ Map.fromFoldable
        [ "RawError" /\ D.content (RawError <$> D.value)
        , "CompilationError" /\
            ( CompilationError <$> D.object "CompilationError"
                { filename: D.value :: _ String
                , row: D.value :: _ Int
                , column: D.value :: _ Int
                , text: D.value :: _ (Array String)
                }
            )
        ]

derive instance genericCompilationError :: Generic CompilationError _

--------------------------------------------------------------------------------

_RawError :: Prism' CompilationError String
_RawError = prism' RawError case _ of
  (RawError a) -> Just a
  _ -> Nothing

_CompilationError :: Prism' CompilationError
  { filename :: String, row :: Int, column :: Int, text :: Array String }
_CompilationError = prism' CompilationError case _ of
  (CompilationError a) -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

data InterpreterError
  = CompilationErrors (Array CompilationError)
  | TimeoutError String

instance encodeJsonInterpreterError :: EncodeJson InterpreterError where
  encodeJson = defer \_ -> case _ of
    CompilationErrors a -> E.encodeTagged "CompilationErrors" a E.value
    TimeoutError a -> E.encodeTagged "TimeoutError" a E.value

instance decodeJsonInterpreterError :: DecodeJson InterpreterError where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "InterpreterError"
    $ Map.fromFoldable
        [ "CompilationErrors" /\ D.content (CompilationErrors <$> D.value)
        , "TimeoutError" /\ D.content (TimeoutError <$> D.value)
        ]

derive instance genericInterpreterError :: Generic InterpreterError _

--------------------------------------------------------------------------------

_CompilationErrors :: Prism' InterpreterError (Array CompilationError)
_CompilationErrors = prism' CompilationErrors case _ of
  (CompilationErrors a) -> Just a
  _ -> Nothing

_TimeoutError :: Prism' InterpreterError String
_TimeoutError = prism' TimeoutError case _ of
  (TimeoutError a) -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

newtype Warning = Warning String

instance encodeJsonWarning :: EncodeJson Warning where
  encodeJson = defer \_ -> E.encode $ unwrap >$< E.value

instance decodeJsonWarning :: DecodeJson Warning where
  decodeJson = defer \_ -> D.decode $ (Warning <$> D.value)

derive instance genericWarning :: Generic Warning _

derive instance newtypeWarning :: Newtype Warning _

--------------------------------------------------------------------------------

_Warning :: Iso' Warning String
_Warning = _Newtype

--------------------------------------------------------------------------------

newtype InterpreterResult a = InterpreterResult
  { warnings :: Array Warning
  , result :: a
  }

instance encodeJsonInterpreterResult ::
  ( EncodeJson a
  ) =>
  EncodeJson (InterpreterResult a) where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { warnings: E.value :: _ (Array Warning)
        , result: E.value :: _ a
        }
    )

instance decodeJsonInterpreterResult ::
  ( DecodeJson a
  ) =>
  DecodeJson (InterpreterResult a) where
  decodeJson = defer \_ -> D.decode $
    ( InterpreterResult <$> D.record "InterpreterResult"
        { warnings: D.value :: _ (Array Warning)
        , result: D.value :: _ a
        }
    )

derive instance genericInterpreterResult :: Generic (InterpreterResult a) _

derive instance newtypeInterpreterResult :: Newtype (InterpreterResult a) _

--------------------------------------------------------------------------------

_InterpreterResult
  :: forall a
   . Iso' (InterpreterResult a) { warnings :: Array Warning, result :: a }
_InterpreterResult = _Newtype