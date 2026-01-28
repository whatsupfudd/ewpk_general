{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Wapp.AppDef where

import qualified Data.ByteString.Lazy as Lbs
import qualified Data.Map as Mp
import Data.Text (Text)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.:), (.:?), (.!=), parseIndexedJSON, withText, genericToJSON)

import qualified Hasql.Pool as Hp

import qualified Wapp.Utils.Json as Uj


type NativeFctArgs = (Value, Maybe Text)
type NativeLibFunction = Hp.Pool -> NativeFctArgs -> IO (Either String Lbs.ByteString)
type NativeLibMap = Mp.Map Text (Mp.Map Text NativeLibFunction)


data RequestParams = RequestParams {
    hxParamsRP :: Maybe Value
    , formFieldsRP :: Maybe Value
  }
  deriving (Show, Generic)

instance FromJSON RequestParams where
  parseJSON (Object obj) = RequestParams <$> obj .:? "jsonParams" <*> obj .:? "formFields"
  parseJSON _ = fail "Expected JSON Object for RequestParams"

instance ToJSON RequestParams where
  toJSON = genericToJSON Uj.cutFieldP2

data FunctionReply =
  BasicFR (Lbs.ByteString, Maybe Text)
  | AppendChildFR (Lbs.ByteString, Maybe Text)
  | AfterEndFR (Lbs.ByteString, Maybe Text)
 deriving (Generic, Show)
