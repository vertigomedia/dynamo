{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.Query
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.Query
    ( -- * Types
      defaultQuery
    , Query          (..)
    , Condition      (..)
    , AttributeValue (..)
    ) where

import Control.Applicative
import Data.Aeson
import Data.Text ( Text )
import Data.Typeable ( Typeable )
import Data.Vector
import qualified Data.Vector as V
import Prelude hiding ( Ordering(..) )

import Web.AWS.DynamoDB.Client
import Web.AWS.DynamoDB.Util
import Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Query helper
defaultQuery :: Text -> [Condition] -> Query
defaultQuery t cs = Query t cs Nothing Nothing Nothing
                               Nothing Nothing Nothing
                               Nothing Nothing Nothing
                               Nothing Nothing 

------------------------------------------------------------------------------
-- | You can query a table, primary key
data Query = Query {
    queryTableName                 :: Text           -- ^ Required
  , queryKeyConditions             :: [Condition]    -- ^ Required
  , queryConsistentRead            :: Maybe Bool     -- ^ Not Required, defaults to False (eventually consistent)
  , queryLimit                     :: Maybe Int      -- ^ Not Required
  , queryExpressionAttributeNames  :: Maybe Text     -- ^ Not Required
  , queryExpressionAttributeValues :: Maybe Text     -- ^ Not Required
  , queryFilterExpression          :: Maybe Text     -- ^ Not Required
  , queryProjectionExpression      :: Maybe Text     -- ^ Not Required
  , queryConditionExpression       :: Maybe Text     -- ^ Not Required
  , queryIndexName                 :: Maybe Text     -- ^ Not Required
  , queryReturnedConsumedCapacity  :: Maybe Capacity -- ^ Not Required
  , queryScanIndexForward          :: Maybe Bool     -- ^ Not Required
  , querySelect                    :: Maybe Select   -- ^ Not Required
  } deriving (Show, Typeable)

------------------------------------------------------------------------------
-- | `KeyCondition` Type
data KeyCondition =
  KeyCondition [Condition]
  deriving (Show)

------------------------------------------------------------------------------
-- | `Condition` Type
data Condition =
  Condition Name [AttributeValue] ComparisonOperator
  deriving (Show)

------------------------------------------------------------------------------
-- | `AttributeValue` Type
data AttributeValue =
  AttributeValue DynamoType Text
  deriving (Show)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `AttributeValue` object
instance ToJSON AttributeValue where
  toJSON (AttributeValue dtype ivalue) =
    object [ toText dtype .= ivalue ]

------------------------------------------------------------------------------
-- | `ToJSON` instance for `Query` object
instance ToJSON Query where
  toJSON Query{..} = object [
      "TableName" .= queryTableName
    , "Select" .= querySelect
    , "KeyConditions" .= let x = Prelude.map (\(Condition iname vlist op) ->
                                       iname .= object [
                                         "ComparisonOperator" .= op
                                       , "AttributeValueList" .= vlist
                                       ]) queryKeyConditions
                         in object x
    ]

------------------------------------------------------------------------------
-- | Response to a 'Query' query.
data QueryResponse = QueryResponse {
      qrItems    :: V.Vector Item
    , qrCount    :: Int
    , qrScanned  :: Int
    } deriving (Eq,Show,Read,Ord)

instance FromJSON QueryResponse where
    parseJSON (Object v) = QueryResponse
      <$> v .:?  "Items" .!= V.empty
      <*> v .:  "Count"
      <*> v .:  "ScannedCount"
    parseJSON _ = fail "QueryResponse must be an object." 

------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction Query QueryResponse
