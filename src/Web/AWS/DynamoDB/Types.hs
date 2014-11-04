{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- |
-- Module      : Web.AWS.DynamoDB.Types
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX

module Web.AWS.DynamoDB.Types where

import           Control.Applicative (pure)
import           Data.Text    (Text,pack)
import           Data.Char
import           Data.Aeson

import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Web.AWS.DynamoDB.Helpers

------------------------------------------------------------------------------
-- | Dyna-Monad :P
type DynamoDB = forall a . FromJSON a => EitherT String (ReaderT String IO) a

------------------------------------------------------------------------------
-- | DynamoDB Types: 
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html#DataModel.DataTypes>
data DynamoType =
    S    -- ^ String Type 
  | N    -- ^ Number Type
  | B    -- ^ Binary Type
  | BOOL -- ^ Boolean Type
  | NULL -- ^ Null Type
  | SS   -- ^ String set
  | NS   -- ^ Number set
  | BS   -- ^ Binary set
  | L    -- ^ List
  | M    -- ^ Map
  deriving (Show, Eq)

instance ToJSON DynamoType where
  toJSON = String . toText

------------------------------------------------------------------------------
-- | Attribute Defintions
data AttributeDefinitions = AttributeDefinitions {
      attributeName :: Text        -- ^ A name for the attribute, Minimum length of 1. Maximum length of 255, Required 
    , attributeType :: DynamoType  -- ^ Required, valid values : S | N | B
  } deriving (Show, Eq)

instance ToJSON AttributeDefinitions where
  toJSON AttributeDefinitions{..} =
    object [
        "AttributeName" .= attributeName
      , "AttributeType" .= attributeType
      ]

------------------------------------------------------------------------------
-- | Key Type
data KeyType =
    Hash
  | Range
  deriving (Show, Eq)

instance ToJSON KeyType where
  toJSON = String . pack . map toUpper . show

------------------------------------------------------------------------------
-- | Key Schema
data KeySchema = KeySchema {
      keyAttributeName :: Text    -- ^ Required, Minimum length of 1. Maximum length of 255.
    , keyType          :: KeyType -- ^ Required, HASH or RANGE
  } deriving (Show, Eq)

instance ToJSON KeySchema where
  toJSON KeySchema{..} = object [ "AttributeName" .= keyAttributeName
                                , "KeyType" .= keyType
                                ]

------------------------------------------------------------------------------
-- | Provisioned ThroughPut
-- Represents the provisioned throughput settings for a specified
-- table or index. The settings can be modified using the UpdateTable operation.
data Throughput = Throughput {
     readCapacityUnits :: Int -- ^ Required, Long, The maximum number of strongly consistent reads consumed per second before DynamoDB returns a ThrottlingException
   , writeCapacityUnits :: Int -- ^ Required, Long, The maximum number of writes consumed per second before DynamoDB returns a ThrottlingException
  } deriving (Show, Eq)

instance ToJSON Throughput where
  toJSON Throughput{..} =
    object [ "ReadCapacityUnits" .= readCapacityUnits
           , "WriteCapacityUnits" .= writeCapacityUnits
           ]

------------------------------------------------------------------------------
-- | Item for insertion or retrieval
type ItemName = Text
type ItemValue = Text

data Item = Item ItemName DynamoType ItemValue
data Capacity = INDEXES | TOTAL | NONE deriving (Show)

-- ------------------------------------------------------------------------------
-- -- | Batch Get Item
-- data BatchGetItem = BatchGetItem {
--      batchGetItemRequestItems :: [RequestItem] -- ^ Required
--    , returnConsumedCapacity :: Bool -- ^ Not Required, -- Valid Values, INDEXES, TOTAL, NONE
--   } deriving (Show, Eq)

-- data RequestItem = RequestItem
--     { requestItemTableName :: Text
--     , requestItemKeys      :: [Item]
--     , requestItemConsistentRead :: Bool -- ^ Not Required, default False
--     } deriving (Show, Eq)

-- instance ToJSON RequestItem where
--   toJSON RequestItem{..} = object [ "RequestItems" .=   ]



