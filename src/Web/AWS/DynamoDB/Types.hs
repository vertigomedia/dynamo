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
-- | Run Dynamo
--dynamo config requests = runReaderT config $ runEitherT requests

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
data Capacity = INDEXES | TOTAL  -- NONE
                deriving (Show)

------------------------------------------------------------------------------
-- | Select on Query's
-- If not specified DynamoDB will default to ALL_Attributes
data Select =
    AllAttributes -- ^ Returns all of the item attributes from the specified table or index
  | AllProjectedAttributes -- ^ Only for querying an Index
  | Count -- ^ Returns the number of matching items rather than the matching items themselves
  | SpecificAttributes -- ^ Returns only the attributes listed in 'AttributesToGet'

instance ToJSON Select where
  toJSON AllAttributes = String "ALL_ATTRIBUTES"
  toJSON AllProjectedAttributes = String "ALL_PROJECTED_ATTRIBUTES"
  toJSON SpecificAttributes = String "SPECIFIC_ATTRIBUTES"
  toJSON Count = String "COUNT"


data ComparisonOperator =
  GT | LT | EQ | LE | GE | BEGINS_WITH | BETWEEN
  deriving (Show)

instance ToJSON ComparisonOperator where
  toJSON = String . toText 
