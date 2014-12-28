{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.PutItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.PutItem
       (  -- * Types
         PutItem (..)
       , defaultPutItem
       ) where

import Control.Applicative
import Data.Aeson
import Data.Text (Text)
import Data.Typeable ( Typeable )

import Web.AWS.DynamoDB.Client
import Web.AWS.DynamoDB.Types
import Web.AWS.DynamoDB.Util

------------------------------------------------------------------------------
-- | Default Put Item Object
defaultPutItem x y = PutItem x y Nothing Nothing Nothing
                                 Nothing Nothing Nothing

------------------------------------------------------------------------------
-- | `PutItem` object
-- <<http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_PutItem.html>>
data PutItem = PutItem {
     putItems                           :: Item       -- ^ Required 
   , putItemTableName                   :: Text       -- ^ Required 
   , putItemConditionExpression         :: Maybe Text -- ^ Not Required
   , putItemExpressionAttributeNames    :: Maybe Text -- ^ Not Required 
   , putItemExpressionAttributeValues   :: Maybe Text -- ^ Not Required 
   , putItemReturnConsumedCapacity      :: Maybe Text -- ^ Not Required 
   , putItemReturnItemCollectionMetrics :: Maybe Text -- ^ Not Required 
   , putItemReturnValue                 :: Maybe ReturnValue -- ^ Not Required 
  } deriving (Show, Typeable)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `PutItem` object
instance ToJSON PutItem where
  toJSON PutItem{..} =
    object [  "Item" .= putItems
           ,  "TableName" .= putItemTableName
           ,  "ReturnValues" .= NONE
           ]

------------------------------------------------------------------------------
-- | PutItem Response type
data PutItemResponse = PutItemResponse {
      putItemAttributes :: Maybe Item
    } deriving (Show, Eq, Typeable)

------------------------------------------------------------------------------
-- | PutItem Responses are always ignored since ReturnValues is always NONE
instance FromJSON PutItemResponse where
   parseJSON (Object o) = PutItemResponse <$> o .:? "Attributes"

------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction PutItem PutItemResponse
