{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Module      : Web.AWS.DynamoDB.PutItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.PutItem
       ( -- * API
          putItem
        , putItemDefault
          -- * Types
        , PutItem (..)
       ) where

import           Data.Aeson
import           Data.Text    (Text)

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.Helpers

------------------------------------------------------------------------------
-- | Make Request
putItem :: (Show a, FromJSON a) => PutItem -> IO (Either DynamoError a)
putItem = callDynamo "PutItem" 

------------------------------------------------------------------------------
-- | Default method for making a `PutItem` Request
putItemDefault :: (Show a, FromJSON a) => Text -> [Item] -> IO (Either DynamoError a)
putItemDefault name items = do
  print items
  print $ toJSON $ PutItem items name Nothing Nothing Nothing Nothing Nothing Nothing 
  callDynamo "PutItem" $
    PutItem items name Nothing Nothing Nothing Nothing Nothing Nothing 
       
------------------------------------------------------------------------------
-- | `PutItem` object
-- <<http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_PutItem.html>>
data PutItem = PutItem {
     putItems                           :: [Item]     -- ^ Required 
   , putItemTableName                   :: Text       -- ^ Required 
   , putItemConditionExpression         :: Maybe Text -- ^ Not Required
   , putItemExpressionAttributeNames    :: Maybe Text -- ^ Not Required 
   , putItemExpressionAttributeValues   :: Maybe Text -- ^ Not Required 
   , putItemReturnConsumedCapacity      :: Maybe Text -- ^ Not Required 
   , putItemReturnItemCollectionMetrics :: Maybe Text -- ^ Not Required 
   , putItemReturnValue                 :: Maybe ReturnValue -- ^ Not Required 
  } deriving (Show)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `PutItem` object
instance ToJSON PutItem where
  toJSON PutItem{..} =
    object [  "Item" .= let x = map (\(Item k t v) -> k .= object [ toText t .= v ]) putItems
                        in object x
           ,  "TableName" .= putItemTableName
           ,  "ReturnValues" .= putItemReturnValue
           ]

------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction PutItem
