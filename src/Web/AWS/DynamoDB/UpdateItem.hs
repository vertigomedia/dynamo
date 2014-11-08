{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.AWS.DynamoDB.UpdateItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.UpdateItem where

import           Data.Aeson 
import           Data.Text    (Text)

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.Helpers

------------------------------------------------------------------------------
-- | Make Request
updateItem :: FromJSON a => UpdateItem -> IO (Either DynamoError a)
updateItem = callDynamo "UpdateItem" 

------------------------------------------------------------------------------
-- | Default request method for `UpdateItem`
updateItemDefault :: Text -> [Item] -> UpdateItem 
updateItemDefault name keys =
  UpdateItem name keys
  Nothing Nothing Nothing
  Nothing Nothing Nothing
  Nothing 

------------------------------------------------------------------------------
-- | Types
data UpdateItem = UpdateItem {
      updateItemTableName                   :: Text   -- ^ Required
    , updateItemKeys                        :: [Item] -- ^ Required
    , updateItemConditionExpression         :: Maybe Text
    , updateItemExpressionAttributeNames    :: Maybe Text
    , updateItemExpressionAttributeValues   :: Maybe [Item]
    , updateItemReturnConsumedCapacity      :: Maybe Text
    , updateItemReturnItemCollectionMetrics :: Maybe Text
    , updateItemReturnValues                :: Maybe ReturnValues
    , updateItemUpdateExpression            :: Maybe Text
  } 

------------------------------------------------------------------------------
-- | `ReturnValues` object
data ReturnValues =
  NONE | ALL_OLD | UPDATED_OLD | ALL_NEW | UPDATED_NEW deriving (Show)

------------------------------------------------------------------------------
-- | `ToJSON` instances for `ReturnValues` object
instance ToJSON ReturnValues where
  toJSON = String . toText

------------------------------------------------------------------------------
-- | `ToJSON` instances for `UpdateItem` object
instance ToJSON UpdateItem where
  toJSON UpdateItem{..} =
    object [ "TableName" .= updateItemTableName
           , "Key" .= let x = map (\(Item k t v) -> k .= object [ toText t .= v ]) updateItemKeys
                      in object x
           , "UpdateExpression" .= updateItemUpdateExpression
           , "ConditionExpression" .= updateItemConditionExpression
           , "ExpressionAttributeValues" .=
               case updateItemExpressionAttributeValues of
                 Nothing -> Nothing
                 Just xs -> let x = map (\(Item k t v) -> k .= object [ toText t .= v ]) xs
                            in Just $ object x
           , "ReturnValues" .= updateItemReturnValues
           ]
