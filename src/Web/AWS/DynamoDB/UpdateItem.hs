{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.AWS.DynamoDB.UpdateItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.UpdateItem
       ( -- * Update Item
         updateItem
       , updateItemDefault
       , UpdateItem (..)
       ) where
       
import           Data.Aeson 
import           Data.Text    (Text)

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.Helpers

------------------------------------------------------------------------------
-- | Make Request
updateItem :: (FromJSON a, Show a) => UpdateItem -> IO (Either DynamoError a)
updateItem = callDynamo "UpdateItem" 

------------------------------------------------------------------------------
-- | Default request method for `UpdateItem`
updateItemDefault :: (FromJSON a, Show a) => Text -> [Item] -> Text -> [Item] -> IO (Either DynamoError a)
updateItemDefault name keys exp avals = updateItem $ 
  UpdateItem name keys exp
  avals Nothing Nothing
  Nothing Nothing Nothing
  
------------------------------------------------------------------------------
-- | Types
data UpdateItem = UpdateItem {
      updateItemTableName                   :: Text   -- ^ Required
    , updateItemKeys                        :: [Item] -- ^ Required
    , updateItemUpdateExpression            :: Text
    , updateItemExpressionAttributeValues   :: [Item]
    , updateItemConditionExpression         :: Maybe Text
    , updateItemExpressionAttributeNames    :: Maybe Text
    , updateItemReturnConsumedCapacity      :: Maybe Text
    , updateItemReturnItemCollectionMetrics :: Maybe Text
    , updateItemReturnValues                :: Maybe ReturnValue
  } 

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
                let x = map (\(Item k t v) -> k .= object [ toText t .= v ]) updateItemExpressionAttributeValues
                in object x
           , "ReturnValues" .= updateItemReturnValues
           ]
