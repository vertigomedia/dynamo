{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.UpdateItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.UpdateItem
       ( -- * Update Item
         UpdateItem (..)
       ) where
       
import           Data.Aeson 
import           Data.Text    (Text)
import           Data.Typeable  ( Typeable )

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.Util 

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
  } deriving (Show, Typeable)

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

------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction UpdateItem
