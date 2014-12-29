{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
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
       , updateItemDefault
       ) where

import Data.Maybe
import Control.Applicative
import Data.Aeson
import Data.Text ( Text )
import Data.Typeable ( Typeable )

import Web.AWS.DynamoDB.Client
import Web.AWS.DynamoDB.Types
import Web.AWS.DynamoDB.Util

------------------------------------------------------------------------------
-- | Default Update Expression
updateItemDefault
    :: Text       -- ^ Table Name
    -> PrimaryKey -- ^ Primary Key
    -> Text       -- ^ Update Item Expression
    -> Item       -- ^ Values to set
    -> UpdateItem
updateItemDefault name key expr item =
    UpdateItem name key expr item
      Nothing Nothing Nothing Nothing Nothing 
  
------------------------------------------------------------------------------
-- | Types
data UpdateItem = UpdateItem {
      updateItemTableName                   :: Text       -- ^ Required
    , updateItemKeys                        :: PrimaryKey -- ^ Required
    , updateItemUpdateExpression            :: Text
    , updateItemExpressionAttributeValues   :: Item
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
    object $ [ "TableName"                 .= updateItemTableName
           , "Key"                       .= updateItemKeys
           , "UpdateExpression"          .= updateItemUpdateExpression
           , "ExpressionAttributeValues" .= updateItemExpressionAttributeValues
           , "ReturnValues"              .= updateItemReturnValues
           ] ++ [ "ConditionExpression"       .= updateItemConditionExpression
                | isJust updateItemConditionExpression
                ]

------------------------------------------------------------------------------
-- | UpdateItem Response
data UpdateItemResponse = UpdateItemResponse {
      uirAttrs    :: Maybe Item
    } deriving (Show)

------------------------------------------------------------------------------
-- | FromJSON UpdateItem
instance FromJSON UpdateItemResponse where
   parseJSON (Object o) =
       UpdateItemResponse <$> o .:? "Attributes"

------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction UpdateItem UpdateItemResponse
