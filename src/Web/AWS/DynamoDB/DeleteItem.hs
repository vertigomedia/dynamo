{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.AWS.DynamoDB.DeleteItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.DeleteItem where

import           Data.Aeson
import           Data.Text    (Text)

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.Helpers

------------------------------------------------------------------------------
-- | Delete Item
deleteItem
  :: (Show a, FromJSON a)
  => DeleteItem
  -> IO (Either DynamoError a)
deleteItem = callDynamo "DeleteItem"

------------------------------------------------------------------------------
-- | Types
data DeleteItem = DeleteItem {
    deleteItemKey       :: [Item] 
  , deleteItemTableName :: Text
  , deleteItemReturnValue :: ReturnValue
  } deriving (Show)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `DeleteItem`
instance ToJSON DeleteItem where
  toJSON DeleteItem{..} =
    object [
        "Key" .= let x = map (\(Item k t v) -> k .= object [ toText t .= v ]) deleteItemKey
                 in object x
      , "TableName" .= deleteItemTableName                  
      , "ReturnValues" .= deleteItemReturnValue
      ]

