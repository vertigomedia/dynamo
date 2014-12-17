{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.DeleteItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.DeleteItem
    ( -- * Types
      DeleteItem (..)
    ) where

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Typeable

import           Web.AWS.DynamoDB.Client 
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.Util   ( toText )

------------------------------------------------------------------------------
-- | Types
data DeleteItem = DeleteItem {
    deleteItemKey         :: [Item] 
  , deleteItemTableName   :: Text
  , deleteItemReturnValue :: ReturnValue
  } deriving (Show, Typeable)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `DeleteItem`
instance ToJSON DeleteItem where
  toJSON DeleteItem{..} =
    object [
        "Key"          .= let x = map (\(Item k t v) -> k .= object [ toText t .= v ]) deleteItemKey
                          in object x
      , "TableName"    .= deleteItemTableName                  
      , "ReturnValues" .= deleteItemReturnValue
      ]

------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction DeleteItem

