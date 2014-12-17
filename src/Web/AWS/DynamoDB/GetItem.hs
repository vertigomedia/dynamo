{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.GetItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.GetItem where

import           Data.Aeson
import           Data.Text           (Text)
import           Data.Typeable

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Util
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | `GetItem` object
data GetItem = GetItem {
    getItemKey       :: [Item] 
  , getItemTableName :: Text
  } deriving (Show, Typeable)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `GetItem`
instance ToJSON GetItem where
  toJSON GetItem{..} =
    object [
        "Key" .= let x = map (\(Item k t v) -> k .= object [ toText t .= v ]) getItemKey
                 in object x
      , "TableName" .= getItemTableName                  
      ]

------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction GetItem


