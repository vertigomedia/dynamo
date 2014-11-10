{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module      : Web.AWS.DynamoDB.GetItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.GetItem where

import           Control.Applicative
import           Data.Aeson
import           Data.Text           (Text)

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Helpers
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make Request
getItem :: FromJSON a => GetItem -> IO (Either DynamoError a)
getItem = callDynamo "GetItem" 

------------------------------------------------------------------------------
-- | `GetItem` object
data GetItem = GetItem {
    getItemKey       :: [Item] 
  , getItemTableName :: Text
  } deriving (Show)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `GetItem`
instance ToJSON GetItem where
  toJSON GetItem{..} =
    object [
        "Key" .= let x = map (\(Item k t v) -> k .= object [ toText t .= v ]) getItemKey
                 in object x
      , "TableName" .= getItemTableName                  
      ]

