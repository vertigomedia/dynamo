{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import Control.Applicative
import Data.Aeson
import Data.Text (Text)
import Data.Typeable

import Web.AWS.DynamoDB.Client
import Web.AWS.DynamoDB.Util
import Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | `GetItem` object
data GetItem = GetItem {
    getItemKey       :: PrimaryKey
  , getItemTableName :: Text
  } deriving (Show, Typeable)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `GetItem`
instance ToJSON GetItem where
  toJSON GetItem{..} =
    object [
        "Key"       .= getItemKey
      , "TableName" .= getItemTableName                  
      ]

------------------------------------------------------------------------------
-- | Get Item Response
data GetItemResponse = GetItemResponse {
        getItemResponse :: Maybe Item
    } deriving (Show)

------------------------------------------------------------------------------
-- | FromJSON for GetItemResponse
instance FromJSON GetItemResponse where
   parseJSON (Object o) =
       GetItemResponse <$> o .:? "Item"

------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction GetItem GetItemResponse


