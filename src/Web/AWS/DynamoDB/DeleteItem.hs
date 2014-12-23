{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE OverloadedStrings     #-}
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

import           Control.Applicative (pure)
import           Data.Aeson
import           Data.Text    (Text)
import           Data.Typeable

import           Web.AWS.DynamoDB.Client 
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.Util   ( toText )

------------------------------------------------------------------------------
-- | Types
data DeleteItem = DeleteItem {
    deleteItemKey         :: PrimaryKey
  , deleteItemTableName   :: Text
--, deleteItemReturnValue :: ReturnValue -- will default to NONE
  } deriving (Show, Typeable)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `DeleteItem`
instance ToJSON DeleteItem where
  toJSON DeleteItem{..} =
    object [
        "Key"          .= deleteItemKey
      , "TableName"    .= deleteItemTableName                  
      ]

------------------------------------------------------------------------------
-- | Delete item response
data DeleteItemResponse = DeleteItemResponse {
        
      } deriving (Show)

------------------------------------------------------------------------------
-- | ToJSON Delete Item Response
instance FromJSON DeleteItemResponse where
   parseJSON (Object o) = pure DeleteItemResponse

------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction DeleteItem DeleteItemResponse

