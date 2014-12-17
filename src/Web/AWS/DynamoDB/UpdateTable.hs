{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.UpdateTable
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.UpdateTable
    ( -- * Types
      UpdateTable (..)
    ) where

import           Data.Aeson
import           Data.Text      ( Text )
import           Data.Typeable  ( Typeable )

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Update Provisioned Throughput on Table
data UpdateTable = UpdateTable {
     updateTableName                 :: Text
   , updateTableProvisionedThrouhput :: Throughput 
  } deriving (Show, Typeable)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `UpdateTable`
instance ToJSON UpdateTable where
  toJSON UpdateTable{..} =
    object [ "TableName" .= updateTableName
           , "ProvisionedThroughput" .= updateTableProvisionedThrouhput
           ]

------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction UpdateTable
